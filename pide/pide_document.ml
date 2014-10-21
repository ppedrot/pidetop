open Coq_messages
open Coq_output

(* Pipelining operators. These are defined in SML, not in Ocaml. *)
let (|>) x f = f x
let (@>) f g x = g (f x)


type id = int
type version_id = id
type command_id = id
type instance_id = id
type exec_id = Stateid.t

let no_id = 0

let new_id_counter = ref 0
let new_id_lock = Mutex.create ()

let new_id () =
  Mutex.lock new_id_lock;
  let i = !new_id_counter + 1 in
  if (i > 0) then new_id_counter := i;
  Mutex.unlock new_id_lock;
  if (i > 0) then i
  else raise (Failure "Counter overflow")

(* Overlay: print functions on a specific command span. *)
(* The type encodes the command to print on, the query to execute and its 
 * arguments *)
type overlay = (command_id * (string * (string list))) list
(* Routing queries to instances. *)
type routing_table = (exec_id * instance_id) list
type perspective = command_id list

type entries = (command_id * exec_id) list
type node = Node of entries * perspective * overlay
let empty_node = Node ([], [], [])

type version = Version of (string * node) list
let empty_version = Version []

type state =
  State of (version_id * version) list * (command_id * (bool * string)) list *
           routing_table

let init_state = State ([(no_id, empty_version)], [], [])
let global_state = ref init_state
let change_state f = global_state := f !global_state

let set_route (State (v, c, _)) table = State (v, c, table)

let define_version id version (State (versions, commands, routes)) =
  let versions' =
    if List.mem_assoc id versions then raise (Failure "Dup")
    else (id, version) :: versions
  in State (versions', commands, routes) (* TODO... *)


let remove_version (id: version_id) (State (versions, commands, routes)) =
  let versions' =
    if not (List.mem_assoc id versions) then raise (Failure "Does not exist")
    else List.remove_assoc id versions
  in State(versions', commands, routes)

let remove_versions (ids: version_id list) (s: state) =
  List.fold_right (fun (i: version_id) (s': state) -> remove_version i s') ids s

let the_version (State (versions, _, _)) (id: version_id) =
  List.assoc id versions

let the_command (State (_, commands, _)) (id: command_id) : (bool * string) =
  List.assoc id commands

let the_route (State (_, _, routes)) (id: exec_id): instance_id option =
  try Some (List.assoc id routes)
  with Not_found -> None

let parse_id = int_of_string
let print_id = string_of_int
let print_exec_id = Stateid.to_string

let initial_state: Stateid.t ref = ref Stateid.dummy

type node_edit = 
  | Edits of (command_id option * command_id option) list
  | Perspective of (command_id list * overlay)
type edit = string * node_edit

let define_command id (is_ignored: bool) (text: string) (State (versions, commands, routes)) =
  let commands' =
    if List.mem_assoc id commands then raise (Failure "Dup")
    else (id, (is_ignored, text)) :: commands
  in State (versions, commands', routes)

let default_node name nodes = 
  if List.mem_assoc name nodes then nodes
  else (name, empty_node) :: nodes

let update_node name f nodes =
  List.map (function ((name', node) as pair) ->
    if name = name' then (name', f node) else pair) (default_node name nodes)


let insert_here id2 (entries: entries) =
  if List.mem_assoc id2 entries then raise (Failure "Dup")
  else (id2, Stateid.dummy) :: entries

let insert_after hook id2 (entries: entries) =
  match hook with
  | None -> insert_here id2 entries
  | Some id1 -> 
      let rec insert l =
        match l with
        | [] -> raise (Failure "Undefined insert")
        | (x, y) :: rest ->
            if x = id1 then (x, y) :: insert_here id2 rest
            else (x, y) :: insert rest
      in insert entries

let remove_here (entries: entries) =
  match entries with
  | [] -> raise (Failure "Undef")
  | [_] -> []
  | _ :: (x, _) :: rest -> (x, Stateid.dummy) :: rest

let remove_after hook (entries: entries) =
  match hook with
  | None -> remove_here entries
  | Some id1 ->
      let rec remove l =
        match l with
        | [] -> raise (Failure "Undefined remove")
        | (x, y) :: rest -> 
            if x = id1 then (x, y) :: remove_here rest
            else (x, y) :: remove rest
      in remove entries

let edit_node (Node (entries, p, o)) edit =
    Node (
    (match edit with
    | (hook, Some id2) -> insert_after hook id2 entries
    | (hook, None) -> remove_after hook entries), p, o)

let set_perspective (Node (entries, _, _)) perspective overlay =
  Node (entries, perspective, overlay)

let edit_nodes (Version nodes) (name, node_edit) =
  Version 
    (match node_edit with
      | Edits edits ->
          update_node name (fun x -> List.fold_left edit_node x edits) nodes
      | Perspective (commands, overlay) ->
          update_node name (fun n ->
              set_perspective n commands overlay) nodes

      )

  let put_node (Version nodes) (name, node) =
    Version ((name, node) :: List.remove_assoc name nodes)

  let get_node nodes name = 
    try List.assoc name nodes
    with Not_found -> empty_node

  let rec chop_common (entries0 : entries) (entries1: entries) =
    match (entries0, entries1) with
    | (x :: rest0, y :: rest1) when x = y -> 
        let (common', rest') = chop_common rest0 rest1 in
        (x :: common', rest')
    | _ -> ([], (entries0, entries1))


  let add task_queue exec_id tip edit_id text =
    Queue.push (`Add (lazy (
    let position = Position.id_only (print_exec_id exec_id) in
    Coq_output.status position [Xml_datatype.Element ("running", [], [])];
    try
      ignore(Stm.add ~newtip:exec_id ~ontop:tip true edit_id text);
      let ast = Stm.print_ast exec_id in
      Coq_output.report (Position.id_only (Stateid.to_string exec_id)) [ast];
      Some exec_id
    with e when Errors.noncritical e -> None))) task_queue;
    exec_id

  let query task_queue at query_id text =
    Queue.push (`Query (lazy (
      let position = Position.id_only (print_exec_id query_id) in
      Coq_output.status position [Xml_datatype.Element ("running", [], [])]; (* TODO: potential for refactoring with the add. *)
      Stm.query ~at:at ~report_with:query_id text))) task_queue


  let set_overlay stmq cid at ov st: exec_id list * routing_table =
    List.fold_right (fun (oid, (command, args)) (acc, rt) ->
      if command = "coq_query" then
        match args with 
        | instance :: query_text :: args ->
          if oid = cid then
            let eid = Stateid.fresh () in
            let iid = int_of_string instance in
            query stmq at eid query_text;
            (eid :: acc, (eid, iid) :: rt)
          else acc, rt
        | _ -> acc, rt
      else acc, rt)
    ov ([], [])

  let to_exec_list p (execs: (command_id * exec_id list) list): exec_id list =
    List.fold_right
      (fun (c: command_id) (acc: exec_id list) ->
        if (List.mem_assoc c execs) then
          match List.assoc c execs with
          | [] -> acc
          | es -> es @ acc
        else acc)
      p
      []

  let update (v_old: version_id) (v_new: version_id) (edits: edit list) (st : state)
    (*(command_id * exec_id list) list * Pide_protocol.task Queue.t * state*) =
    let Version old_nodes as old_version = the_version st v_old in
    let Version new_nodes as new_version = List.fold_left edit_nodes old_version edits in
    let tasks = Queue.create () in
    let updated =
      new_nodes |> List.map (fun (name, Node (entries, perspective, overlay)) ->
        if List.mem_assoc name edits then
          let Node (old_entries, _, _) = get_node old_nodes name in
          let (common, (outdated_computation, new_computation)) = 
            chop_common old_entries entries in
          let (common_execs, routing) = List.fold_right
            (fun (id, exec_id) (acc, acc_route) ->
              let id_overlay, routes = set_overlay tasks id exec_id overlay st in
              if id_overlay = [] then (acc, acc_route)
              else (id, exec_id :: id_overlay) :: acc, routes @ acc_route
              ) common ([], []) in
          let tip = if common = [] then !initial_state else snd (CList.last common) in
          Queue.push (`EditAt tip) tasks;

          let new_computation' = List.map 
            (fun (id, _) -> id, Stateid.fresh ()) 
            new_computation in
    
          ignore(List.fold_left
            (fun curr_tip (cid, exec_id) -> 
              let (is_ignored, cmd_text) = the_command st cid in
              if not is_ignored then add tasks exec_id curr_tip cid cmd_text
              else curr_tip
            )
          tip new_computation');
        let (overlay_execs, routing') =
          List.fold_right (fun (id, exec_id) (acc, acc_route) ->
            let id_overlay, routes = set_overlay tasks id exec_id overlay st in
            (id, exec_id :: id_overlay):: acc, routes @ acc_route) 
          new_computation' ([], routing) in
        let command_execs =
          List.map (fun (id, _) -> (id, [])) outdated_computation @
          common_execs @ overlay_execs in
        Queue.push `Observe tasks;
        let updated_node =
          match command_execs with
          | [] -> []
          | _  -> [(name, 
                    Node ((common @ new_computation'), perspective, overlay))]
        in
        Stm.set_perspective (to_exec_list perspective command_execs);
        (command_execs, updated_node, routing')
      else
        ([], [], []))
    in
    let command_execs = List.flatten (List.map (fun (e, _, _) -> e) updated) in
    let updated_nodes = List.flatten (List.map (fun (_, n, _) -> n) updated) in
    let updated_route = List.flatten (List.map (fun (_, _, r) -> r) updated) in
    let st' = set_route st updated_route in
    let state' = define_version v_new
      (List.fold_left put_node new_version updated_nodes) st' in
    (command_execs, tasks, state')

let execute stmq task_queue =
  Queue.iter (fun t -> TQueue.push stmq t) task_queue;
  Queue.clear task_queue

let already_printed = ref Stateid.Set.empty

let position_of_loc loc =
  if Loc.is_ghost loc then Position.id_only
  else let i, j = Loc.unloc loc in Position.make_id (i+1) (j+1)

let goal_printer exec_id exec_id_str = function
  | Feedback.StructuredGoals (loc, goals) ->
      let pos = position_of_loc loc exec_id_str in
      report pos [goals];
      true

  | Feedback.Goals (loc,goalstate) ->
      (if Stateid.Set.mem exec_id !already_printed then ()
       else (
         already_printed := Stateid.Set.add exec_id !already_printed;
         let pos = position_of_loc loc exec_id_str in
         let source = Properties.put ("source", "goal") Properties.empty in
         writeln pos ~props:source goalstate));
      true
  | _ -> false

let error_printer exec_id exec_id_str = function
  | Feedback.ErrorMsg (loc, txt) ->
    let pos = position_of_loc loc exec_id_str in
    Coq_output.status pos [Xml_datatype.Element ("finished", [], [])];
    error_msg pos txt;
    true
  | _ -> false

let rest_printer exec_id exec_id_str = function
  | Feedback.Processed ->
      let position = Position.id_only exec_id_str in
      Coq_output.status position [Xml_datatype.Element ("finished", [], [])];
      true
    | Feedback.Message { Feedback.message_content = s } ->
        let position = Position.id_only exec_id_str in
        (match the_route !global_state exec_id with
        | None ->
            let source = Properties.put ("source", "query") Properties.empty in
            writeln position ~props:source s
        | Some i -> result position i s
        ); 
        true
    | _ -> false

type entry_location =
  | Local of string
  | ExtFile of string

module S = struct type t = string * string * string let compare = compare end
module M = CMap.Make(S)
let def_map : (Loc.t * entry_location) M.t ref = ref (M.empty)
let lookup m k cont =
  try cont (M.find k !m); true
  with Not_found -> false

(* TODO: Basically the same as in tools/coqdoc/index.ml; except no refs. *)
let load_globs (f: string) (id: string) =
  let bare_name = Filename.chop_extension f in
  let glob_name = bare_name ^ ".glob" in
  let v_name = bare_name ^ ".v" in
  try
  let c = open_in glob_name in
    try
    while true do
      let s = input_line c in
      try Scanf.sscanf s "%s %d:%d %s %s"
        (fun ty loc1 loc2 secpath name ->
           let loc = Loc.make_loc (loc1, loc2) in
           (* TODO: Store interpreted type, not raw ty string *)
           let ty = if ty = "prf" then "thm" else ty in
           def_map := M.add (ty, name,  secpath) (loc, ExtFile v_name) !def_map)
      with Scanf.Scan_failure _ | End_of_file -> ()
    done
    with End_of_file ->
      close_in c
  with Sys_error s ->
    warning_msg (Position.id_only id)
      ("Warning: " ^ glob_name ^
       ": No such file or directory (links will not be available)")

let glob_printer exec_id exec_id_str = function
  | Feedback.FileLoaded(dirname, filename) ->
      load_globs filename exec_id_str; true
  | Feedback.GlobDef (loc, name, secpath, ty) ->
      (* TODO: This works for proofs, but will break on other 'synonyms' *)
      let ty = if ty = "prf" then "thm" else  ty in
      def_map := M.add (ty, name, secpath) (loc, Local exec_id_str) !def_map; true
  | Feedback.GlobRef (loc, _fp, mp, name, ty) ->
      let _li, _lj = Loc.unloc loc in
      lookup def_map (ty, name, mp) (fun (dest, dest_id) ->
        let (i, j) = Loc.unloc loc in
        let (dest_i, dest_j) = Loc.unloc dest in
        let location =
          match dest_id with
          | Local dest_id' -> "def_id", dest_id'
          | ExtFile fname  -> "def_file", fname
          in
        let report_body = location :: ["id", exec_id_str;
                               "offset", (string_of_int (i + 1));
                               "end_offset", (string_of_int (j + 1));

                               "def_offset", (string_of_int (dest_i + 1));
                               "def_end_offset", (string_of_int (dest_j + 1));
                               "name", name;
                               "kind", ty] in

        let position = position_of_loc loc exec_id_str in
        Coq_output.report position [Xml_datatype.Element ("entity", report_body, [])]
      )
  | _ -> false

let dependency_printer exec_id exec_id_str = function
  | Feedback.FileDependency (from, depends_on) ->
      (*TODO: Report to Scala, process there. *)
      true
  | _ -> false

let lift f {Feedback.id = id; Feedback.content} =
  match id with
  | Feedback.State exec_id ->
      f exec_id (print_exec_id exec_id) content
  | _ -> false

let (>>=) f1 f2 feedback =
  if f1 feedback then true
  else lift f2 feedback



let init_printers () =
  Pp.set_feeder (fun f ->
    ignore(((lift error_printer) >>= goal_printer >>=
            glob_printer >>= dependency_printer >>= rest_printer) f));
  Pp.log_via_feedback ()

let initialize () =
  init_printers ();
  initial_state := Stm.get_current_state ()