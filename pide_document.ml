(* Pipelining operators. These are defined in SML, not in Ocaml. *)
open Coq_messages

let (|>) x f = f x
let (@>) f g x = g (f x)


type id = int
type version_id = id
type command_id = id
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
type perspective = command_id list

type entries = (command_id * exec_id) list
type node = Node of entries * perspective * overlay
let empty_node = Node ([], [], [])

type version = Version of (string * node) list
let empty_version = Version []

type state = State of (version_id * version) list * (command_id * string) list

let init_state = State ([(no_id, empty_version)], [])
let global_state = ref init_state
let change_state f = global_state := f !global_state

let define_version (id : version_id) version (State (versions, commands)) =
  let versions' =
    if List.mem_assoc id versions then raise (Failure "Dup")
    else (id, version) :: versions
  in State (versions', commands)


let remove_version (id: version_id) (State (versions, commands)) =
  let versions' =
    if not (List.mem_assoc id versions) then raise (Failure "Does not exist")
    else List.remove_assoc id versions
  in State(versions', commands)

let remove_versions (ids: version_id list) (s: state) =
  List.fold_right (fun (i: version_id) (s': state) -> remove_version i s') ids s

let the_version (State (versions, _)) (id: version_id) =
  List.assoc id versions

let the_command (State (_, commands)) (id: command_id) =
  List.assoc id commands

let parse_id = int_of_string
let print_id = string_of_int
let print_exec_id = Stateid.to_string

let initial_state: Stateid.t ref = ref Stateid.dummy

type node_edit = 
  | Edits of (command_id option * command_id option) list
  | Perspective of (command_id list * overlay)
type edit = string * node_edit

let define_command id (text: string) (State (versions, commands)) =
  let commands' =
    if List.mem_assoc id commands then raise (Failure "Dup")
    else (id, text ) :: commands
  in State (versions, commands')

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


let set_overlay stmq (cid: command_id) (at: exec_id) (ov: overlay) (st: state): exec_id list =
  List.fold_right (function (oid, (command, args)) -> fun acc ->
    if command = "coq_query" then
      match args with 
      | instance :: query_text :: args ->
        (* TODO: use the instance id to report query results. *)
        if oid = cid then
          let eid = Stateid.fresh () in
          query stmq at eid query_text;
          eid :: acc
        else acc
      | _ -> acc
    else acc)
  ov []

let to_exec_list (p: perspective) (execs: (command_id * exec_id list) list): exec_id list =
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
        let (common, (outdated_computation, new_computation)) = chop_common old_entries entries in
        let common_execs = List.fold_right
          (fun (id, exec_id) acc ->
            let id_overlay = set_overlay tasks id exec_id overlay st in
            (* We avoid re-sending a previous assignment here. Probably doesn't
             * matter too much, though, so consider removing if statement.
             *)
            if id_overlay = [] then acc
            else (id, exec_id :: id_overlay) :: acc
          ) common [] in
        let tip = if common = [] then !initial_state else snd (CList.last common) in
        Queue.push (`EditAt tip) tasks;

        let new_computation' = List.map (fun (id, _) -> id, Stateid.fresh ()) new_computation in

        (* TODO: Adding to the queue here is a bit overkill, can be combined with setting the assignment. *)
        ignore(List.fold_left
          (fun curr_tip (cid, exec_id) -> add tasks exec_id curr_tip cid (the_command st cid))
          tip new_computation');
        let command_execs =
          List.map (fun (id, _) -> (id, [])) outdated_computation @
          common_execs @
          List.map (fun (id, exec_id) -> (id, exec_id :: (set_overlay tasks id exec_id overlay st))) new_computation' in
        Queue.push `Observe tasks;
        let updated_node =
          match command_execs with
          | [] -> []
          | _  -> [(name, Node ((common @ new_computation'), perspective, overlay))] in

        let exec_perspective = to_exec_list perspective command_execs in
        Stm.set_perspective exec_perspective;

        (command_execs, updated_node)
      else
        ([], []))
    in
    let command_execs = List.flatten (List.map fst updated) in
    let updated_nodes = List.flatten (List.map snd updated) in
    let state' =
      define_version v_new (List.fold_left put_node new_version updated_nodes) st in
    (command_execs, tasks, state')

let execute stmq task_queue =
  Queue.iter (fun t -> TQueue.push stmq t) task_queue;
  Queue.clear task_queue

let initialize () =
  initial_state := Stm.get_current_state ()
