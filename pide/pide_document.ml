open Coq_messages
open Coq_output
open Coq_markup

type transaction_outcome =
  [ `NotCommitted
  | `CommittedUpTo of int
  | `FullyCommitted ]

let string_of_outcome o = match o with
  | `NotCommitted -> "`NotCommitted"
  | `CommittedUpTo i -> "`CommittedUpTo " ^ (string_of_int i)
  | `FullyCommitted -> "`FullyCommitted"

type task =
  [ `Observe of Stateid.t list
  | `Add of Stateid.t * int * string * Stateid.t ref
  | `EditAt of Stateid.t
  | `Query of Stateid.t * Feedback.route_id * Stateid.t * string
  | `Bless of int * (transaction_outcome ref)]

let string_of_task = function
  | `Observe il ->
    "`Observe [" ^ String.concat "; " (List.map Stateid.to_string il) ^ "]"
  | `Add (s,e,t,_) ->
    "`Add (" ^ Stateid.to_string s ^ ", " ^ string_of_int e ^ ", " ^ t ^ ", _)"
  | `EditAt id ->
    "`EditAt (" ^ Stateid.to_string id ^ ")"
  | `Query (id1,route,id2,s) ->
    "`Query (" ^ Stateid.to_string id1 ^ ", " ^
                   string_of_int route ^ ", " ^
                 Stateid.to_string id2 ^ ", " ^ s ^ ")"
  | `Bless (id, good) ->
    "`Bless (" ^ string_of_int id ^ ", _)"

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
type perspective = command_id list

type entries = (command_id * exec_id * exec_id ref) list
type node = Node of entries * perspective * overlay
let empty_node = Node ([], [], [])

type version = Version of (transaction_outcome ref * (string * node) list)
let empty_version = Version (ref `FullyCommitted, [])

type state =
  State of (version_id * version) list * (command_id * (bool * string)) list

let init_state = State ([(no_id, empty_version)], [])
let global_state = ref init_state
let change_state f = global_state := f !global_state

let define_version id version (State (versions, commands)) =
  let versions' =
    if List.mem_assoc id versions then raise (Failure "Dup")
    else (id, version) :: versions
  in State (versions', commands) (* TODO... *)

let the_last_good_version (State (versions, _)) (id: version_id) =
  CList.find_map (fun (id', Version (outcome, the_stuff)) ->
    if id' >= id && !outcome <> `NotCommitted then
      Some (id', Version (outcome, the_stuff))
    else None) versions
let the_version (State (versions, _)) (id: version_id) =
  List.assoc id versions

(* XXX: We need some way of garbage-collecting old good versions *)
let remove_version (id: version_id) (State (versions, commands)) =
  let last_good = the_last_good_version (State(versions, commands)) id in
  let matched_version =
    (try
      Some (List.assoc id versions)
    with
      | Not_found -> None) in
  let versions' = match matched_version with
    | Some x when (id, x) <> last_good -> List.remove_assoc id versions
    | Some x -> versions
    | None -> raise (Failure "Does not exist")
  in State(versions', commands)

let remove_versions (ids: version_id list) (s: state) =
  List.fold_right (fun (i: version_id) (s': state) -> remove_version i s') ids s

let the_command (State (_, commands)) (id: command_id) : (bool * string) =
  List.assoc id commands

let parse_id = int_of_string
let print_id = string_of_int
let print_exec_id = Stateid.to_string

let initial_state: Stateid.t ref = ref Stateid.dummy

type node_edit = 
  | Edits of (command_id option * command_id option) list
  | Perspective of (command_id list * overlay)
type edit = string * node_edit

let define_command id (is_ignored: bool) (text: string) (State (versions, commands)) =
  let commands' =
    if List.mem_assoc id commands then raise (Failure "Dup")
    else (id, (is_ignored, text)) :: commands
  in State (versions, commands')

let default_node name nodes = 
  if List.mem_assoc name nodes then nodes
  else (name, empty_node) :: nodes

let update_node name f nodes =
  List.map (function ((name', node) as pair) ->
    if name = name' then (name', f node) else pair) (default_node name nodes)


let insert_here id2 (entries: entries) =
  if List.exists (fun (id,_,_) -> id = id2) entries then raise (Failure "Dup")
  else (id2, Stateid.dummy, ref Stateid.dummy) :: entries

let insert_after hook id2 (entries: entries) =
  match hook with
  | None -> insert_here id2 entries
  | Some id1 -> 
      let rec insert l =
        match l with
        | [] -> raise (Failure ("insertion failed: hook " ^
                                (string_of_int id1) ^ " was not found"))
        | (x, y, z) :: rest ->
            if x = id1 then (x, y, z) :: insert_here id2 rest
            else (x, y, z) :: insert rest
      in insert entries

let remove_here (entries: entries) =
  match entries with
  | [] -> raise (Failure "Undef")
  | [_] -> []
  | _ :: (x, _,_) :: rest -> (x, Stateid.dummy, ref Stateid.dummy) :: rest

let remove_after hook (entries: entries) =
  match hook with
  | None -> remove_here entries
  | Some id1 ->
      let rec remove l =
        match l with
        | [] -> raise (Failure ("removal failed: hook " ^
                                (string_of_int id1) ^ " was not found"))
        | (x, y,z) :: rest -> 
            if x = id1 then (x, y,z) :: remove_here rest
            else (x, y,z) :: remove rest
      in remove entries

let edit_node (Node (entries, p, o)) edit =
    Node (
    (match edit with
    | (hook, Some id2) -> insert_after hook id2 entries
    | (hook, None) -> remove_after hook entries), p, o)

let set_perspective (Node (entries, _, _)) perspective overlay =
  Node (entries, perspective, overlay)

let edit_nodes (Version (outcome, nodes)) (name, node_edit) =
  Version 
    (ref `NotCommitted, match node_edit with
      | Edits edits ->
          update_node name (fun x -> List.fold_left edit_node x edits) nodes
      | Perspective (commands, overlay) ->
          update_node name (fun n ->
              set_perspective n commands overlay) nodes
    )

let put_node (Version (outcome, nodes)) (name, node) =
  Version (outcome, (name, node) :: List.remove_assoc name nodes)

let get_node nodes name =
  try List.assoc name nodes
  with Not_found -> empty_node

let rec chop_common (entries0 : entries) (up_to : transaction_outcome) (entries1: entries) =
  match (entries0, entries1) with
  | ((x,_,_ as hd) :: rest0, (y,_,_) :: rest1) when x = y && up_to <> `CommittedUpTo x ->
      let (common', rest') = chop_common rest0 up_to rest1 in
      (hd :: common', rest')
  | _ -> ([], (entries0, entries1))


let log = open_out "/tmp/log"

let add task_queue exec_id tip_exec_id edit_id text =
  Queue.push (`Add (exec_id, edit_id, text, tip_exec_id)) task_queue

let query at route_id query_id text =
  `Query (at,route_id,query_id,text)

let get_queries cid at ov =
  List.fold_right (fun (oid, (command, args)) acc ->
    if command = "coq_query" then
      match args with
      | instance :: query_text :: args ->
        if oid = cid then
          let eid = Stateid.fresh () in
          let iid = int_of_string instance in
          let q = query at iid eid query_text in
          ((eid, q) :: acc)
        else acc
      | _ -> acc
    else acc)
  ov []


let emit_goal at =
    let eid = Stateid.fresh () in
    (* TODO: Factor this differently, hardcoded query... *)
    [eid, query at Feedback.default_route eid "PideFeedbackGoals."]

let set_overlay cid at ov =
  let mandatory = emit_goal at in (* Queries that execute on all states *)
  let queries = get_queries cid at ov in
  mandatory @ queries

let to_exec_list p (execs: (command_id * exec_id list) list): exec_id list =
  List.fold_right
    (fun (c: command_id) (acc: exec_id list) ->
      if List.mem_assoc c execs then List.assoc c execs @ acc else acc)
    p
    []

let abbreviate_list l stringise = match l with
  | [] -> "[]"
  | hd :: [] -> Printf.sprintf "[%s]" (stringise hd)
  | hd :: tl :: [] -> Printf.sprintf "[%s; %s]" (stringise hd) (stringise tl)
  | hd :: tl ->
    let rev = List.rev tl in
    let last = List.hd rev in
    Printf.sprintf "[%s; (... %d item(s) omitted...); %s]"
        (stringise hd) ((List.length rev) - 1) (stringise last)

let entry_to_id e = match e with
  | (command_id, _, _) -> string_of_int command_id

let string_of_node (p : (string * node)) = match p with
  | (filename, Node (entries, perspective, overlay)) ->
    Printf.sprintf "%s -> %s" filename
        (abbreviate_list entries entry_to_id)

let string_of_version v = match v with
  | Version (outcome, nodes) ->
    Printf.sprintf "Version(outcome = %s, nodes = [%s])"
        (string_of_outcome !outcome)
        (String.concat "; " (List.map string_of_node nodes))

let string_of_command s = match s with
  | (command_id, _) -> string_of_int command_id

let string_of_vid_p p = match p with
  | (version_id, version) ->
    Printf.sprintf "%d -> %s" version_id (string_of_version version)

let string_of_state s = match s with
  | State (versions, commands) ->
    Printf.sprintf "State(versions = %s, commands = %s)"
      (String.concat "; " (List.map string_of_vid_p versions))
      (abbreviate_list commands string_of_command)

let log = if !Flags.debug then Some (open_out "/tmp/pide_document.log") else None
let writelog s = match log with
  | Some f -> Printf.fprintf f "%s\n%!" s
  | _ -> ()

let update (v_old: version_id) (v_new: version_id) (edits: edit list) (st : state)
  (*(command_id * exec_id list) list * Pide_protocol.task Queue.t * state*) =
  writelog (Printf.sprintf
      "update(v_old = %d, v_new = %d, edits = _, st = %s)"
      v_old v_new (string_of_state st));
  let Version (outcome, new_nodes) as new_version =
    let old_version = the_version st v_old in
    List.fold_left edit_nodes old_version edits in
  writelog (Printf.sprintf "\tnew_version is %s"
      (string_of_vid_p (v_old, new_version)));
  let (v_old, Version (old_outcome, old_nodes)) as tlgv = the_last_good_version st v_old in
  writelog (Printf.sprintf "\ttlgv is %s" (string_of_vid_p tlgv));
  let tasks = Queue.create () in
  let query_list = ref [] in
  let updated =
    new_nodes |> List.map (fun (name, Node (entries, perspective, overlay)) ->
      if List.mem_assoc name edits then
        let Node (old_entries, _, _) = get_node old_nodes name in
        let (common, (outdated_computation, new_computation)) =
          chop_common old_entries !old_outcome entries in
        let common_execs = List.fold_right
          (fun (id, exec_id, tip_exec_id) acc ->
            if List.mem id perspective then (
              let queries = set_overlay id exec_id overlay in
              let ids_queries = List.map fst queries in
              let query_tasks = List.map snd queries in
              query_list := (exec_id, query_tasks) :: !query_list;
              if ids_queries = [] then acc
              else (id, exec_id :: ids_queries) :: acc)
            else acc
            ) common [] in
        let common_tip =
          if common = [] then !initial_state
          else
            let not_ignored =
              List.filter (fun (cid, _, _) ->
                not (fst (the_command st cid)))
              common in
            !(Util.pi3 (CList.last not_ignored)) in
        Queue.push (`EditAt common_tip) tasks;

        let new_computation' = List.map (fun (cid, _,_) ->
            let req_exec_id = Stateid.fresh () in
            let tip_exec_id = ref req_exec_id in (* Stm.add may change it *)
            cid, req_exec_id, tip_exec_id)
          new_computation in

        List.iter (fun (cid, exec_id, tip_exec_id) ->
            let (is_ignored, cmd_text) = the_command st cid in
            if not is_ignored then add tasks exec_id tip_exec_id cid cmd_text
          ) new_computation';
      let overlay_execs =
        List.fold_right (fun (id, exec_id,_) acc ->
          if List.mem id perspective then (
            let queries = set_overlay id exec_id overlay in
            let ids_queries = List.map fst queries in
            let query_tasks = List.map snd queries in
            query_list := (exec_id, query_tasks) :: !query_list;
            (id, exec_id :: ids_queries):: acc)
          else (id, [exec_id]) :: acc)
        new_computation' [] in
      let command_execs = common_execs @ overlay_execs in
      let updated_node =
        if List.length command_execs + List.length outdated_computation = 0
        then []
        else [(name, Node ((common @ new_computation'), perspective, overlay))]
      in
      let cancel_outdated = List.map (fun (id, _,_) -> (id, [])) 
        outdated_computation in
      let assignment = cancel_outdated @ command_execs in
      Queue.push (`Bless (v_new, outcome)) tasks;
      Queue.push (`Observe (to_exec_list perspective command_execs)) tasks;
      (assignment, updated_node)
    else begin
      Queue.push (`Bless (v_new, outcome)) tasks;
      [], [] end)
  in
  let command_execs = List.flatten (List.map fst updated) in
  let updated_nodes = List.flatten (List.map snd updated) in
  let state' = define_version v_new
    (List.fold_left put_node new_version updated_nodes) st in
  writelog (Printf.sprintf "\tnew state is %s" (string_of_state state'));
  (command_execs, (tasks, !query_list), state')

let lift f {Feedback.id; Feedback.contents; Feedback.route} =
  let i = match id with
  | Feedback.State exec_id ->
      Stateid.to_int exec_id
  | Feedback.Edit i -> i
  in
  f i route contents

let installed_printers : (module Pide_printer.Printer) list ref = ref []

let install_printer (p:  (module Pide_printer.Printer)) =
  installed_printers := p :: !installed_printers

let run_printers f = List.iter (fun (module P : Pide_printer.Printer) ->
    try lift (P.print_func) f
    with
      Pide_printer.Unhandled -> ())
  !installed_printers

let initialize () =
  Pp.set_feeder (fun f -> ignore (run_printers f));
  Pp.log_via_feedback ()

let initialize_state () =
  initial_state := Stm.get_current_state ()
