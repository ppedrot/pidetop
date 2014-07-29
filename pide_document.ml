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


type entries = (command_id * exec_id) list
type node = Node of entries
let empty_node = Node []

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

let the_version (State (versions, _)) (id: version_id) =
  List.assoc id versions

let the_command (State (_, commands)) (id: command_id) =
  List.assoc id commands


let parse_id = int_of_string
let print_id = string_of_int
let print_exec_id = Stateid.to_string

type node_edit = 
  | Edits of (command_id option * command_id option) list

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

let edit_node (Node entries) edit =
    Node
    (match edit with
    | (hook, Some id2) -> insert_after hook id2 entries
    | (hook, None) -> remove_after hook entries)

let edit_nodes (Version nodes) (name, node_edit) =
  Version 
    (match node_edit with
      | Edits edits -> 
          update_node name (fun x -> List.fold_left edit_node x edits) nodes
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

let initial_state: Stateid.t ref = ref Stateid.dummy

let errors = ref []
let update (v_old: version_id) (v_new: version_id) (edits: edit list) (st : state) = 
  let Version old_nodes as old_version = the_version st v_old in
  let Version new_nodes as new_version = List.fold_left edit_nodes old_version edits in
  let updated = 
    new_nodes |> List.map (fun (name, Node entries) ->
      if List.mem_assoc name edits then
        let Node entries0 = get_node old_nodes name in
        let (common, (rest0, rest)) = chop_common entries0 entries in
        let tip = if common = [] then !initial_state else snd (CList.last common) in
        let rest' = List.map (fun (id, _) -> id, Stateid.fresh ()) rest in
        let command_execs =
          List.map (fun (id, _) -> (id, None)) rest0 @
          List.map (fun (id, exec_id) -> (id, Some exec_id)) rest' in
        let updated_node =
          match command_execs with
          | [] -> []
          | _  -> [(name, Node (common @ rest'))] in
        (command_execs, tip, updated_node)
      else
        ([], !initial_state, []))
    in 
    let command_execs = List.flatten (List.map (fun (x, _, _) -> x) updated) in
    let tip = List.fold_left (fun _ (_, t, _) -> t) !initial_state updated in  
    let updated_nodes = List.flatten (List.map (fun (_, _, y) -> y) updated) in
    let state' =
      define_version v_new (List.fold_left put_node new_version updated_nodes) st in
    (command_execs, tip, state')

let add stmq exec_id tip edit_id text = 

  TQueue.push stmq (`Add (lazy (
  let position = Position.id_only (print_exec_id exec_id) in
  Coq_output.report position [(Yxml.string_of_body [
          Pide_xml.Elem (("running", []), [])])];
  try
    ignore(Stm.add ~newtip:exec_id ~ontop:tip true edit_id text);
    Some exec_id
  with e when Errors.noncritical e ->
    let e = Errors.push e in
    let message = Pp.string_of_ppcmds (Errors.print e) in
    let exec_id_string = Stateid.to_string exec_id in
    let pos = match Loc.get_loc e with 
      | Some t ->
          let i, j = Loc.unloc t in Position.make_id (i+1) (j+1) exec_id_string
      | None -> Position.id_only exec_id_string in
    Coq_output.error_msg pos message;
    None)));
  exec_id

let execute stmq (execs : (command_id * exec_id option) list) tip version =
  let st = !global_state in
  TQueue.push stmq (`EditAt tip);
  let _ = (List.fold_left (fun curr_tip (cid, eid) -> 
      match eid with
      | Some exec_id -> add stmq exec_id curr_tip cid (the_command st cid)
      | None -> curr_tip 
      )
    tip execs) in
  TQueue.push stmq `Observe
  
let initialize () =
  initial_state := Stm.get_current_state ()
