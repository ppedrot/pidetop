open Coq_output
open Coq_input

(* Helper function. *)
let quote s = "\"" ^ s ^ "\""

type task = [ `Observe | `Add of Stateid.t option lazy_t | `EditAt of Stateid.t | `Query of unit lazy_t ]

let commands =
  ref ([]: (string * (task TQueue.t -> string list -> unit)) list)

let register_protocol_command name cmd = commands := (name, cmd) :: !commands

let run_command name args stmq = 
  let cmd = 
    try List.assoc name !commands 
    with Not_found ->
       raise (Failure ("Undefined Coq process command " ^ quote name))
  in
    try cmd stmq args
    with e -> 
      let e = Errors.push e in
      raise (Failure ("Coq process protocol failure: " ^ quote name ^ "\n" ^
                      Pp.string_of_ppcmds (Errors.print e)))

let initialize_commands () =
  register_protocol_command "echo" (fun _ args ->
     List.iter (writeln Position.none) args);

  register_protocol_command "Document.discontinue_execution" (fun stmq _ ->
(*    TQueue.clear stmq; *)
    Control.interrupt := true);

  register_protocol_command "Document.cancel_execution" (fun stmq _ ->
    TQueue.clear stmq;
    Control.interrupt := true);

  register_protocol_command "Document.define_command" (fun stmq args ->
    match args with
    | [id; is_ignored; text] ->
        let cmd_id = Pide_document.parse_id id in
        let is_ignored = Pide_xml.Decode.bool (Yxml.parse_body is_ignored) in
        let new_state = Pide_document.define_command cmd_id is_ignored text in
        Pide_document.change_state new_state
    | _ -> assert false); 

  register_protocol_command "Document.update" (fun stmq args ->
    match args with
    | [old_id_str; new_id_str; edits_yxml] -> 
        let old_id = Pide_document.parse_id old_id_str in
        let new_id = Pide_document.parse_id new_id_str in
        let new_transaction = ref (Queue.create ()) in
        let edits = obtain_edits edits_yxml in
        Pide_document.change_state (fun state ->
          let (assignment, tasks, state') =
            Pide_document.update old_id new_id edits state in
          assignment_message new_id assignment;
          new_transaction := tasks;
          state');
        Pide_document.execute stmq !new_transaction
    | _ -> assert false);
  register_protocol_command "Document.remove_versions" (fun _ args ->
    match args with
    | [stale_versions_yxml] ->
        let ids = obtain_version_ids stale_versions_yxml in (* <- TODO: Parse stale_versions_yxml *)
        Pide_document.change_state (fun state -> Pide_document.remove_versions ids state);
        Coq_output.removed_versions_message ids
    | _ -> assert false)


let initialize () =
  Coq_messages.initialize ();
  initialize_commands ()

let rec loop stmq =
  (try match read_command () with
  | None -> ()
  | Some [] -> error_msg Position.none "Coq process: no input"
  | Some (name :: args) ->
      prerr_endline ("got message: "^ name);
      run_command name args stmq
  with e when Errors.noncritical e ->
    let e = Errors.push e in
    prerr_endline (Printexc.to_string e);
    error_msg Position.none (Printexc.to_string e));
  loop stmq