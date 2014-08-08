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
    | [id;_;_;text] ->
        let cmd_id = Pide_document.parse_id id in
        let new_state = Pide_document.define_command cmd_id text in
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
    | _ -> assert false)

let (<|>) f1 f2 feedback =
  if f1 feedback then true
                 else f2 feedback

let exec_printer id msg f=
  match id with
  | Feedback.State exec_id -> 
      f exec_id (Pide_document.print_exec_id exec_id) msg
(*  | Feedback.Edit eid -> (* FIXME: Sometimes, a feedback is sent with a command id. *)
      f eid (Pide_document.print_id eid) msg*)
  | _ -> false

let already_printed = ref Stateid.Set.empty

let goal_printer {Feedback.id = id; Feedback.content = content} =
  exec_printer id content (fun exec_id exec_id_str msg ->
    match msg with
    | Feedback.Goals (loc,goalstate) when Loc.is_ghost loc ->
        (if Stateid.Set.mem exec_id !already_printed then ()
         else (
           already_printed := Stateid.Set.add exec_id !already_printed;
           writeln (Position.id_only exec_id_str) goalstate));
        true
    | Feedback.Goals (loc,goalstate) ->
        (if Stateid.Set.mem exec_id !already_printed then ()
         else (
           already_printed := Stateid.Set.add exec_id !already_printed;
           let i, j = Loc.unloc loc in
              writeln (Position.make_id i j exec_id_str) goalstate));
        true 
    | _ -> false
    )

let error_printer {Feedback.id = id; Feedback.content = content} =
  exec_printer id content (fun exec_id exec_id_str msg -> 
    match msg with
    | Feedback.ErrorMsg (loc, txt) ->
      let pos = (if Loc.is_ghost loc then Position.id_only 
                 else let i, j = Loc.unloc loc in Position.make_id (i+1) (j+1)) 
        exec_id_str in
      report pos [(Yxml.string_of_body [
          Pide_xml.Elem (("finished", []), [])])];
      error_msg pos txt;
      true
    | _ -> false  )

let rest_printer {Feedback.id = id; Feedback.content = content } =
  exec_printer id content (fun exec_id exec_id_str msg ->
    match msg with
    | Feedback.Processed ->
        let position = Position.id_only exec_id_str in
        report position [(Yxml.string_of_body [
          Pide_xml.Elem (("finished", []), [])])];
        true
    | Feedback.Message { Feedback.message_content = s } ->
        let position = Position.id_only exec_id_str in
        writeln position s;
        true
    | _ -> false)

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

let glob_printer {Feedback.id = id; Feedback.content = content} =
  exec_printer id content (fun exec_id exec_id_str msg ->
    match msg with
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
          
          let position = Position.make_id (i+1) (j+1) exec_id_str in
          report position [(Yxml.string_of_body [Pide_xml.Elem (("entity", report_body  ), [])])]
        )
    | _ -> false
  )
let state_printer ~id _ content =
  exec_printer id content (fun exec_id exec_id_str msg -> 
    writeln (Position.id_only exec_id_str) (Pp.string_of_ppcmds msg);
    true)

let init_printers () =
  Pp.set_feeder (fun f ->
    ignore((error_printer<|>goal_printer <|> glob_printer <|> rest_printer) f));
  Pp.log_via_feedback ()

let initialize () =
  Coq_messages.initialize ();
  init_printers ();
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
