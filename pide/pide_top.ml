let () = Coqtop.toploop_init := (fun args ->
  Dumpglob.feedback_glob ();
  Flags.make_silent true;
  Flags.async_proofs_never_reopen_branch := true;
  Flags.async_proofs_full := true;
  Hook.set Stm.unreachable_state_hook
    (fun id ->
      Pp.feedback ~state_id:id Feedback.Processed);
  Pide_slave.init_stdout ();
  Pide_flags.pide_slave := true;
  Flags.async_proofs_flags_for_workers := ["-feedback-glob"];
  Stm.ProofTask.name := "pideproofworker";
  args)

let stm_queue = TQueue.create ()
let () = Hook.set Stm.state_ready_hook (fun stateid ->
  try
    let queries = List.assoc stateid !Pide_protocol.query_list in
    List.iter (fun query -> TQueue.push stm_queue (query :> Pide_protocol.task))
      queries
  with Not_found -> ()
)

let protect f arg =
  try Some (f arg)
  with e when Errors.noncritical e ->
  let e = Errors.push e in
  let msg = Pp.string_of_ppcmds (Errors.iprint e) in
  prerr_endline msg;
  None

let consumer_thread () =
  let cur_tip = ref None in
  let skipping = ref false in
while true do
  let task = TQueue.pop stm_queue in
  try
    Control.interrupt := false;
    match task, !cur_tip with
    | `EditAt here, _ ->
       cur_tip := Some here;
       skipping := false;
       ignore(protect Stm.edit_at here)
    | `Observe p, Some id ->
       Stm.set_perspective p;
       cur_tip := None;
       ignore(protect Stm.observe id)
    | `Observe p, None -> Stm.set_perspective p           
    | `Query (at,route_id,query_id,text), _ ->
         if Stm.state_of_id at <> `Expired then
           let position = Position.id_only (Stateid.to_int query_id) in
           Coq_output.status position Coq_markup.status_running;
           ignore(protect(Stm.query ~at ~report_with:(query_id,route_id)) text)
    | `Add _, _ when !skipping -> ()
    | `Add _, None -> assert false
    | `Add (exec_id, edit_id, text, tip_exec_id), Some tip ->
         let position = Position.id_only (Stateid.to_int exec_id) in
         Coq_output.status position Coq_markup.status_running;
         try
           let tip, _ =
             Stm.add ~newtip:exec_id ~ontop:tip true edit_id text in
           tip_exec_id := tip;
           let ast = Stm.print_ast exec_id in
           Coq_output.report
             (Position.id_only (Stateid.to_int exec_id)) [ast];
           cur_tip := Some tip
         with e when Errors.noncritical e ->
           Coq_output.status position Coq_markup.status_finished;
           skipping := true
  with
  | Sys.Break -> ()
  | e -> prerr_endline ("An exception has escaped: "^
           Pp.string_of_ppcmds (Errors.print e))
done
;;


let main_loop () =
  Sys.catch_break true;
  let t_proto = Thread.create Pide_slave.loop stm_queue in
  let t_stm = Thread.create consumer_thread () in
  Thread.join t_proto;
  Thread.join t_stm


let () = Coqtop.toploop_run := main_loop

