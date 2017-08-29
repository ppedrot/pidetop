let () = Coqtop.toploop_init := (fun args ->
  Dumpglob.feedback_glob ();
  Flags.quiet := true;
  Flags.async_proofs_never_reopen_branch := true;
  Flags.async_proofs_full := true;
  Hook.set Stm.unreachable_state_hook
    (fun id (e, info) ->
      match e with
        | Sys.Break -> ()
        | _ -> Feedback.(feedback ~id Processed));
  Pide_slave.init_stdout ();
  Pide_flags.pide_slave := true;
  Flags.async_proofs_flags_for_workers := ["-feedback-glob"];
  Stm.ProofTask.name := "pideproofworker";
  args)

let stm_queue = TQueue.create ()
let () = Hook.set Stm.state_ready_hook (fun stateid ->
  try
    let queries = List.assoc stateid !Pide_protocol.query_list in
    List.iter (fun query -> TQueue.push stm_queue (query :> Pide_document.task))
      queries
  with Not_found -> ()
)

let protect f arg =
  try Some (f arg)
  with e when CErrors.noncritical e ->
  let e = CErrors.push e in
  let msg = Pp.string_of_ppcmds (CErrors.iprint e) in
  prerr_endline msg;
  None

type consumer_mode =
  | Everything
  | QueriesOnly (* and `Observe, and `Bless... *)
  | Nothing

let string_of_mode m = match m with
  | Everything -> "Everything"
  | QueriesOnly -> "QueriesOnly"
  | Nothing -> "Nothing"

let log = if !Flags.debug then Some (open_out "/tmp/pide_top.log") else None
let writelog s = match log with
  | Some f -> Printf.fprintf f "%s\n%!" s
  | _ -> ()

let consumer_thread () =
  let cur_tip = ref None in
  let last_edit_id = ref None in
  let mode = ref Everything in
  let current_document = ref (Stm.backup ()) in
while true do
  let task = TQueue.pop stm_queue in
  let old_mode = !mode in
  writelog (Pide_document.string_of_task task);
  writelog (Printf.sprintf "\tbegin %f" (Unix.gettimeofday ()));
  (try
    match task, !cur_tip with
    | `EditAt here, _ ->
       Stm.restore !current_document;
       cur_tip := Some here;
       mode := Everything;
       last_edit_id := None;
       Control.interrupt := false;
       ignore(protect Stm.edit_at here)
    | `Observe p, Some id ->
       Stm.set_perspective p;
       cur_tip := None;
       ignore(protect Stm.observe id)
    | `Observe p, None -> Stm.set_perspective p
    | `Query (at,route_id,query_id,text), _ when !mode <> Nothing ->
         Control.check_for_interrupt ();
         if Stm.state_of_id at <> `Expired then
           let position = Position.id_only (Stateid.to_int query_id) in
           let is_goal_print = text = Pide_document.goal_query in
           if is_goal_print &&
              Stateid.Set.mem at !Goal_printer.goal_already_printed
           then writelog ("skip " ^ Pide_document.string_of_task task)
           else begin
            Coq_output.status position Coq_markup.status_running;
            ignore(protect(Stm.query ~at ~route:route_id) (Pcoq.Gram.parsable (Stream.of_string text)))
           end;
           if is_goal_print then
             Pide_document.goal_printed_at ~at ~exec:query_id
    | `Query _, _ -> ()
    | `Add _, _ when !mode <> Everything -> ()
    | `Add _, None -> assert false
    | `Add (exec_id, edit_id, text, tip_exec_id), Some tip ->
         let position = Position.id_only (Stateid.to_int exec_id) in
         Coq_output.status position Coq_markup.status_running;
         (try
           last_edit_id := Some edit_id;
           Control.check_for_interrupt ();
           let ast = Stm.parse_sentence tip (Pcoq.Gram.parsable (Stream.of_string text)) in
           let tip, _ =
             Stm.add ~newtip:exec_id ~ontop:tip true ast in
           tip_exec_id := tip;
           cur_tip := Some tip;
         with e when CErrors.noncritical e ->
           Feedback.(feedback ~id:exec_id Processed);
           mode := QueriesOnly)
    | `Bless (new_id, outcome), _ ->
        outcome :=
          if !mode = Everything then
            `FullyCommitted
          else if !last_edit_id <> None then
            `CommittedUpTo (Option.get !last_edit_id)
          else
            `NotCommitted;
        writelog (Printf.sprintf "\t\toutcome for %d is %s" new_id
                      (Pide_document.string_of_outcome !outcome));
        current_document := Stm.backup ()
    | `Signal (mutex, condition, spurious), _ ->
      Mutex.lock mutex;
      spurious := false;
      Condition.signal condition;
      Mutex.unlock mutex
  with
  | Sys.Break ->
    (* This is how we receive discontinue_execution messages from the other
       thread; in that case, we don't discontinue execution as much as we
       ignore all the execution we still have to do *)
    mode := Nothing
  | e ->
    let desc = Pp.string_of_ppcmds (CErrors.print e) in
    prerr_endline ("An exception has escaped while processing: "^
        Pide_document.string_of_task task^"\n" ^ desc);
    writelog (Printf.sprintf "\texception:\n%s" desc));
  writelog (Printf.sprintf "\tend %f" (Unix.gettimeofday ()));
  if old_mode <> !mode then
    writelog (Printf.sprintf "state change: %s -> %s"
                  (string_of_mode old_mode) (string_of_mode !mode))
done
;;


let main_loop () =
  Sys.catch_break true;
  let t_proto = Thread.create Pide_slave.loop stm_queue in
  let t_stm = Thread.create consumer_thread () in
  Thread.join t_proto;
  Thread.join t_stm


let () = Coqtop.toploop_run := main_loop

