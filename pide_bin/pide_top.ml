let pidetop_init ~opts extra_args =
  let open Coqargs in
  Dumpglob.feedback_glob ();
  Flags.quiet := true;
  let opts = {
    opts with
    stm_flags =
      { opts.stm_flags with
        Stm.AsyncOpts.async_proofs_full = true;
        Stm.AsyncOpts.async_proofs_never_reopen_branch = true; }
  } in
  Hook.set Stm.unreachable_state_hook
    (fun ~doc:_ id (e, info) ->
      match e with
        | Sys.Break -> ()
        | _ -> Feedback.(feedback ~id Processed));
  Pide_document.initialize ();
  Pide_protocol.initialize ();
  Pide_flags.pide_slave := true;
  AsyncTaskQueue.async_proofs_flags_for_workers := ["-feedback-glob"];
  Stm.ProofTask.name := "pideproofworker";
  opts, extra_args

module SidMap = Map.Make(Stateid)
let delayed_queries : Pide_document.task list SidMap.t ref = ref SidMap.empty

let delay_query_until_ready stateid q =
  try let qs = SidMap.find stateid !delayed_queries in
    delayed_queries := SidMap.add stateid (q :: qs) !delayed_queries
  with Not_found ->
    delayed_queries := SidMap.add stateid [q] !delayed_queries

let stm_queue = TQueue.create ()
let () = Hook.set Stm.state_ready_hook (fun ~doc:_ stateid ->
  try
    let queries = SidMap.find stateid !delayed_queries in
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

(* XXX current_doc is still a dummy value, shall replace backup/restore and
 * current_doc_deprecated *)
let consumer_thread { Vernac.State.doc = initial_doc } =
  let cur_tip = ref None in
  let last_edit_id = ref None in
  let mode = ref Everything in
  let current_doc_deprecated  = ref (Stm.backup ()) in
  let current_doc = ref initial_doc in
while true do
  let task = TQueue.pop stm_queue in
  let old_mode = !mode in
  writelog (Pide_document.string_of_task task);
  writelog (Printf.sprintf "\tbegin %f" (Unix.gettimeofday ()));
  (try
    match task, !cur_tip with
    | `EditAt here, _ ->
       Stm.restore !current_doc_deprecated;
       cur_tip := Some here;
       mode := Everything;
       last_edit_id := None;
       Control.interrupt := false;
       ignore(protect (Stm.edit_at ~doc:!current_doc) here)
    | `Observe p, Some id ->
       Stm.set_perspective ~doc:!current_doc p;
       cur_tip := None;
       ignore(protect (Stm.observe ~doc:!current_doc) id)
    | `Observe p, None -> Stm.set_perspective ~doc:!current_doc p
    | `Query (at,route_id,query_id,text) as q, _ when !mode <> Nothing ->
         Control.check_for_interrupt ();
         begin match Stm.state_of_id ~doc:!current_doc at with
         | `Expired | `Error _ -> ()
         | `Valid None -> delay_query_until_ready at q
         | `Valid (Some _) ->
           let position = Position.id_only (Stateid.to_int query_id) in
           let is_goal_print = text = Pide_document.goal_query in
           if is_goal_print &&
              Stateid.Set.mem at !Goal_printer.goal_already_printed
           then writelog ("skip " ^ Pide_document.string_of_task task)
           else begin
            Coq_output.status position Coq_markup.status_running;
            ignore(protect(Stm.query ~doc:!current_doc ~at ~route:route_id)
              (Pcoq.Gram.parsable (Stream.of_string text)))
           end;
           if is_goal_print then
             Pide_document.goal_printed_at ~at ~exec:query_id
         end
    | `Query _, _ -> ()
    | `Add _, _ when !mode <> Everything -> ()
    | `Add _, None -> assert false
    | `Add (exec_id, edit_id, text, tip_exec_id), Some tip ->
         let position = Position.id_only (Stateid.to_int exec_id) in
         Coq_output.status position Coq_markup.status_running;
         (try
           last_edit_id := Some edit_id;
           Control.check_for_interrupt ();
           let ast = Stm.parse_sentence ~doc:!current_doc tip (Pcoq.Gram.parsable (Stream.of_string text)) in
           let doc, tip, _ =
             Stm.add ~doc:!current_doc ~newtip:exec_id ~ontop:tip true ast in
           tip_exec_id := tip;
           cur_tip := Some tip;
           current_doc := doc
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
        (* XXX when you remove this double check that the update of
         * current_doc is OK wrt the new STM API (add cannot fail) *)
        current_doc_deprecated := Stm.backup ()
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


let main_loop ~opts ~state =
  Sys.catch_break true;
  Pide_document.initialize_state state;
  let t_proto = Thread.create Pide_protocol.loop stm_queue in
  let t_stm = Thread.create consumer_thread state in
  Thread.join t_proto;
  Thread.join t_stm

let () =
  Coqtop.(start_coq { init = pidetop_init; run = main_loop; })

