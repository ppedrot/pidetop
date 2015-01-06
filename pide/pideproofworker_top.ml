module W = AsyncTaskQueue.MakeWorker(Stm.ProofTask)

let () = Coqtop.toploop_init := (fun args ->
        Flags.make_silent true;
        W.init_stdout ();
        CoqworkmgrApi.init !Flags.async_proofs_worker_priority;
        args)

let () = Coqtop.toploop_run := W.main_loop


(* TODO: This was removed in feature/goalprint_overlay, not sure if it still is
 * necessary, in particular the unreachable_state_hook.
 *)
let () = 
  Hook.set Stm.unreachable_state_hook
    (fun id ->
      Pp.feedback ~state_id:id Feedback.Processed)
