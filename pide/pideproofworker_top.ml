module W = AsyncTaskQueue.MakeWorker(Stm.ProofTask)

let () = Coqtop.toploop_init := (fun args ->
        Flags.make_silent true;
        W.init_stdout ();
        CoqworkmgrApi.init !Flags.async_proofs_worker_priority;
        args)

let () = Coqtop.toploop_run := W.main_loop

let () = 
  Hook.set Stm.state_computed_hook
    (fun id ~in_cache ->
       Pp.feedback ~state_id:id Feedback.Processed;
       Option.iter
         (Pide_goalprint.feedback_structured_goals id) (Stm.state_of_id id))
