module W = AsyncTaskQueue.MakeWorker(Stm.ProofTask)

let () = Coqtop.toploop_init := (fun args ->
        Flags.make_silent true;
        W.init_stdout ();
        CoqworkmgrApi.init !Flags.async_proofs_worker_priority;
        args)

let () = Coqtop.toploop_run := W.main_loop

