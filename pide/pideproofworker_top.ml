module W = AsyncTaskQueue.MakeWorker(Stm.ProofTask)()

let () = Coqtop.toploop_init := (fun opts args ->
        Flags.quiet := true;
        W.init_stdout ();
        opts, args)

let () = Coqtop.toploop_run := (fun _ ~state:_ -> W.main_loop ())


(* TODO: This was removed in feature/goalprint_overlay, not sure if it still is
 * necessary, in particular the unreachable_state_hook.
 *)
let () = 
  Hook.set Stm.unreachable_state_hook
    (fun ~doc:_ id (e, info) ->
      match e with
        | Sys.Break -> ()
        | _ -> Feedback.(feedback ~id:id Feedback.Processed));
  Hook.set Stm.state_computed_hook
    (fun ~doc id ~in_cache ->
       Feedback.(feedback ~id:id Feedback.Processed);
       if not in_cache then
       match Stm.state_of_id ~doc id with
       | `Expired | `Valid None | `Error _ -> ()
       | `Valid (Some { Vernacstate.proof }) ->
         try
           Pide_goalprint.feedback_structured_goals ~state_id:id
             (Proof_global.proof_of_state proof)
         with Proof_global.NoCurrentProof -> ())

;;

