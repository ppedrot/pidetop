(* STM customization *)

let print_goals_of_state, forward_feedback =
  let already_printed = ref Stateid.Set.empty in
  let add_to_already_printed =
    let m = Mutex.create () in
    fun id ->
      Mutex.lock m;
      already_printed := Stateid.Set.add id !already_printed;
      Mutex.unlock m in
  (fun id state ->
    if Stateid.Set.mem id !already_printed then ()
    else begin
      add_to_already_printed id;
      Pide_goalprint.feedback_structured_goals id state
  end),
  (let open Feedback in let open Pp in function
   | {id = State state_id; route; contents = (Goals _ as m)} ->
      add_to_already_printed state_id; feedback ~state_id ~route m
   | {id = Edit edit_id; route; contents} -> feedback ~edit_id ~route contents
   | {id = State state_id;route;contents} -> feedback ~state_id ~route contents)

(* end STM customization *)

let () = Coqtop.toploop_init := (fun args ->
  Dumpglob.feedback_glob ();
  Flags.make_silent true;
  Flags.async_proofs_never_reopen_branch := true;
  Flags.async_proofs_always_delegate := true;
  Hook.set Stm.state_computed_hook
    (fun id ~in_cache ->
       Pp.feedback ~state_id:id Feedback.Processed;
       Option.iter (print_goals_of_state id) (Stm.state_of_id id));
  Hook.set Stm.forward_feedback_hook forward_feedback;
  Pide_slave.init_stdout ();
  Pide_flags.pide_slave := true;
  Flags.async_proofs_flags_for_workers := ["-feedback-glob"];
  Stm.ProofTask.name := "pideproofworker";
  args)

let stm_queue = TQueue.create ()

let protect f arg =
  try Some (f arg)
  with e when Errors.noncritical e ->
  let e = Errors.push e in
  let msg = Pp.string_of_ppcmds (Errors.print e) in
  prerr_endline msg;
  None

let main_loop () =
  Sys.catch_break true;
  let t_proto = Thread.create Pide_slave.loop stm_queue in
  let t_stm = Thread.create (fun () ->
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
        | `Observe, Some id ->
           cur_tip := None;
           ignore(protect Stm.observe id)
        | `Observe, None -> ()
        | `Query _, _ when !skipping -> ()
        | `Query f, _ -> Lazy.force f
        | `Add _, _ when !skipping -> ()
        | `Add f, _ ->
            match Lazy.force f with
            | None -> skipping := true
            | Some tip -> cur_tip := Some tip
      with
      | Sys.Break -> ()
      | e -> prerr_endline ("An exception has escaped: "^
               Pp.string_of_ppcmds (Errors.print e))
    done) () in
  Thread.join t_proto;
  Thread.join t_stm


let () = Coqtop.toploop_run := main_loop

