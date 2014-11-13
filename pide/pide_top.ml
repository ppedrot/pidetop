let () = Coqtop.toploop_init := (fun args ->
  Dumpglob.feedback_glob ();
  Flags.make_silent true;
  Flags.async_proofs_never_reopen_branch := true;
  Flags.async_proofs_always_delegate := true;
  Flags.feedback_goals := true;
  Pide_slave.init_stdout ();
  Pide_flags.pide_slave := true;
  Flags.async_proofs_flags_for_workers := ["-feedback-glob"; "-feedback-goals"];
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

