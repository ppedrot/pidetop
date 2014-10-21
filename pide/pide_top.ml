let rec parse = function
  | [] -> []
  | "-W" :: fifos :: rest ->
    (* TODO (CT): -W is the option that PIDE processes send to provide a FIFO
       channel it packs fifo_in with fifo_out in the following format:
       fifo_in:fifo_out *)
    (* TODO: put these option outside Flags (they are PIDE specific) *)
          Pide_flags.pide_fifos := 
            begin match (Str.split (Str.regexp_string ":") fifos) with
            | [fifo_in; fifo_out] -> (fifo_in, fifo_out)
            | _ ->
               prerr_endline "Error: fifo_in:fifo_out expected";
               exit 1
            end;
          Pide_flags.pide_slave := true;
          Flags.feedback_goals := true;
     parse rest
  | x :: rest -> x :: parse rest


let () = Coqtop.toploop_init := (fun args ->
  let args = parse args in
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
           ignore(Stm.edit_at here)
        | `Observe, Some id ->
           cur_tip := None;
           Stm.observe id
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
