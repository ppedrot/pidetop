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
  Pide_slave.init_stdout ();
  Pide_flags.pide_slave := true;
  args)

let stm_queue = TQueue.create ()

let main_loop () =
  Sys.catch_break true;
  let t_proto = Thread.create Pide_slave.loop stm_queue in
  let t_stm = Thread.create (fun () ->
    while true do
      let task = TQueue.pop stm_queue in
      try Control.interrupt := false; Lazy.force task
      with
      | Sys.Break -> ()
      | e -> prerr_endline ("An exception has escaped: "^ Printexc.to_string e)
    done) () in
  Thread.join t_proto;
  Thread.join t_stm


let () = Coqtop.toploop_run := main_loop

