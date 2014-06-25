let rec parse = function
  | [] -> []
  | "-W" :: fifos :: rest ->
    (* TODO (CT): -W is the option that PIDE processes send to provide a FIFO
       channel it packs fifo_in with fifo_out in the following format:
       fifo_in:fifo_out *)
    (* TODO: put these option outside Flags (they are PIDE specific) *)
          Flags.pide_fifos := 
            begin match (Str.split (Str.regexp_string ":") fifos) with
            | [fifo_in; fifo_out] -> (fifo_in, fifo_out)
            | _ ->
               prerr_endline "Error: fifo_in:fifo_out expected";
               exit 1
            end;
          Flags.pide_slave := true;
          Flags.feedback_goals := true;
     parse rest
  | x :: rest -> x :: parse rest



let () = Coqtop.toploop_init := (fun args ->
  let args = parse args in
  Dumpglob.feedback_glob ();
  Flags.make_silent true;
  Pide_slave.init_stdout ();
  args)

let () = Coqtop.toploop_run := Pide_slave.loop

