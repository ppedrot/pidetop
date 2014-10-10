open Coq_output

let initialize () =
  Yxml.initialize ();
  init_message ("Coq " ^ Coq_config.date)
