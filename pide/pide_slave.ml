let loop q =
  Pide_document.initialize_state ();
  Pide_protocol.loop q

let init_stdout ()=
  Pide_document.initialize ();
  Pide_protocol.initialize ()
