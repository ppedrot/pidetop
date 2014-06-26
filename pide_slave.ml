let loop q =
  Pide_document.initialize ();
  Pide_protocol.loop q

let init_stdout ()=
  Pide_protocol.initialize ()
