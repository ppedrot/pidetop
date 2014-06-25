let loop () =
  Pide_document.initialize ();
  Pide_protocol.loop ()

let init_stdout ()=
  Pide_protocol.initialize ()
