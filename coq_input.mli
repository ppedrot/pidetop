(* Read command from lower layers:
 * will return None when the lower channel is closed, otherwise
 * will return Some list containing the command name and any arguments to the command.
 *)
val read_command : unit -> string list option

(* Decodes YXML-encoded edits into Pide_document edits. *)
val obtain_edits : string -> Pide_document.edit list
