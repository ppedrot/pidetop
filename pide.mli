val protocol_command : string -> (string list -> unit) -> unit

type position

val status : string -> unit
val report : position -> string -> unit
val writeln : string -> unit
val warning : string -> unit
val error_msg : string -> unit
val protocol_message : (string * string) list -> string -> unit

val main : string * string -> unit

