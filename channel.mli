(* Bottom layer of the protocol stack: byte channels. 
 * Currently, we only have a FIFO implementation of this module.
 * At some point, we need to look into TCP/IP sockets.
 * *)

val initialize : unit -> unit
val send : string -> string -> unit
val read_command : unit -> string list option
