(* The message layer. This protocol layer provides basic functionality for 
 * sending data to, and obtaining data from the PIDE layer. 
 * Sends are provided as message functions, reads as both 'raw' reads
 * and parse functions for embedded data types.
 *)
(* Initialization of layer: initializes lower layers and send initial message to PIDE. *)
val initialize : unit -> unit
