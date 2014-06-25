(* Initialize protocol layer: initializes lower layers as a result, and 
 * installs hooks for protocol execution.
 *)
val initialize : unit -> unit

(* Run the PIDE protocol. This is the main loop for the PIDE layer: it will 
 * react to messages by pre-installed (by initialize) hooks. 
 * The hooks dispatch their actions to the model maintained by the PIDE document module.
 *)
val loop : unit -> unit
