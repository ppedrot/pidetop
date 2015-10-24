(* Initialize protocol layer: initializes lower layers as a result, and 
 * installs hooks for protocol execution.
 *)
val initialize : unit -> unit

val loop : Pide_document.task TQueue.t -> unit

val query_list : (Stateid.t * [`Query of Stateid.t * Feedback.route_id * Stateid.t * string] list) list ref
