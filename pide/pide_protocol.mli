(* Initialize protocol layer: initializes lower layers as a result, and 
 * installs hooks for protocol execution.
 *)
val initialize : unit -> unit

type transaction_outcome =
  [ `NotCommitted
  | `CommittedUpTo of int
  | `FullyCommitted ]

val string_of_outcome : transaction_outcome -> string

(* Run the PIDE protocol. This is the main loop for the PIDE layer: it will 
 * react to messages by pre-installed (by initialize) hooks. 
 * The hooks dispatch their actions to the model maintained by the PIDE document module.
 *)
type task =
  [ `Observe of Stateid.t list
  | `Add of Stateid.t * int * string * Stateid.t ref
  | `EditAt of Stateid.t
  | `Query of Stateid.t * Feedback.route_id * Stateid.t * string
  | `Bless of int * (transaction_outcome ref)]

val string_of_task : task -> string

val loop : task TQueue.t -> unit

val query_list : (Stateid.t * [`Query of Stateid.t * Feedback.route_id * Stateid.t * string] list) list ref
