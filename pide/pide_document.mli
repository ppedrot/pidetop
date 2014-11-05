type id = int
type command_id = id
type version_id = id
type exec_id = Stateid.t
type state

val parse_id: string -> id
val print_id: id -> string
val print_exec_id : exec_id -> string

type perspective = command_id list

(* Overlay: print functions on a specific command span. *)
(* The type encodes the command to print on, the query to execute and its 
 * arguments *)
type overlay = (command_id * (string * (string list))) list

(*
 * node_edit = 
   Insert_afer of (command_id option, command_id): in
   Delete_after (command_id option):  (first, or later)
 *
 *)
type node_edit = 
  | Edits of (command_id option * command_id option) list
  | Perspective of (perspective * overlay)
type edit = string * node_edit

val define_command: command_id -> bool -> string -> state -> state

val update: version_id -> version_id -> edit list -> state ->
  (command_id * exec_id list) list * Pide_protocol.task Queue.t * state

val remove_versions: version_id list -> state -> state

(* Executes the given 'transaction': 
 *  FIXME: Enrico says that there might be situations when the tip needs to be retracted when a later error is encountered
 *  TODO: Is this the right place to start shuffling the queues?
 *)
val execute:
  Pide_protocol.task TQueue.t -> Pide_protocol.task Queue.t -> unit

val change_state: (state -> state) -> unit

val initialize: unit -> unit

val initialize_state: unit -> unit
