type id = int
type command_id = id
type version_id = id
type exec_id = Stateid.t
type state

val parse_id: string -> id
val print_id: id -> string
val print_exec_id : exec_id -> string

(*
 * node_edit = 
   Insert_afer of (command_id option, command_id): in
   Delete_after (command_id option):  (first, or later)
 *
 *)
type node_edit = 
  | Edits of (command_id option * command_id option) list
type edit = string * node_edit

val define_command: command_id -> string -> state -> state
val update:
  version_id -> version_id -> edit list -> state -> 
  (command_id * exec_id option) list * exec_id * state

(* Executes the given 'transaction': 
 * the first argument is the list of new assignments, 
 *  the second argument the list of 'common ancestors'. 
 *  The version_id argument is the tip from which to execute the given commands.
 *  FIXME: Enrico says that there might be situations when the tip needs to be retracted when a later error is encountered.
 *  TODO: Maybe enrich global states to contain STM tip?
 *)
val execute :
  unit lazy_t TQueue.t -> (command_id * exec_id option) list -> exec_id ->
  version_id -> unit

val change_state: (state -> state) -> unit

val initialize : unit -> unit
