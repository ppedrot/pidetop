(* Send initialization message *)
val init_message : string -> unit

(* Write a line *)
val writeln : Position.t -> string -> unit

(* Report an error message. 
 * Takes the position the error occurred (built by any of the Position functions) and an error message. 
 *)
val error_msg : Position.t -> string -> unit

(* Report a warning, with the id of where the warning occurs. *)
val warning_msg : Position.t -> string -> unit

(* Lexer reports from the prover: takes the execution id (as a string) to reported on, and some syntax tree (as string)  *)
(* TODO: Make second argument report type? *)
val report : Position.t -> string list -> unit

(* Acknowledgement of an update: takes the version updated to, and a list of command id -> Stateid mappings. *)
val assignment_message : int -> (int * Stateid.t list) list -> unit
