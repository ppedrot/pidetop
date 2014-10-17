(* Send initialization message *)
val init_message : string -> unit

type standard_message_t = Position.t -> ?props: Properties.t -> string -> unit

(* Write a line *)
val writeln : standard_message_t

val result : Position.t -> int -> string -> unit

(* Report an error message. 
 *)
val error_msg : standard_message_t

(* Report a warning, with the id of where the warning occurs. *)
val warning_msg : standard_message_t

(* Lexer reports from the prover: takes the execution id (as a string) to reported on, and some syntax tree (as string)  *)
(* TODO: Make second argument report type? *)
val report : Position.t -> Xml_datatype.xml list -> unit

val status : Position.t -> Xml_datatype.xml list -> unit

(* Acknowledgement of an update: takes the version updated to, and a list of command id -> Stateid mappings. *)
val assignment_message : int -> (int * Stateid.t list) list -> unit

(* Acknowledgement of version removal. Takes the removed versions. *)
val removed_versions_message : int list -> unit
