type t
val make_id: int -> int -> string -> t
val make: int -> int -> t
val none: t
val id_only: string -> t
val properties_of: t -> Properties.t

(* These need to move to Markup. *)
val markup: t -> Markup.t -> Markup.t
type report = t * Markup.t

val reported_text: report -> string -> string
