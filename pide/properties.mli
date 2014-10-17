type t = (string * string) list
val defined: t -> string -> bool

val empty : t
val get: t -> string -> string option
val put: string * string -> t -> t
val remove: string -> t -> t
val append: t -> t -> t
