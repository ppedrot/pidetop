type t = string * Properties.t

val empty: t
val is_empty: t -> bool

val properties: Properties.t -> t -> t
val name: string -> t -> t

val instanceN: string
val kindN: string
val nameN: string
val serialN: string
