type t = string * Properties.t
val empty: t
val is_empty: t -> bool
val properties: Properties.t -> t -> t
val nameN: string
val name: string -> t -> t
val kindN: string
val serialN: string
