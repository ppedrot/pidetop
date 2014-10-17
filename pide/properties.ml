(*** Property lists ***)
type t = (string * string) list

let defined (props: t) name = List.mem_assoc name props
let empty = []
let get (props: t) name = try Some (List.assoc name props) with Not_found -> None
let put ((name, _) as entry) (props: t) = entry :: List.remove_assoc name props
let remove name (props: t) = List.remove_assoc name props
let append = (@)
