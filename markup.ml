type t = string * Properties.t

let empty = ("", [])

let is_empty (elem, _) = elem = ""

let properties more_props ((elem, props): t) =
  (elem, List.fold_right Properties.put more_props props)


(* misc properties *)

let nameN = "name"
let name a = properties [(nameN, a)]

let kindN = "kind"

let serialN = "serial"

