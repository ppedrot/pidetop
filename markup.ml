type t = string * Properties.t

let empty = ("", [])

let is_empty (elem, _) = elem = ""

let properties more_props ((elem, props): t) =
  (elem, List.fold_right Properties.put more_props props)


(* misc properties *)
let instanceN = "instance"
let kindN = "kind"
let nameN = "name"
let serialN = "serial"

let name a = properties [(nameN, a)]

