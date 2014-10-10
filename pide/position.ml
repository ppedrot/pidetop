type t = int * int * string  (*start/end offset counting from 1, id*)

let make_id i j id = (i, j, id)
let make i j = make_id i j ""

let none = make 0 0

let id_only id = make_id 0 0 id


(* properties *)

let valid i = i > 0

let value k i = if valid i then [(k, string_of_int i)] else []

let properties_of (i, j, id) =
  value Coq_markup.offsetN i @
  value Coq_markup.end_offsetN j @
  (if id = "" then [] else [(Coq_markup.idN, id)])

let markup pos = Markup.properties (properties_of pos)


(* reports *)

type report = t * Markup.t

let is_reported (i, j, id) = valid i  (* FIXME id!? *)

let reported_text (pos, m) txt =
  if is_reported pos then Yxml.markup (markup pos m) txt else "";


