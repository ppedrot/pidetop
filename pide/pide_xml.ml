open Xml_datatype
(*** Untyped XML trees and typed representation of ML values ***)
module type Xml_data_ops = 
sig
  type 'a a
  type 'a t
  type 'a v
  val int_atom: int a
  val bool_atom: bool a
  val unit_atom: unit a
  val properties: (string * string) list t
  val string: string t
  val int: int t
  val bool: bool t
  val unit: unit t
  val pair: 'a t -> 'b t -> ('a * 'b) t
  val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val list: 'a t -> 'a list t
  val option: 'a t -> 'a option t
  val variant: 'a v list -> 'a t
end

module type Pide_xml = 
sig
  type tree = xml
  type body = tree list
  exception XML_Atom of string
  exception XML_Body of tree list

  module Encode: Xml_data_ops with
    type 'a a = 'a -> string and
    type 'a t = 'a -> body and
    type 'a v = 'a -> string list * body
  module Decode: Xml_data_ops with
    type 'a a = string -> 'a and
    type 'a t = body -> 'a and
    type 'a v = string list * body -> 'a
end

type tree = xml
type body = tree list

let map_index f =
  let rec mapp i = function
    | [] -> []
    | x :: xs -> f (i, x) :: mapp (i + 1) xs
  in mapp 0

exception XML_Atom of string
exception XML_Body of tree list

module Encode =
struct

type 'a a = 'a -> string
type 'a t = 'a -> body
type 'a v = 'a -> string list * body


(* atomic values *)

let int_atom = string_of_int

let bool_atom = function false -> "0" | true -> "1"

let unit_atom () = ""


(* structural nodes *)

let node ts = Element (":", [], ts)

let vector = map_index (fun (i, x) -> (int_atom i, x))

let tagged (tag, (xs, ts)) : Xml_datatype.xml = Element (int_atom tag, vector xs, ts)


(* representation of standard types *)

let properties props = [Element (":", props, [])]

let string = function "" -> [] | s -> [PCData s]

let int i = string (int_atom i)

let bool b = string (bool_atom b)

let unit () = string (unit_atom ())

let pair f g (x, y) = [node (f x); node (g y)]

let triple f g h (x, y, z) = [node (f x); node (g y); node (h z)]

let list f xs = List.map (fun x -> node (f x)) xs

let option f = function None -> [] | Some x -> [node (f x)]

let variant fns x =
  let rec get_index i = function
    | [] -> raise (Failure "XML.Encode.variant")
    | f :: fs -> try (i, f x) with Match_failure _ -> get_index (i + 1) fs
  in [tagged (get_index 0 fns)]

end


module Decode =
struct

type 'a a = string -> 'a
type 'a t = body -> 'a
type 'a v = string list * body -> 'a


(* atomic values *)

let int_atom s =
  try int_of_string s
    with Invalid_argument _ -> raise (XML_Atom s)

let bool_atom = function
  | "0" -> false
  | "1" -> true
  | s -> raise (XML_Atom s)

let unit_atom s =
  if s = "" then () else raise (XML_Atom s)


(* structural nodes *)
let node = function
  | Element (":", [], ts) -> ts
  | t -> raise (XML_Body [t])

let vector =
  map_index (function (i, (a, x)) -> if int_atom a = i then x else raise (XML_Atom a))

let tagged = function
  | Element (name, atts, ts) -> (int_atom name, (vector atts, ts))
  | t -> raise (XML_Body [t])


(* representation of standard types *)

let properties = function
  | [Element (":", props, [])] -> props
  | ts -> raise (XML_Body ts)

let string = function
  | [] -> ""
  | [PCData s] -> s
  | ts -> raise (XML_Body ts)

let int ts = int_atom (string ts)

let bool ts = bool_atom (string ts)

let unit ts = unit_atom (string ts)

let pair f g = function
  | [t1; t2] -> (f (node t1), g (node t2))
  | ts -> raise (XML_Body ts)

let triple f g h = function
  | [t1; t2; t3] -> (f (node t1), g (node t2), h (node t3))
  | ts -> raise (XML_Body ts)

let list f = List.map (fun t -> f (node t))

let option f = function
  | [] -> None
  | [t] -> Some (f (node t))
  | ts -> raise (XML_Body ts)

let variant fs = function
  | [t] ->
      let (tag, (xs, ts)) = tagged t in
      let f = try List.nth fs tag with Invalid_argument _ -> raise (XML_Body [t]) in
      f (xs, ts)
  | ts -> raise (XML_Body ts)

end
