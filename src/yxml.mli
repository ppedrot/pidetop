(*
Efficient text representation of XML trees using extra characters X
and Y -- no escaping, may nest marked text verbatim.  Suitable for
direct inlining into plain text.

Markup <elem att="val" ...>...body...</elem> is encoded as:

  X Y name Y att=val ... X
  ...
  body
  ...
  X Y X
*)

(* TODO: See how much these are used outside of Yxml module. *)
val implode: string list -> string
val split: bool -> char -> string -> string list
val no_output: string * string
val output_markup: Markup.t -> string * string
val markup: Markup.t -> string -> string
val markup_only: Markup.t -> string
val parse_body: string -> Pide_xml.body
val parse: string -> Pide_xml.tree

val initialize : unit -> unit
val yxml_send: Xml_datatype.xml -> Pide_xml.body -> unit

val read_command : unit -> string list option

val string_of_body : Xml_datatype.xml list -> string
