(*** YXML transfer syntax ***)
(* markers *)

let char_X = '\005'
let char_Y = '\006'

let str_X = String.make 1 char_X
let str_Y = String.make 1 char_Y

let str_XY = str_X ^ str_Y
let str_XYX = str_XY ^ str_X

let detect s = String.contains s char_X || String.contains s char_Y


(* ML basics *)

let (|>) x f = f x
let (@>) f g x = g (f x)

let rec fold f list y =
  match list with
    [] -> y
  | x :: xs -> fold f xs (f x y)


(* output *)

let implode = String.concat ""
let content xs = implode (List.rev xs)
let add x xs = if x = "" then xs else x :: xs

let no_output = ("", "")

let output_markup (name, atts) =
  if name = "" then no_output
  else
    (str_XY ^ name ^ implode (List.map (fun (a, x) -> str_Y ^ a ^ "=" ^ x) atts) ^ str_X, str_XYX)

let markup m txt =
  let (bg, en) = output_markup m
  in bg ^ txt ^ en

let markup_only m = markup m ""

let string_of_body body =
  let attrib (l, v) = add str_Y @> add l @> add "=" @> add v in
  let rec tree = function
    | Xml_datatype.Element (name, atts, children) ->
        add str_XY @> add name @> fold attrib atts @> add str_X @>
        trees children @>
        add str_XYX
    | Xml_datatype.PCData s -> add s
  and trees ts = fold tree ts
  in content (trees body [])
let string_of tree = string_of_body [tree]



(** parsing *)

(* split *)

let split fields sep str =
  let cons i n result =
    if i = 0 && n = String.length str && n > 0 then str :: result
    else if n > 0 then String.sub str i n :: result
    else if fields then "" :: result
    else result
  in
  let rec explode i result =
    let j = try String.index_from str i sep with Not_found -> -1 in
      if j >= 0 then explode (j + 1) (cons i (j - i) result)
      else List.rev (cons i (String.length str - i) result)
  in explode 0 []


(* parse *)

let err msg = raise (Failure ("Malformed YXML: " ^ msg))
let err_attribute () = err "bad attribute"
let err_element () = err "bad element"
let err_unbalanced name =
  if name = "" then err "unbalanced element"
  else err ("unbalanced element \"" ^ name ^ "\"")

let parse_attrib s =
  try
    let i = String.index s '=' in
    let _ = if i = 0 then err_attribute () in
    let j = i + 1 in
      (String.sub s 0 i, String.sub s j (String.length s - j))
  with Not_found -> err_attribute ()

let parse_body source =

  (* stack operations *)

  let add x = function
    | [] -> assert false
    | (elem, body) :: pending -> (elem, x :: body) :: pending
  in

  let push name atts pending =
    if name = "" then err_element ()
    else ((name, atts), []) :: pending
  in

  let pop = function
    | [] -> assert false
    | ((name, atts) , body) :: pending ->
        if name = "" then err_unbalanced ""
        else add (Xml_datatype.Element (name, atts, List.rev body)) pending
  in

  (* parse chunks *)

  let rev_rev_map f l = List.rev (List.rev_map f l) in
  let chunks = split false char_X source |> rev_rev_map (split true char_Y) in

  let parse_chunk = function
    | [] -> assert false
    | [""; ""] -> pop
    | ("" :: name :: atts) -> push name (List.map parse_attrib atts)
    | txts -> fold (fun s -> add (Xml_datatype.PCData s)) txts
  in
  match fold parse_chunk chunks [(("", []), [])] with
  | [] -> assert false
  | [(("", _), result)] -> List.rev result
  | ((name, _), _) :: _ -> err_unbalanced name

let parse source =
  match parse_body source with
  | [result] -> result
  | [] -> Xml_datatype.PCData ""
  | _ -> err "multiple results"

let m = Mutex.create ()

let yxml_send header body =
  Mutex.lock m;
  try 
    let hdr = string_of header in
    let bdy = string_of_body body in
    Channel.send hdr bdy; 
    Mutex.unlock m
  with e -> let e = Errors.push e in Mutex.unlock m; Util.iraise e

let read_command () = Channel.read_command () 

let initialize () =
  Channel.initialize ()
