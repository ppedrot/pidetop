open Coq_markup
open Position

type standard_message_t = Position.t -> ?props: Properties.t ->
  Xml_datatype.xml list -> unit

let send_message ch props body =
  let xml_header = Xml_datatype.Element (ch, props, []) in
  Yxml.yxml_send xml_header body

let counter () =
  let count = ref 0 in
  fun () ->
    let i = !count + 1 in
    if (i > 0) then (count := i; i)
    else raise (Failure "Counter overflow")

let serial = counter ()

let standard_message ch make_serial pos
  ?props:(p=Properties.empty) body =
  if body <> [] then
    let properties_pos = properties_of pos in
    let properties = Properties.append properties_pos p in
    let properties =
      if make_serial then
        Properties.put (Markup.serialN, string_of_int (serial ())) properties
      else properties in
    send_message ch properties body


let init_message message =
  let xml_message = Pide_xml.Encode.string message in
  send_message initN [] xml_message

let writeln = standard_message writelnN true 

let result pos instance message_body =
  let properties = properties_of pos in
  let properties = Properties.put (Markup.serialN, string_of_int (serial ()))
                                  properties in
  let properties = Properties.put (Markup.instanceN, string_of_int instance)
                                  properties in
  send_message resultN properties message_body

let error_msg pos = standard_message errorN true pos 
let warning_msg pos = standard_message warningN true pos
let report pos = standard_message reportN false pos ?props:None
let status pos = standard_message statusN false pos ?props:None
let protocol_message = send_message protocolN

let assignment_message new_id assignment =
  let body = Pide_xml.Encode.(
    let stateid x = int (Stateid.to_int x) in
    pair int (list (pair int (list stateid))) (new_id, assignment))
  in protocol_message assign_update body

let removed_versions_message ids =
  let body = Pide_xml.Encode.(
    list int ids)
  in protocol_message removed_versions body
