open Pide_xml
open Coq_markup
open Position

let send_message ch props body =
  let xml_header = Elem ((ch, props), []) in
  Yxml.yxml_send xml_header body

let counter () =
  let count = ref 0 in
  fun () ->
    let i = !count + 1 in
    if (i > 0) then (count := i; i)
    else raise (Failure "Counter overflow")

let serial = counter ()

let standard_message ch make_serial pos body = 
  if body <> "" then
    let properties_pos = (properties_of pos) in
    let properties = if make_serial then Properties.put (Markup.serialN, string_of_int (serial ()))
                                                        properties_pos 
                                    else properties_pos in
    send_message ch properties (Encode.string body)


let init_message message =
  let xml_message = Encode.string message in
  send_message initN [] xml_message

let writeln = standard_message writelnN true 
let error_msg pos = standard_message errorN true pos 
let report pos body = standard_message reportN false pos  (Yxml.implode body)
let protocol_message = send_message protocolN

let assignment_message new_id assignment =
  let body = Pide_xml.Encode.(
    let stateid x = int (Stateid.to_int x) in
    pair int (list (pair int (list stateid))) (new_id, assignment))
  in protocol_message assign_update body
