let rest_printer : (module Pide_printer.Printer) = (module struct
  let can_print = function
    | Feedback.Processed | Feedback.Message _ -> true
    | _ -> false

  let status_element = Xml_datatype.Element(Coq_markup.statusN, [],
    Coq_markup.status_finished)

  let make_body = function
    | Feedback.Processed -> status_element
    | Feedback.Message { Feedback.message_content = s } ->
        Xml_datatype.Element(Coq_markup.writelnN, [], Pide_xml.Encode.string s)
    | _ -> raise Pide_printer.Unhandled

  let props = Properties.put ("source", "query") Properties.empty
  let output_function = function
    | Feedback.Processed -> Coq_output.status
    | Feedback.Message _ -> Coq_output.writeln ~props:props
    | _ -> raise Pide_printer.Unhandled

  let children = function
    | Xml_datatype.Element(_, _, c) -> c
    | _ -> []

  let print_func id route content =
    let position = Position.id_only id in
    let body = make_body content in
    if route = Feedback.default_route then
      (* Main result *)
      (output_function content) position  (children body)
    else begin
      (* Query result *)
      Coq_output.result position route [status_element];
      Coq_output.result position route [body]
    end
end)

let () = Pide_document.install_printer rest_printer
