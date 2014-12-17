module Rest_printer_s = struct
 (* : (module Pide_printer.Printer_spec) = (module struct*)
  let can_print = function
    | Feedback.Processed | Feedback.Message _ -> true
    | _ -> false

   let make_body = function
    | Feedback.Processed -> Coq_markup.status_finished_element
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
end

let () =
  let module P = Pide_printer.Make_printer(Rest_printer_s) in
  Pide_document.install_printer (module P)
