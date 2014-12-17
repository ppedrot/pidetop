let error_printer: (module Pide_printer.Printer) = (module struct
  let print_func id route = function
  | Feedback.ErrorMsg (loc, txt) ->
    let pos = Position.of_loc loc id in
    (* TODO: Factor this out to generic code... *)
    if route <> Feedback.default_route then begin
      Coq_output.result pos route Coq_markup.status_finished;
      let message_body =
        Xml_datatype.Element(Coq_markup.errorN, [], Pide_xml.Encode.string txt) in
      Coq_output.result pos route [message_body]
    end
    else begin
      Coq_output.status pos Coq_markup.status_finished;
      Coq_output.error_msg pos (Pide_xml.Encode.string txt)
    end
  | _ -> raise Pide_printer.Unhandled
end)

let () = Pide_document.install_printer error_printer
