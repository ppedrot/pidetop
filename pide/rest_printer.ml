let rest_printer : (module Pide_printer.Printer) = (module struct
  let print_func id route = function
  | Feedback.Processed ->
      let position = Position.id_only id in
      Coq_output.status position Coq_markup.status_finished
  | Feedback.AddedAxiom ->
      let position = Position.id_only id in
      Coq_output.warning_msg position "Axiom added."
  | Feedback.Message { Feedback.message_content = s } ->
      let position = Position.id_only id in
      (* TODO: Factor this out... *)
      if route <> Feedback.default_route then begin
        Coq_output.result position route Coq_markup.status_finished;
        let message_body =
          Xml_datatype.Element(Coq_markup.writelnN, [], Pide_xml.Encode.string s) in
        Coq_output.result position route [message_body]
      end
      else begin
        let source = Properties.put ("source", "query") Properties.empty in
        Coq_output.writeln position ~props:source s
      end
  | _ -> raise Pide_printer.Unhandled
end)

let () = Pide_document.install_printer rest_printer
