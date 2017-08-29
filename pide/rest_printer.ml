module Rest_printer_s = struct
  let can_print = function
    | Feedback.Message _ | Feedback.Processed | Feedback.AddedAxiom -> true
    | _ -> false

  let make_pos = function
    | Feedback.Message _ | Feedback.Processed | Feedback.AddedAxiom ->
        Position.id_only
    | _ -> raise Pide_printer.Unhandled

   let make_body _ = function
    | Feedback.Processed -> Some(Coq_markup.status_finished_element)
    | Feedback.AddedAxiom ->
        Some(Xml_datatype.Element(Coq_markup.warningN, [],
          (Pide_xml.Encode.string "Axiom added.")))
    | Feedback.Message (_, _, s) ->
        Some(Xml_datatype.Element(Coq_markup.writelnN, [],
          Pide_xml.Encode.string (Pp.string_of_ppcmds s)))
    | _ -> raise Pide_printer.Unhandled

  let props = Properties.put ("source", "query") Properties.empty
  let output_function = function
    | Feedback.Processed -> Coq_output.status
    | Feedback.Message _ -> Coq_output.writeln ~props:props
    | Feedback.AddedAxiom -> Coq_output.warning_msg ?props:None
    | _ -> raise Pide_printer.Unhandled
end

let () =
  let module P = Pide_printer.Make_printer(Rest_printer_s) in
  Pide_document.install_printer (module P)
