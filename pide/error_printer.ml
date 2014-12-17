module Error_printer: (Pide_printer.Printer_spec) = struct
  let can_print = function
    | Feedback.ErrorMsg _ -> true
    | _ -> false

  let output_function = function
    | Feedback.ErrorMsg _ -> Coq_output.error_msg ?props:None
    | _ -> raise Pide_printer.Unhandled

  let make_pos = function
    | Feedback.ErrorMsg (loc, _) -> Position.of_loc loc
    | _ -> raise Pide_printer.Unhandled

  let make_body _ = function
    | Feedback.ErrorMsg (loc, txt) ->
        Some (Xml_datatype.Element(Coq_markup.errorN, [],
          Pide_xml.Encode.string txt))
    | _ -> raise Pide_printer.Unhandled
end

let () =
  let module P = Pide_printer.Make_printer(Error_printer) in
  Pide_document.install_printer(module P)
