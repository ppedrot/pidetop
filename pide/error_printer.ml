module Error_printer: (Pide_printer.Printer_spec) = struct
  let can_print = function
    | Feedback.Message(Feedback.Error, _, _) -> true
    | _ -> false

  let output_function = function
    | Feedback.Message(Feedback.Error, _, _) -> Coq_output.error_msg ?props:None
    | _ -> raise Pide_printer.Unhandled

  let make_pos = function
    | Feedback.Message(Feedback.Error, loc, _) -> Position.of_loc (Option.default Loc.ghost loc)
    | _ -> raise Pide_printer.Unhandled

  let make_body _ = function
    | Feedback.Message(Feedback.Error, _loc, msg) ->
      let txt = Richpp.raw_print msg in
        if Str.string_match (Str.regexp "^[\t\r\n ]*User interrupt\\.") txt 0
        then
          None
        else
          Some (Xml_datatype.Element(Coq_markup.errorN, [],
            Pide_xml.Encode.string txt))
    | _ -> raise Pide_printer.Unhandled
end

let () =
  let module P = Pide_printer.Make_printer(Error_printer) in
  Pide_document.install_printer(module P)
