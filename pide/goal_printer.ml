let goal_printer: (module Pide_printer.Printer) = (module struct
  let already_printed = ref Int.Set.empty
  let print_func id route = function
  | Feedback.Custom(loc,"structured_goals",goals) ->
      let pos = Position.of_loc loc id in
      Coq_output.report pos [goals]

  | Feedback.Goals (loc,goalstate) ->
      (if Int.Set.mem id !already_printed then ()
       else (
         already_printed := Int.Set.add id !already_printed;
         let pos = Position.of_loc loc id in
         let source = Properties.put ("source", "goal") Properties.empty in
         Coq_output.writeln pos ~props:source (Pide_xml.Encode.string goalstate)))
  | _ -> raise Pide_printer.Unhandled
end)

let () = Pide_document.install_printer goal_printer
