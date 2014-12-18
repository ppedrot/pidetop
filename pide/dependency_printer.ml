module Dependency_printer : (Pide_printer.Printer_spec) = struct
  let can_print = function
    | Feedback.FileDependency _ -> true
    | _ -> raise Pide_printer.Unhandled

  let make_pos _ = Position.id_only
  let output_function _ _ _= () (* TODO! *)
  let make_body _ = function
  | Feedback.FileDependency (from, depends_on) -> (* TODO! *) None
  | _ -> raise Pide_printer.Unhandled
end

let () =
  let module P = Pide_printer.Make_printer(Dependency_printer) in
  Pide_document.install_printer (module P)
