let dependency_printer : (module Pide_printer.Printer) = (module struct
  let print_func id route = function
  | Feedback.FileDependency (from, depends_on) -> (* TODO! *) ()
  | _ -> raise Pide_printer.Unhandled
end)

let () = Pide_document.install_printer dependency_printer
