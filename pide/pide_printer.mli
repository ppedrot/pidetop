(** Printers *)
module type Printer = sig
  (* Printers are modular: they contain a number of related functions related
   * to print asynchronously.
   * This is mainly to allow printers to be defined in external libraries.
   *)
  val print_func: int -> Feedback.route_id -> Feedback.feedback_content -> unit

  (* Print functions should throw an Unhandled exception when they are not able to
   * deal with a feedback
   *)
end

module type Printer_spec = sig
  val can_print: Feedback.feedback_content -> bool

  val make_pos: Feedback.feedback_content -> int -> Position.t

  (* Can have side effects *)
  val make_body: Feedback.feedback_content -> Xml_datatype.xml option


  val output_function: Feedback.feedback_content ->
      Position.t -> Xml_datatype.xml list -> unit
end


module Make_printer(P: Printer_spec) : Printer


exception Unhandled
