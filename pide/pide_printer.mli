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

exception Unhandled
