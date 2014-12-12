module type Printer = sig
  val print_func : int -> Feedback.route_id -> Feedback.feedback_content -> unit
end

exception Unhandled
