module type Printer = sig
  val print_func : int -> Feedback.route_id -> Feedback.feedback_content -> unit
end

module type Printer_spec = sig
  val can_print: Feedback.feedback_content -> bool

  val make_pos: Feedback.feedback_content -> int -> Position.t

  (* Can have side effects *)
  val make_body: int -> Feedback.feedback_content -> Xml_datatype.xml option

  val output_function: Feedback.feedback_content ->
    Position.t -> Xml_datatype.xml list -> unit
end

module Make_printer(P: Printer_spec) = struct
  open P

  let children = function
    | Xml_datatype.Element(_, _, c) -> c
    | _ -> []

  let print_func id route content =
    if can_print content then begin
      match make_body id content with
      | Some body ->
        let position = make_pos content id in
        if route = Feedback.default_route then
          (* Main result *)
          (output_function content) position (children body)
        else begin
          (* Query result *)
          Coq_output.result position route [Coq_markup.status_finished_element];
          Coq_output.result position route [body]
        end
      | None -> ()
    end
end


exception Unhandled
