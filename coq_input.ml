let read_command () = Yxml.read_command ()

let obtain_edits edits_yxml =
  Pide_xml.Decode.(list (pair 
    string
    (variant
     [(function
       | [], edits -> Pide_document.Edits(list (pair (option int) (option int)) edits)
       | _ -> assert false);
      (function  (* Document.Node.deps *)
       | [], a -> Pide_document.Edits []
       | _ -> assert false);
      (function
       | (_ :: perspective), overlays ->
        (* Ignored head of list is a 'node-required' argument for Isabelle *)
           Pide_document.Perspective (List.map (fun cmd_id -> int_of_string cmd_id) perspective)
       | _ -> assert false)])) 
    (Yxml.parse_body edits_yxml))
