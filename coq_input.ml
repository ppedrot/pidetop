let read_command () = Yxml.read_command ()

let obtain_edits edits_yxml =
  Pide_xml.Decode.(list (pair 
    string
    (variant
     [(function
       | [], a -> Pide_document.Edits(list (pair (option int) (option int)) a)
       | _ -> assert false);
      (function
       | [], a -> Pide_document.Edits []
       | _ -> assert false);
      (function
       | [a], [] -> Pide_document.Edits []
       | _ -> assert false);
      (function
       | a, [] -> Pide_document.Edits []
       | _ -> assert false)])) 
    (Yxml.parse_body edits_yxml))
