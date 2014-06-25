let read_command () = Yxml.read_command ()

let obtain_edits edits_yxml =
  Pide_xml.Decode.(list (pair 
    string
    (variant
     [(fun ([], a) -> Pide_document.Edits(list (pair (option int) (option int)) a));
      (fun ([], a) -> Pide_document.Edits []);
      (fun ([a], []) -> Pide_document.Edits []);
      (fun (a, []) -> Pide_document.Edits [])])) 
    (Yxml.parse_body edits_yxml))
