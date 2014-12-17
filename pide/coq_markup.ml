(* markup elements *)
let markup_elem elem = ((elem, []): Markup.t)


(* position *)

let offsetN = "offset"
let end_offsetN = "end_offset"
let idN = "id"


(* outer syntax *)

let comment = markup_elem "comment"
let keyword1 = markup_elem "keyword1"
let declaration = markup_elem "declaration"
let proof_declaration = markup_elem "proof_declaration"
let qed = markup_elem "qed"
let string = markup_elem "string"
let delimiter = markup_elem "delimiter"


(* messages *)

let initN = "init"
let entityN = "entity"
let statusN = "status"
let reportN = "report"
let report = markup_elem reportN
let resultN = "result"
let writelnN = "writeln"
let warningN = "warning"
let errorN = "error"
let protocolN = "protocol"


(* protocol message functions *)

let functionN = "function"

let ready = [(functionN, "ready")]

let assign_update = [(functionN, "assign_update")]
let removed_versions = [(functionN, "removed_versions")]


(* Status messages *)
let childless_node n = [Xml_datatype.Element (n, [], [])] 
let status_running = childless_node "running"
let status_finished = childless_node "finished"
let status_finished_element = Xml_datatype.Element(statusN, [], status_finished)

type entry_location =
  | Local of int
  | ExtFile of string

let entity ref_id ref_loc def_id def_loc name kind =
  let ref_offset, ref_end_offset = Loc.unloc ref_loc in
  let def_offset, def_end_offset = Loc.unloc def_loc in
  let def_location = match def_id with
    | Local dest_id -> "def_id", (string_of_int dest_id)
    | ExtFile fname -> "def_file", fname
  in
  let attrs = 
    ["id", string_of_int ref_id;
     "offset", string_of_int (ref_offset + 1);
     "end_offset", string_of_int (ref_end_offset + 1);
     def_location;
     "def_offset", string_of_int (def_offset + 1);
     "def_end_offset", string_of_int (def_end_offset + 1);
     "name", name;
     "kind", kind]
  in
  [Xml_datatype.Element (entityN, attrs, [])]

