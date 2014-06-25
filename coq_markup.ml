(* markup elements *)

let markup_elem elem = ((elem, []): Markup.t)


(* position *)

let offsetN = "offset"
let end_offsetN = "end_offset"
let idN = "id"


(* outer syntax *)

let comment = markup_elem "comment"
let keyword = markup_elem "keyword"
let declaration = markup_elem "declaration"
let proof_declaration = markup_elem "proof_declaration"
let qed = markup_elem "qed"
let string = markup_elem "string"
let delimiter = markup_elem "delimiter"


(* messages *)

let initN = "init"
let statusN = "status"
let reportN = "report"
let report = markup_elem reportN
let writelnN = "writeln"
let warningN = "warning"
let errorN = "error"
let protocolN = "protocol"


(* protocol message functions *)

let functionN = "function"

let ready = [(functionN, "ready")]

let assign_update = [(functionN, "assign_update")]
let removed_versions = [(functionN, "removed_versions")]


