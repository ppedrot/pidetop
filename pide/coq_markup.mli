val offsetN: string
val end_offsetN: string
val idN: string
val comment: Markup.t
val keyword1: Markup.t
val declaration: Markup.t
val proof_declaration: Markup.t
val qed: Markup.t
val string: Markup.t
val delimiter: Markup.t
val initN: string
val statusN: string
val reportN: string
val report: Markup.t
val resultN: string
val writelnN: string
val warningN: string
val errorN: string
val protocolN: string
val functionN: string
val ready: Properties.t
val assign_update: Properties.t
val removed_versions: Properties.t

val status_running: Xml_datatype.xml list
val status_running_element : Xml_datatype.xml
val status_finished: Xml_datatype.xml list
val status_finished_element: Xml_datatype.xml


type entry_location =
  | Local of int
  | ExtFile of string

val entity: int -> Loc.t -> (* The refering entity + offsets *)
            entry_location -> Loc.t -> (* The location of the entity, including offsets *)
            string -> string ->  (* The name and kind of the entity. *)
            Xml_datatype.xml list
