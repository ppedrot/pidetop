let glob_printer : (module Pide_printer.Printer) = (module struct
  (* TODO: These definitions are yanked from the Coqdoc implementation.
   * It is probably a good idea to be more principled, and factor the functions
   * into some shared library with Coq...
   *)
  type entry_type =
  | Library
  | Module
  | Definition
  | Inductive
  | Constructor
  | Lemma
  | Record
  | Projection
  | Instance
  | Class
  | Method
  | Variable
  | Axiom
  | TacticDefinition
  | Abbreviation
  | Notation
  | Section

  let type_of_string = function
    | "def" | "coe" | "subclass" | "canonstruc" | "fix" | "cofix"
    | "ex" | "scheme" -> Definition
    | "prf" | "thm" -> Lemma
    | "ind" | "variant" | "coind" -> Inductive
    | "constr" -> Constructor
    | "indrec" | "rec" | "corec" -> Record
    | "proj" -> Projection
    | "class" -> Class
    | "meth" -> Method
    | "inst" -> Instance
    | "var" -> Variable
    | "defax" | "prfax" | "ax" -> Axiom
    | "syndef" -> Abbreviation
    | "not" -> Notation
    | "lib" -> Library
    | "mod" | "modtype" -> Module
    | "tac" -> TacticDefinition
    | "sec" -> Section
    | s -> invalid_arg ("type_of_string:" ^ s)

  module S = struct type t = entry_type * string * string let compare = compare end
  module M = CMap.Make(S)

  let def_map : (Loc.t * Coq_markup.entry_location) M.t ref = ref (M.empty)

  let lookup m k cont =
    (try cont (M.find k !m)
    with Not_found -> ())

  (* TODO: Basically the same as in tools/coqdoc/index.ml; except no refs. *)
  let load_globs (f: string) (id: int) =
    let bare_name = Filename.chop_extension f in
    let glob_name = bare_name ^ ".glob" in
    let v_name = bare_name ^ ".v" in
    try
      let c = open_in glob_name in
      try
        while true do
          let s = input_line c in
          try Scanf.sscanf s "%s %d:%d %s %s"
            (fun ty loc1 loc2 secpath name ->
               let loc = Loc.make_loc (loc1, loc2) in
               let typ = type_of_string ty in
               def_map := M.add (typ, name,  secpath) (loc, Coq_markup.ExtFile v_name) !def_map)
          with Scanf.Scan_failure _ | End_of_file -> ()
        done
      with End_of_file ->
        close_in c
    with Sys_error s ->
      Coq_output.warning_msg (Position.id_only id)
        (Pide_xml.Encode.string ("Warning: " ^ glob_name ^
         ": No such file or directory (links will not be available)"))

  let print_func id route = function
  | Feedback.FileLoaded(dirname, filename) ->
      load_globs filename id
  | Feedback.GlobDef (loc, name, secpath, ty) ->
      let typ = type_of_string ty in
      def_map := M.add (typ, name, secpath) (loc, Coq_markup.Local id) !def_map
  | Feedback.GlobRef (loc, _fp, mp, name, ty) ->
      let typ = type_of_string ty in
      lookup def_map (typ, name, mp) (fun (dest, dest_id) ->
        let position = Position.of_loc loc id in
        Coq_output.report position (Coq_markup.entity id loc dest_id dest name ty)
      )
  | _ -> raise Pide_printer.Unhandled
end)

let () = Pide_document.install_printer glob_printer
