(* TODO: Currently, this mimics the process_goal function of ide/ide_slave.ml, 
 * but we have the opportunity to provide more structure in the xml, here. *)
let process_goal sigma g =
  let open Pp in
  let env = Goal.V82.env sigma g in
  let id = Goal.uid g in
  let ccl =
    let norm_constr = Reductionops.nf_evar sigma (Goal.V82.concl sigma g) in
    let ccl_string = string_of_ppcmds (Printer.pr_goal_concl_style_env env sigma norm_constr) in
    Xml_datatype.Element ("conclusion", [], [Xml_datatype.PCData ccl_string]) in
  let process_hyp env h_env acc =
    let hyp_string = (string_of_ppcmds (Printer.pr_var_decl env sigma h_env)) in
    (Xml_datatype.Element ("hypothesis", [], [Xml_datatype.PCData hyp_string])) :: acc in
  let hyps = Xml_datatype.Element ("hypotheses", [], 
    (List.rev (Environ.fold_named_context process_hyp env ~init:[]))) in
  Xml_datatype.Element ("goal", [("id", id)], [hyps; ccl])

let feedback_structured_goals state_id { Stm.proof } =
  try
      let pfts = Proof_global.proof_of_state proof in
      let structured_goals = Proof.map_structured_proof pfts process_goal in
      let xml_bg_goal = fun (l, r) -> Xml_datatype.Element("bg_goal", [], 
          [Xml_datatype.Element("left_bg_goals", [], l); 
           Xml_datatype.Element("right_bg_goals", [], r)]) in

      let xml_structured_goals = Xml_datatype.Element("goals", [],
        [ Xml_datatype.Element("focussed_goals", [], 
            structured_goals.Proof.fg_goals);
          Xml_datatype.Element("bg_goals", [], 
            List.map xml_bg_goal structured_goals.Proof.bg_goals);
          Xml_datatype.Element("shelved_goals", [], 
            structured_goals.Proof.shelved_goals);
          Xml_datatype.Element("given_up_goals", [], 
            structured_goals.Proof.given_up_goals)
        ]
      ) in
      Pp.feedback ~state_id
       (Feedback.Custom(Loc.ghost, "structured_goals",xml_structured_goals));
      Pp.feedback ~state_id
        (Feedback.Goals
          (Loc.ghost, Pp.string_of_ppcmds
            (Printer.pr_open_subgoals
              ~proof:pfts ())))
  with Proof_global.NoCurrentProof -> ()
