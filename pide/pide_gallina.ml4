let use_me () = ()

VERNAC COMMAND EXTEND PideFeedbackGoals CLASSIFIED AS QUERY
| [ "PideFeedbackGoals" ] -> [
  try
    Pide_goalprint.feedback_structured_goals (Proof_global.give_me_the_proof ())
  with Proof_global.NoCurrentProof -> ()
]
END
