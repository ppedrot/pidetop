let use_me () = ()

VERNAC COMMAND EXTEND PideFeedbackGoals CLASSIFIED AS QUERY
| [ "PideFeedbackGoals" ] -> [ Pide_goalprint.feedback_structured_goals () ]
END
