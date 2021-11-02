
; Prepreocesses test_invalid_payload_input.ml
(rule
 (targets test_invalid_payload_actual)
 (deps (:pp pp.exe) (:input test_invalid_payload_input.ml))
 (action
  (with-stderr-to
    %{targets}
   (bash "./%{pp} -no-color --impl %{input} || true"))))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_invalid_payload_output test_invalid_payload_actual)))

; Prepreocesses test_more_than_once_input.ml
(rule
 (targets test_more_than_once_actual)
 (deps (:pp pp.exe) (:input test_more_than_once_input.ml))
 (action
  (with-stderr-to
    %{targets}
   (bash "./%{pp} -no-color --impl %{input} || true"))))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_more_than_once_output test_more_than_once_actual)))

; Prepreocesses test_no_payload_input.ml
(rule
 (targets test_no_payload_actual)
 (deps (:pp pp.exe) (:input test_no_payload_input.ml))
 (action
  (with-stderr-to
    %{targets}
   (bash "./%{pp} -no-color --impl %{input} || true"))))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_no_payload_output test_no_payload_actual)))
