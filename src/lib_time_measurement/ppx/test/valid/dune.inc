
; Prepreocesses test_addition_input.ml
(rule
 (targets test_addition_actual.ml)
 (deps (:pp pp.exe) (:input test_addition_input.ml))
 (action
   (run ./%{pp} --impl %{input} -o %{targets})))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_addition_output.ml test_addition_actual.ml)))

; Ensures that test_addition_input.ml compiles
(library
 (name test_addition_input)
 (modules test_addition_input)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))

; Ensures that test_addition_output.ml compiles
(library
 (name test_addition_output)
 (modules test_addition_output)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))

; Prepreocesses test_benchmark_flow_input.ml
(rule
 (targets test_benchmark_flow_actual.ml)
 (deps (:pp pp.exe) (:input test_benchmark_flow_input.ml))
 (action
   (run ./%{pp} --impl %{input} -o %{targets})))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_benchmark_flow_output.ml test_benchmark_flow_actual.ml)))

; Ensures that test_benchmark_flow_input.ml compiles
(library
 (name test_benchmark_flow_input)
 (modules test_benchmark_flow_input)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))

; Ensures that test_benchmark_flow_output.ml compiles
(library
 (name test_benchmark_flow_output)
 (modules test_benchmark_flow_output)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))

; Prepreocesses test_checkpoint_metadata_input.ml
(rule
 (targets test_checkpoint_metadata_actual.ml)
 (deps (:pp pp.exe) (:input test_checkpoint_metadata_input.ml))
 (action
   (run ./%{pp} --impl %{input} -o %{targets})))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_checkpoint_metadata_output.ml test_checkpoint_metadata_actual.ml)))

; Ensures that test_checkpoint_metadata_input.ml compiles
(library
 (name test_checkpoint_metadata_input)
 (modules test_checkpoint_metadata_input)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))

; Ensures that test_checkpoint_metadata_output.ml compiles
(library
 (name test_checkpoint_metadata_output)
 (modules test_checkpoint_metadata_output)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))

; Prepreocesses test_flush_binding_input.ml
(rule
 (targets test_flush_binding_actual.ml)
 (deps (:pp pp.exe) (:input test_flush_binding_input.ml))
 (action
   (run ./%{pp} --impl %{input} -o %{targets})))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_flush_binding_output.ml test_flush_binding_actual.ml)))

; Ensures that test_flush_binding_input.ml compiles
(library
 (name test_flush_binding_input)
 (modules test_flush_binding_input)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))

; Ensures that test_flush_binding_output.ml compiles
(library
 (name test_flush_binding_output)
 (modules test_flush_binding_output)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))

; Prepreocesses test_flush_identifier_input.ml
(rule
 (targets test_flush_identifier_actual.ml)
 (deps (:pp pp.exe) (:input test_flush_identifier_input.ml))
 (action
   (run ./%{pp} --impl %{input} -o %{targets})))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_flush_identifier_output.ml test_flush_identifier_actual.ml)))

; Ensures that test_flush_identifier_input.ml compiles
(library
 (name test_flush_identifier_input)
 (modules test_flush_identifier_input)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))

; Ensures that test_flush_identifier_output.ml compiles
(library
 (name test_flush_identifier_output)
 (modules test_flush_identifier_output)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))

; Prepreocesses test_funapp_input.ml
(rule
 (targets test_funapp_actual.ml)
 (deps (:pp pp.exe) (:input test_funapp_input.ml))
 (action
   (run ./%{pp} --impl %{input} -o %{targets})))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_funapp_output.ml test_funapp_actual.ml)))

; Ensures that test_funapp_input.ml compiles
(library
 (name test_funapp_input)
 (modules test_funapp_input)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))

; Ensures that test_funapp_output.ml compiles
(library
 (name test_funapp_output)
 (modules test_funapp_output)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))

; Prepreocesses test_lwt_addition_input.ml
(rule
 (targets test_lwt_addition_actual.ml)
 (deps (:pp pp.exe) (:input test_lwt_addition_input.ml))
 (action
   (run ./%{pp} --impl %{input} -o %{targets})))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_lwt_addition_output.ml test_lwt_addition_actual.ml)))

; Ensures that test_lwt_addition_input.ml compiles
(library
 (name test_lwt_addition_input)
 (modules test_lwt_addition_input)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))

; Ensures that test_lwt_addition_output.ml compiles
(library
 (name test_lwt_addition_output)
 (modules test_lwt_addition_output)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))

; Prepreocesses test_metadata_variable_input.ml
(rule
 (targets test_metadata_variable_actual.ml)
 (deps (:pp pp.exe) (:input test_metadata_variable_input.ml))
 (action
   (run ./%{pp} --impl %{input} -o %{targets})))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_metadata_variable_output.ml test_metadata_variable_actual.ml)))

; Ensures that test_metadata_variable_input.ml compiles
(library
 (name test_metadata_variable_input)
 (modules test_metadata_variable_input)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))

; Ensures that test_metadata_variable_output.ml compiles
(library
 (name test_metadata_variable_output)
 (modules test_metadata_variable_output)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))

; Prepreocesses test_pattern_matching_input.ml
(rule
 (targets test_pattern_matching_actual.ml)
 (deps (:pp pp.exe) (:input test_pattern_matching_input.ml))
 (action
   (run ./%{pp} --impl %{input} -o %{targets})))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_pattern_matching_output.ml test_pattern_matching_actual.ml)))

; Ensures that test_pattern_matching_input.ml compiles
(library
 (name test_pattern_matching_input)
 (modules test_pattern_matching_input)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))

; Ensures that test_pattern_matching_output.ml compiles
(library
 (name test_pattern_matching_output)
 (modules test_pattern_matching_output)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))

; Prepreocesses test_preserve_attributes_input.ml
(rule
 (targets test_preserve_attributes_actual.ml)
 (deps (:pp pp.exe) (:input test_preserve_attributes_input.ml))
 (action
   (run ./%{pp} --impl %{input} -o %{targets})))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_preserve_attributes_output.ml test_preserve_attributes_actual.ml)))

; Ensures that test_preserve_attributes_input.ml compiles
(library
 (name test_preserve_attributes_input)
 (modules test_preserve_attributes_input)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))

; Ensures that test_preserve_attributes_output.ml compiles
(library
 (name test_preserve_attributes_output)
 (modules test_preserve_attributes_output)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))

; Prepreocesses test_sub_expr_input.ml
(rule
 (targets test_sub_expr_actual.ml)
 (deps (:pp pp.exe) (:input test_sub_expr_input.ml))
 (action
   (run ./%{pp} --impl %{input} -o %{targets})))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_sub_expr_output.ml test_sub_expr_actual.ml)))

; Ensures that test_sub_expr_input.ml compiles
(library
 (name test_sub_expr_input)
 (modules test_sub_expr_input)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))

; Ensures that test_sub_expr_output.ml compiles
(library
 (name test_sub_expr_output)
 (modules test_sub_expr_output)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))

; Prepreocesses test_try_input.ml
(rule
 (targets test_try_actual.ml)
 (deps (:pp pp.exe) (:input test_try_input.ml))
 (action
   (run ./%{pp} --impl %{input} -o %{targets})))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff test_try_output.ml test_try_actual.ml)))

; Ensures that test_try_input.ml compiles
(library
 (name test_try_input)
 (modules test_try_input)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))

; Ensures that test_try_output.ml compiles
(library
 (name test_try_output)
 (modules test_try_output)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))
