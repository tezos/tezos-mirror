(rule
 (targets v3.ml)
 (deps
   v3/data_encoding.ml
   v3/replicated_signatures.ml
   v3/lwtreslib_list_combine.ml
   v1/bls12_381.ml
   v1/error_monad_preallocated_values.ml
   v1/hex.ml
   v0/error_monad_infix_globals.ml
   v0/error_monad_trace_eval.ml
   v0/error_monad_classification.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{libexec:tezos-protocol-environment:s_packer} "structs" %{deps})))))
