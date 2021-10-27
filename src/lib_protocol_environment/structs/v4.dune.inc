(rule
 (targets v4.ml)
 (deps
   v0/error_monad_trace_eval.ml
   v4/hex.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{libexec:tezos-protocol-environment-packer:s_packer} "structs" %{deps})))))

