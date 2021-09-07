(rule
 (targets v4.ml)
 (deps
   v1/error_monad_preallocated_values.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{libexec:tezos-protocol-environment-packer:s_packer} "structs" %{deps})))))

