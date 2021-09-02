(rule
 (targets v3.ml)
 (deps
   v1/bls12_381.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{libexec:tezos-protocol-environment-packer:s_packer} "structs" %{deps})))))
