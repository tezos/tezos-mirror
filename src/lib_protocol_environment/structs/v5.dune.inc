(rule
 (targets v5.ml)
 (deps
   v4/hex.ml
   v3/lwtreslib_list_combine.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{libexec:tezos-protocol-environment-packer:s_packer} "structs" %{deps})))))
