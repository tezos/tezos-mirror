(rule
 (targets v4.ml)
 (deps
   v3/lwtreslib_list_combine.ml
   v4/hex.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{libexec:tezos-protocol-environment-packer:s_packer} "structs" %{deps})))))

