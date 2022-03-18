(rule
 (targets v6.ml)
 (deps
   v0/error_monad_infix_globals.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{libexec:tezos-protocol-environment:s_packer} "structs" %{deps})))))
