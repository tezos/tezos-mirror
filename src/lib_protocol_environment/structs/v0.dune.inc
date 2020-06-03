(rule
 (targets v0.ml)
 (deps
   v0/mBytes.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{exe:../s_packer/s_packer.exe} "structs" %{deps})))))
