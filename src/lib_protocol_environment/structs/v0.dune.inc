(rule
 (targets v0.ml)
 (deps
   v0/mBytes.ml
 )

(action (with-stdout-to %{targets} (chdir %{workspace_root}}
 (run %{exe:../structs_packer/structs_packer.exe} %{deps})))))
