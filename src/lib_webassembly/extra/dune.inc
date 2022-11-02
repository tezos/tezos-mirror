(rule
 (target lexer.ml)
 (deps lexer.mll)
 (action
  (chdir
   %{workspace_root}
   (run %{bin:ocamllex} -ml -q -o %{target} %{deps}))))

(ocamlyacc
 (modules parser))
