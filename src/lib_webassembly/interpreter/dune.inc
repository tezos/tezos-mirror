(include_subdirs unqualified)

(subdir
 text
 (rule
  (target lexer.ml)
  (deps lexer.mll)
  (action
   (chdir
    %{workspace_root}
    (run %{bin:ocamllex} -ml -q -o %{target} %{deps}))))
 (ocamlyacc
  (modules parser)))

(env
 (_
  (flags
   (-w +a-4-27-42-44-45 -warn-error +a-3))))
