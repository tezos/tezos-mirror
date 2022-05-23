; This file is used because expressing the same in the package manifest (located
; in mainfest/main.ml) not very nice.

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

(rule
 ; Run the WebAssembly core tests.
 ; See src/lib_webassembly/test/core/README.md for details.
 (alias runtest-python)
 (deps
  ./main.exe
  (source_tree ../test))
 (action
  (run poetry run ../test/core/run.py --wasm ./main.exe)))
