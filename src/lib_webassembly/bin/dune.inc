; This file is used because expressing the same in the package manifest (located
; in mainfest/main.ml) not very nice.

(include_subdirs unqualified)

(data_only_dirs test)

(rule
 ; Run the WebAssembly core tests.
 ; See src/bin_webassembly/test/core/README.md for details.
 (alias runtest-python)
 (deps
  ./main.exe
  (source_tree .)
  (source_tree ..))
 (action
  (run poetry run ./test/core/run.py --wasm ./main.exe)))
