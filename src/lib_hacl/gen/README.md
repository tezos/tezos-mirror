This generator is used to automatically generate js_of_ocaml stubs for
hacl-star.  It uses both hacl-star ctypes bindings and the 'api.json'
file (provides with the hacl-wasm npm package).

There is a test making sure that 'api.json' is up to date.

  dune build @@src/lib_hacl/gen/runtest_js

One can update the api.json file with

  dune build @@src/lib_hacl/gen/runtest_js --auto-promote

To generate the stubs, run:

  ./gen.exe -api ./api.json -stubs manual-stubs.js -stubs manual-stubs-2.js

The result will be printed on stdout.
Proper stubs will be generated for functions present in 'api.json'.
Dummy stubs will be generated for the missing ones.
Manually written stubs will take precedence over the generation mechanism.