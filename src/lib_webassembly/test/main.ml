let tests =
  [
    ("Lazy_map", Lazy_map_tests.tests);
    ("Chunked_byte_vector", Chunked_byte_vector_tests.tests);
  ]

let () = Alcotest.run "WebAssembly reference interpreter tests" tests
