let tests =
  [
    ("Smallint", Smallint.tests);
    ("Lazy_vector", Lazy_vector_tests.tests);
    ("Chunked_byte_vector", Chunked_byte_vector_tests.tests);
  ]

let () = Alcotest.run "WebAssembly reference interpreter tests" tests
