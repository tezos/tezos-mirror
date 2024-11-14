let () =
  Random.self_init ();
  Alcotest.run ~__FILE__ "brassaia/data"
    [ ("Fixed_size_string_set", Test_fixed_size_string_set.tests) ]
