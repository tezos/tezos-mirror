let () =
  Alcotest_lwt.run
    "delegate_011_PtHangz2"
    [("client_baking_forge", Test_client_baking_forge.tests)]
  |> Lwt_main.run
