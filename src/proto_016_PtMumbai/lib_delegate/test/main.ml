(** Testing
    -------
    Component:    Baking
    Invocation:   dune build @src/proto_alpha/lib_delegate/runtest
    Subject:      Entrypoint
 *)

let () =
  Lwt_main.run
    (Alcotest_lwt.run "protocol_alpha" [("scenario", Test_scenario.tests)])
