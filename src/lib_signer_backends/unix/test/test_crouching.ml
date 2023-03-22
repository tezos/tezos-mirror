(** Testing
    -------
    Component:    Remote-signature Backends
    Invocation:   dune exec src/lib_signer_backends/unix/test/main.exe
    Subject:      On pseudo-unique names for Ledger Nano S devices.
*)

(** Deterministically derive a sentence with the form
    adjective-animal-adjective-animal from "12345". It is asserted that
    the tuple is "calculating-meerkat-straight-beetle".
*)
let test_example () =
  let name = Ledger_names.crouching_tiger "12345" in
  assert (
    name = {c = "calculating"; t = "meerkat"; h = "straight"; d = "beetle"})

let tests = [Alcotest.test_case "print_example" `Quick test_example]

let () = Alcotest.run "tezos-signer-backends" [("ledger-names", tests)]
