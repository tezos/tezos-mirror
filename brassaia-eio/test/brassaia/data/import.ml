let check typ _pos ~expected actual = Alcotest.(check typ "" expected actual)

let check_bool pos ~expected actual = check Alcotest.bool pos ~expected actual

let check_ok_or_duplicate pos ~expected actual =
  let pp : [`Ok | `Duplicate] Fmt.t =
    Fmt.of_to_string (function `Ok -> "`Ok" | `Duplicate -> "`Duplicate")
  in
  check (Alcotest.testable pp ( = )) pos ~expected actual

let check_invalid_arg _pos f =
  let fail got =
    Alcotest.failf
      "Expected function to raise `Invalid_argument`, but raised: %a"
      Fmt.(Dump.option exn)
      got
  in
  match f () with
  | _ -> fail None
  | exception Invalid_argument _ -> ()
  | exception exn -> fail (Some exn)
