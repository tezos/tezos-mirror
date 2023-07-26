(** Testing
    -------
    Component:  Protocol (time repr)
    Invocation: dune exec src/proto_018_Proxford/lib_protocol/test/unit/main.exe \
                  -- --file test_time_repr.ml
    Subject:    Error handling of time operations 
*)

open Protocol

let test_nominal_add () =
  let t = Time_repr.of_seconds (Int64.of_int 2) in
  let addition =
    Period_repr.of_seconds Int64.one >>? fun p -> Time_repr.( +? ) t p
  in
  match addition with
  | Ok v ->
      Assert.equal
        ~loc:__LOC__
        Time_repr.equal
        "test_nominal_add"
        Time_repr.pp_hum
        v
        (Time_repr.of_seconds (Int64.of_int 3))
  | Error _ -> failwith "Addition has overflowed"

let test_overflow_add () =
  let t = Time_repr.of_seconds Int64.max_int in
  match Period_repr.of_seconds Int64.one with
  | Error _ -> failwith "period_repr conversion"
  | Ok p -> (
      match Time_repr.( +? ) t p with
      | Error _ -> return_unit
      | Ok tres ->
          failwith
            "No overflow: %Ld + %Ld = %Ld"
            (Time_repr.to_seconds t)
            (Period_repr.to_seconds p)
            (Time_repr.to_seconds tres))

let tests =
  [
    Tztest.tztest "non-overflowing addition" `Quick test_nominal_add;
    Tztest.tztest "overflowing addition" `Quick test_overflow_add;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("time", tests)] |> Lwt_main.run
