(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (quantities)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_percentage.ml
    Subject:    On percentages.
*)

open Protocol

let pct_of_int n =
  Percentage.of_ratio_bounded Ratio_repr.{numerator = n; denominator = 100}

let assert_equal ~loc n (pct : Percentage.t) =
  let pct_q = Percentage.to_q pct in
  Assert.equal_q ~loc Q.(n // 100) pct_q

let assert_equal_tez ~loc t1 t2 =
  Assert.equal ~loc Tez_repr.equal "Tez aren't equal" Tez_repr.pp t1 t2

let f = Tez_repr.mul_percentage

let test_constant_values () =
  let open Lwt_result_syntax in
  let* () = assert_equal ~loc:__LOC__ 0 Percentage.p0 in
  let* () = assert_equal ~loc:__LOC__ 5 Percentage.p5 in
  let* () = assert_equal ~loc:__LOC__ 50 Percentage.p50 in
  let* () = assert_equal ~loc:__LOC__ 51 Percentage.p51 in
  let* () = assert_equal ~loc:__LOC__ 100 Percentage.p100 in
  return_unit

let test_neg () =
  let open Lwt_result_syntax in
  let open Percentage in
  let* () = assert_equal ~loc:__LOC__ 95 (neg p5) in
  let* () = assert_equal ~loc:__LOC__ 50 (neg p50) in
  let* () = assert_equal ~loc:__LOC__ 31 (neg (pct_of_int 69)) in
  let* () = assert_equal ~loc:__LOC__ 100 (neg (pct_of_int 0)) in
  let* () = assert_equal ~loc:__LOC__ 5 (neg (neg p5)) in
  return_unit

let test_bounded () =
  let open Lwt_result_syntax in
  let open Percentage in
  let* () = assert_equal ~loc:__LOC__ 100 (pct_of_int 200) in
  let* () = assert_equal ~loc:__LOC__ 0 (pct_of_int (-100)) in
  let* () = assert_equal ~loc:__LOC__ 100 (add_bounded p50 p50) in
  let* () = assert_equal ~loc:__LOC__ 100 (add_bounded p50 (neg p5)) in
  return_unit

let test_mul_percentage () =
  let open Lwt_result_syntax in
  let open Tez_repr in
  let rounding = `Down in
  let* () =
    assert_equal_tez
      ~loc:__LOC__
      (of_mutez_exn 50L)
      (mul_percentage ~rounding (of_mutez_exn 100L) Percentage.p50)
  in
  let* () =
    assert_equal_tez
      ~loc:__LOC__
      (of_mutez_exn 5L)
      (mul_percentage ~rounding (of_mutez_exn 100L) Percentage.p5)
  in
  (* round down *)
  let* () =
    assert_equal_tez
      ~loc:__LOC__
      (of_mutez_exn 49L)
      (mul_percentage ~rounding (of_mutez_exn 99L) Percentage.p50)
  in
  (* round up *)
  let* () =
    assert_equal_tez
      ~loc:__LOC__
      (of_mutez_exn 50L)
      (mul_percentage ~rounding:`Up (of_mutez_exn 99L) Percentage.p50)
  in
  let tz = 123456L in
  let* () =
    assert_equal_tez
      ~loc:__LOC__
      (of_mutez_exn tz)
      (mul_percentage ~rounding (of_mutez_exn tz) (pct_of_int 200))
  in
  (* no overflow *)
  let* () =
    assert_equal_tez
      ~loc:__LOC__
      (of_mutez_exn 9131138316486228048L)
      (mul_percentage ~rounding Tez_repr.max_mutez (pct_of_int 99))
  in
  return_unit

let tests =
  Tztest.
    [
      tztest "Test constant values" `Quick test_constant_values;
      tztest "Test neg" `Quick test_neg;
      tztest "Test bounded" `Quick test_bounded;
      tztest "Test mul_percentage" `Quick test_mul_percentage;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("percentage", tests)]
  |> Lwt_main.run
