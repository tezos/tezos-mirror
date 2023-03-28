(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (saturated arithmetic)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe
    Subject:    The gas is represented using saturated arithmetic.
                These unit tests check that saturated arithmetic operations
                are correctly implemented.
*)

open Protocol

let valid (z : _ Saturation_repr.t) =
  let x = z |> Saturation_repr.to_int in
  x >= 0 && x < max_int

exception Saturating_test_error of string

let err x = Exn (Saturating_test_error x)

let small_enough (z : _ Saturation_repr.t) =
  Saturation_repr.(Compare.Int.((z |> to_int) land 0x7fffffff80000000 = 0))

let ok_int x =
  match Saturation_repr.of_int_opt x with None -> assert false | Some x -> x

let n = ok_int 123123

let m = ok_int 377337

let add () =
  Saturation_repr.(
    fail_unless
      (add saturated (ok_int 1) = saturated)
      (err "saturated + 1 <> saturated")
    >>=? fun () ->
    fail_unless (add zero n = n) (err "zero + n <> n") >>=? fun () ->
    fail_unless (add n zero = n) (err "n + zero <> n") >>=? fun () ->
    let r = add n m in
    fail_unless
      (valid r && r = ok_int ((n |> to_int) + (m |> to_int)))
      (err "add does not behave like + on small numbers."))

let sub () =
  Saturation_repr.(
    fail_unless (sub zero n = zero) (err "zero - n <> zero") >>=? fun () ->
    let n = max n m and m = min n m in
    let r = sub n m in
    fail_unless
      (valid r && r = ok_int ((n |> to_int) - (m |> to_int)))
      (err "sub does not behave like - on small numbers."))

let mul_safe_of_int x =
  Saturation_repr.(
    match mul_safe (ok_int x) with Some x -> x | None -> assert false)

let n' = mul_safe_of_int 1000

let m' = mul_safe_of_int 10000

let mul_fast () =
  Saturation_repr.(
    fail_unless (mul_fast zero n' = zero) (err "mul_fast zero x <> zero")
    >>=? fun () ->
    fail_unless (mul_fast n' zero = zero) (err "mul_fast x zero <> zero")
    >>=? fun () ->
    let r = mul_fast n' m' in
    fail_unless
      (valid r && r = ok_int ((n' |> to_int) * (m' |> to_int)))
      (err "mul_fast does not behave like * on small numbers."))

let scale_fast () =
  Saturation_repr.(
    fail_unless (scale_fast zero n = zero) (err "scale_fast zero x <> zero")
    >>=? fun () ->
    fail_unless (scale_fast n' zero = zero) (err "scale_fast x zero <> zero")
    >>=? fun () ->
    fail_unless
      (scale_fast n' saturated = saturated)
      (err "scale_fast x saturated <> saturated")
    >>=? fun () ->
    let r = scale_fast n' m in
    fail_unless
      (valid r && r = ok_int ((n' |> to_int) * (m |> to_int)))
      (err "mul_fast does not behave like * on small numbers."))

let mul () =
  Saturation_repr.(
    fail_unless
      (mul saturated saturated = saturated)
      (err "saturated * saturated <> saturated")
    >>=? fun () ->
    fail_unless (mul zero saturated = zero) (err "zero * saturated <> zero")
    >>=? fun () ->
    fail_unless (mul saturated zero = zero) (err "saturated * zero <> zero")
    >>=? fun () ->
    let max_squared = ok_int (1 lsl 31) in
    let r = mul max_squared max_squared in
    fail_unless (r = saturated) (err "2 ^ 31 * 2 ^ 31 should be saturated")
    >>=? fun () ->
    let safe_squared = ok_int ((1 lsl 31) - 1) in
    let r = mul safe_squared safe_squared in
    fail_unless
      (valid r && r <> saturated)
      (err "(2 ^ 31 - 1) * (2 ^ 31 - 1) should not be saturated")
    >>=? fun () ->
    let r = mul n m in
    fail_unless
      (valid r && r = ok_int ((n |> to_int) * (m |> to_int)))
      (err "mul does not behave like * on small numbers."))

let shift_left () =
  Saturation_repr.(
    let must_saturate flag (k, v) =
      fail_unless
        (Bool.equal flag (shift_left k v = saturated))
        (err
           (Printf.sprintf
              "shift_left %d %d %s saturated"
              (k |> to_int)
              v
              (if flag then "<>" else "=")))
    in
    List.iter_es
      (must_saturate true)
      [(saturated, 1); (shift_right saturated 1, 2); (ok_int 1, 62)]
    >>=? fun () ->
    List.iter_es
      (must_saturate false)
      [
        (ok_int 1, 0);
        (ok_int 1, 31);
        (ok_int 1, 61);
        (ok_int 0, 99);
        (ok_int ((1 lsl 62) - 2), 0);
      ])

let sqrt () =
  Saturation_repr.(
    fail_unless (sqrt saturated = saturated) (err "sqrt saturated <> saturated")
    >>=? fun () ->
    fail_unless (sqrt zero = zero) (err "sqrt zero <> zero") >>=? fun () ->
    fail_unless (sqrt one = one) (err "sqrt one <> one") >>=? fun () ->
    fail_unless (sqrt (ok_int 4) = ok_int 2) (err "sqrt 4 <> 2") >>=? fun () ->
    fail_unless
      (sqrt (ok_int 5) = ok_int 2)
      (err "sqrt 5 <> 2 (sqrt should round down)")
    >>=? fun () ->
    let safe_squared = ok_int ((1 lsl 31) - 1) in
    let r = mul safe_squared safe_squared in
    fail_unless
      (sqrt r = safe_squared)
      (err "sqrt (2 ^ 31 - 1) * (2 ^ 31 - 1) <> (2 ^ 31 - 1)"))

let of_z_opt () =
  fail_unless
    (Saturation_repr.(of_z_opt (Z.succ (Z.of_int max_int))) = None)
    (err
       "of_z_opt should saturate when given a z integer greater than max_int.")
  >>=? fun () ->
  fail_unless
    (Saturation_repr.(of_z_opt (Z.pred Z.zero)) = None)
    (err "of_z_opt should fail on a z negative integer.")
  >>=? fun () ->
  fail_unless
    (Saturation_repr.(of_z_opt (Z.of_int min_int)) = None)
    (err "of_z_opt should fail on a z negative integer.")

let encoding encoder () =
  let check_encode_decode x =
    Data_encoding.Binary.(
      match to_bytes encoder (ok_int x) with
      | Error _ ->
          fail (err (Printf.sprintf "Problem during binary encoding of %d" x))
      | Ok bytes -> (
          match of_bytes encoder bytes with
          | Error _ ->
              fail
                (err (Printf.sprintf "Problem during binary decoding of %d" x))
          | Ok x' ->
              fail_unless
                (ok_int x = x')
                (err
                   (Printf.sprintf
                      "decode (encode %d) = %d <> %d"
                      x
                      (x' :> int)
                      x))))
  in
  Error_monad.Lwt_result_syntax.tzjoin
    (List.map check_encode_decode [0; 7373737373; max_int - 1])

let tests =
  [
    Tztest.tztest "Addition" `Quick add;
    Tztest.tztest "Subtraction" `Quick sub;
    Tztest.tztest "Multiplication" `Quick mul;
    Tztest.tztest "Multiplication (fast version)" `Quick mul_fast;
    Tztest.tztest "Shift left" `Quick shift_left;
    Tztest.tztest "Scale fast" `Quick scale_fast;
    Tztest.tztest "Square root" `Quick sqrt;
    Tztest.tztest "Conversion from Z" `Quick of_z_opt;
    Tztest.tztest
      "Encoding through z"
      `Quick
      (encoding Saturation_repr.z_encoding);
    Tztest.tztest
      "Encoding through n"
      `Quick
      (encoding Saturation_repr.n_encoding);
  ]
