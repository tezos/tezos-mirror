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
    Component:  Protocol (fixed-point decimals)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "^\[Unit\] fixed point computation$"
    Subject:    On fixed-point decimal numbers.
*)

open Protocol

exception Fixed_point_test_error of string

let err x = Exn (Fixed_point_test_error x)

module type Arith = sig
  type t

  val zero : t

  val equal : t -> t -> bool

  val random : unit -> t

  val add : t -> t -> t

  val sub : t -> t -> t
end

let n = Z.of_int 42

let n' = Z.of_int 43

let basic_arith name (module A : Arith) =
  let err msg = err (Format.asprintf "%s test: %s" name msg) in
  let x = A.random () in
  fail_unless A.(add zero x = x) (err "zero is neutral for +") >>=? fun () ->
  let x = A.random () in
  let y = A.random () in
  fail_unless A.(add x y = add y x) (err "addition is commutative")
  >>=? fun () ->
  let x = A.random () in
  fail_unless
    A.(sub (add zero x) x = zero)
    (err "addition and subtraction cancel")
  >>=? fun () ->
  let x = A.random () in
  let y = A.random () in
  let z = A.random () in
  fail_unless
    A.(add x (add y z) = add (add x y) z)
    (err "addition is associative")

let arith_from_integral : (module Fixed_point_repr.Full) -> (module Arith) =
 fun (module FP) ->
  let module Arith = struct
    type t = FP.integral

    let zero = FP.zero

    let equal = FP.equal

    let random () = FP.integral_of_int_exn (Random.int 898987)

    let add = FP.add

    let sub = FP.sub
  end in
  (module Arith)

let arith_from_fp : (module Fixed_point_repr.Full) -> (module Arith) =
 fun (module FP) ->
  let module Arith = struct
    type t = FP.fp

    let zero = FP.zero

    let equal = FP.equal

    let random () = FP.unsafe_fp (Z.of_int (Random.int 898987))

    let add = FP.add

    let sub = FP.sub
  end in
  (module Arith)

let integral_tests () =
  let module FP = Gas_limit_repr.Arith in
  (* test roundtrips *)
  fail_unless (FP.(integral_to_z (integral_exn n)) = n) (err "roundtrip > 0")
  >>=? fun () ->
  fail_unless
    (FP.(integral_to_z (integral_exn Z.zero)) = Z.zero)
    (err "roundtrip = 0")
  >>=? fun () ->
  (* test ceil/floor on integral *)
  fail_unless
    FP.(ceil (fp (integral_exn n)) = integral_exn n)
    (err "integral;fp;ceil = integral")
  >>=? fun () ->
  fail_unless
    FP.(floor (fp (integral_exn n)) = integral_exn n)
    (err "integral;fp;floor = integral")
  >>=? fun () ->
  fail_unless
    (Format.asprintf "%a" FP.pp FP.(fp (integral_exn n))
    = Format.asprintf "%a" FP.pp_integral (FP.integral_exn n))
    (err "pp_integral(integral) = pp(fp(integral))")
  >>=? fun () -> basic_arith "integral arith" (arith_from_integral (module FP))

let fp_nonzero () =
  let decimals = 3 in
  let module FP = Gas_limit_repr.Arith in
  let prefix msg = Format.asprintf "(%d decimals) %s" decimals msg in
  let err msg = err (prefix msg) in
  basic_arith (prefix "integral arith") (arith_from_integral (module FP))
  >>=? fun () ->
  basic_arith (prefix "fp arith") (arith_from_fp (module FP)) >>=? fun () ->
  let epsilon = FP.unsafe_fp Z.one in
  fail_unless FP.(ceil epsilon = integral_exn Z.one) (err "ceil eps = 1")
  >>=? fun () ->
  fail_unless FP.(floor epsilon = integral_exn Z.zero) (err "floor eps = 1")
  >>=? fun () ->
  let x = Z.of_int (Random.int 980812) in
  fail_unless
    FP.(
      ceil (add (fp (integral_exn x)) (unsafe_fp Z.one))
      = integral_exn (Z.succ x))
    (err "ceil (x + eps) = x + 1")

let fp_pp () =
  let module FP = Gas_limit_repr.Arith in
  let prefix msg = Format.asprintf "(%d decimals) %s" 3 msg in
  let err msg = err (prefix msg) in
  let epsilon = FP.unsafe_fp Z.one in
  let ( =:= ) x expected = Format.asprintf "%a" FP.pp x = expected in
  fail_unless (epsilon =:= "0.001") (err "eps = 0.001") >>=? fun () ->
  fail_unless (FP.unsafe_fp (Z.of_int 1000) =:= "1") (err "1.000 = 1")
  >>=? fun () ->
  fail_unless (FP.unsafe_fp (Z.of_int 1001) =:= "1.001") (err "1.001")
  >>=? fun () ->
  fail_unless (FP.unsafe_fp (Z.of_int 10001) =:= "10.001") (err "10.001")
  >>=? fun () -> fail_unless (FP.zero =:= "0") (err "0")

let tests =
  [
    Tztest.tztest "Integral tests (3 decimals)" `Quick integral_tests;
    Tztest.tztest "FP tests (3 decimals)" `Quick fp_nonzero;
    Tztest.tztest "FP pp tests (3 decimals)" `Quick fp_pp;
  ]
