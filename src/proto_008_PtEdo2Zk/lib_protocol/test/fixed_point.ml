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

let nn = Z.neg n

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
    A.(add x (sub zero x) = zero)
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

    let random () = FP.integral_of_int (Random.int 898987)

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

let integral_tests decimals () =
  let module FP = Fixed_point_repr.Make (struct
    let decimals = decimals
  end) in
  (* test roundtrips *)
  fail_unless (FP.(integral_to_z (integral n)) = n) (err "roundtrip > 0")
  >>=? fun () ->
  fail_unless
    (FP.(integral_to_z (integral Z.zero)) = Z.zero)
    (err "roundtrip = 0")
  >>=? fun () ->
  fail_unless (FP.(integral_to_z (integral nn)) = nn) (err "roundtrip < 0")
  >>=? fun () ->
  (* test ceil/floor on integral *)
  fail_unless
    FP.(ceil (fp (integral n)) = integral n)
    (err "integral;fp;ceil = integral")
  >>=? fun () ->
  fail_unless
    FP.(floor (fp (integral n)) = integral n)
    (err "integral;fp;floor = integral")
  >>=? fun () ->
  fail_unless
    FP.(ceil (fp (integral nn)) = integral nn)
    (err "integral;fp;ceil = integral")
  >>=? fun () ->
  fail_unless
    FP.(floor (fp (integral nn)) = integral nn)
    (err "integral;fp;floor = integral")
  >>=? fun () ->
  fail_unless FP.(add (integral n) (integral nn) = zero) (err "x + -x = zero")
  >>=? fun () ->
  fail_unless
    (Format.asprintf "%a" FP.pp FP.(fp (integral n))
    = Format.asprintf "%a" FP.pp_integral (FP.integral n))
    (err "pp_integral(integral) = pp(fp(integral))")
  >>=? fun () -> basic_arith "integral arith" (arith_from_integral (module FP))

let fp_zero () =
  let decimals = 0 in
  let module FP = Fixed_point_repr.Make (struct
    let decimals = decimals
  end) in
  let err msg = err (Format.asprintf "(%d decimals) %s" decimals msg) in
  fail_unless FP.(ceil (unsafe_fp n) = integral n) (err "ceil = id (> 0)")
  >>=? fun () ->
  fail_unless FP.(ceil (unsafe_fp nn) = integral nn) (err "ceil = id (< 0)")
  >>=? fun () ->
  fail_unless
    FP.(
      ceil (fp (add (integral n) (integral n))) = add (integral n) (integral n))
    (err "ceil (fp (i1 + i2)) = i1 + i2")
  >>=? fun () ->
  fail_unless
    (Format.asprintf "%a" FP.pp FP.(unsafe_fp n)
    = Format.asprintf "%a" FP.pp_integral (FP.integral n))
    (err "pp_integral(integral) = pp(fp(integral))")
  >>=? fun () -> basic_arith "fp (0 decimals) arith" (arith_from_fp (module FP))

let fp_nonzero decimals () =
  let module FP = Fixed_point_repr.Make (struct
    let decimals = decimals
  end) in
  let prefix msg = Format.asprintf "(%d decimals) %s" decimals msg in
  let err msg = err (prefix msg) in
  basic_arith (prefix "integral arith") (arith_from_integral (module FP))
  >>=? fun () ->
  basic_arith (prefix "fp arith") (arith_from_fp (module FP)) >>=? fun () ->
  let epsilon = FP.unsafe_fp Z.one in
  let neg_epsilon = FP.unsafe_fp Z.minus_one in
  fail_unless FP.(ceil epsilon = integral Z.one) (err "ceil eps = 1")
  >>=? fun () ->
  fail_unless FP.(floor epsilon = integral Z.zero) (err "floor eps = 1")
  >>=? fun () ->
  fail_unless FP.(ceil neg_epsilon = zero) (err "ceil neg_eps = 0")
  >>=? fun () ->
  fail_unless
    FP.(floor neg_epsilon = integral Z.minus_one)
    (err "floor neg_eps = -1")
  >>=? fun () ->
  let x = Z.of_int (Random.int 980812) in
  fail_unless
    FP.(ceil (add (fp (integral x)) (unsafe_fp Z.one)) = integral (Z.succ x))
    (err "ceil (x + eps) = x + 1")

let fp_pp () =
  let module FP = Fixed_point_repr.Make (struct
    let decimals = 3
  end) in
  let prefix msg = Format.asprintf "(%d decimals) %s" 3 msg in
  let err msg = err (prefix msg) in
  let epsilon = FP.unsafe_fp Z.one in
  let neg_epsilon = FP.unsafe_fp Z.minus_one in
  let ( =:= ) x expected = Format.asprintf "%a" FP.pp x = expected in
  fail_unless (epsilon =:= "0.001") (err "eps = 0.001") >>=? fun () ->
  fail_unless (neg_epsilon =:= "-0.001") (err "eps = -0.001") >>=? fun () ->
  fail_unless (FP.unsafe_fp (Z.of_int 1000) =:= "1") (err "1.000 = 1")
  >>=? fun () ->
  fail_unless (FP.unsafe_fp (Z.of_int 1001) =:= "1.001") (err "1.001")
  >>=? fun () ->
  fail_unless (FP.unsafe_fp (Z.of_int 10001) =:= "10.001") (err "10.001")
  >>=? fun () ->
  fail_unless
    (FP.unsafe_fp (Z.neg (Z.of_int 10001)) =:= "-10.001")
    (err "-10.001")
  >>=? fun () -> fail_unless (FP.zero =:= "0") (err "0")

let tests =
  [
    Tztest.tztest "Integral tests (0 decimals)" `Quick (integral_tests 0);
    Tztest.tztest "Integral tests (1 decimals)" `Quick (integral_tests 1);
    Tztest.tztest "Integral tests (10 decimals)" `Quick (integral_tests 10);
    Tztest.tztest "FP tests (0 decimals)" `Quick fp_zero;
    Tztest.tztest "FP tests (1 decimals)" `Quick (fp_nonzero 1);
    Tztest.tztest "FP tests (3 decimals)" `Quick (fp_nonzero 3);
    Tztest.tztest "FP pp tests (3 decimals)" `Quick fp_pp;
  ]
