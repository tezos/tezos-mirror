(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Protocol (Operation compare)
    Invocation:   dune exec src/proto_024_PtTALLiN/lib_protocol/test/pbt/main.exe \
                  -- --file test_compare_operations.ml
    Subject:      Valid operations Comparison
*)

open Protocol
open Alpha_context
open Operation_generator
open QCheck2

let lt = -1

let gt = 1

let eq = 0

let cmp_op op1 op2 res = Compare.Int.equal (Operation.compare op1 op2) res

(** A strict order has an equality predicate that is symmetric,
   reflexive and transitive and an lt (and gt) predicates that is
   antisymmetric and transitive.

   Testing that Operation.compare is a strict order on
   operations is then testing that it is symmetric, transitive and
   reflexive, when Operation.compare x y = 0; that it is transitive
   when Operation.compare x y = -1 and Operation.compare x y = -1; and
   that Operation.compare x y = - (Operation.compare y x) when differ
   from 0. *)
let eq_sym op1 op2 = if cmp_op op1 op2 eq then assert (cmp_op op2 op1 eq)

let eq_refl op = assert (cmp_op op op eq)

let eq_trans op1 op2 op3 =
  if cmp_op op1 op2 eq && cmp_op op2 op3 eq then assert (cmp_op op1 op3 eq)

let lt_antisym op1 op2 = if cmp_op op1 op2 lt then assert (cmp_op op2 op1 gt)

let lt_trans op1 op2 op3 =
  if cmp_op op1 op2 lt && cmp_op op2 op3 lt then assert (cmp_op op1 op3 lt)

let gt_trans op1 op2 op3 =
  if cmp_op op1 op2 gt && cmp_op op2 op3 gt then assert (cmp_op op1 op3 gt)

let gt_antisym op1 op2 = if cmp_op op1 op2 gt then assert (cmp_op op2 op1 lt)

(** Testing that Operation.compare is a strict order on operations. *)
let strorder op1 op2 op3 =
  eq_sym op1 op2 ;
  eq_refl op1 ;
  eq_trans op1 op2 op3 ;
  lt_antisym op1 op2 ;
  lt_trans op1 op2 op3 ;
  gt_trans op1 op2 op3 ;
  gt_antisym op1 op2

let test_compare_is_strorder =
  Test.make
    ~name:"Compare operations is a strict total order"
    (Gen.triple generate_operation generate_operation generate_operation)
    (fun ((k1, op1), (k2, op2), (k3, op3)) ->
      try
        strorder op1 op2 op3 ;
        true
      with exn ->
        Format.eprintf "%a vs. %a vs. %a@." pp_kind k1 pp_kind k2 pp_kind k3 ;
        raise exn)

let tests = [test_compare_is_strorder]

let () =
  Alcotest.run
    ~__FILE__
    Protocol.name
    [("Compare_operations", Qcheck2_helpers.qcheck_wrap tests)]
