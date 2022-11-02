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
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "\[Unit\] compare operations"
    Subject:      Valid operations Comparison
*)

open Protocol
open Alpha_context
open Operation_generator
open Lwt_result_syntax

let () = Stdlib.Random.self_init ()

(** A strict order has an equality predicate that is symmetric,
   reflexive and transitive and an lt (and gt) predicates that is
   antisymmetric and transitive.

   Testing that Operation.compare is a strict order on
   operations is then testing that it is symmetric, transitive and
   reflexive, when Operation.compare x y = 0; that it is transitive
   when Operation.compare x y = -1 and Operation.compare x y = -1; and
   that Operation.compare x y = - (Operation.compare y x) when differ
   from 0. *)
let eq_sym op1 op2 =
  if Operation.compare op1 op2 = 0 then assert (Operation.compare op2 op1 = 0)

let eq_refl op = assert (Operation.compare op op = 0)

let eq_trans op1 op2 op3 =
  if Operation.compare op1 op2 = 0 && Operation.compare op2 op3 = 0 then
    assert (Operation.compare op1 op3 = 0)

let lt_antisym op1 op2 =
  if Operation.compare op1 op2 = -1 then assert (Operation.compare op2 op1 = 1)

let lt_trans op1 op2 op3 =
  if Operation.compare op1 op2 = -1 && Operation.compare op2 op3 = -1 then
    assert (Operation.compare op1 op3 = -1)

let gt_trans op1 op2 op3 =
  if Operation.compare op1 op2 = 1 && Operation.compare op2 op3 = 1 then
    assert (Operation.compare op1 op3 = 1)

let gt_antisym op1 op2 =
  if Operation.compare op1 op2 = 1 then assert (Operation.compare op2 op1 = -1)

(** Testing that Operation.compare is a strict order on operations. *)
let strorder op1 op2 op3 =
  eq_sym op1 op2 ;
  eq_refl op1 ;
  eq_trans op1 op2 op3 ;
  lt_antisym op1 op2 ;
  lt_trans op1 op2 op3 ;
  gt_trans op1 op2 op3 ;
  gt_antisym op1 op2

let run ?seed n =
  assert (n >= 0) ;
  let seed =
    match seed with Some s -> s | None -> Stdlib.Random.int (1 lsl 29)
  in
  Format.printf "Starting fuzzing with seed: %d@." seed ;
  let random_state = {seed; rnd_state = Random.make [|seed|]} in
  let rec loop = function
    | 0 -> ()
    | n' ->
        (try
           let k1, op1 = generate_operation random_state in
           let k2, op2 = generate_operation random_state in
           let k3, op3 = generate_operation random_state in
           try strorder op1 op2 op3
           with exn ->
             Format.eprintf
               "%a vs. %a vs. %a@."
               pp_kind
               k1
               pp_kind
               k2
               pp_kind
               k3 ;
             raise exn
         with Failure _ -> ()) ;
        loop (pred n')
  in
  loop n

let test_compare () =
  run 1_000_000 ;
  return_unit

let tests =
  Tztest.
    [tztest "Compare operations is a strict total order." `Slow test_compare]
