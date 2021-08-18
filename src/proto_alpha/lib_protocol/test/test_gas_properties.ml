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
    Component:  Protocol (gas properties)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/test_gas_properties.exe
    Subject:    Arithmetic properties around gas.
*)

open Protocol
open Lib_test.Qcheck_helpers

(** Extract a Tezos result for compatibility with QCheck. *)
let extract_qcheck_result = function
  | Ok pure_result -> pure_result
  | Error err ->
      Format.printf "@\n%a@." Environment.Error_monad.pp_trace err ;
      false

(** [Gas.free] is the neutral element of gas addition: [any_cost +@ Gas.free = Gas.free +@ any_cost = any_cost]. *)
let test_free_neutral (start, any_cost) =
  let open Alpha_context in
  extract_qcheck_result
    ( Gas.consume start Gas.free >>? fun free_first ->
      Gas.consume free_first any_cost >>? fun branch1 ->
      Gas.consume start any_cost >>? fun cost_first ->
      Gas.consume cost_first Gas.free >|? fun branch2 ->
      let equal_consumption_from_start t1 t2 =
        Gas.Arith.(
          qcheck_eq
            ~pp
            ~eq:equal
            (Gas.consumed ~since:start ~until:t1)
            (Gas.consumed ~since:start ~until:t2))
      in
      equal_consumption_from_start branch1 branch2
      && equal_consumption_from_start branch1 cost_first )

(** Consuming [Gas.free] is equivalent to consuming nothing. *)
let test_free_consumption start =
  let open Alpha_context in
  extract_qcheck_result
    ( Gas.consume start Gas.free >|? fun after_empty_consumption ->
      Gas.Arith.(
        qcheck_eq
          ~pp
          ~eq:equal
          (Gas.consumed ~since:start ~until:after_empty_consumption)
          zero) )

(** Consuming [cost1] then [cost2] is equivalent to consuming
    [Gas.(cost1 +@ cost2)]. *)
let test_consume_commutes (start, cost1, cost2) =
  let open Alpha_context in
  extract_qcheck_result
    ( Gas.consume start cost1 >>? fun after_cost1 ->
      Gas.consume after_cost1 cost2 >>? fun branch1 ->
      Gas.consume start Gas.(cost1 +@ cost2) >|? fun branch2 ->
      Gas.Arith.(
        qcheck_eq
          ~pp
          ~eq:equal
          (Gas.consumed ~since:start ~until:branch1)
          (Gas.consumed ~since:start ~until:branch2)) )

(** Arbitrary context with a gas limit of 100_000_000. *)
let context_arb : Alpha_context.t QCheck.arbitrary =
  QCheck.always
    (Lwt_main.run
       ( Context.init 1 >>=? fun (b, _contracts) ->
         Incremental.begin_construction b >|=? fun inc ->
         let state = Incremental.validation_state inc in
         Alpha_context.Gas.set_limit
           state.ctxt
           Alpha_context.Gas.Arith.(fp (integral_of_int_exn 100_000_000)) )
     |> function
     | Ok a -> a
     | Error _ -> assert false)

(** This arbitrary could be improved (pretty printer and shrinker) if there was a way to convert a [cost] back to an [int]. Otherwise one needs to write a custom [arbitrary] instance, but I wanted to stick to the former design of this test for the time being. *)
let gas_cost_arb : Alpha_context.Gas.cost QCheck.arbitrary =
  let open Alpha_context.Gas in
  let open QCheck in
  let rand = 0 -- 1000 in
  let safe_rand = map Saturation_repr.safe_int rand in
  choose
    [
      map atomic_step_cost safe_rand;
      map step_cost safe_rand;
      map alloc_cost safe_rand;
      map alloc_bytes_cost rand;
      map alloc_mbytes_cost rand;
      map read_bytes_cost rand;
      map write_bytes_cost rand;
    ]

let tests =
  [
    QCheck.Test.make
      ~count:1000
      ~name:"Consuming commutes"
      QCheck.(triple context_arb gas_cost_arb gas_cost_arb)
      test_consume_commutes;
    QCheck.Test.make
      ~count:1000
      ~name:"Consuming [free] consumes nothing"
      context_arb
      test_free_consumption;
    QCheck.Test.make
      ~count:1000
      ~name:"[free] is the neutral element of Gas addition"
      QCheck.(pair context_arb gas_cost_arb)
      test_free_neutral;
  ]

let () = Alcotest.run "gas properties" [("gas properties", qcheck_wrap tests)]
