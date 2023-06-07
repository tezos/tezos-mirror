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
    Invocation: dune exec src/proto_016_PtMumbai/lib_protocol/test/pbt/main.exe \
                  -- --file test_gas_properties.ml
    Subject:    Arithmetic properties around gas.
*)

open Protocol
open Qcheck2_helpers

(** Extract a Tezos result for compatibility with QCheck2. *)
let extract_qcheck_result = function
  | Ok pure_result -> pure_result
  | Error err ->
      Format.printf "@\n%a@." Environment.Error_monad.pp_trace err ;
      false

(** [Gas.free] is the neutral element of gas addition: [any_cost +@ Gas.free = Gas.free +@ any_cost = any_cost]. *)
let test_free_neutral (start, any_cost) =
  let open Alpha_context in
  extract_qcheck_result
    (let open Result_syntax in
    let* free_first = Gas.consume start Gas.free in
    let* branch1 = Gas.consume free_first any_cost in
    let* cost_first = Gas.consume start any_cost in
    let+ branch2 = Gas.consume cost_first Gas.free in
    let equal_consumption_from_start t1 t2 =
      Gas.Arith.(
        qcheck_eq
          ~pp
          ~eq:equal
          (Gas.consumed ~since:start ~until:t1)
          (Gas.consumed ~since:start ~until:t2))
    in
    equal_consumption_from_start branch1 branch2
    && equal_consumption_from_start branch1 cost_first)

(** Consuming [Gas.free] is equivalent to consuming nothing. *)
let test_free_consumption start =
  let open Alpha_context in
  extract_qcheck_result
    (let open Result_syntax in
    let+ after_empty_consumption = Gas.consume start Gas.free in
    Gas.Arith.(
      qcheck_eq
        ~pp
        ~eq:equal
        (Gas.consumed ~since:start ~until:after_empty_consumption)
        zero))

(** Consuming [cost1] then [cost2] is equivalent to consuming
    [Gas.(cost1 +@ cost2)]. *)
let test_consume_commutes (start, cost1, cost2) =
  let open Alpha_context in
  extract_qcheck_result
    (let open Result_syntax in
    let* after_cost1 = Gas.consume start cost1 in
    let* branch1 = Gas.consume after_cost1 cost2 in
    let+ branch2 = Gas.consume start Gas.(cost1 +@ cost2) in
    Gas.Arith.(
      qcheck_eq
        ~pp
        ~eq:equal
        (Gas.consumed ~since:start ~until:branch1)
        (Gas.consumed ~since:start ~until:branch2)))

(** Arbitrary context with a gas limit of 100_000_000. *)
let context_gen : Alpha_context.t QCheck2.Gen.t =
  QCheck2.Gen.return
    (Lwt_main.run
       (let open Lwt_result_syntax in
       let* b, _contract = Context.init1 () in
       let+ inc = Incremental.begin_construction b in
       Alpha_context.Gas.set_limit
         (Incremental.alpha_ctxt inc)
         Alpha_context.Gas.Arith.(fp (integral_of_int_exn 100_000_000)))
     |> function
     | Ok a -> a
     | Error _ -> assert false)

(** This arbitrary could be improved (pretty printer and shrinker) if there was a way to convert a [cost] back to an [int]. Otherwise one needs to write a custom [arbitrary] instance, but I wanted to stick to the former design of this test for the time being. *)
let gas_cost_gen : Alpha_context.Gas.cost QCheck2.Gen.t =
  let open Alpha_context.Gas in
  let open QCheck2.Gen in
  let rand = 0 -- 1000 in
  let safe_rand = map Saturation_repr.safe_int rand in
  oneof
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
    QCheck2.Test.make
      ~count:1000
      ~name:"Consuming commutes"
      QCheck2.Gen.(triple context_gen gas_cost_gen gas_cost_gen)
      test_consume_commutes;
    QCheck2.Test.make
      ~count:1000
      ~name:"Consuming [free] consumes nothing"
      context_gen
      test_free_consumption;
    QCheck2.Test.make
      ~count:1000
      ~name:"[free] is the neutral element of Gas addition"
      QCheck2.Gen.(pair context_gen gas_cost_gen)
      test_free_neutral;
  ]

let () =
  Alcotest.run ~__FILE__ Protocol.name [("gas properties", qcheck_wrap tests)]
