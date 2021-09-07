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
    Invocation: dune exec src/proto_alpha/lib_protocol/test/main.exe -- test "^gas properties$"
    Subject:    Arithmetic properties around gas.
*)

open Protocol
module S = Saturation_repr

type cost_kind =
  | Atomic_step
  | Step
  | Alloc
  | Alloc_bytes
  | Alloc_mbytes
  | Read_bytes
  | Write_bytes

let random_cost_kind () =
  let i = Random.int 7 in
  match i with
  | 0 -> Atomic_step
  | 1 -> Step
  | 2 -> Alloc
  | 3 -> Alloc_bytes
  | 4 -> Alloc_mbytes
  | 5 -> Read_bytes
  | 6 -> Write_bytes
  | _ -> assert false

let random_cost_of_kind (cost_kind : cost_kind) =
  let open Alpha_context.Gas in
  let rand = Random.int 1000 in
  match cost_kind with
  | Atomic_step -> atomic_step_cost (S.safe_int rand)
  | Step -> step_cost (S.safe_int rand)
  | Alloc -> alloc_cost (S.safe_int rand)
  | Alloc_bytes -> alloc_bytes_cost rand
  | Alloc_mbytes -> alloc_mbytes_cost rand
  | Read_bytes -> read_bytes_cost rand
  | Write_bytes -> write_bytes_cost rand

let random_cost () = random_cost_of_kind (random_cost_kind ())

(** Consuming [Gas.free] is equivalent to consuming nothing. *)
let test_free_neutral since =
  let open Alpha_context in
  let open Environment.Error_monad in
  let cost = random_cost () in
  Gas.consume since cost >>? fun ctxt ->
  Gas.consume ctxt Gas.free >>? fun branch1 ->
  Gas.consume since cost >>? fun branch2 ->
  if
    Gas.Arith.(
      Gas.consumed ~since:ctxt ~until:branch1
      = Gas.consumed ~since:ctxt ~until:branch2)
  then Result.return_none
  else Ok (Some (cost, Gas.free))

(** Consuming [cost1] then [cost2] is equivalent to consuming
    [Gas.(cost1 +@ cost2)]. *)
let test_consume_commutes since =
  let open Alpha_context in
  let open Environment.Error_monad in
  let cost1 = random_cost () in
  let cost2 = random_cost () in
  Gas.consume since cost1 >>? fun ctxt ->
  Gas.consume ctxt cost2 >>? fun branch1 ->
  Gas.consume since Gas.(cost1 +@ cost2) >>? fun branch2 ->
  if
    Gas.Arith.(
      Gas.consumed ~since:ctxt ~until:branch1
      = Gas.consumed ~since:ctxt ~until:branch2)
  then Result.return_none
  else Ok (Some (cost1, cost2))

let rec loop_check check n ctxt =
  let open Environment.Error_monad in
  if n = 0 then Result.return_none
  else
    check ctxt >>? function
    | None -> loop_check check (n - 1) ctxt
    | counterexample -> Ok counterexample

let check_property prop () =
  Random.init 89809344 ;
  Context.init 1 >>=? fun (b, _contracts) ->
  Incremental.begin_construction b >>=? fun inc ->
  let state = Incremental.validation_state inc in
  let ctxt =
    Alpha_context.Gas.set_limit
      state.ctxt
      Alpha_context.Gas.Arith.(fp (integral_of_int_exn 100_000_000))
  in
  let result = prop ctxt in
  match result with
  | Ok None -> return_unit
  | Ok (Some (cost1, cost2)) ->
      let msg =
        Format.asprintf
          "gas consume commutation falsified for %a ; %a"
          Alpha_context.Gas.pp_cost
          cost1
          Alpha_context.Gas.pp_cost
          cost2
      in
      failwith "%s" msg
  | Error _err -> failwith "gas_consume_commutes: protocol error"

let tests =
  [
    Tztest.tztest
      "Gas.free is a neutral element"
      `Quick
      (check_property (loop_check test_free_neutral 1000));
    Tztest.tztest
      "Gas.consume commutes"
      `Quick
      (check_property (loop_check test_consume_commutes 1000));
  ]
