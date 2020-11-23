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

open Test
open Protocol
open Raw_context

exception Gas_levels_test_error of string

let err x = Exn (Gas_levels_test_error x)

let succeed x = match x with Ok _ -> true | _ -> false

let failed x = not (succeed x)

let dummy_context () =
  Context.init 1
  >>=? fun (block, _) ->
  Raw_context.prepare
    ~level:Int32.zero
    ~predecessor_timestamp:Time.Protocol.epoch
    ~timestamp:Time.Protocol.epoch
    ~fitness:[]
    (block.context : Environment_context.Context.t)
  >|= Environment.wrap_error

let detect_gas_exhaustion_in_fresh_context () =
  dummy_context ()
  >>=? fun context ->
  fail_unless
    (consume_gas context (Z.of_int max_int) |> succeed)
    (err "In a fresh context, gas consumption is unlimited.")

let make_context initial_operation_gas =
  dummy_context ()
  >>=? fun context ->
  return
    ( Gas_limit_repr.Arith.integral_of_int initial_operation_gas
    |> set_gas_limit context )

let detect_gas_exhaustion_when_operation_gas_hits_zero () =
  make_context 10
  >>=? fun context ->
  fail_unless
    (consume_gas context (Z.of_int max_int) |> failed)
    (err "Fail when consuming more than the remaining operation gas.")

let detect_gas_exhaustion_when_block_gas_hits_zero () =
  make_context max_int
  >>=? fun context ->
  fail_unless
    (consume_gas context (Z.of_int max_int) |> failed)
    (err "Fail when consuming more than the remaining block gas.")

let monitor initial_operation_level gas_level expectation () =
  let open Gas_limit_repr.Arith in
  make_context initial_operation_level
  >>=? fun context ->
  fail_unless
    ( match consume_gas context (Z.of_int 10000) (* in milligas. *) with
    | Ok context ->
        let remaining = gas_level context in
        remaining = integral_of_int expectation
    | _ ->
        false )
    (err "Monitor operation gas at each gas consumption")

let operation_gas_level context =
  match gas_level context with
  | Gas_limit_repr.Limited {remaining} ->
      remaining
  | _ ->
      (* because this function is called after [set_gas_limit]. *)
      assert false

(*

   Monitoring runs differently depending on the minimum between the
   operation gas level and the block gas level. Hence, we check that
   in both situations, the gas levels are correctly reported.

*)
let monitor_operation_gas_level = monitor 100 operation_gas_level 90

let monitor_operation_gas_level' =
  monitor max_int operation_gas_level (max_int - 10)

let monitor_block_gas_level = monitor 100 block_gas_level 10399990

let monitor_block_gas_level' = monitor max_int block_gas_level 10399990

let quick (what, how) = tztest what `Quick how

let tests =
  List.map
    quick
    [ ( "Detect gas exhaustion in fresh context",
        detect_gas_exhaustion_in_fresh_context );
      ( "Detect gas exhaustion when operation gas as hits zero",
        detect_gas_exhaustion_when_operation_gas_hits_zero );
      ( "Detect gas exhaustion when block gas as hits zero",
        detect_gas_exhaustion_when_block_gas_hits_zero );
      ( "Each gas consumption impacts operation gas level (operation < block)",
        monitor_operation_gas_level );
      ( "Each gas consumption impacts operation gas level (block < operation)",
        monitor_operation_gas_level' );
      ( "Each gas consumption has an impact on block gas level (operation < \
         block)",
        monitor_block_gas_level );
      ( "Each gas consumption has an impact on block gas level (block < \
         operation)",
        monitor_block_gas_level' ) ]
