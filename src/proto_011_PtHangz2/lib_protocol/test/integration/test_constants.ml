(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:  Protocol (baking)
    Invocation: dune exec src/proto_011_PtHangz2/lib_protocol/test/integration/main.exe -- test "^constants$"
    Subject:    the consistency of parametric constants
 *)

open Test_tez

let test_constants_consistency () =
  let open Tezos_protocol_011_PtHangz2_parameters.Default_parameters in
  List.iter_es
    Block.check_constants_consistency
    [constants_mainnet; constants_sandbox; constants_test]

let test_max_operations_ttl () =
  let open Protocol in
  (* max_operations_ttl is hard-coded for mainnet to avoid any
     recomputation and is not reconfigured for other networks. *)
  let minimal_block_delay =
    Tezos_protocol_011_PtHangz2_parameters.Default_parameters.constants_mainnet
      .minimal_block_delay
  in
  let time_between_blocks =
    Tezos_protocol_011_PtHangz2_parameters.Default_parameters.constants_mainnet
      .time_between_blocks
  in
  Context.init ~time_between_blocks ~minimal_block_delay 1 >>=? fun (b, _) ->
  Context.get_constants (Context.B b) >>=? fun constants ->
  Environment.wrap_tzresult
    (Alpha_context.Period.mult
       (Int32.of_int Alpha_context.max_operations_ttl)
       constants.parametric.minimal_block_delay)
  >>?= fun result ->
  Assert.equal
    ~loc:__LOC__
    (fun x y -> Alpha_context.Period.compare x y = 0)
    "max_operations_ttl"
    Alpha_context.Period.pp
    Alpha_context.Period.one_hour
    result

(* Test that the amount of the liquidity baking subsidy is 1/16th of total rewards
   of a fully-endorsed block with priority zero. *)
let liquidity_baking_subsidy_param () =
  Context.init 1 >>=? fun (blk, _contracts) ->
  Context.get_constants (B blk) >>=? fun csts ->
  let hd l = Option.value_fe ~error:(fun () -> assert false) (List.hd l) in
  hd csts.parametric.baking_reward_per_endorsement
  >>?= fun baking_reward_per_endorsement ->
  hd csts.parametric.endorsement_reward >>?= fun endorsement_reward ->
  let endorsers_per_block = csts.parametric.endorsers_per_block in
  let actual_subsidy = csts.parametric.liquidity_baking_subsidy in
  Tez.(baking_reward_per_endorsement +? endorsement_reward)
  >>?= fun total_reward ->
  Tez.(mul_exn total_reward endorsers_per_block /? 16L)
  >>?= fun expected_subsidy ->
  Assert.equal_tez ~loc:__LOC__ actual_subsidy expected_subsidy

let tests =
  [
    Tztest.tztest "constants consistency" `Quick test_constants_consistency;
    Tztest.tztest "max_operations_ttl" `Quick test_max_operations_ttl;
    Tztest.tztest
      "test liquidity_baking_subsidy parameter is 1/16th of total baking \
       rewards"
      `Quick
      liquidity_baking_subsidy_param;
  ]
