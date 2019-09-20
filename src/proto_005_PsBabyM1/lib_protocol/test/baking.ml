(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
open Alpha_context
open Test_utils

(** Tests for [bake_n] and [bake_until_end_cycle]. *)
let test_cycle () =
  Context.init 5 >>=? fun (b,_) ->
  Context.get_constants (B b) >>=? fun csts ->
  let blocks_per_cycle = csts.parametric.blocks_per_cycle in

  let pp = fun fmt x -> Format.fprintf fmt "%ld" x in

  (* Tests that [bake_until_cycle_end] returns a block at
     level [blocks_per_cycle]. *)
  Block.bake b >>=? fun b ->
  Block.bake_until_cycle_end b >>=? fun b ->
  Context.get_level (B b) >>=? fun curr_level ->
  Assert.equal ~loc:__LOC__ Int32.equal "not the right level" pp
    (Alpha_context.Raw_level.to_int32 curr_level)
    blocks_per_cycle >>=? fun () ->

  (* Tests that [bake_n n] bakes [n] blocks. *)
  Context.get_level (B b) >>=? fun l ->
  Block.bake_n 10 b >>=? fun b ->
  Context.get_level (B b) >>=? fun curr_level ->
  Assert.equal ~loc:__LOC__ Int32.equal "not the right level" pp
    (Alpha_context.Raw_level.to_int32 curr_level)
    (Int32.add (Alpha_context.Raw_level.to_int32 l) 10l)


(** Tests the formula introduced in Emmy+ for block reward:
    (16/(p+1)) * (0.8 + 0.2 * e / 32)
    where p is the block priority and
    e is the number of included endorsements *)
let test_block_reward priority () =
  begin match priority with
    | 0 -> Test_tez.Tez.((of_int 128) /? Int64.of_int 10) >>?= fun min ->
        return (Test_tez.Tez.of_int 16, min)
    | 1 -> Test_tez.Tez.((of_int 64) /? Int64.of_int 10) >>?= fun min ->
        return (Test_tez.Tez.of_int 8, min)
    | 3 -> Test_tez.Tez.((of_int 32) /? Int64.of_int 10) >>?= fun min ->
        return (Test_tez.Tez.of_int 4, min)
    | _ -> fail (invalid_arg "prio should be 0, 1, or 3")
  end >>=? fun (expected_reward_max_endo, expected_reward_min_endo) ->
  let endorsers_per_block = 32 in
  Context.init ~endorsers_per_block 32 >>=? fun (b, _) ->

  Context.get_endorsers (B b) >>=? fun endorsers ->
  fold_left_s (fun ops (endorser : Alpha_services.Delegate.Endorsing_rights.t) ->
      let delegate = endorser.delegate in
      Op.endorsement ~delegate (B b) () >>=? fun op ->
      return (Operation.pack op :: ops)
    ) [] endorsers >>=? fun ops ->
  Block.bake
    ~policy:(By_priority 0)
    ~operations:ops
    b >>=? fun b ->
  (* bake a block at priority 0 and 32 endorsements;
     the reward is 16 tez *)
  Context.get_baking_reward (B b) ~priority ~endorsing_power:32 >>=? fun baking_reward ->
  Assert.equal_tez ~loc:__LOC__ baking_reward expected_reward_max_endo >>=? fun () ->
  (* bake a block at priority 0 and 0 endorsements;
     the reward is 12.8 tez *)
  Context.get_baking_reward (B b) ~priority ~endorsing_power:0 >>=? fun baking_reward ->
  Assert.equal_tez ~loc:__LOC__ baking_reward expected_reward_min_endo


let tests = [
  Test.tztest "cycle" `Quick (test_cycle) ;
  Test.tztest "block_reward for priority 0" `Quick (test_block_reward 0) ;
  Test.tztest "block_reward for priority 1" `Quick (test_block_reward 1) ;
  Test.tztest "block_reward for priority 3" `Quick (test_block_reward 3) ;
]
