(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
    Component:    Protocol (baking)
    Invocation:   dune exec src/proto_011_PtHangz2/lib_protocol/test/integration/consensus/main.exe -- test "^baking$"
    Subject:      Rewards and bakers. Tests based on RPCs.
*)

open Protocol
open Alpha_context

(** Verify the level is correctly computed when the first cycle is
    passed and after baking a certain fixed number of blocks (10 for
    the moment). The result should be [blocks_per_cycle + 10] where
    [blocks_per_cycle] comes from the constants of the selected
    protocol.

    IMPROVEMENTS:
    - Randomize the number of cycle.
    - Randomize the number of accounts created at the beginning
    - Randomize the blocks per cycle.
    - Randomize the number of blocks baked after the n cycles baked
      previously. *)
let test_cycle () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_constants (B b) >>=? fun csts ->
  let blocks_per_cycle = csts.parametric.blocks_per_cycle in
  let pp fmt x = Format.fprintf fmt "%ld" x in
  Block.bake b >>=? fun b ->
  Block.bake_until_cycle_end b >>=? fun b ->
  Context.get_level (B b) >>?= fun curr_level ->
  Assert.equal
    ~loc:__LOC__
    Int32.equal
    "not the right level"
    pp
    (Alpha_context.Raw_level.to_int32 curr_level)
    blocks_per_cycle
  >>=? fun () ->
  Context.get_level (B b) >>?= fun l ->
  Block.bake_n 10 b >>=? fun b ->
  Context.get_level (B b) >>?= fun curr_level ->
  Assert.equal
    ~loc:__LOC__
    Int32.equal
    "not the right level"
    pp
    (Alpha_context.Raw_level.to_int32 curr_level)
    (Int32.add (Alpha_context.Raw_level.to_int32 l) 10l)

(** After baking and/or endorsing a block, the baker and the endorsers
    get their reward. *)
let test_rewards_retrieval () =
  let endorsers_per_block = 32 in
  (* we want to have sufficient accounts so that find_block succeeds *)
  Context.init (endorsers_per_block * 10) ~endorsers_per_block
  >>=? fun (b, _) ->
  Context.get_constants (B b)
  >>=? fun Constants.
             {
               parametric =
                 {
                   endorsers_per_block;
                   block_security_deposit;
                   endorsement_security_deposit;
                   _;
                 };
               _;
             } ->
  (* find block with endorsers_per_block different endorsers *)
  let open Plugin.RPC.Endorsing_rights in
  let rec find_block b =
    Context.get_endorsers (B b) >>=? fun endorsers ->
    if List.compare_length_with endorsers endorsers_per_block = 0 then return b
    else Block.bake b >>=? fun b -> find_block b
  in
  let balance_update delegate before after =
    Context.Delegate.info (B before) delegate >>=? fun info_before ->
    Context.Delegate.info (B after) delegate >>=? fun info_after ->
    Lwt.return
      Test_tez.Tez.(info_after.frozen_balance -? info_before.frozen_balance)
  in
  find_block b >>=? fun good_b ->
  Context.get_endorsers (B good_b) >>=? fun endorsers ->
  (* test 3 different priorities, too long otherwise *)
  let block_priorities = 0 -- 2 in
  let included_endorsements = 0 -- endorsers_per_block in
  let ranges = List.product block_priorities included_endorsements in
  List.iter_es
    (fun (priority, endorsing_power) ->
      (* bake block at given priority and with given endorsing_power *)
      let real_endorsers = List.sub endorsers endorsing_power in
      List.map_ep
        (fun endorser ->
          Op.endorsement_with_slot
            ~delegate:(endorser.delegate, endorser.slots)
            (B good_b)
            ()
          >|=? fun operation -> Operation.pack operation)
        real_endorsers
      >>=? fun operations ->
      let policy = Block.By_priority priority in
      Block.get_next_baker ~policy good_b >>=? fun (baker, _, _) ->
      Block.bake ~policy ~operations good_b >>=? fun b ->
      Context.get_baking_reward (B b) ~priority ~endorsing_power
      >>=? fun baking_reward ->
      Test_tez.Tez.(block_security_deposit +? baking_reward)
      >>?= fun baking_frozen_balance ->
      Context.get_endorsing_reward (B b) ~priority ~endorsing_power:1
      >>=? fun endorsing_reward ->
      Test_tez.Tez.(endorsement_security_deposit +? endorsing_reward)
      >>?= fun endorsing_frozen_balance ->
      let baker_is_not_an_endorser =
        List.for_all (fun endorser -> endorser.delegate <> baker) real_endorsers
      in
      Test_tez.Tez.(baking_frozen_balance +? endorsing_frozen_balance)
      >>?= fun accumulated_frozen_balance ->
      (* check the baker was rewarded the right amount *)
      balance_update baker good_b b >>=? fun baker_frozen_balance ->
      (if baker_is_not_an_endorser then
       Assert.equal_tez ~loc:__LOC__ baker_frozen_balance baking_frozen_balance
      else
        Assert.equal_tez
          ~loc:__LOC__
          baker_frozen_balance
          accumulated_frozen_balance)
      >>=? fun () ->
      (* check the each endorser was rewarded the right amount *)
      List.iter_ep
        (fun endorser ->
          balance_update endorser.delegate good_b b
          >>=? fun endorser_frozen_balance ->
          if baker <> endorser.delegate then
            Assert.equal_tez
              ~loc:__LOC__
              endorser_frozen_balance
              endorsing_frozen_balance
          else
            Assert.equal_tez
              ~loc:__LOC__
              endorser_frozen_balance
              accumulated_frozen_balance)
        real_endorsers)
    ranges

(** Checks the baking and endorsing rewards formulas against a precomputed
    table. *)
let test_rewards_formulas () =
  Context.init 1 >>=? fun (b, _) ->
  Context.get_constants (B b)
  >>=? fun Constants.{parametric = {endorsers_per_block; _}; _} ->
  let block_priorities = 0 -- 2 in
  let included_endorsements = 0 -- endorsers_per_block in
  let ranges = List.product block_priorities included_endorsements in
  List.iter_ep
    (fun (priority, endorsing_power) ->
      Context.get_baking_reward (B b) ~priority ~endorsing_power
      >>=? fun reward ->
      let expected_reward =
        Test_tez.Tez.of_mutez_exn
          (Int64.of_int Rewards.baking_rewards.(priority).(endorsing_power))
      in
      Assert.equal_tez ~loc:__LOC__ reward expected_reward >>=? fun () ->
      Context.get_endorsing_reward (B b) ~priority ~endorsing_power
      >>=? fun reward ->
      let expected_reward =
        Test_tez.Tez.of_mutez_exn
          (Int64.of_int Rewards.endorsing_rewards.(priority).(endorsing_power))
      in
      Assert.equal_tez ~loc:__LOC__ reward expected_reward >>=? fun () ->
      return_unit)
    ranges

let wrap e = Lwt.return (Environment.wrap_tzresult e)

(** Check that the rewards formulas from Context are equivalent with
    the ones from Baking. *)
let test_rewards_formulas_equivalence () =
  Context.init 1 >>=? fun (b, _) ->
  Context.get_constants (B b)
  >>=? fun Constants.{parametric = {endorsers_per_block; _}; _} ->
  Alpha_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness
  >>= wrap
  >>=? fun (ctxt, _, _) ->
  let block_priorities = 0 -- 64 in
  let endorsing_power = 0 -- endorsers_per_block in
  let ranges = List.product block_priorities endorsing_power in
  List.iter_ep
    (fun (block_priority, endorsing_power) ->
      Baking.baking_reward
        ctxt
        ~block_priority
        ~included_endorsements:endorsing_power
      |> wrap
      >>=? fun reward1 ->
      Context.get_baking_reward (B b) ~priority:block_priority ~endorsing_power
      >>=? fun reward2 ->
      Assert.equal_tez ~loc:__LOC__ reward1 reward2 >>=? fun () ->
      Baking.endorsing_reward ctxt ~block_priority endorsing_power |> wrap
      >>=? fun reward1 ->
      Context.get_endorsing_reward
        (B b)
        ~priority:block_priority
        ~endorsing_power
      >>=? fun reward2 -> Assert.equal_tez ~loc:__LOC__ reward1 reward2)
    ranges

(** Test baking [n] cycles in a raw works smoothly. *)
let test_bake_n_cycles n () =
  let open Block in
  let policy = By_priority 0 in
  Context.init 1 >>=? fun (block, _contracts) ->
  Block.bake_until_n_cycle_end ~policy n block >>=? fun _block -> return ()

(** Check the voting power is constant between cycles when number of
    rolls are constant and in presence of one account. *)
let test_voting_power_cache () =
  let open Block in
  let policy = By_priority 0 in
  Context.init 1 >>=? fun (block, _contracts) ->
  Context.get_bakers (B block) >>=? fun bakers ->
  let baker = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bakers in
  let assert_voting_power n block =
    let ctxt = Context.B block in
    Context.get_voting_power ctxt baker >>=? fun voting_power ->
    Assert.equal_int ~loc:__LOC__ n (Int32.to_int voting_power)
  in
  assert_voting_power 500 block >>=? fun () ->
  Block.bake_until_n_cycle_end ~policy 2 block >>=? fun block ->
  assert_voting_power 500 block >>=? fun () ->
  Block.bake_until_n_cycle_end ~policy 5 block >>=? fun block ->
  assert_voting_power 500 block >>=? fun () ->
  Block.bake_until_n_cycle_end ~policy 1 block >>=? fun block ->
  assert_voting_power 500 block

let tests =
  [
    Tztest.tztest "cycle" `Quick test_cycle;
    Tztest.tztest
      "test rewards are correctly accounted for"
      `Slow
      test_rewards_retrieval;
    Tztest.tztest
      "test rewards formula for various input values"
      `Quick
      test_rewards_formulas;
    Tztest.tztest
      "check equivalence of rewards formulas"
      `Quick
      test_rewards_formulas_equivalence;
    Tztest.tztest
      "test_bake_n_cycles for 12 cycles"
      `Quick
      (test_bake_n_cycles 12);
    Tztest.tztest "voting_power" `Quick test_voting_power_cache;
  ]
