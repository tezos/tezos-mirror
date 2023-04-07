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
    Component:  Protocol (participation monitoring)
    Invocation: dune exec src/proto_016_PtMumbai/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_participation.ml
    Subject:    Participation monitoring in Tenderbake
*)

open Protocol
open Alpha_context

(** [baker] bakes and [endorser] endorses *)
let bake_and_endorse_once (_b_pred, b_cur) baker endorser =
  let open Context in
  Context.get_endorsers (B b_cur) >>=? fun endorsers_list ->
  List.find_map
    (function
      | {Plugin.RPC.Validators.delegate; slots; _} ->
          if Tezos_crypto.Signature.Public_key_hash.equal delegate endorser then
            Some (delegate, slots)
          else None)
    endorsers_list
  |> function
  | None -> assert false
  | Some (delegate, _slots) ->
      Block.get_round b_cur >>?= fun round ->
      Op.endorsement ~round ~delegate b_cur >>=? fun endorsement ->
      Block.bake ~policy:(By_account baker) ~operation:endorsement b_cur

(** We test that:
  - a delegate that participates enough, gets its endorsing rewards at the end of the cycle,
  - a delegate that does not participating enough during a cycle, doesn't get rewarded.

  The case distinction is made by the boolean argument [sufficient_participation].
  If [sufficient_participation] is true,
  then a validator endorses for as long as the minimal required activity is not reached,
  otherwise it does not endorse.
  Finally, we check the validator's balance at the end of the cycle.
*)
let test_participation ~sufficient_participation () =
  let n_accounts = 2 in
  Context.init_n ~consensus_threshold:1 n_accounts () >>=? fun (b0, accounts) ->
  Context.get_constants (B b0) >>=? fun csts ->
  let blocks_per_cycle = Int32.to_int csts.parametric.blocks_per_cycle in
  let mpr = csts.parametric.minimal_participation_ratio in
  assert (blocks_per_cycle mod mpr.denominator = 0) ;
  (* if this assertion does not hold, then the test might be incorrect *)
  let committee_size = csts.parametric.consensus_committee_size in
  let expected_nb_slots = blocks_per_cycle * committee_size / n_accounts in
  let minimal_nb_active_slots =
    mpr.numerator * expected_nb_slots / mpr.denominator
  in
  let account1, account2 =
    match accounts with a1 :: a2 :: _ -> (a1, a2) | _ -> assert false
  in
  let del1 = Context.Contract.pkh account1 in
  let del2 = Context.Contract.pkh account2 in
  Block.bake ~policy:(By_account del1) b0 >>=? fun b1 ->
  (* To separate concerns, only [del1] bakes: this way, we don't need to
     consider baking rewards for [del2]. Delegate [del2] endorses only
     if the target [minimal_nb_active_slots] is not reached; for the
     rest, it is [del1] that endorses. *)
  List.fold_left_es
    (fun (b_pred, b_crt, endorsing_power) level ->
      let int_level = Int32.of_int level in
      Environment.wrap_tzresult (Raw_level.of_int32 int_level) >>?= fun level ->
      Context.get_endorsing_power_for_delegate (B b_crt) ~levels:[level] del1
      >>=? fun endorsing_power_for_level ->
      let endorser, new_endorsing_power =
        if sufficient_participation && endorsing_power < minimal_nb_active_slots
        then (del2, endorsing_power + endorsing_power_for_level)
        else (del1, endorsing_power)
      in
      bake_and_endorse_once (b_pred, b_crt) del1 endorser >>=? fun b ->
      return (b_crt, b, new_endorsing_power))
    (b0, b1, 0)
    (2 -- (blocks_per_cycle - 1))
  >>=? fun (pred_b, b, _) ->
  Context.Contract.balance (B pred_b) account2 >|=? Tez.to_mutez
  >>=? fun bal2_at_pred_b ->
  Context.Contract.balance (B b) account2 >|=? Tez.to_mutez
  >>=? fun bal2_at_b ->
  (* - If not sufficient_participation, we check that the balance of del2 at b is the
     balance of del2 at pred_b; consequently, no rewards could have been given
     to del2.
     - If sufficient participation, we check that the balance of del2 at b is the
     balance of del2 at pred_b plus the endorsing rewards. *)
  Context.get_endorsing_reward (B b) ~expected_endorsing_power:expected_nb_slots
  >|=? Tez.to_mutez
  >>=? fun er ->
  let endorsing_rewards = if sufficient_participation then er else 0L in
  let expected_bal2_at_b = Int64.add bal2_at_pred_b endorsing_rewards in
  Assert.equal_int64 ~loc:__LOC__ bal2_at_b expected_bal2_at_b

(* We bake and endorse with 1 out of 2 accounts; we monitor the result
   returned by the '../delegates/<pkh>/participation' RPC for the
   non-participating account. *)
let test_participation_rpc () =
  let n_accounts = 2 in
  Context.init2 ~consensus_threshold:1 () >>=? fun (b0, (account1, account2)) ->
  let del1 = Context.Contract.pkh account1 in
  let del2 = Context.Contract.pkh account2 in
  Context.get_constants (B b0) >>=? fun csts ->
  let blocks_per_cycle = Int32.to_int csts.parametric.blocks_per_cycle in
  let Ratio.{numerator; denominator} =
    csts.parametric.minimal_participation_ratio
  in
  let expected_cycle_activity =
    blocks_per_cycle * csts.parametric.consensus_committee_size / n_accounts
  in
  let minimal_cycle_activity =
    expected_cycle_activity * numerator / denominator
  in
  let allowed_missed_slots = expected_cycle_activity - minimal_cycle_activity in
  let expected_endorsing_rewards =
    Tez.mul_exn
      csts.parametric.endorsing_reward_per_slot
      expected_cycle_activity
  in
  Block.bake ~policy:(By_account del1) b0 >>=? fun b1 ->
  List.fold_left_es
    (fun (b_pred, b_crt, total_endorsing_power) level_int ->
      Context.Delegate.participation (B b_crt) del2 >>=? fun info ->
      Assert.equal_int
        ~loc:__LOC__
        info.expected_cycle_activity
        expected_cycle_activity
      >>=? fun () ->
      Assert.equal_int
        ~loc:__LOC__
        info.minimal_cycle_activity
        minimal_cycle_activity
      >>=? fun () ->
      Assert.equal_int ~loc:__LOC__ info.missed_levels (level_int - 1)
      >>=? fun () ->
      let missed_slots = total_endorsing_power in
      Assert.equal_int ~loc:__LOC__ info.missed_slots missed_slots
      >>=? fun () ->
      let remaining_allowed_missed_slots =
        allowed_missed_slots - missed_slots
      in
      Assert.equal_int
        ~loc:__LOC__
        info.remaining_allowed_missed_slots
        (max 0 remaining_allowed_missed_slots)
      >>=? fun () ->
      let endorsing_rewards =
        if remaining_allowed_missed_slots >= 0 then expected_endorsing_rewards
        else Tez.zero
      in
      Assert.equal_tez
        ~loc:__LOC__
        info.expected_endorsing_rewards
        endorsing_rewards
      >>=? fun () ->
      bake_and_endorse_once (b_pred, b_crt) del1 del1 >>=? fun b ->
      (* [level_int] is the level of [b_crt] *)
      level_int |> Int32.of_int |> Raw_level.of_int32
      |> Environment.wrap_tzresult
      >>?= fun level ->
      Context.get_endorsing_power_for_delegate (B b_crt) ~levels:[level] del2
      >>=? fun endorsing_power ->
      return (b_crt, b, total_endorsing_power + endorsing_power))
    (b0, b1, 0)
    (1 -- (blocks_per_cycle - 2))
  >>=? fun (_, _, _) -> return_unit

let tests =
  [
    Tztest.tztest
      "insufficient participation"
      `Quick
      (test_participation ~sufficient_participation:false);
    Tztest.tztest
      "minimal participation"
      `Quick
      (test_participation ~sufficient_participation:true);
    Tztest.tztest "participation RPC" `Quick test_participation_rpc;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("participation monitoring", tests)]
  |> Lwt_main.run
