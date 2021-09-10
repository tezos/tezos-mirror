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
    Invocation: dune exec src/proto_alpha/lib_protocol/test/main.exe -- test "^participation"
    Subject:    Participation monitoring in Tenderbake
*)

open Protocol
open Alpha_context

(** [baker] bakes and [endorser] endorses *)
let bake_and_endorse_once (b_pred, b_cur) baker endorser =
  let open Context in
  Context.get_endorsers (B b_cur) >>=? fun endorsers_list ->
  List.find_map
    (function
      | {Plugin.RPC.Validators.delegate; slots; _} ->
          if Signature.Public_key_hash.equal delegate endorser then
            Some (delegate, slots)
          else None)
    endorsers_list
  |> function
  | None -> assert false
  | Some delegate ->
      Block.get_round b_cur >>?= fun round ->
      Op.endorsement ~round ~delegate ~endorsed_block:b_cur (B b_pred) ()
      >>=? fun endorsement ->
      let endorsement = Operation.pack endorsement in
      Block.bake ~policy:(By_account baker) ~operation:endorsement b_cur

(** We test that:
  - a delegate that participates enough, gets its endorsing rewards at the end of the cycle,
  - a delegate that does not participating enough during a cycle, doesn't get rewarded.

  The case distinction is made by the boolean argument [sufficient_participation].
  To perform these checks we let
    [sufficient_endorsed_levels = minimal_participation_ratio * blocks_per_cycle].
  If [sufficient_participation] is true,
  then a validator endorses for [sufficient_endorsed_levels] levels,
  otherwise it endorses for [sufficient_endorsed_level - 1] levels.
  Finally, we check the validator's balance at the end of the cycle.
*)
let test_participation ~sufficient_participation () =
  let n_accounts = 2 in
  Context.init ~consensus_threshold:1 n_accounts >>=? fun (b0, accounts) ->
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
  let (account1, account2) =
    match accounts with a1 :: a2 :: _ -> (a1, a2) | _ -> assert false
  in
  Context.Contract.pkh account1 >>=? fun del1 ->
  Context.Contract.pkh account2 >>=? fun del2 ->
  Block.bake ~policy:(By_account del1) b0 >>=? fun b1 ->
  (* To separate concerns, only del1 bakes: this way, we don't need to consider
     baking rewards for del2. Delegate del2 endorses for [target_endorsements]
     times; for the rest, it is del1 that endorses. *)
  List.fold_left_es
    (fun (b_pred, b_crt, endorsing_power) level ->
      let int_level = Int32.of_int level in
      Environment.wrap_tzresult (Raw_level.of_int32 int_level) >>?= fun level ->
      Context.get_endorsing_power_for_delegate (B b_crt) ~levels:[level] del1
      >>=? fun endorsing_power_for_level ->
      let (endorser, new_endorsing_power) =
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

let tests =
  [
    Tztest.tztest
      "test insufficient participation"
      `Quick
      (test_participation ~sufficient_participation:false);
    Tztest.tztest
      "test minimal participation"
      `Quick
      (test_participation ~sufficient_participation:true);
  ]
