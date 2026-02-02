(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Alpha_context

type error +=
  | (* `Permanent *)
      Insufficient_attesting_power of {
      attesting_power : int64;
      consensus_threshold : int64;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"baking.insufficient_attesting_power"
    ~title:"Insufficient attestation power"
    ~description:
      "The attesting power is insufficient to satisfy the consensus threshold."
    ~pp:(fun ppf (attesting_power, consensus_threshold) ->
      Format.fprintf
        ppf
        "The attesting power (%Ld) is insufficient to satisfy the consensus \
         threshold (%Ld)."
        attesting_power
        consensus_threshold)
    Data_encoding.(
      obj2 (req "attesting_power" int64) (req "consensus_threshold" int64))
    (function
      | Insufficient_attesting_power {attesting_power; consensus_threshold} ->
          Some (attesting_power, consensus_threshold)
      | _ -> None)
    (fun (attesting_power, consensus_threshold) ->
      Insufficient_attesting_power {attesting_power; consensus_threshold})

let bonus_baking_reward ctxt ~attested_level ~attesting_power =
  let open Lwt_result_syntax in
  let attesting_power =
    Attesting_power.get ctxt ~attested_level attesting_power
  in
  let* ctxt, consensus_threshold =
    Attesting_power.consensus_threshold ctxt ~attested_level
  in
  let* ctxt, consensus_committee =
    Attesting_power.consensus_committee ctxt ~attested_level
  in
  let*? baking_reward_bonus_per_block =
    Delegate.Rewards.baking_reward_bonus_per_block ctxt
  in
  let extra_attesting_power = Int64.sub attesting_power consensus_threshold in
  let*? () =
    error_when
      Compare.Int64.(extra_attesting_power < 0L)
      (Insufficient_attesting_power {attesting_power; consensus_threshold})
  in
  let max_extra_attesting_power =
    Int64.sub consensus_committee consensus_threshold
  in
  (* Reward computation depends on the flag. If not activated, we keep the same
     rewards as before: division then multiplication. It activated, we have to change
     the order of operations (because the division will always result in 0 otherwise). *)
  let* reward =
    if Compare.Int64.(max_extra_attesting_power <= 0L) then return (Ok Tez.zero)
    else if
      Attesting_power.check_all_bakers_attest_at_level ctxt ~attested_level
    then
      return
      @@ Tez.mul_ratio
           ~rounding:`Up
           ~num:extra_attesting_power
           ~den:max_extra_attesting_power
           baking_reward_bonus_per_block
    else
      let*? part =
        Tez.(baking_reward_bonus_per_block /? max_extra_attesting_power)
      in
      return Tez.(part *? extra_attesting_power)
  in
  let*? reward in
  return (ctxt, reward)

type ordered_slots = {
  delegate : Signature.public_key_hash;
  consensus_key : Signature.public_key_hash;
  companion_key : Bls.Public_key_hash.t option;
  rounds : Round.t list;
  attesting_power : int64;
  attestation_slot : Slot.t;
}

(* Slots returned by this function are assumed by consumers to be in increasing
   order, hence the use of [Slot.Range.rev_fold_es]. *)
let attesting_rights (ctxt : t) ~attested_level =
  let consensus_committee_size = Constants.consensus_committee_size ctxt in
  let all_bakers_attest_enabled =
    Attesting_power.check_all_bakers_attest_at_level ctxt ~attested_level
  in
  let open Lwt_result_syntax in
  let* ctxt, _, delegates = Stake_distribution.stake_info ctxt attested_level in
  let* attesting_power_map, _ =
    List.fold_left_es
      (fun (acc_map, i) ((consensus_pk : Consensus_key.pk), power) ->
        let acc_map =
          Signature.Public_key_hash.Map.add
            consensus_pk.delegate
            (power, i, consensus_pk)
            acc_map
        in
        let*? succ_i = Slot.succ i in
        return (acc_map, succ_i))
      (Signature.Public_key_hash.Map.empty, Slot.zero)
      delegates
  in
  let*? slots = Slot.Range.create ~min:0 ~count:consensus_committee_size in
  let* ctxt, map =
    Slot.Range.rev_fold_es
      (fun (ctxt, map) slot ->
        let*? round = Round.of_slot slot in
        let* ctxt, _, consensus_pk =
          Stake_distribution.baking_rights_owner ctxt attested_level ~round
        in
        let map =
          Signature.Public_key_hash.Map.update
            consensus_pk.delegate
            (function
              | None ->
                  Some
                    {
                      delegate = consensus_pk.delegate;
                      consensus_key = consensus_pk.consensus_pkh;
                      companion_key = consensus_pk.companion_pkh;
                      rounds = [round];
                      attesting_power = 0L;
                      (* Updated after, once all the slots are attributed *)
                      attestation_slot = slot;
                    }
              | Some info ->
                  Some
                    {
                      info with
                      rounds = round :: info.rounds;
                      attestation_slot = slot;
                    })
            map
        in
        return (ctxt, map))
      (ctxt, Signature.Public_key_hash.Map.empty)
      slots
  in
  let map =
    Signature.Public_key_hash.Map.map
      (fun ({rounds; delegate; _} as t) ->
        if all_bakers_attest_enabled then
          match
            Signature.Public_key_hash.Map.find delegate attesting_power_map
          with
          | Some (attesting_power, attestation_slot, _) ->
              (* We override the attestation slot to be accurate wrt the feature flag *)
              {t with attesting_power; attestation_slot}
          | None -> t
        else
          (* The attestation slot already has the correct value *)
          {t with attesting_power = List.length rounds |> Int64.of_int})
      map
  in
  (* Add delegates without rounds *)
  let map =
    if all_bakers_attest_enabled then
      Signature.Public_key_hash.Map.fold
        (fun pkh
             ( attesting_power,
               attestation_slot,
               (consensus_pk : Consensus_key.pk) )
             acc
           ->
          match Signature.Public_key_hash.Map.find pkh map with
          | Some _ -> acc
          | None ->
              Signature.Public_key_hash.Map.add
                pkh
                {
                  delegate = consensus_pk.delegate;
                  consensus_key = consensus_pk.consensus_pkh;
                  companion_key = consensus_pk.companion_pkh;
                  rounds = [];
                  attesting_power;
                  attestation_slot;
                }
                acc)
        attesting_power_map
        map
    else map
  in
  return (ctxt, map)

let incr_slot att_rights =
  let one = Attesting_power.make ~slots:1 ~baking_power:0L in
  Attesting_power.add one att_rights

let attesting_rights_by_first_slot ctxt ~attested_level :
    (t * Consensus_key.power Slot.Map.t) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let*? slots =
    Slot.Range.create ~min:0 ~count:(Constants.consensus_committee_size ctxt)
  in
  let* ctxt, (delegates_map, slots_map) =
    Slot.Range.fold_es
      (fun (ctxt, (delegates_map, slots_map)) slot ->
        let*? round = Round.of_slot slot in
        let+ ctxt, _, consensus_key =
          Stake_distribution.baking_rights_owner ctxt attested_level ~round
        in
        let initial_slot, delegates_map =
          match
            Signature.Public_key_hash.Map.find
              consensus_key.delegate
              delegates_map
          with
          | None ->
              ( slot,
                Signature.Public_key_hash.Map.add
                  consensus_key.delegate
                  slot
                  delegates_map )
          | Some initial_slot -> (initial_slot, delegates_map)
        in
        let slots_map =
          Slot.Map.update
            initial_slot
            (function
              | None ->
                  Some
                    {
                      consensus_key;
                      attesting_power =
                        incr_slot Attesting_power.zero
                        (* stake added in the next step *);
                    }
              | Some ({consensus_key; attesting_power} : Consensus_key.power) ->
                  Some
                    {consensus_key; attesting_power = incr_slot attesting_power})
            slots_map
        in
        (ctxt, (delegates_map, slots_map)))
      (ctxt, (Signature.Public_key_hash.Map.empty, Slot.Map.empty))
      slots
  in
  let all_bakers_attest_enabled =
    Attesting_power.check_all_bakers_attest_at_level ctxt ~attested_level
  in
  let* ctxt, _, stake_info_list =
    Stake_distribution.stake_info ctxt attested_level
  in
  let* slots_map, _ =
    List.fold_left_es
      (fun (acc, i) ((consensus_key, weight) : Consensus_key.pk * Int64.t) ->
        match
          Signature.Public_key_hash.Map.find
            consensus_key.delegate
            delegates_map
        with
        | None ->
            if all_bakers_attest_enabled then
              (* The delegate doesn't have a round, but under all bakers attest,
                 it is still added to the map *)
              let*? succ_i = Slot.succ i in
              return
                ( Slot.Map.add
                    i
                    {
                      Consensus_key.consensus_key;
                      attesting_power =
                        Attesting_power.make ~slots:0 ~baking_power:weight;
                    }
                    acc,
                  succ_i )
            else return (acc, i)
        | Some slot -> (
            match Slot.Map.find slot slots_map with
            | None -> return (acc, i) (* Impossible by construction *)
            | Some v ->
                let v =
                  {
                    v with
                    attesting_power =
                      Attesting_power.(
                        add
                          (make ~slots:0 ~baking_power:weight)
                          v.attesting_power);
                  }
                in
                if all_bakers_attest_enabled then
                  let*? succ_i = Slot.succ i in
                  return (Slot.Map.add i v acc, succ_i)
                else return (Slot.Map.add slot v acc, i)))
      (Slot.Map.empty, Slot.zero)
      stake_info_list
  in
  return (ctxt, slots_map)

let delegate_to_shard_count ctxt level =
  let open Lwt_result_syntax in
  let*? slots =
    Slot.Range.create ~min:0 ~count:(Constants.dal_number_of_shards ctxt)
  in
  let* ctxt, map =
    Slot.Range.fold_es
      (fun (ctxt, map) shard_index ->
        let*? round = Round.of_slot shard_index in
        let* ctxt, _, consensus_pk =
          Stake_distribution.baking_rights_owner ctxt level ~round
        in
        let map =
          Signature.Public_key_hash.Map.update
            consensus_pk.delegate
            (function
              | None -> Some 1 | Some num_shards -> Some (num_shards + 1))
            map
        in
        return (ctxt, map))
      (ctxt, Signature.Public_key_hash.Map.empty)
      slots
  in
  return (ctxt, map)
