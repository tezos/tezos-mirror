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

let bonus_baking_reward ctxt level ~attesting_power =
  let open Lwt_result_syntax in
  let attesting_power = Attesting_power.get ctxt level attesting_power in
  let* ctxt, consensus_threshold =
    Attesting_power.consensus_threshold ctxt level
  in
  let* ctxt, consensus_committee =
    Attesting_power.consensus_committee ctxt level
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
    else if Attesting_power.check_all_bakers_attest_at_level ctxt level then
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
  slots : Slot.t list;
  attesting_power : int64;
}

(* Slots returned by this function are assumed by consumers to be in increasing
   order, hence the use of [Slot.Range.rev_fold_es]. *)
let attesting_rights (ctxt : t) level =
  let consensus_committee_size = Constants.consensus_committee_size ctxt in
  let all_bakers_attest_enabled =
    Attesting_power.check_all_bakers_attest_at_level ctxt level
  in
  let open Lwt_result_syntax in
  let* ctxt, _, delegates = Stake_distribution.stake_info ctxt level in
  let* attesting_power_map =
    List.fold_left_es
      (fun acc_map ((consensus_pk : Consensus_key.pk), power) ->
        let acc_map =
          Signature.Public_key_hash.Map.add consensus_pk.delegate power acc_map
        in
        return acc_map)
      Signature.Public_key_hash.Map.empty
      delegates
  in
  let*? slots = Slot.Range.create ~min:0 ~count:consensus_committee_size in
  let* ctxt, map =
    Slot.Range.rev_fold_es
      (fun (ctxt, map) slot ->
        let* ctxt, consensus_pk =
          Stake_distribution.slot_owner ctxt level slot
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
                      slots = [slot];
                      attesting_power = 0L;
                      (* Updated after, once all the slots are attributed *)
                    }
              | Some slots -> Some {slots with slots = slot :: slots.slots})
            map
        in
        return (ctxt, map))
      (ctxt, Signature.Public_key_hash.Map.empty)
      slots
  in
  let map =
    Signature.Public_key_hash.Map.map
      (fun ({slots; delegate; _} as t) ->
        if all_bakers_attest_enabled then
          let attesting_power =
            Option.value ~default:0L
            @@ Signature.Public_key_hash.Map.find delegate attesting_power_map
          in
          {t with attesting_power}
        else {t with attesting_power = List.length slots |> Int64.of_int})
      map
  in
  return (ctxt, map)

let incr_slot att_rights =
  let one = Attesting_power.make ~slots:1 ~stake:0L in
  Attesting_power.add one att_rights

let attesting_rights_by_first_slot ctxt level :
    (t * Consensus_key.power Slot.Map.t) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let*? slots =
    Slot.Range.create ~min:0 ~count:(Constants.consensus_committee_size ctxt)
  in
  let number_of_shards = Constants.dal_number_of_shards ctxt in
  let* ctxt, (delegates_map, slots_map) =
    Slot.Range.fold_es
      (fun (ctxt, (delegates_map, slots_map)) slot ->
        let+ ctxt, consensus_key =
          Stake_distribution.slot_owner ctxt level slot
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
        (* [slots_map]'keys are the minimal slots of delegates because
           we fold on slots in increasing order *)
        let in_dal_committee =
          if Compare.Int.(Slot.to_int slot < number_of_shards) then 1 else 0
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
                      dal_power = in_dal_committee;
                    }
              | Some
                  ({consensus_key; attesting_power; dal_power} :
                    Consensus_key.power) ->
                  Some
                    {
                      consensus_key;
                      attesting_power = incr_slot attesting_power;
                      dal_power = dal_power + in_dal_committee;
                    })
            slots_map
        in
        (ctxt, (delegates_map, slots_map)))
      (ctxt, (Signature.Public_key_hash.Map.empty, Slot.Map.empty))
      slots
  in
  let* ctxt, _, stake_info_list = Stake_distribution.stake_info ctxt level in
  let slots_map =
    List.fold_left
      (fun acc ((consensus_pk, weight) : Consensus_key.pk * Int64.t) ->
        match
          Signature.Public_key_hash.Map.find consensus_pk.delegate delegates_map
        with
        | None -> acc
        | Some slot -> (
            match Slot.Map.find slot slots_map with
            | None -> acc (* Impossible by construction *)
            | Some v ->
                Slot.Map.add
                  slot
                  {
                    v with
                    attesting_power =
                      Attesting_power.(
                        add (make ~slots:0 ~stake:weight) v.attesting_power);
                  }
                  acc))
      Slot.Map.empty
      stake_info_list
  in
  return (ctxt, slots_map)
