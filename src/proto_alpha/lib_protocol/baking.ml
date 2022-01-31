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
open Misc

type error +=
  | (* `Permanent *)
      Insufficient_endorsing_power of {
      endorsing_power : int;
      consensus_threshold : int;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"baking.insufficient_endorsing_power"
    ~title:"Insufficient endorsing power"
    ~description:
      "The endorsing power is insufficient to satisfy the consensus threshold."
    ~pp:(fun ppf (endorsing_power, consensus_threshold) ->
      Format.fprintf
        ppf
        "The endorsing power (%d) is insufficient to satisfy the consensus \
         threshold (%d)."
        endorsing_power
        consensus_threshold)
    Data_encoding.(
      obj2 (req "endorsing_power" int31) (req "consensus_threshold" int31))
    (function
      | Insufficient_endorsing_power {endorsing_power; consensus_threshold} ->
          Some (endorsing_power, consensus_threshold)
      | _ -> None)
    (fun (endorsing_power, consensus_threshold) ->
      Insufficient_endorsing_power {endorsing_power; consensus_threshold})

let bonus_baking_reward ctxt ~endorsing_power =
  let consensus_threshold = Constants.consensus_threshold ctxt in
  let baking_reward_bonus_per_slot =
    Constants.baking_reward_bonus_per_slot ctxt
  in
  let extra_endorsing_power = endorsing_power - consensus_threshold in
  error_when
    Compare.Int.(extra_endorsing_power < 0)
    (Insufficient_endorsing_power {endorsing_power; consensus_threshold})
  >>? fun () ->
  Tez.(baking_reward_bonus_per_slot *? Int64.of_int extra_endorsing_power)

let baking_rights c level =
  let rec f c round =
    Stake_distribution.baking_rights_owner c level ~round
    >>=? fun (c, _slot, (delegate, _)) ->
    return (LCons (delegate, fun () -> f c (Round.succ round)))
  in
  f c Round.zero

type ordered_slots = Slot.t list

(* Slots returned by this function are assumed by consumers to be in increasing
   order, hence the use of [Slot.Range.rev_fold_es]. *)
let endorsing_rights (ctxt : t) level =
  let consensus_committee_size = Constants.consensus_committee_size ctxt in
  Slot.Range.create ~min:0 ~count:consensus_committee_size >>?= fun slots ->
  Slot.Range.rev_fold_es
    (fun (ctxt, map) slot ->
      Stake_distribution.slot_owner ctxt level slot >>=? fun (ctxt, (_, pkh)) ->
      return (ctxt, (slot, pkh) :: acc))
    (ctxt, [])
    slots
  >>=? fun (ctxt, right_owners) ->
  let rights =
    List.fold_left
      (fun acc (slot, pkh) ->
        let slots =
          match Signature.Public_key_hash.Map.find pkh acc with
          | None -> [slot]
          | Some slots -> slot :: slots
        in
        Signature.Public_key_hash.Map.add pkh slots acc)
      Signature.Public_key_hash.Map.empty
      right_owners
  in
  return (ctxt, rights)

let endorsing_rights_by_first_slot ctxt level =
  Slot.Range.create ~min:0 ~count:(Constants.consensus_committee_size ctxt)
  >>?= fun slots ->
  Slot.Range.fold_es
    (fun (ctxt, (delegates_map, slots_map)) slot ->
      Stake_distribution.slot_owner ctxt level slot
      >|=? fun (ctxt, (pk, pkh)) ->
      let (initial_slot, delegates_map) =
        match Signature.Public_key_hash.Map.find pkh delegates_map with
        | None ->
            (slot, Signature.Public_key_hash.Map.add pkh slot delegates_map)
        | Some initial_slot -> (initial_slot, delegates_map)
      in
      (* [slots_map]'keys are the minimal slots of delegates because
         we fold on slots in increasing order *)
      let slots_map =
        Slot.Map.update
          initial_slot
          (function
            | None -> Some (pk, pkh, 1)
            | Some (pk, pkh, count) -> Some (pk, pkh, count + 1))
          slots_map
      in
      (ctxt, (delegates_map, slots_map)))
    (ctxt, (Signature.Public_key_hash.Map.empty, Slot.Map.empty))
    slots
  >>=? fun (ctxt, (_, slots_map)) -> return (ctxt, slots_map)
