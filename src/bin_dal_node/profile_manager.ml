(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Slot_set = Set.Make (Int)
module Pkh_set = Signature.Public_key_hash.Set

(** An operator DAL node can play three different roles:
    - attester for some pkh: checks that the shards assigned to this pkh are published    
    - slot producer for some slot index: splits slots into shards and publishes the shards    
    - slot observer for some slot index: collects the shards
    corresponding to some slot index, reconstructs slots when enough
    shards are seen, and republishes missing shards.

    A single DAL node can play several of these roles at once. We call a profile the
    set of roles played by a DAL node and represent it as a triple of sets. *)
type operator_sets = {
  producers : Slot_set.t;
  attesters : Pkh_set.t;
  observers : Slot_set.t;
}

(** A profile context stores profile-specific data used by the daemon.  *)
type t = Bootstrap | Operator of operator_sets

let empty =
  Operator
    {
      producers = Slot_set.empty;
      attesters = Pkh_set.empty;
      observers = Slot_set.empty;
    }

let is_bootstrap_profile = function Bootstrap -> true | Operator _ -> false

let bootstrap_profile = Bootstrap

let init_attester operator_sets number_of_slots gs_worker pkh =
  if Pkh_set.mem pkh operator_sets.attesters then operator_sets
  else (
    List.iter
      (fun slot_index ->
        Join {slot_index; pkh} |> Gossipsub.Worker.(app_input gs_worker))
      Utils.Infix.(0 -- (number_of_slots - 1)) ;
    {operator_sets with attesters = Pkh_set.add pkh operator_sets.attesters})

let init_producer operator_sets slot_index =
  {
    operator_sets with
    producers = Slot_set.add slot_index operator_sets.producers;
  }

let init_observer operator_sets slot_index =
  {
    operator_sets with
    observers = Slot_set.add slot_index operator_sets.observers;
  }

let add_operator_profiles t proto_parameters gs_worker
    (operator_profiles : Types.operator_profiles) =
  match t with
  | Bootstrap -> None
  | Operator operator_sets ->
      let operator_sets =
        List.fold_left
          (fun operator_sets operator ->
            match operator with
            | Types.Attester pkh ->
                init_attester
                  operator_sets
                  proto_parameters.Dal_plugin.number_of_slots
                  gs_worker
                  pkh
            | Observer {slot_index} -> init_observer operator_sets slot_index
            | Producer {slot_index} -> init_producer operator_sets slot_index)
          operator_sets
          operator_profiles
      in
      Some (Operator operator_sets)

let validate_slot_indexes t ~number_of_slots =
  let open Result_syntax in
  match t with
  | Bootstrap -> return_unit
  | Operator o -> (
      match
        Slot_set.find_first (fun i -> i < 0 || i >= number_of_slots) o.producers
      with
      | Some slot_index ->
          tzfail (Errors.Invalid_slot_index {slot_index; number_of_slots})
      | None -> return_unit)

(* TODO https://gitlab.com/tezos/tezos/-/issues/5934
   We need a mechanism to ease the tracking of newly added/removed topics. *)
let join_topics_for_operator gs_worker committee slots =
  Slot_set.iter
    (fun slot_index ->
      Signature.Public_key_hash.Map.iter
        (fun pkh _shards ->
          let topic = Types.Topic.{slot_index; pkh} in
          if not (Gossipsub.Worker.is_subscribed gs_worker topic) then
            Join topic |> Gossipsub.Worker.(app_input gs_worker))
        committee)
    slots

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/5934
   We need a mechanism to ease the tracking of newly added/removed topics.
   Especially important for bootstrap nodes as the cross product can grow quite large. *)
let join_topics_for_bootstrap proto_parameters gs_worker committee =
  (* Join topics for all combinations of (all slots) * (all pkh in comittee) *)
  for slot_index = 0 to proto_parameters.Dal_plugin.number_of_slots - 1 do
    Signature.Public_key_hash.Map.iter
      (fun pkh _shards ->
        let topic = Types.Topic.{slot_index; pkh} in
        if not (Gossipsub.Worker.is_subscribed gs_worker topic) then
          Join topic |> Gossipsub.Worker.(app_input gs_worker))
      committee
  done

let on_new_head t proto_parameters gs_worker committee =
  match t with
  | Bootstrap -> join_topics_for_bootstrap proto_parameters gs_worker committee
  | Operator {producers; attesters = _; observers} ->
      (* The topics associated to observers and producers can change
         if there new active bakers. However, for attesters, new slots
         are not created on new head, only on a new protocol. *)
      let slots = Slot_set.union producers observers in
      join_topics_for_operator gs_worker committee slots

let get_profiles t =
  match t with
  | Bootstrap -> Types.Bootstrap
  | Operator {producers; attesters; observers} ->
      let producer_profiles =
        Slot_set.fold
          (fun slot_index acc -> Types.Producer {slot_index} :: acc)
          producers
          []
      in
      let observer_profiles =
        Slot_set.fold
          (fun slot_index acc -> Types.Observer {slot_index} :: acc)
          observers
          []
      in
      let attester_profiles =
        Pkh_set.fold
          (fun pkh acc -> Types.Attester pkh :: acc)
          attesters
          producer_profiles
      in
      Types.Operator (attester_profiles @ producer_profiles @ observer_profiles)

let get_attestable_slots ~shard_indices store proto_parameters ~attested_level =
  let open Lwt_result_syntax in
  let expected_number_of_shards = List.length shard_indices in
  if expected_number_of_shards = 0 then return Types.Not_in_committee
  else
    let published_level =
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4612
         Correctly compute [published_level] in case of protocol changes, in
         particular a change of the value of [attestation_lag]. *)
      Int32.(
        sub attested_level (of_int proto_parameters.Dal_plugin.attestation_lag))
    in
    let are_shards_stored slot_index =
      let*! r =
        Slot_manager.get_commitment_by_published_level_and_index
          ~level:published_level
          ~slot_index
          store
      in
      let open Errors in
      match r with
      | Error `Not_found -> return_false
      | Error (#decoding as e) -> fail (e :> [Errors.decoding | Errors.other])
      | Ok commitment ->
          Store.Shards.are_shards_available
            store.shard_store
            commitment
            shard_indices
          |> lwt_map_error (fun e -> `Other e)
    in
    let all_slot_indexes =
      Utils.Infix.(0 -- (proto_parameters.number_of_slots - 1))
    in
    let* flags = List.map_es are_shards_stored all_slot_indexes in
    return (Types.Attestable_slots {slots = flags; published_level})
