(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

(** A profile context stores profile-specific data used by the daemon.  *)
type t = {producers : Slot_set.t; attestors : Pkh_set.t}

let empty = {producers = Slot_set.empty; attestors = Pkh_set.empty}

let init_attestor ctxt number_of_slots gs_worker pkh =
  List.iter
    (fun slot_index ->
      Join Gossipsub.{slot_index; pkh} |> Gossipsub.Worker.(app_input gs_worker))
    Utils.Infix.(0 -- (number_of_slots - 1)) ;
  {ctxt with attestors = Pkh_set.add pkh ctxt.attestors}

let init_producer ctxt slot_index =
  {ctxt with producers = Slot_set.add slot_index ctxt.producers}

let add_profile ctxt proto_parameters gs_worker profile =
  let Dal_plugin.{number_of_slots; _} = proto_parameters in
  let ctxt =
    match profile with
    | Services.Types.Attestor pkh ->
        init_attestor ctxt number_of_slots gs_worker pkh
    | Services.Types.Producer {slot_index} -> init_producer ctxt slot_index
  in
  ctxt

(* TODO https://gitlab.com/tezos/tezos/-/issues/5934
   We need a mechanism to ease the tracking of newly added/removed topics. *)
let join_topics_for_producer gs_worker committee ctxt =
  Slot_set.iter
    (fun slot_index ->
      Signature.Public_key_hash.Map.iter
        (fun pkh _shards ->
          let topic = Gossipsub.{slot_index; pkh} in
          if not (Gossipsub.Worker.is_subscribed gs_worker topic) then
            Join topic |> Gossipsub.Worker.(app_input gs_worker))
        committee)
    ctxt.producers

let on_new_head ctxt gs_worker committee =
  join_topics_for_producer gs_worker committee ctxt

let get_profiles ctxt =
  let producer_profiles =
    Slot_set.fold
      (fun slot_index acc -> Services.Types.Producer {slot_index} :: acc)
      ctxt.producers
      []
  in
  let attestor_profiles =
    Pkh_set.fold
      (fun pkh acc -> Services.Types.Attestor pkh :: acc)
      ctxt.attestors
      producer_profiles
  in
  attestor_profiles @ producer_profiles

let get_attestable_slots ~shard_indices store proto_parameters ~attested_level =
  let open Lwt_result_syntax in
  let expected_number_of_shards = List.length shard_indices in
  if expected_number_of_shards = 0 then return Services.Types.Not_in_committee
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
      | Error `Not_found -> return false
      | Error (#decoding as e) -> fail (e :> [Errors.decoding | Errors.other])
      | Ok commitment ->
          Store.Shards.are_shards_available
            store.shard_store
            commitment
            shard_indices
          |> Errors.other_lwt_result
    in
    let all_slot_indexes =
      Utils.Infix.(0 -- (proto_parameters.number_of_slots - 1))
    in
    let* flags = List.map_es are_shards_stored all_slot_indexes in
    return (Services.Types.Attestable_slots flags)
