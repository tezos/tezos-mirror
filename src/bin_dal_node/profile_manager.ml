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

(** A profile context stores profile-specific data used by the daemon.  *)
type t = Types.profiles

let empty = Types.(empty_operator)

let encoding = Types.profiles_encoding

let is_empty = function
  | Types.Bootstrap -> false
  | Random_observer -> false
  | Operator profile -> Types.is_empty profile

let is_bootstrap_profile = function
  | Types.Bootstrap -> true
  | Random_observer | Operator _ -> false

let add_operator_profiles t proto_parameters gs_worker
    (operator_profiles : Types.operator_profile) =
  match t with
  | Types.Bootstrap -> None
  | Operator operator_sets ->
      let on_new_attester pkh =
        List.iter
          (fun slot_index ->
            Join {slot_index; pkh} |> Gossipsub.Worker.(app_input gs_worker))
          Utils.Infix.(0 -- (proto_parameters.Dal_plugin.number_of_slots - 1))
      in
      Some
        Types.(
          Operator
            (Types.merge_operators
               ~on_new_attester
               operator_sets
               operator_profiles))
  | Random_observer ->
      Stdlib.failwith
        "Profile_manager.add_operator_profiles: random observer should have a \
         slot index assigned at this point"

let add_profiles t proto_parameters gs_worker = function
  | Types.Bootstrap -> if is_empty t then Some Types.Bootstrap else None
  | Random_observer ->
      let slot_index = Random.int proto_parameters.Dal_plugin.number_of_slots in
      add_operator_profiles
        t
        proto_parameters
        gs_worker
        (Types.make_operator_profile ~observers:[slot_index] ())
  | Operator operator_profiles ->
      add_operator_profiles t proto_parameters gs_worker operator_profiles

let validate_slot_indexes t ~number_of_slots =
  let open Result_syntax in
  match t with
  | Types.Bootstrap -> return_unit
  | Random_observer ->
      Stdlib.failwith
        "Profile_manager.add_operator_profiles: random observer should have a \
         slot index assigned at this point"
  | Operator o -> (
      match Types.producer_slot_out_of_bounds number_of_slots o with
      | Some slot_index ->
          tzfail (Errors.Invalid_slot_index {slot_index; number_of_slots})
      | None -> return_unit)

(* TODO https://gitlab.com/tezos/tezos/-/issues/5934
   We need a mechanism to ease the tracking of newly added/removed topics. *)
let join_topics_for_operator gs_worker committee slots =
  List.iter
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
  | Types.Random_observer ->
      Stdlib.failwith
        "Profile_manager.add_operator_profiles: random observer should have a \
         slot index assigned at this point"
  | Bootstrap -> join_topics_for_bootstrap proto_parameters gs_worker committee
  | Operator op ->
      (* The topics associated to observers and producers can change
         if there new active bakers. However, for attesters, new slots
         are not created on new head, only on a new protocol. *)
      let slots = Types.get_all_slot_indexes op in
      join_topics_for_operator gs_worker committee slots

let get_profiles t =
  match t with
  | Types.Bootstrap -> Types.Bootstrap
  | Operator profiles -> Operator profiles
  | Random_observer ->
      Stdlib.failwith
        "Profile_manager.add_operator_profiles: random observer should have a \
         slot index assigned at this point"

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
            store.shards
            commitment
            shard_indices
          |> lwt_map_error (fun e -> `Other e)
    in
    let all_slot_indexes =
      Utils.Infix.(0 -- (proto_parameters.number_of_slots - 1))
    in
    let* flags = List.map_es are_shards_stored all_slot_indexes in
    return (Types.Attestable_slots {slots = flags; published_level})

let get_default_shard_store_period proto_parameters t =
  (* For each profile we double the period, to give some slack just in case
     (finalisation period, off by one, attestation_lag, ...).
     With current mainnet parameters, this total period is 3 months
     for observer & slot producer *)
  let rollup_period =
    2
    * (proto_parameters.Dal_plugin.sc_rollup_challenge_window_in_blocks
     + proto_parameters.commitment_period_in_blocks
     + proto_parameters.dal_attested_slots_validity_lag)
  in
  let attestation_period = 2 * proto_parameters.attestation_lag in
  match get_profiles t with
  (* For observer & Producer, we keep the shards long enough for rollup to work
     correctly; for attester & other profiles, we only want to keep the shards
     during attestation lag period *)
  | Random_observer -> rollup_period
  | Operator op ->
      if Types.(is_producer op || is_observer op) then rollup_period
      else attestation_period
  | Bootstrap -> attestation_period

let profiles_filename = "profiles.json"

(* TODO https://gitlab.com/tezos/tezos/-/issues/7033
   One could use a promise to wait the completion of the previous operation, if
   any. *)
let lock = Lwt_mutex.create ()

let load_profile_ctxt ~base_dir =
  let open Lwt_result_syntax in
  Lwt_mutex.with_lock lock @@ fun () ->
  Lwt.catch
    (fun () ->
      let file_path = Filename.concat base_dir profiles_filename in
      let*! ic = Lwt_io.open_file ~mode:Lwt_io.Input file_path in
      let*! json_str = Lwt_io.read ic in
      let*! () = Lwt_io.close ic in
      match Data_encoding.Json.from_string json_str with
      | Error _ ->
          failwith "DAL node. Failed to load profile, error parsing JSON value"
      | Ok json -> Data_encoding.Json.destruct encoding json |> return)
    (fun exn ->
      failwith
        "DAL node: failed to load the profile context. Exception: %s"
        (Printexc.to_string exn))

let save_profile_ctxt ctxt ~base_dir =
  let open Lwt_result_syntax in
  Lwt_mutex.with_lock lock @@ fun () ->
  Lwt.catch
    (fun () ->
      let value =
        Data_encoding.Json.construct encoding ctxt
        |> Data_encoding.Json.to_string
      in
      let file_path = Filename.concat base_dir profiles_filename in
      let*! oc = Lwt_io.open_file ~mode:Lwt_io.Output file_path in
      let*! () = Lwt_io.write_line oc value in
      let*! () = Lwt_io.close oc in
      return_unit)
    (fun exn ->
      failwith
        "DAL node: failed to save the profile context. Exception: %s"
        (Printexc.to_string exn))
