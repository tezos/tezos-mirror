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
type t = Types.profile

let empty = Types.Operator Operator_profile.empty

let encoding = Types.profile_encoding

let bootstrap = Types.Bootstrap

let random_observer = Types.Random_observer

let operator operator_profile = Types.Operator operator_profile

let is_empty = function
  | Types.Bootstrap -> false
  | Random_observer -> false
  | Operator profile -> Operator_profile.is_empty profile

let is_bootstrap_profile = function
  | Types.Bootstrap -> true
  | Random_observer | Operator _ -> false

let is_prover_profile = function
  | Types.Bootstrap -> false
  | Types.Random_observer -> true
  | Types.Operator p -> Operator_profile.(has_observer p || has_producer p)

let is_attester_only_profile = function
  | Types.Bootstrap -> false
  | Types.Random_observer -> false
  | Types.Operator p -> Operator_profile.(attester_only p)

let merge_profiles ~lower_prio ~higher_prio =
  match (lower_prio, higher_prio) with
  | Types.Bootstrap, Types.Bootstrap -> Types.Bootstrap
  | Operator _, Bootstrap -> Bootstrap
  | Bootstrap, Operator op -> Operator op
  | Operator op1, Operator op2 -> Operator (Operator_profile.merge op1 op2)
  | Random_observer, Random_observer -> Random_observer
  | Random_observer, ((Operator _ | Bootstrap) as profile) -> profile
  | (Operator _ | Bootstrap), Random_observer -> Random_observer

let add_operator_profiles t proto_parameters gs_worker
    (operator_profiles : Operator_profile.t) =
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
            (Operator_profile.merge
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
        (Operator_profile.make ~observers:[slot_index] ())
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
      match Operator_profile.producer_slot_out_of_bounds number_of_slots o with
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
      let slots = Operator_profile.get_all_slot_indexes op in
      join_topics_for_operator gs_worker committee slots

let get_profiles t =
  match t with
  | Types.Bootstrap -> Types.Bootstrap
  | Operator profiles -> Operator profiles
  | Random_observer ->
      Stdlib.failwith
        "Profile_manager.add_operator_profiles: random observer should have a \
         slot index assigned at this point"

let supports_refutations t =
  match get_profiles t with
  | Random_observer -> true
  | Operator op -> Operator_profile.(has_producer op || has_observer op)
  | Bootstrap -> false

(* Returns the period relevant for a refutation game. With a block time of 10
   seconds, this corresponds to about 42 days. *)
let get_refutation_game_period proto_parameters =
  proto_parameters.Dal_plugin.sc_rollup_challenge_window_in_blocks
  + proto_parameters.commitment_period_in_blocks
  + proto_parameters.dal_attested_slots_validity_lag

(* This function returns whether the node should store skip list cells, in
   addition to the retention period for attested data (slots, shards, etc). *)
let get_attested_data_default_store_period t proto_parameters =
  (* We double the period (no matter the profile), to give some slack, just in
     case (finalisation period, off by one, attestation_lag, refutation games
     reset after a protocol upgrade, ...). *)
  let refutation_game_period =
    2 * get_refutation_game_period proto_parameters
  in
  let attestation_period = 2 * proto_parameters.attestation_lag in
  let supports_refutations_bis, period =
    match get_profiles t with
    (* For observer & Producer, we keep the data long enough for rollups to work
       correctly; for attester & other profiles, we only want to keep the data
       during attestation lag period *)
    | Random_observer -> (true, refutation_game_period)
    | Operator op ->
        let is_not_attester_only =
          Operator_profile.(has_producer op || has_observer op)
        in
        let period =
          if is_not_attester_only then refutation_game_period
          else attestation_period
        in
        (is_not_attester_only, period)
    | Bootstrap -> (false, attestation_period)
  in
  (* This is just to keep this function synced with {!supports_refutations}. *)
  assert (supports_refutations_bis = supports_refutations t) ;
  period

let get_attested_data_default_store_period t proto_parameters =
  get_attested_data_default_store_period t proto_parameters

let profiles_filename = "profiles.json"

(* TODO https://gitlab.com/tezos/tezos/-/issues/7033
   One could use a promise to wait the completion of the previous operation, if
   any. *)
let lock = Lwt_mutex.create ()

type error +=
  | Failed_to_load_profile of {reason : string}
  | Failed_to_save_profile of {reason : string}

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.failed_to_load_profiles_file"
    ~title:"Failed to load profiles file"
    ~description:"Failed to load profiles file"
    ~pp:(fun ppf reason ->
      Format.fprintf
        ppf
        "Failed to load %s file. Reason: %s"
        profiles_filename
        reason)
    Data_encoding.(obj1 (req "reason" Data_encoding.string))
    (function Failed_to_load_profile {reason} -> Some reason | _ -> None)
    (fun reason -> Failed_to_load_profile {reason}) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.failed_to_save_profiles_file"
    ~title:"Failed to save profiles file"
    ~description:"Failed to save profiles file"
    ~pp:(fun ppf reason ->
      Format.fprintf
        ppf
        "Failed to save %s file. Reason: %s"
        profiles_filename
        reason)
    Data_encoding.(obj1 (req "reason" Data_encoding.string))
    (function Failed_to_save_profile {reason} -> Some reason | _ -> None)
    (fun reason -> Failed_to_save_profile {reason})

let load_profile_ctxt ~base_dir =
  let open Lwt_result_syntax in
  Lwt_mutex.with_lock lock @@ fun () ->
  Lwt.catch
    (fun () ->
      let file_path = Filename.concat base_dir profiles_filename in
      let*! json_str = Lwt_utils_unix.read_file file_path in
      match Data_encoding.Json.from_string json_str with
      | Error err ->
          fail
            [
              Failed_to_load_profile
                {reason = "error parsing JSON value: " ^ err};
            ]
      | Ok json -> Data_encoding.Json.destruct encoding json |> return)
    (fun exn -> fail [Failed_to_load_profile {reason = Printexc.to_string exn}])

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
      let*! () = Lwt_utils_unix.create_file file_path value in
      return_unit)
    (fun exn -> fail [Failed_to_save_profile {reason = Printexc.to_string exn}])
