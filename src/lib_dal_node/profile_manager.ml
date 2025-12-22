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

let encoding = Types.profile_encoding

type unresolved_profile = Profile of t | Random_observer | Empty

let unresolved_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"Profile"
        (Tag 1)
        (obj2
           (req "kind" (constant "controller"))
           (req "controller_profile" encoding))
        (function Profile t -> Some ((), t) | _ -> None)
        (function (), t -> Profile t);
      case
        ~title:"Random_observer"
        (Tag 2)
        (obj1 (req "kind" (constant "random_observer")))
        (function Random_observer -> Some () | _ -> None)
        (function () -> Random_observer);
      case
        ~title:"Empty"
        (Tag 3)
        (obj1 (req "kind" (constant "empty")))
        (function Empty -> Some () | _ -> None)
        (function () -> Empty);
    ]

let empty = Types.Controller Controller_profiles.empty

let bootstrap = Profile Types.Bootstrap

let random_observer = Random_observer

let controller controller_profile =
  Profile (Types.Controller controller_profile)

let is_bootstrap_profile = function
  | Types.Bootstrap -> true
  | Controller _ -> false

let is_prover_profile = function
  | Types.Bootstrap -> false
  | Types.Controller p -> Controller_profiles.(has_observer p || has_operator p)

let is_empty = function
  | Types.Bootstrap -> false
  | Types.Controller controller_profiles ->
      Controller_profiles.is_empty controller_profiles

let is_attester_only_profile = function
  | Types.Bootstrap -> false
  | Types.Controller p -> Controller_profiles.(attester_only p)

let can_publish_on_slot_index slot_index = function
  | Types.Bootstrap -> false
  | Types.Controller p ->
      Controller_profiles.(can_publish_on_slot_index slot_index p)

let merge_profiles ~lower_prio ~higher_prio =
  match (lower_prio, higher_prio) with
  | Profile (Controller c1), Profile (Controller c2) ->
      Profile (Controller (Controller_profiles.merge c1 c2))
  | lower_prio, Empty -> lower_prio
  | _ -> higher_prio

let add_and_register_controller_profiles t ~number_of_slots gs_worker
    (controller_profiles : Controller_profiles.t) =
  match t with
  | Types.Bootstrap -> None
  | Controller controller_sets ->
      let on_new_attester pkh =
        List.iter
          (fun slot_index ->
            Join {slot_index; pkh} |> Gossipsub.Worker.(app_input gs_worker))
          Utils.Infix.(0 -- (number_of_slots - 1))
      in
      Some
        Types.(
          Controller
            (Controller_profiles.merge
               ~on_new_attester
               controller_sets
               controller_profiles))

let resolve_profile t ~number_of_slots =
  match t with
  | Profile t -> t
  | Random_observer ->
      let slot_index = Random.int number_of_slots in
      let controller_profile =
        Controller_profiles.make ~observers:[slot_index] ()
      in
      Types.Controller controller_profile
  | Empty -> empty

let register_profile t ~number_of_slots gs_worker =
  match t with
  | Types.Bootstrap -> t
  | Controller controller_profile -> (
      let t_opt =
        add_and_register_controller_profiles
          empty
          ~number_of_slots
          gs_worker
          controller_profile
      in
      match t_opt with
      | Some t -> t
      | None ->
          (* unreachable*)
          Stdlib.failwith
            "Profile_manager.register_profile: unexpected profile \
             incompatibility")

let validate_slot_indexes t ~number_of_slots =
  let open Result_syntax in
  match t with
  | Types.Bootstrap -> return_unit
  | Controller c -> (
      match
        Controller_profiles.operator_slot_out_of_bounds number_of_slots c
      with
      | Some slot_index ->
          tzfail (Errors.Invalid_slot_index {slot_index; number_of_slots})
      | None -> (
          match
            Controller_profiles.observer_slot_out_of_bounds number_of_slots c
          with
          | Some slot_index ->
              tzfail (Errors.Invalid_slot_index {slot_index; number_of_slots})
          | None -> return_unit))

(* TODO https://gitlab.com/tezos/tezos/-/issues/5934
   We need a mechanism to ease the tracking of newly added/removed topics. *)
let join_topics_for_controller gs_worker committee slots =
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
let join_topics_for_bootstrap ~number_of_slots gs_worker committee =
  (* Join topics for all combinations of (all slots) * (all pkh in comittee) *)
  for slot_index = 0 to number_of_slots - 1 do
    Signature.Public_key_hash.Map.iter
      (fun pkh _shards ->
        let topic = Types.Topic.{slot_index; pkh} in
        if not (Gossipsub.Worker.is_subscribed gs_worker topic) then
          Join topic |> Gossipsub.Worker.(app_input gs_worker))
      committee
  done

let on_new_head t ~number_of_slots gs_worker committee =
  match t with
  | Types.Bootstrap ->
      join_topics_for_bootstrap ~number_of_slots gs_worker committee
  | Controller c ->
      (* The topics associated to observers and operators can change
         if there new active bakers. However, for attesters, new slots
         are not created on new head, only on a new protocol. *)
      let slots = Controller_profiles.get_all_slot_indexes c in
      join_topics_for_controller gs_worker committee slots

let get_profiles t =
  match t with
  | Types.Bootstrap -> Types.Bootstrap
  | Controller profiles -> Controller profiles

let supports_refutations t =
  match get_profiles t with
  | Controller c -> Controller_profiles.(has_operator c)
  | Bootstrap -> false

(* Returns the period relevant for a refutation game. With a block time of 10
   seconds, this corresponds to about 42 days. *)
let get_refutation_game_period proto_parameters =
  proto_parameters.Types.sc_rollup_challenge_window_in_blocks
  + proto_parameters.commitment_period_in_blocks
  + proto_parameters.dal_attested_slots_validity_lag

(* This function returns whether the node should store skip list cells, in
   addition to the retention period for attested data (slots, shards, etc). *)
let get_attested_data_default_store_period t proto_parameters =
  let supports_refutations_bis, period =
    match get_profiles t with
    | Controller c ->
        let has_operator = Controller_profiles.(has_operator c) in
        let period =
          if has_operator then
            (* We double the period (no matter the profile), to give some slack,
               just in case (finalisation period, off by one, attestation_lag,
               refutation games reset after a protocol upgrade, ...). *)
            2 * get_refutation_game_period proto_parameters
          else Constants.shard_retention_period_in_levels
        in
        (has_operator, period)
    | Bootstrap ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/7772
     This period should be zero. *)
        let bootstrap_node_period = 2 * proto_parameters.attestation_lag in
        (false, bootstrap_node_period)
  in
  (* This is just to keep this function synced with {!supports_refutations}. *)
  assert (supports_refutations_bis = supports_refutations t) ;
  period

let get_memory_cache_size t proto_parameters =
  let slots_to_follow =
    match get_profiles t with
    | Controller _ ->
        (* We take the [number_of_slots] from the [proto_parameters] in any case
           to make sure that a DAL node started without any attester profile,
           but who registers one afterwards, will have a cache of the right
           size. *)
        proto_parameters.Types.number_of_slots
    | Bootstrap ->
        (* As the bootstrap is not expected to hold any value, we set it to 1 to
           get an empty cache. *)
        1
  in
  let attestation_lag = proto_parameters.attestation_lag in
  (* We assume the Tenderbake finality to be 2. *)
  let tenderbake_finality = 2 in
  (* Adding a few blocks for safety. *)
  let additional_blocks = 3 in
  slots_to_follow * (attestation_lag + tenderbake_finality + additional_blocks)

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

(* This function determines the storage period taking into account the node's
   [first_seen_level]. Indeed, if the node started for the first time, we do not
   expect it to store skip list cells (even if it supports refutations) for the
   entire required period of 3 months, because it will anyway not have the slot
   pages for this period. Note that this could be further refined by taking into
   account when the corresponding rollup was originated. *)
let get_storage_period profile_ctxt proto_parameters ~head_level
    ~first_seen_level =
  let supports_refutations = supports_refutations profile_ctxt in
  let default_storage_period =
    get_attested_data_default_store_period profile_ctxt proto_parameters
  in
  if supports_refutations then
    (* This deliberately does not take into account the [last_processed_level]. *)
    let online_period =
      match first_seen_level with
      | None -> 0
      | Some first_seen_level ->
          Int32.sub head_level first_seen_level |> Int32.to_int
    in
    (* Even if the node was not online previously, or it was online only for a
       few levels, we still need to store data for the minimal period, defined
       in [get_attested_data_default_store_period]. TODO: refactor to expose
       this value. *)
    let max_period = max (2 * proto_parameters.attestation_lag) online_period in
    min max_period default_storage_period
  else default_storage_period
