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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/6958
   Unify this type with the one from `src/lib_dal_node_services/types.ml` *)

(** A profile context stores profile-specific data used by the daemon.  *)
type t = Bootstrap | Operator of operator_sets

let operator_sets_encoding =
  let open Data_encoding in
  conv
    (fun {producers; observers; attesters} ->
      ( Slot_set.elements producers,
        Slot_set.elements observers,
        Pkh_set.elements attesters ))
    (fun (producers, observers, attesters) ->
      {
        producers = Slot_set.of_list producers;
        observers = Slot_set.of_list observers;
        attesters = Pkh_set.of_list attesters;
      })
    (obj3
       (req "producers" (list int16))
       (req "observers" (list int16))
       (req "attesters" (list Signature.Public_key_hash.encoding)))

let encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Bootstrap"
        (Tag 0)
        (obj1 (req "kind" (constant "bootstrap")))
        (function Bootstrap -> Some () | _ -> None)
        (fun () -> Bootstrap);
      case
        ~title:"Operator"
        (Tag 1)
        (obj2
           (req "kind" (constant "operator"))
           (req "operator_profiles" operator_sets_encoding))
        (function Operator s -> Some ((), s) | _ -> None)
        (function (), s -> Operator s);
    ]

let empty =
  Operator
    {
      producers = Slot_set.empty;
      attesters = Pkh_set.empty;
      observers = Slot_set.empty;
    }

let is_empty = function
  | Bootstrap -> false
  | Operator {producers; attesters; observers} ->
      Slot_set.is_empty producers
      && Pkh_set.is_empty attesters
      && Slot_set.is_empty observers

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

let add_profiles t proto_parameters gs_worker = function
  | Types.Bootstrap -> if is_empty t then Some bootstrap_profile else None
  | Random_observer ->
      let slot_index = Random.int proto_parameters.Dal_plugin.number_of_slots in
      add_operator_profiles t proto_parameters gs_worker [Observer {slot_index}]
  | Operator operator_profiles ->
      add_operator_profiles t proto_parameters gs_worker operator_profiles

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
      let producer_profiles acc =
        Slot_set.fold
          (fun slot_index acc -> Types.Producer {slot_index} :: acc)
          producers
          acc
      in
      let observer_profiles acc =
        Slot_set.fold
          (fun slot_index acc -> Types.Observer {slot_index} :: acc)
          observers
          acc
      in
      let attester_profiles acc =
        Pkh_set.fold (fun pkh acc -> Types.Attester pkh :: acc) attesters acc
      in
      Types.Operator
        (attester_profiles @@ producer_profiles @@ observer_profiles @@ [])

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
