(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Profiler =
  (val Tezos_profiler.Profiler.wrap Gossipsub.Profiler.gossipsub_profiler)

let string_of_validation_error = function
  | `Invalid_degree_strictly_less_than_expected Cryptobox.{given; expected} ->
      Format.sprintf
        "Invalid_degree_strictly_less_than_expected. Given: %d, expected: %d"
        given
        expected
  | `Invalid_shard -> "Invalid_shard"
  | `Shard_index_out_of_range s ->
      Format.sprintf "Shard_index_out_of_range(%s)" s
  | `Shard_length_mismatch -> "Shard_length_mismatch"
  | `Prover_SRS_not_loaded -> "Prover_SRS_not_loaded"

(** [gossipsub_app_message_payload_validation ~disable_shard_validation cryptobox message
    message_id] allows checking whether the given [message] identified by
    [message_id] is valid with the current [cryptobox] parameters. The validity check is
    done by verifying that the shard in the message effectively belongs to the
    commitment given by [message_id]. The whole validation can be bypassed if
    [~disable_shard_validation] is set to [true]. *)
let gossipsub_app_message_payload_validation ~disable_shard_validation cryptobox
    message_id message =
  if disable_shard_validation then `Valid
  else
    let Types.Message.{share; shard_proof} = message in
    let Types.Message_id.{commitment; shard_index; _} = message_id in
    let shard = Cryptobox.{share; index = shard_index} in
    let res =
      (Dal_metrics.sample_time
         ~sampling_frequency:Constants.shards_verification_sampling_frequency
         ~metric_updater:Dal_metrics.update_shards_verification_time
         ~to_sample:(fun () ->
           Cryptobox.verify_shard cryptobox commitment shard shard_proof)
      [@profiler.wrap_f
        {driver_ids = [Opentelemetry]}
          (Opentelemetry_helpers.trace_slot
             ~attrs:[("shard_index", `Int shard_index)]
             ~name:"verify_shard"
             ~slot_commitment:commitment
             ~slot_id:
               Types.Slot_id.
                 {
                   slot_level = message_id.level;
                   slot_index = message_id.slot_index;
                 })])
    in
    match res with
    | Ok () -> `Valid
    | Error err ->
        let validation_error = string_of_validation_error err in
        Event.emit_dont_wait__message_validation_error
          ~message_id
          ~validation_error ;
        `Invalid
    | exception exn ->
        (* Don't crash if crypto raised an exception. *)
        let validation_error = Printexc.to_string exn in
        Event.emit_dont_wait__message_validation_error
          ~message_id
          ~validation_error ;
        `Invalid

let gossipsub_message_id_commitment_validation ctxt proto_parameters message_id
    =
  let store = Node_context.get_store ctxt in
  let slot_index = message_id.Types.Message_id.slot_index in
  match
    Store.Slot_id_cache.find_opt
      (Store.finalized_commitments store)
      Types.Slot_id.{slot_level = message_id.Types.Message_id.level; slot_index}
  with
  | Some commitment ->
      if
        Cryptobox.Commitment.equal
          commitment
          message_id.Types.Message_id.commitment
      then `Valid
      else `Invalid
  | None ->
      if slot_index >= 0 && slot_index < proto_parameters.Types.number_of_slots
      then
        (* We know the message is not [Outdated], because this has already
           been checked in {!gossipsub_app_messages_validation}. *)
        `Unknown
      else `Invalid

let gossipsub_message_id_topic_validation ctxt proto_parameters message_id =
  let attestation_level =
    Int32.(
      pred
      @@ add
           message_id.Types.Message_id.level
           (of_int proto_parameters.Types.attestation_lag))
  in
  let shard_indices_opt =
    Node_context.get_fetched_assigned_shard_indices
      ctxt
      ~pkh:message_id.Types.Message_id.pkh
      ~level:attestation_level
  in
  match shard_indices_opt with
  | None ->
      (* If DAL committees of [attestation_level] are fetched each time the
         corresponding published/finalized_level is processed, this should not
         happen. *)
      `Unknown
  | Some shard_indices ->
      if
        List.mem
          ~equal:( = )
          message_id.Types.Message_id.shard_index
          shard_indices
      then `Valid
      else `Invalid

let gossipsub_message_id_validation ctxt proto_parameters message_id =
  match
    gossipsub_message_id_commitment_validation ctxt proto_parameters message_id
  with
  | `Valid ->
      gossipsub_message_id_topic_validation ctxt proto_parameters message_id
  | other -> other

(** [gossipsub_app_messages_validation ctxt cryptobox ~head_level
    attestation_lag ?message ~message_id ()] checks for the validity of the
    given message (if any) and message id.

    First, the message id's validity is checked if the application cares about
    it and is not outdated (Otherwise `Unknown or `Outdated is returned,
    respectively). This is done thanks to
    {!gossipsub_message_id_validation}. Then, if a message is given,
    {!gossipsub_app_message_payload_validation} is used to check its
    validity. *)
let gossipsub_app_messages_validation ctxt cryptobox ~head_level
    proto_parameters ?message ~message_id () =
  if Node_context.is_bootstrap_node ctxt then
    (* 1. As bootstrap nodes advertise their profiles to controller
       nodes, they shouldn't receive messages or messages ids. If this
       happens, received data are considered as spam (invalid), and the remote
       peer might be punished, depending on the Gossipsub implementation. *)
    `Invalid
  else if
    Int32.(
      sub head_level message_id.Types.Message_id.level
      > of_int
          (proto_parameters.Types.attestation_lag + Constants.validation_slack))
  then
    (* 2. Nodes don't care about messages whose ids are too old. Gossipsub
         should only be used for the dissemination of fresh data. Old data could
         be retrieved using another method. *)
    `Outdated
  else
    match gossipsub_message_id_validation ctxt proto_parameters message_id with
    | `Valid ->
        (* 3. Only check for message validity if the message_id is valid. *)
        let res =
          Option.fold
            message
            ~none:`Valid
            ~some:
              (gossipsub_app_message_payload_validation
                 ~disable_shard_validation:
                   (Node_context.get_disable_shard_validation ctxt)
                 cryptobox
                 message_id)
        in
        (if res = `Valid then
           let store = Node_context.get_store ctxt in
           let traps_store = Store.traps store in
           (* TODO: https://gitlab.com/tezos/tezos/-/issues/7742
                The [proto_parameters] are those for the last known finalized
                level, which may differ from those of the slot level. This
                will be an issue when the value of the [traps_fraction]
                changes. (We cannot use {!Node_context.get_proto_parameters},
                as it is not monad-free; we'll need to use mapping from levels
                to parameters.) *)
           Option.iter
             (Slot_manager.maybe_register_trap
                traps_store
                ~traps_fraction:proto_parameters.traps_fraction
                message_id)
             message) ;
        res
    | other ->
        (* 4. In the case the message_id is not valid. *)
        other

type batch_identifier = {level : int32; slot_index : int}

module Batch_identifier_hashable = struct
  type t = batch_identifier

  let equal a b = a.level = b.level && a.slot_index = b.slot_index

  let hash = Hashtbl.hash
end

module Batch_tbl = Hashtbl.Make (Batch_identifier_hashable)

type batch_elem = {
  index : int;
  id : Types.Message_id.t;
  message : Types.Message.t;
}

type 'a batch_result = {
  to_check_in_batch : batch_elem list Batch_tbl.t;
  not_valid : (int * 'a) list;
      (* List of indices in the original batch with the error to output for
         this element. *)
}

let triage ctxt head_level proto_parameters batch =
  let to_check_in_batch =
    Batch_tbl.create proto_parameters.Types.number_of_slots
  in
  let not_valid =
    List.fold_left_i
      (fun index
           not_valid
           ( _peer,
             _topic,
             (Types.Message_id.{level; slot_index; _} as id),
             message,
             _peers )
         ->
        if
          Int32.(
            sub head_level level
            > of_int
                (proto_parameters.Types.attestation_lag
               + Constants.validation_slack))
        then (index, `Outdated) :: not_valid
        else
          match gossipsub_message_id_validation ctxt proto_parameters id with
          | `Valid ->
              (* The shard is added to the right batch.
                 Since the message id check has already been performed, we have
                 the guarantee that all shards associated to a given
                 (level, slot_index) pair have the commitment published on the L1,
                 hence they all belong to the same slot, hence the shards can be
                 passed to the function verifying several shards simultaneously. *)
              let already_sorted =
                Batch_tbl.find_opt to_check_in_batch {level; slot_index}
              in
              let new_entry =
                match already_sorted with
                | None -> [{index; id; message}]
                | Some list -> {index; id; message} :: list
              in
              Batch_tbl.replace to_check_in_batch {level; slot_index} new_entry ;
              not_valid
          | other ->
              (* In the case the message id is not Valid. *)
              (index, other) :: not_valid)
      []
      batch
  in
  {to_check_in_batch; not_valid}

module Batches_stats =
  Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
    (struct
      include Int32

      let hash = Hashtbl.hash
    end)

type stat = {total_shards : int; total_duration : float; batches_count : int}

let batches_stats_tbl = Batches_stats.create 5

let last_stat_level = ref None

let update_batches_stats number_of_slots cryptobox batch_size head_level
    batch_ll duration =
  let {Cryptobox.number_of_shards; _} = Cryptobox.parameters cryptobox in
  let shard_distribution = Array.init number_of_slots (fun _ -> 0) in
  List.iter
    (fun l ->
      List.iter
        (fun ({slot_index; _}, elems) ->
          shard_distribution.(slot_index) <- List.length elems)
        l)
    batch_ll ;
  let shard_percentage =
    float_of_int (Array.fold_left ( + ) 0 shard_distribution)
    /. (float_of_int number_of_shards *. float_of_int number_of_slots)
    *. 100.
  in
  let s = Batches_stats.find_opt batches_stats_tbl head_level in
  let batch_id =
    match s with
    | None ->
        Batches_stats.replace
          batches_stats_tbl
          head_level
          {
            total_shards = batch_size;
            total_duration = duration;
            batches_count = 1;
          } ;
        1
    | Some v ->
        let next_batch_id = v.batches_count + 1 in
        Batches_stats.replace
          batches_stats_tbl
          head_level
          {
            total_shards = v.total_shards + batch_size;
            total_duration = v.total_duration +. duration;
            batches_count = next_batch_id;
          } ;
        next_batch_id
  in
  Event.emit_dont_wait__batch_validation_stats
    ~batch_id
    ~head_level
    ~number_of_shards:batch_size
    ~shard_percentage
    ~duration ;
  Event.emit_dont_wait__batch_validation_distribution_stats
    ~batch_id
    ~head_level
    ~shard_distribution

let may_finalize_batch_stats last_stat_level head_level =
  match !last_stat_level with
  | None -> last_stat_level := Some head_level
  | Some last_level ->
      (* When a new head is received, we assume the completion of the validation
         of the shards of the previous level. This might be an incorrect
         assumption when the shard distribution and validation duration exceed
         the block time but it is good enough to observe the overall
         behaviour. *)
      if last_level != head_level then
        match Batches_stats.find_opt batches_stats_tbl last_level with
        | None -> ()
        | Some {total_shards; total_duration; batches_count} ->
            let () =
              Event.emit_dont_wait__batch_validation_completion_stats
                ~level:last_level
                ~total_shard_processed:total_shards
                ~total_batches_processed:batches_count
                ~duration:total_duration
            in
            last_stat_level := Some head_level
      else ()

(* Utility function that splits the given [seq] into a list of [n] lists in a
   round robin fashion, all sub-lists having roughly the same number of
   elements. *)
let round_robin_seq n seq =
  let rec loop acc_full acc seq =
    match seq () with
    | Seq.Nil -> acc @ acc_full
    | Seq.Cons (hd, tl) -> (
        match acc with
        | [] -> loop [] acc_full seq
        | acc_hd :: acc_tl -> loop ((hd :: acc_hd) :: acc_full) acc_tl tl)
  in
  loop [] (Stdlib.List.init n (fun _ -> [])) seq

let gossipsub_batch_validation ctxt cryptobox ~head_level proto_parameters batch
    =
  if Node_context.is_bootstrap_node ctxt then
    (* As bootstrap nodes advertise their profiles to controller
       nodes, they shouldn't receive messages or messages ids. If this
       happens, received data are considered as spam (invalid), and the remote
       peer might be punished, depending on the Gossipsub implementation. *)
    List.map (fun _ -> `Invalid) batch
  else
    let batch_size = List.length batch in
    let result = Array.init batch_size (fun _ -> `Unknown) in
    let {to_check_in_batch; not_valid} =
      (triage
         ctxt
         head_level
         proto_parameters
         batch [@profiler.record_f {verbosity = Notice} "triage"])
    in
    List.iter (fun (index, error) -> result.(index) <- error) not_valid ;

    let store = Node_context.get_store ctxt in
    let traps_store = Store.traps store in

    (* [treat_batch] does not invalidate a whole slot when a single shard is
       invalid, otherwise it would be possible for a byzantine actor to craft
       a wrong message with the id of the published slot to prevent the validation
       of all the valid shards associated to this slot.
       Important: this function is in the scope of Eio runtime and prohibits the
       usage of Lwt. Lwt calls must be wrapped in a proper way. See
       https://gitlab.com/tezos/tezos/-/issues/8140. *)
    let rec treat_batch = function
      | [] -> ()
      | ({level; slot_index}, batch_list) :: remaining_to_treat -> (
          let shards, proofs =
            List.fold_left
              (fun (shards, proofs) {message; id; _} ->
                let Types.Message.{share; shard_proof} = message in
                let Types.Message_id.{shard_index; _} = id in
                let shard = Cryptobox.{share; index = shard_index} in
                (shard :: shards, shard_proof :: proofs))
              ([], [])
              batch_list
          in
          (* We never add empty list in the to_check_in_batch table. *)
          let {id = {commitment; _}; _} =
            Option.value_f
              ~default:(fun () -> assert false)
              (List.hd batch_list)
          in
          let res =
            if Node_context.get_disable_shard_validation ctxt then Ok ()
            else
              Dal_metrics.sample_time
                ~sampling_frequency:
                  Constants.shards_verification_sampling_frequency
                ~metric_updater:Dal_metrics.update_shards_verification_time
                ~to_sample:(fun () ->
                  Cryptobox.verify_shard_multi
                    cryptobox
                    commitment
                    shards
                    proofs)
              [@profiler.wrap_f
                {driver_ids = [Opentelemetry]}
                  (Opentelemetry_helpers.trace_slot
                     ~attrs:[("number_of_shards", `Int (List.length shards))]
                     ~name:"verify_shards"
                     ~slot_commitment:commitment
                     ~slot_id:{Types.Slot_id.slot_level = level; slot_index})]
          in
          match res with
          | Ok () ->
              List.iter
                (fun {index; id; message} ->
                  (* We register traps only if the message is valid. *)
                  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7742
                      The [proto_parameters] are those for the last known finalized
                      level, which may differ from those of the slot level. This
                      will be an issue when the value of the [traps_fraction]
                      changes. (We cannot use {!Node_context.get_proto_parameters},
                      as it is not monad-free; we'll need to use mapping from levels
                      to parameters.) *)
                  Slot_manager.maybe_register_trap
                    traps_store
                    ~traps_fraction:proto_parameters.traps_fraction
                    id
                    message ;
                  result.(index) <- `Valid)
                batch_list ;
              treat_batch remaining_to_treat
          | Error err ->
              let validation_error = string_of_validation_error err in
              Tezos_bees.Hive.async_lwt (fun () ->
                  Event.emit_batch_validation_error
                    ~level
                    ~slot_index
                    ~validation_error) ;
              let batch_size = List.length batch_list in
              if batch_size = 1 then
                List.iter
                  (fun {index; _} -> result.(index) <- `Invalid)
                  batch_list
              else
                (* Since [verify_multi] does not outputs which shards are the
                   invalid ones and since we do not want to invalidate legitimate
                   shards because a byzantine actor added some false shards, we
                   revalidate all the shards by half recursiveley until we have
                   identified the invalid shards. *)
                let first_half, second_half =
                  List.split_n (batch_size / 2) batch_list
                in
                treat_batch
                  (({level; slot_index}, first_half)
                  :: ({level; slot_index}, second_half)
                  :: remaining_to_treat)
          | exception exn ->
              (* Don't crash if crypto raised an exception. *)
              let validation_error = Printexc.to_string exn in
              Tezos_bees.Hive.async_lwt (fun () ->
                  Event.emit_batch_validation_error
                    ~level
                    ~slot_index
                    ~validation_error) ;
              let batch_size = List.length batch_list in
              if batch_size = 1 then
                List.iter
                  (fun {index; _} -> result.(index) <- `Invalid)
                  batch_list
              else
                let first_half, second_half =
                  List.split_n (batch_size / 2) batch_list
                in
                treat_batch
                  (({level; slot_index}, first_half)
                  :: ({level; slot_index}, second_half)
                  :: remaining_to_treat))
    in
    let domains = Tezos_bees.Task_worker.number_of_domains in
    let s = Unix.gettimeofday () in
    (* Split the batches into [domains] sub-batches to feed each bee workers.
       The load's distribution is likely to be slightly unbalanced. *)
    let sub_batches =
      round_robin_seq domains (Batch_tbl.to_seq to_check_in_batch)
    in
    let results =
      Tezos_bees.Task_worker.launch_tasks_and_wait
        "batch"
        treat_batch
        sub_batches
    in
    (* Ensure that all tasks are successful. *)
    let _ : unit list = Tezos_bees.Task_worker.bind_and_raise_all results in
    let duration = Unix.gettimeofday () -. s in
    update_batches_stats
      proto_parameters.number_of_slots
      cryptobox
      batch_size
      head_level
      sub_batches
      duration ;
    let () = may_finalize_batch_stats last_stat_level head_level in
    Array.to_list result
