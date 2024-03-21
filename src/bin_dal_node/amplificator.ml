(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let with_amplification_lock (ready_ctxt : Node_context.ready_ctxt) slot_id f =
  let open Lwt_result_syntax in
  let open Types.Slot_id.Set in
  if mem slot_id ready_ctxt.ongoing_amplifications then return_unit
  else
    let () =
      ready_ctxt.ongoing_amplifications <-
        add slot_id ready_ctxt.ongoing_amplifications
    in
    Lwt.catch f (fun exn ->
        ready_ctxt.ongoing_amplifications <-
          remove slot_id ready_ctxt.ongoing_amplifications ;
        (* Silently catch Unix errors to let the DAL node survive in
           these cases. *)
        match exn with
        | Unix.Unix_error _ -> return_unit
        | exn -> Lwt.reraise exn)

let amplify (shard_store : Store.Shards.t) (slot_store : Store.node_store)
    commitment ~published_level ~slot_index gs_worker node_ctxt =
  let open Lwt_result_syntax in
  match Node_context.get_status node_ctxt with
  | Starting ->
      (* The cryptobox is not yet available so we cannot reconstruct
         slots yet. *)
      return_unit
  | Ready
      ({cryptobox; shards_proofs_precomputation; proto_parameters; _} as
      ready_ctxt) ->
      let dal_parameters = Cryptobox.parameters cryptobox in
      let number_of_shards = dal_parameters.number_of_shards in
      let redundancy_factor = dal_parameters.redundancy_factor in
      let number_of_needed_shards = number_of_shards / redundancy_factor in
      let* number_of_already_stored_shards =
        Store.Shards.count_values shard_store commitment
      in
      let slot_id : Types.slot_id =
        {slot_level = published_level; slot_index}
      in
      (* There are two situations where we don't want to reconstruct:
         if we don't have enough shards or if we already have all the
         shards. *)
      if
        number_of_already_stored_shards < number_of_needed_shards
        || number_of_already_stored_shards = number_of_shards
      then return_unit
      else
        (* We have enough shards to reconstruct the whole slot. *)
        (* TODO: #7089
           add a random time before starting the reconstruction;
           this will give some slack to receive all the shards while
           the reconstruction is not needed, and also could avoid
           having multiple node reconstruct at once. *)
        with_amplification_lock ready_ctxt slot_id @@ fun () ->
        let*! () = Event.(emit reconstruct_started commitment) in
        let shards =
          Store.Shards.read_all shard_store commitment ~number_of_shards
          |> Seq_s.filter_map (function
                 | _, index, Ok share -> Some Cryptobox.{index; share}
                 | _ -> None)
        in
        let* polynomial =
          Slot_manager_legacy.polynomial_from_shards_lwt
            cryptobox
            shards
            ~number_of_needed_shards
        in
        let slot = Cryptobox.polynomial_to_slot cryptobox polynomial in
        let* commitment =
          Slot_manager.add_commitment slot_store slot cryptobox
          |> Errors.to_tzresult
        in
        let* (_ : unit option) =
          Slot_manager.add_commitment_shards
            ~shards_proofs_precomputation
            slot_store
            cryptobox
            commitment
            ~with_proof:true
          |> Errors.to_option_tzresult
        in
        let* () =
          Slot_manager.publish_slot_data
            ~level_committee:(Node_context.fetch_committee node_ctxt)
            slot_store
            gs_worker
            cryptobox
            proto_parameters
            commitment
            published_level
            slot_index
        in
        let*! () = Event.(emit reconstruct_finished commitment) in
        return_unit
