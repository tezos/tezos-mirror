(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [with_amplification_lock ready_ctxt slot_id ~on_error f] returns
    nothing if the amplification lock for [slot_id] is already taken,
    otherwise it aquires the lock and starts the [f ()] promise
    without waiting for its resolution.

    The promise can either succeed, return an error, or raise an
    exception. In any case, the amplification lock is released after
    the completion of the promise.

    If the promise returns an error, the error is passed to the
    [on_error] handler after the release of the lock.

    If the promise raises an exception, it is turned into an error
    using [Error_monad.fail_with_exn] and passed to the [on_error]
    handler after the release of the lock. *)
let with_amplification_lock (ready_ctxt : Node_context.ready_ctxt) slot_id
    ~on_error (f : unit -> unit tzresult Lwt.t) =
  let open Lwt_result_syntax in
  let open Types.Slot_id.Set in
  let aquire_lock () =
    ready_ctxt.ongoing_amplifications <-
      add slot_id ready_ctxt.ongoing_amplifications
  in
  let release_lock () =
    ready_ctxt.ongoing_amplifications <-
      remove slot_id ready_ctxt.ongoing_amplifications
  in
  if mem slot_id ready_ctxt.ongoing_amplifications then return_unit
  else
    let () = aquire_lock () in
    let () =
      Lwt.dont_wait
        (fun () ->
          (* Convert any exception into an error *)
          let*! res = Lwt.catch f Error_monad.fail_with_exn in
          let () = release_lock () in
          match res with
          | Ok () -> Lwt_syntax.return_unit
          | Error err -> on_error err)
        (fun _exn ->
          (* This exception handler can only run if an exception was
             raised by the on_error call. In that edge case, we
             prefer to ignore the exception and let the DAL node
             continue to run. *)
          ())
    in
    return_unit

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
        with_amplification_lock
          ready_ctxt
          slot_id
          ~on_error:
            Event.(
              fun err ->
                emit reconstruct_error (published_level, slot_index, err))
        @@ fun () ->
        (* Wait a random delay between 1 and 2 seconds before starting
           the reconstruction; this is to give some slack to receive
           all the shards so that the reconstruction is not needed, and
           also avoids having multiple nodes reconstruct at once. *)
        let random_delay =
          Constants.(
            amplification_random_delay_min
            +. Random.float
                 (amplification_random_delay_max
                -. amplification_random_delay_min))
        in
        let*! () =
          Event.(
            emit
              reconstruct_starting_in
              (published_level, slot_index, random_delay))
        in
        let*! () = Lwt_unix.sleep random_delay in
        (* Count again the stored shards because we may have received
           more shards during the random delay. *)
        let* number_of_already_stored_shards =
          Store.Shards.count_values shard_store commitment
        in
        (* If we have received all the shards while waiting the random
           delay, there is no point in reconstructing anymore *)
        if number_of_already_stored_shards = number_of_shards then
          let*! () =
            Event.(
              emit reconstruct_no_missing_shard (published_level, slot_index))
          in
          return_unit
        else
          let*! () =
            Event.(
              emit
                reconstruct_started
                ( published_level,
                  slot_index,
                  number_of_already_stored_shards,
                  number_of_shards ))
          in
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
          let*! () =
            Event.(emit reconstruct_finished (published_level, slot_index))
          in
          return_unit
