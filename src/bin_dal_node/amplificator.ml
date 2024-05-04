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

let proved_shards_encoding =
  let open Data_encoding in
  let shards_encoding = list Cryptobox.shard_encoding in
  let shards_proofs_encoding = array Cryptobox.shard_proof_encoding in
  obj2 (req "shards" shards_encoding) (req "proofs" shards_proofs_encoding)

let amplify node_store node_ctxt cryptobox commitment precomputation
    ~published_level ~slot_index ~number_of_already_stored_shards
    ~number_of_needed_shards ~number_of_shards gs_worker proto_parameters =
  let open Lwt_result_syntax in
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
    Store.(Shards.read_all node_store.shards commitment ~number_of_shards)
    |> Seq_s.filter_map (function
           | _, index, Ok share -> Some Cryptobox.{index; share}
           | _ -> None)
  in
  (* We fork a new process to handle the cryptographic aspects of
     amplification (reconstruction of the polynomial + generation of
     the proofs for all the shards). The parent and the forked child
     communicate using a pair of pipes and a very simple protocol:

     - child computes shards, shard proofs, and serializes them,
     - child->parent: length of serialized proved shards,
     - child->parent: serialized proved shards,
     - parent stores shards and shard proofs,
     - parent->child: termination signal (any byte),
     - child exits *)
  let*! () = Lwt_io.flush_all () in
  let ic_parent, oc_child = Lwt_io.pipe ~cloexec:true () in
  let ic_child, oc_parent = Lwt_io.pipe ~cloexec:true () in
  match Lwt_unix.fork () with
  | 0 ->
      (* Child *)
      let*! () = Lwt_io.close ic_parent in
      let*! () = Lwt_io.close oc_parent in
      let* polynomial =
        Slot_manager.polynomial_from_shards_lwt
          cryptobox
          shards
          ~number_of_needed_shards
        |> Errors.to_tzresult
      in
      let shards =
        Cryptobox.shards_from_polynomial cryptobox polynomial |> List.of_seq
      in
      let shard_proofs =
        Cryptobox.prove_shards cryptobox ~precomputation ~polynomial
      in
      let proved_shards_encoded =
        Data_encoding.Binary.to_bytes_exn
          proved_shards_encoding
          (shards, shard_proofs)
      in
      let buffer = Bytes.create 4 in
      let len = Bytes.length proved_shards_encoded in
      Bytes.set_int32_ne buffer 0 (Int32.of_int len) ;
      let*! _ = Lwt_io.write_from oc_child buffer 0 4 in
      let*! _ = Lwt_io.write_from oc_child proved_shards_encoded 0 len in
      let*! () = Lwt_io.close oc_child in
      let*! _ =
        let termination_signal = Bytes.create 1 in
        Lwt_io.read_into_exactly ic_child termination_signal 0 1
      in
      let*! () = Lwt_io.close ic_child in
      exit 0
  | _n ->
      (* Parent *)
      let*! () = Lwt_io.close ic_child in
      let*! () = Lwt_io.close oc_child in
      let*! res =
        Lwt.pick
          [
            (let*! () = Lwt_unix.sleep Constants.amplification_timeout in
             tzfail Timeout);
            (let buffer = Bytes.create 4 in
             let*! () = Lwt_io.read_into_exactly ic_parent buffer 0 4 in
             let len = Bytes.get_int32_ne buffer 0 |> Int32.to_int in
             let encoded = Bytes.create len in
             let*! () = Lwt_io.read_into_exactly ic_parent encoded 0 len in
             let shards, shard_proofs =
               Data_encoding.Binary.of_bytes_exn proved_shards_encoding encoded
             in
             return (List.to_seq shards, shard_proofs));
          ]
      in
      let*! () = Lwt_io.close ic_parent in
      let*! _exit_child =
        (* Any byte sent to the child is interpreted as a termination
           signal. *)
        let termination_signal = Bytes.create 1 in
        Lwt_io.write_from oc_parent termination_signal 0 1
      in
      let*! () = Lwt_io.close oc_parent in
      let*? shards, shard_proofs = res in
      let* () =
        Store.(Shards.write_all node_store.shards commitment shards)
        |> Errors.to_tzresult
      in
      Store.save_shard_proofs node_store commitment shard_proofs ;
      let* () =
        Slot_manager.publish_proved_shards
          ~published_level
          ~slot_index
          ~level_committee:(Node_context.fetch_committee node_ctxt)
          proto_parameters
          (shards
          |> Seq.map (fun Cryptobox.{index; share} ->
                 (commitment, index, Ok share))
          |> Seq_s.of_seq)
          shard_proofs
          gs_worker
      in
      let*! () =
        Event.(emit reconstruct_finished (published_level, slot_index))
      in
      return_unit

let try_amplification (node_store : Store.t) commitment ~published_level
    ~slot_index gs_worker node_ctxt =
  let open Lwt_result_syntax in
  match Node_context.get_status node_ctxt with
  | Starting ->
      (* The cryptobox is not yet available so we cannot reconstruct
         slots yet. *)
      return_unit
  | Ready {shards_proofs_precomputation = None; _} ->
      (* The prover SRS is not loaded so we cannot reconstruct slots
         yet. *)
      let*! () =
        Event.(
          emit reconstruct_missing_prover_srs (published_level, slot_index))
      in
      return_unit
  | Ready
      ({
         cryptobox;
         shards_proofs_precomputation = Some precomputation;
         proto_parameters;
         _;
       } as ready_ctxt) ->
      let dal_parameters = Cryptobox.parameters cryptobox in
      let number_of_shards = dal_parameters.number_of_shards in
      let redundancy_factor = dal_parameters.redundancy_factor in
      let number_of_needed_shards = number_of_shards / redundancy_factor in
      let* number_of_already_stored_shards =
        Store.Shards.count_values node_store.shards commitment
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
          Store.Shards.count_values node_store.shards commitment
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
          amplify
            node_store
            node_ctxt
            cryptobox
            commitment
            precomputation
            ~published_level
            ~slot_index
            ~number_of_already_stored_shards
            ~number_of_needed_shards
            ~number_of_shards
            gs_worker
            proto_parameters
