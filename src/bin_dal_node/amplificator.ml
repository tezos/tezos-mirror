(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* A Lwt worker maintening a queue of calculation jobs to send to the crypto process *)
type query =
  | Query of {
      slot_id : Types.slot_id;
      commitment : Cryptobox.commitment;
      proto_parameters : Dal_plugin.proto_parameters;
          (* Used, but not really necessary as long as parameters don't change. *)
    }

type query_msg = Query_msg of {query_id : int; shards : bytes}

module Query_store = Stdlib.Hashtbl

type t = {
  process : Process_worker.t;
  query_pipe : query_msg Lwt_pipe.Unbounded.t;
  mutable query_id : int;
  query_store : (int, query) Query_store.t;
}

type error +=
  | Reconstruction_process_worker_error of string
  | Amplification_query_sender_job of string
  | Amplification_reply_receiver_job of string

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.reconstruction_process_worker_error"
    ~title:"Error with the reconstruction process worker"
    ~description:"Error in the reconstruction process worker"
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Error in the reconstruction process worker. Reason: %s"
        msg)
    Data_encoding.(obj1 (req "error" string))
    (function Reconstruction_process_worker_error str -> Some str | _ -> None)
    (fun str -> Reconstruction_process_worker_error str) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.reply_receiver_job"
    ~title:"Error with the amplification reply receiver job"
    ~description:"Error in the amplification reply receiver job"
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Error in the amplification reply receiver job. Reason: %s"
        msg)
    Data_encoding.(obj1 (req "error" string))
    (function Amplification_reply_receiver_job str -> Some str | _ -> None)
    (fun str -> Amplification_reply_receiver_job str) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.query_sender_job"
    ~title:"Error with the amplification query sender job"
    ~description:"Error in the amplification query sender job"
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Error in the amplification query sender job. Reason: %s"
        msg)
    Data_encoding.(obj1 (req "error" string))
    (function Amplification_query_sender_job str -> Some str | _ -> None)
    (fun str -> Amplification_query_sender_job str)

(** [with_amplification_lock ready_ctxt slot_id ~on_error f] returns
    nothing if the amplification lock for [slot_id] is already taken,
    otherwise it acquires the lock and starts the [f ()] promise
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
  let acquire_lock () =
    ready_ctxt.ongoing_amplifications <-
      add slot_id ready_ctxt.ongoing_amplifications
  in
  let release_lock () =
    ready_ctxt.ongoing_amplifications <-
      remove slot_id ready_ctxt.ongoing_amplifications
  in
  if mem slot_id ready_ctxt.ongoing_amplifications then return_unit
  else
    let () = acquire_lock () in
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
  let shards_proofs_encoding = array Cryptobox.shard_proof_encoding in
  obj2
    (req "shards" (list Cryptobox.shard_encoding))
    (req "proofs" shards_proofs_encoding)

(* This module contains the logic for a shard proving process worker running in
   background. It communicates with the main dal process via pipe ipc. *)
module Reconstruction_process_worker = struct
  let reconstruct_process_init_cryptobox
      (proto_parameters : Dal_plugin.proto_parameters) =
    let open Lwt_result_syntax in
    let* () =
      let find_srs_files () = Tezos_base.Dal_srs.find_trusted_setup_files () in
      Cryptobox.init_prover_dal ~find_srs_files ()
    in
    match Cryptobox.make proto_parameters.cryptobox_parameters with
    | Ok cryptobox -> (
        match Cryptobox.precompute_shards_proofs cryptobox with
        | Ok precomputation -> return (cryptobox, precomputation)
        | Error (`Invalid_degree_strictly_less_than_expected {given; expected})
          ->
            fail
              [
                Errors.Cryptobox_initialisation_failed
                  (Printf.sprintf
                     "Cryptobox.precompute_shards_proofs: SRS size (= %d) \
                      smaller than expected (= %d)"
                     given
                     expected);
              ])
    | Error (`Fail msg) ->
        fail
          [
            Errors.Cryptobox_initialisation_failed
              (Format.asprintf
                 "Error initializing cryptobox in crypto process \
                  (Cryptobox.make): %s"
                 msg);
          ]

  let read_init_message_from_parent ic =
    let open Lwt_result_syntax in
    let* bytes_proto_parameters =
      let* r = Process_worker.read_message ic in
      match r with
      | `End_of_file ->
          fail
            [
              Reconstruction_process_worker_error
                "Invalid initialization message";
            ]
      | `Message msg -> return msg
    in
    let proto_parameters =
      Data_encoding.Binary.of_bytes_exn
        Dal_plugin.proto_parameters_encoding
        bytes_proto_parameters
    in
    return proto_parameters

  let reconstruct cryptobox precomputation shards =
    let open Lwt_result_syntax in
    let* polynomial =
      Slot_manager.polynomial_from_shards cryptobox shards |> Errors.to_tzresult
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
    return proved_shards_encoded

  (* The main function that is run in the [Process_worker.t].
     Initialization phase:
       receive init message: proto_parameters
     Running phase:
       loop
         - receive shards
         - calculate all shards and their proofs
         - sent shards and proofs *)
  let reconstruct_process_worker ic oc () =
    let open Lwt_result_syntax in
    (* Read init message from parent with parameters required to initialize
       cryptobox *)
    let* proto_parameters = read_init_message_from_parent ic in
    let* cryptobox, shards_proofs_precomputation =
      reconstruct_process_init_cryptobox proto_parameters
    in
    let*! () = Event.(emit crypto_process_started (Unix.getpid ())) in
    let rec loop () =
      let*! query_id = Lwt_io.read_int ic in
      (* Read query from main dal process *)
      let* bytes_shards =
        let* r = Process_worker.read_message ic in
        match r with
        | `End_of_file ->
            fail
              [
                Reconstruction_process_worker_error
                  "Incomplete message received. Terminating.";
              ]
        | `Message msg -> return msg
      in
      let shards =
        Data_encoding.(
          Binary.of_bytes_exn (list Cryptobox.shard_encoding) bytes_shards)
        |> List.to_seq
      in

      let*! () = Event.(emit crypto_process_received_query query_id) in

      (* crypto computation *)
      let* proved_shards_encoded =
        reconstruct cryptobox shards_proofs_precomputation shards
      in

      (* Sends back the proved_shards_encoded to the main dal process *)
      let*! () = Event.(emit crypto_process_sending_reply query_id) in
      let len = Bytes.length proved_shards_encoded in
      let*! () = Lwt_io.write_int oc query_id in
      let*! () = Lwt_io.write_int oc len in
      let*! () = Lwt_io.write_from_exactly oc proved_shards_encoded 0 len in
      loop ()
    in
    Lwt.catch loop (function
        | End_of_file ->
            (* Buffer was closed by the parent (normal termination) *)
            return_unit
        | exn ->
            let err = [error_of_exn exn] in
            Lwt.return_error err)
end

(* Serialize queries to crypto worker process while the query queue is not
 * empty. Wait until the query queue contains
 * anything *)
let query_sender_job {query_pipe; process; _} =
  let open Lwt_result_syntax in
  let rec loop () =
    let*! (Query_msg {query_id; shards}) = Lwt_pipe.Unbounded.pop query_pipe in
    let oc = Process_worker.output_channel process in
    let*! () = Event.(emit main_process_sending_query query_id) in
    (* Serialization: query_id, then shards *)
    let*! () = Lwt_io.write_int oc query_id in
    let* () =
      let* r = Process_worker.write_message oc shards in
      match r with
      | `End_of_file ->
          fail
            [
              Reconstruction_process_worker_error
                "Incomplete message. Terminating";
            ]
      | `Write_ok -> return_unit
    in
    loop ()
  in
  Lwt.catch loop (function
      (* Buffer was closed by the parent before proceeding an entire message
         (sigterm, etc.) *)
      | End_of_file -> return_unit
      (* Unknown exception *)
      | exn ->
          let err = [error_of_exn exn] in
          Lwt.return (Error err))

let reply_receiver_job {process; query_store; _} node_context =
  let open Lwt_result_syntax in
  let ic = Process_worker.input_channel process in
  let gs_worker = Node_context.get_gs_worker node_context in
  let node_store = Node_context.get_store node_context in
  let rec loop () =
    let*! id = Lwt_io.read_int ic in
    let (Query {slot_id; commitment; proto_parameters}) =
      Query_store.find query_store id
    in
    (* Messages queue is unbounded *)
    Query_store.remove query_store id ;
    let* msg =
      let* r = Process_worker.read_message ic in
      match r with
      | `End_of_file ->
          fail
            [
              Amplification_reply_receiver_job
                "Incomplete message received. Terminating";
            ]
      | `Message msg -> return msg
    in
    let*! () = Event.(emit main_process_received_reply id) in
    let shards, shard_proofs =
      Data_encoding.Binary.of_bytes_exn proved_shards_encoding msg
    in
    let shards = List.to_seq shards in
    let* () =
      Store.(Shards.write_all node_store.shards slot_id shards)
      |> Errors.to_tzresult
    in
    let* () =
      Slot_manager.publish_proved_shards
        slot_id
        ~level_committee:(Node_context.fetch_committee node_context)
        proto_parameters
        commitment
        shards
        shard_proofs
        gs_worker
    in
    let*! () =
      Event.(emit reconstruct_finished (slot_id.slot_level, slot_id.slot_index))
    in
    Dal_metrics.reconstruction_done () ;
    loop ()
  in
  Lwt.catch loop (function
      (* Buffer was closed before proceeding an entire message (sigterm, etc.) *)
      | End_of_file -> return_unit
      (* Unknown exception *)
      | exn ->
          let err = [error_of_exn exn] in
          Lwt.return (Error err))

let make () =
  let open Lwt_result_syntax in
  (* Fork a process to offload cryptographic calculations *)
  let* process =
    Process_worker.run
      Reconstruction_process_worker.reconstruct_process_worker
      ()
  in
  let query_pipe = Lwt_pipe.Unbounded.create () in
  let query_store = Query_store.create 23 in
  let amplificator = {process; query_pipe; query_store; query_id = 0} in

  let (_ : Lwt_exit.clean_up_callback_id) =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_code ->
        let pid = Process_worker.pid process in
        Unix.kill pid Sys.sigterm ;
        let _exit = Lwt_unix.waitpid [Lwt_unix.WNOHANG] pid in
        Lwt.return_unit)
  in
  return amplificator

let init amplificator node_context proto_parameters =
  let open Lwt_result_syntax in
  (* Run a job enqueueing all shards calculation tasks *)
  let amplificator_query_sender_job =
    let*! r = query_sender_job amplificator in
    match r with
    | Ok () -> return_unit
    | Error e ->
        Lwt_result_syntax.fail
          (Amplification_query_sender_job "Error running query sender job" :: e)
  in

  (* Run a job retrieving all shards and their proof and publish them *)
  let amplificator_reply_receiver_job =
    let*! r = reply_receiver_job amplificator node_context in
    match r with
    | Ok () -> return_unit
    | Error e ->
        Lwt_result_syntax.fail
          (Amplification_reply_receiver_job "Error in reply receiver job" :: e)
  in

  let (_ : Lwt_exit.clean_up_callback_id) =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_code ->
        let () = Lwt.cancel amplificator_query_sender_job in
        let () = Lwt.cancel amplificator_reply_receiver_job in
        Lwt.return_unit)
  in

  let bytes_proto_parameters =
    Data_encoding.Binary.to_bytes_exn
      Dal_plugin.proto_parameters_encoding
      proto_parameters
  in
  let* () =
    let oc = Process_worker.output_channel amplificator.process in
    let* r = Process_worker.write_message oc bytes_proto_parameters in
    match r with
    | `End_of_file ->
        fail
          [
            Reconstruction_process_worker_error
              "Impossible to write init message. Terminating.";
          ]
    | `Write_ok -> return_unit
  in
  return_unit

let enqueue_job_shards_proof amplificator commitment slot_id proto_parameters
    shards =
  let open Lwt_result_syntax in
  let*! shards = Seq_s.fold_left (fun res it -> it :: res) [] shards in
  let shards =
    Data_encoding.(Binary.to_bytes_exn (list Cryptobox.shard_encoding)) shards
  in

  (* Should be atomic incr in the context of a lwt concurrency
   * Could overflow some day on 32 bits machines, more difficult on 64 *)
  let query_id = amplificator.query_id in
  amplificator.query_id <- amplificator.query_id + 1 ;

  (* Store some arguments in a query table, because they are necessary to publish, but not to calculate,
     so avoid the cost of serializing them *)
  let () =
    Query_store.add
      amplificator.query_store
      query_id
      (Query {slot_id; commitment; proto_parameters})
  in
  let*! () = Event.(emit main_process_enqueue_query query_id) in
  let () =
    Lwt_pipe.Unbounded.push
      amplificator.query_pipe
      (Query_msg {query_id; shards})
  in
  return_unit

let amplify node_store commitment (slot_id : Types.slot_id)
    ~number_of_already_stored_shards ~number_of_shards ~number_of_needed_shards
    proto_parameters amplificator =
  let open Lwt_result_syntax in
  match amplificator with
  | None ->
      let*! () = Event.(emit amplificator_uninitialized ()) in
      return_unit
  | Some amplificator ->
      Dal_metrics.reconstruction_started () ;
      let*! () =
        Event.(
          emit
            reconstruct_started
            ( slot_id.slot_level,
              slot_id.slot_index,
              number_of_already_stored_shards,
              number_of_shards ))
      in
      let shards =
        Store.(Shards.read_all node_store.shards slot_id ~number_of_shards)
        |> Seq_s.filter_map (function
               | _, index, Ok share -> Some Cryptobox.{index; share}
               | _ -> None)
      in
      let*? shards =
        Seq_s.take
          ~when_negative_length:[error_of_exn (Invalid_argument "Seq_s.take")]
          number_of_needed_shards
          shards
      in
      (* Enqueue this reconstruction in a query_pipe to be sent to a reconstruction process *)
      let* () =
        enqueue_job_shards_proof
          amplificator
          commitment
          slot_id
          proto_parameters
          shards
      in
      return_unit

let try_amplification (node_store : Store.t) commitment
    (slot_id : Types.slot_id) node_ctxt amplificator =
  let open Lwt_result_syntax in
  let*! () = Node_context.wait_for_ready_state node_ctxt in
  match Node_context.get_status node_ctxt with
  | Starting _ ->
      (* The node is supposed to be ready thanks to [wait_for_ready_state]
         above. *)
      assert false
  | Ready {shards_proofs_precomputation = None; _} ->
      (* The prover SRS is not loaded so we cannot reconstruct slots
         yet. *)
      let*! () =
        Event.(
          emit
            reconstruct_missing_prover_srs
            (slot_id.slot_level, slot_id.slot_index))
      in
      return_unit
  | Ready ({cryptobox; proto_parameters; _} as ready_ctxt) ->
      let dal_parameters = Cryptobox.parameters cryptobox in
      let number_of_shards = dal_parameters.number_of_shards in
      let redundancy_factor = dal_parameters.redundancy_factor in
      let number_of_needed_shards = number_of_shards / redundancy_factor in
      let* number_of_already_stored_shards =
        Store.Shards.count_values node_store.shards slot_id
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
                emit
                  reconstruct_error
                  (slot_id.slot_level, slot_id.slot_index, err))
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
              (slot_id.slot_level, slot_id.slot_index, random_delay))
        in
        let*! () = Lwt_unix.sleep random_delay in
        (* Count again the stored shards because we may have received
           more shards during the random delay. *)
        let* number_of_already_stored_shards =
          Store.Shards.count_values node_store.shards slot_id
        in
        (* If we have received all the shards while waiting the random
           delay, there is no point in reconstructing anymore *)
        if number_of_already_stored_shards = number_of_shards then (
          let*! () =
            Event.(
              emit
                reconstruct_no_missing_shard
                (slot_id.slot_level, slot_id.slot_index))
          in
          Dal_metrics.reconstruction_aborted () ;
          return_unit)
        else
          amplify
            node_store
            commitment
            slot_id
            ~number_of_already_stored_shards
            ~number_of_shards
            ~number_of_needed_shards
            proto_parameters
            amplificator
