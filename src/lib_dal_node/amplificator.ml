(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Welcome message for ipc between main process and crypto process *)
let welcome = Bytes.of_string "0 Ready"

(* A Lwt worker maintening a queue of calculation jobs to send to the crypto process *)
type query =
  | Query of {
      slot_id : Types.slot_id;
      commitment : Cryptobox.commitment;
      proto_parameters : Types.proto_parameters;
          (* Used, but not really necessary as long as parameters don't change. *)
      reconstruction_start_time : float;
    }

type query_msg = Query_msg of {query_id : int; shards : bytes}

module Query_store = Stdlib.Hashtbl

type t = {
  node_ctxt : Node_context.t;
  process : Process_worker.t;
  query_pipe : query_msg Lwt_pipe.Unbounded.t;
  mutable query_id : int;
  query_store : (int, query) Query_store.t;
  amplification_random_delay_min : float;
  amplification_random_delay_max : float;
      (* When receiving shards from the network, the DAL node does not
         perform the amplification as soon as it has enough shards to do
         the reconstruction but waits a bit in case the missing shards are
         received from the network. The duration of the delay is picked at
         random between [amplification_random_delay_min] and
         [amplification_random_delay_max]. *)
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
let with_amplification_lock (ctxt : Node_context.t) slot_id ~on_error
    (f : unit -> unit tzresult Lwt.t) =
  let open Lwt_result_syntax in
  let open Types.Slot_id.Set in
  let acquire_lock () =
    let ongoing_amplifications = Node_context.get_ongoing_amplifications ctxt in
    Node_context.set_ongoing_amplifications
      ctxt
      (add slot_id ongoing_amplifications)
  in
  let release_lock () =
    let ongoing_amplifications = Node_context.get_ongoing_amplifications ctxt in
    Node_context.set_ongoing_amplifications
      ctxt
      (remove slot_id ongoing_amplifications)
  in
  let ongoing_amplifications = Node_context.get_ongoing_amplifications ctxt in
  if mem slot_id ongoing_amplifications then return_unit
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
  let read_init_message_from_parent ic =
    let open Lwt_result_syntax in
    let* () =
      let* r = Process_worker.read_message ic in
      match r with
      | `End_of_file ->
          fail
            [
              Reconstruction_process_worker_error
                "Invalid initialization message";
            ]
      | `Message b when Bytes.equal b welcome -> return_unit
      | `Message b -> fail_with_exn (Invalid_argument (String.of_bytes b))
    in
    return_unit

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
  let reconstruct_process_worker ic oc (cryptobox, shards_proofs_precomputation)
      =
    let open Lwt_result_syntax in
    let reply_success ~oc ~query_id ~proved_shards_encoded =
      (* Sends back the proved_shards_encoded to the main dal process *)
      let*! () = Event.emit_crypto_process_sending_reply ~query_id in
      let*! () = Lwt_io.write_int oc query_id in
      let*! _ = Process_worker.write_message oc (Bytes.of_string "OK") in
      let*! _ = Process_worker.write_message oc proved_shards_encoded in
      Lwt.return_unit
    in
    let reply_error_query ~oc ~query_id ~error =
      (* Sends back the proved_shards_encoded to the main dal process *)
      let*! () = Event.emit_crypto_process_sending_reply_error ~query_id in
      let*! () = Lwt_io.write_int oc query_id in
      let*! _ = Process_worker.write_message oc (Bytes.of_string "ERR") in
      let bytes = Bytes.of_string error in
      let*! _ = Process_worker.write_message oc bytes in
      Lwt.return_unit
    in
    (* Read init message from parent with parameters required to initialize
       cryptobox *)
    let* () = read_init_message_from_parent ic in
    let*! () = Event.emit_crypto_process_started ~pid:(Unix.getpid ()) in
    let process_query query_id =
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

      let*! () = Event.emit_crypto_process_received_query ~query_id in

      (* crypto computation *)
      let* proved_shards_encoded =
        reconstruct cryptobox shards_proofs_precomputation shards
      in
      return proved_shards_encoded
    in
    let rec loop () =
      Lwt.catch
        (fun () ->
          (* Note: this read_int is very unlikely to appear unless obvious issue *)
          let*! query_id = Lwt_io.read_int ic in
          let* () =
            Lwt.catch
              (fun () ->
                let*! r = process_query query_id in
                match r with
                | Ok proved_shards_encoded ->
                    let*! () =
                      reply_success ~query_id ~proved_shards_encoded ~oc
                    in
                    loop ()
                | Error err ->
                    let err =
                      Format.asprintf "%a" Error_monad.pp_print_trace err
                    in
                    (* send a reply with the error, and continue *)
                    let*! () = reply_error_query ~oc ~query_id ~error:err in
                    loop ())
              (function
                | End_of_file -> raise End_of_file
                | exn ->
                    let error = Printexc.to_string exn in
                    let*! () = reply_error_query ~oc ~query_id ~error in
                    return_unit)
          in
          return_unit)
        (function
          | Lwt.Canceled
            (* if terminated by direct signal (normal termination) *)
          | End_of_file ->
              (* Buffer was closed by the parent (normal termination) *)
              let*! () = Event.emit_crypto_process_stopped () in
              return_unit
          | exn ->
              (* Lwt_io.read_int could fail, in this case, there is an severe
                 issue: pipe issue between the processes, memory full ?
                 In this case, we give up *)
              let msg = Printexc.to_string exn in
              let*! () = Event.emit_crypto_process_error ~msg in
              fail [error_of_exn exn])
    in
    let* () = loop () in
    return_unit
end

(* Serialize queries to crypto worker process while the query queue is not
 * empty. Wait until the query queue contains
 * anything *)
let query_sender_job {query_pipe; process; _} =
  let open Lwt_result_syntax in
  let rec loop () =
    let*! (Query_msg {query_id; shards; _}) =
      Lwt_pipe.Unbounded.pop query_pipe
    in
    let oc = Process_worker.output_channel process in
    let*! () = Event.emit_main_process_sending_query ~query_id in
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
    let (Query
          {slot_id; commitment; proto_parameters; reconstruction_start_time}) =
      Query_store.find query_store id
    in
    (* Messages queue is unbounded *)
    Query_store.remove query_store id ;
    let length = Query_store.length query_store in
    let () = Dal_metrics.update_amplification_queue_length length in
    (* The first message should be a OK | ERR *)
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
    match Bytes.to_string msg with
    | "ERR" ->
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
        (* The error message *)
        let msg = Bytes.to_string msg in
        let*! () =
          Event.emit_main_process_received_reply_error ~query_id:id ~msg
        in
        loop ()
    | "OK" ->
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
        let*! () = Event.emit_main_process_received_reply ~query_id:id in
        let shards, shard_proofs =
          Data_encoding.Binary.of_bytes_exn proved_shards_encoding msg
        in
        let shards = List.to_seq shards in
        let* () =
          Store.Shards.write_all (Store.shards node_store) slot_id shards
          |> Errors.to_tzresult
        in
        let* () =
          Slot_manager.publish_proved_shards
            node_context
            slot_id
            ~level_committee:(Node_context.fetch_committee node_context)
            proto_parameters
            commitment
            shards
            shard_proofs
            gs_worker
        in
        let*! () =
          Event.emit_reconstruct_finished
            ~level:slot_id.slot_level
            ~slot_index:slot_id.slot_index
        in
        let duration = Unix.gettimeofday () -. reconstruction_start_time in
        Dal_metrics.update_amplification_complete_duration duration ;
        Dal_metrics.reconstruction_done () ;
        loop ()
    | _ ->
        let*! () = Event.emit_crypto_process_error ~msg:"Unexpected message" in
        fail [Amplification_reply_receiver_job "Unexpected message"]
  in
  Lwt.catch loop (function
      (* Buffer was closed before proceeding an entire message (sigterm, etc.) *)
      | End_of_file -> return_unit
      (* Unknown exception *)
      | exn ->
          let err = [error_of_exn exn] in
          let*! () = Event.emit_crypto_process_fatal ~msg:"Unexpected error" in
          Lwt.return (Error err))

let determine_amplification_delays node_ctxt =
  let open Result_syntax in
  let+ parameters =
    Node_context.get_proto_parameters ~level:`Last_proto node_ctxt
  in
  (* The propagation window is the attestation lag minus 4 levels, because:
     - the daemon waits 2 levels for the head to be finalized
     - attestation operations are included in the block at the next level
     - the baker asks attestation information one level in advance *)
  let propagation_period =
    (parameters.attestation_lag - 4)
    * Int64.to_int parameters.minimal_block_delay
  in
  (* We split this window in 3: one third for the normal propagation round, one
     third for amplification; one third for the second propagation round. Note,
     currently for Mainnet: 4 * 8s = 32s, so roughly 10s; for Ghostnet: 16s, so
     roughly 5s. We round to the lowest integer. *)
  let amplification_period = float_of_int (propagation_period / 3) in
  let amplification_random_delay_min = amplification_period in
  let amplification_random_delay_max = 2. *. amplification_period in
  (amplification_random_delay_min, amplification_random_delay_max)

let start_amplificator node_ctxt =
  let open Lwt_result_syntax in
  let*? amplification_random_delay_min, amplification_random_delay_max =
    determine_amplification_delays node_ctxt
  in
  let cryptobox = Node_context.get_cryptobox node_ctxt in
  let shards_proofs_precomputation =
    Node_context.get_shards_proofs_precomputation node_ctxt
  in
  let* shards_proofs_precomputation =
    match shards_proofs_precomputation with
    | None -> fail [Errors.Amplificator_initialization_failed]
    | Some v -> return v
  in
  (* Fork a process to offload cryptographic calculations *)
  let* process =
    Process_worker.run
      Reconstruction_process_worker.reconstruct_process_worker
      (cryptobox, shards_proofs_precomputation)
  in
  let query_pipe = Lwt_pipe.Unbounded.create () in
  let query_store = Query_store.create 23 in
  let queue_length = Query_store.length query_store in
  let () = Dal_metrics.update_amplification_queue_length queue_length in
  let amplificator =
    {
      node_ctxt;
      process;
      query_pipe;
      query_store;
      query_id = 0;
      amplification_random_delay_min;
      amplification_random_delay_max;
    }
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_code ->
        let pid = Process_worker.pid process in
        Unix.kill pid Sys.sigterm ;
        let _exit = Lwt_unix.waitpid [Lwt_unix.WNOHANG] pid in
        Lwt.return_unit)
  in
  return amplificator

let make node_ctxt =
  let open Lwt_result_syntax in
  let* amplificator = start_amplificator node_ctxt in
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
    let*! r = reply_receiver_job amplificator node_ctxt in
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
  let* () =
    let oc = Process_worker.output_channel amplificator.process in
    let* r = Process_worker.write_message oc welcome in
    match r with
    | `End_of_file ->
        fail
          [
            Reconstruction_process_worker_error
              "Impossible to write init message. Terminating.";
          ]
    | `Write_ok -> return_unit
  in
  return amplificator

let enqueue_job_shards_proof amplificator commitment slot_id proto_parameters
    shards reconstruction_start_time =
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
      (Query {slot_id; commitment; proto_parameters; reconstruction_start_time})
  in
  let length = Query_store.length amplificator.query_store in
  let () = Dal_metrics.update_amplification_queue_length length in
  let*! () = Event.emit_main_process_enqueue_query ~query_id in
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
  let reconstruction_start_time = Unix.gettimeofday () in
  Dal_metrics.reconstruction_started () ;
  let*! () =
    Event.emit_reconstruct_started
      ~level:slot_id.slot_level
      ~slot_index:slot_id.slot_index
      ~number_of_received_shards:number_of_already_stored_shards
      ~number_of_shards
  in
  let shards =
    Store.Shards.read_all (Store.shards node_store) slot_id ~number_of_shards
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
      reconstruction_start_time
  in
  return_unit

let try_amplification commitment slot_metrics slot_id amplificator =
  let open Lwt_result_syntax in
  let node_ctxt = amplificator.node_ctxt in
  let node_store = Node_context.get_store node_ctxt in
  let*? proto_parameters =
    Node_context.get_proto_parameters
      node_ctxt
      ~level:(`Level slot_id.Types.Slot_id.slot_level)
  in
  let number_of_shards =
    proto_parameters.cryptobox_parameters.number_of_shards
  in
  let redundancy_factor =
    proto_parameters.cryptobox_parameters.redundancy_factor
  in
  let number_of_needed_shards = number_of_shards / redundancy_factor in
  let* number_of_already_stored_shards =
    Store.Shards.count_values (Store.shards node_store) slot_id
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
    with_amplification_lock node_ctxt slot_id ~on_error:(fun error ->
        Event.emit_reconstruct_error
          ~level:slot_id.slot_level
          ~slot_index:slot_id.slot_index
          ~error)
    @@ fun () ->
    (* Wait a random delay between 1 and 2 seconds before starting
       the reconstruction; this is to give some slack to receive
       all the shards so that the reconstruction is not needed, and
       also avoids having multiple nodes reconstruct at once. *)
    let random_delay =
      amplificator.amplification_random_delay_min
      +. Random.float
           (amplificator.amplification_random_delay_max
          -. amplificator.amplification_random_delay_min)
    in
    let*! () =
      Event.emit_reconstruct_starting_in
        ~level:slot_id.slot_level
        ~slot_index:slot_id.slot_index
        ~delay:random_delay
    in
    let*! () = Lwt_unix.sleep random_delay in
    (* Count again the stored shards because we may have received
       more shards during the random delay. *)
    let* number_of_already_stored_shards =
      Store.Shards.count_values (Store.shards node_store) slot_id
    in
    let t = Unix.gettimeofday () in
    let duration = t -. slot_metrics.Dal_metrics.time_first_shard in
    (* There is no point trying a reconstruction if we have not received
       enough shards.
       If we have received all the shards while waiting the random
       delay, there is no point in reconstructing either. *)
    if
      number_of_already_stored_shards < number_of_needed_shards
      || number_of_already_stored_shards = number_of_shards
    then (
      Dal_metrics.update_amplification_abort_reconstruction_duration duration ;
      let*! () =
        Event.emit_reconstruct_no_missing_shard
          ~level:slot_id.slot_level
          ~slot_index:slot_id.slot_index
      in
      Dal_metrics.reconstruction_aborted () ;
      return_unit)
    else (
      Dal_metrics.update_amplification_start_reconstruction_duration duration ;
      amplify
        node_store
        commitment
        slot_id
        ~number_of_already_stored_shards
        ~number_of_shards
        ~number_of_needed_shards
        proto_parameters
        amplificator)
