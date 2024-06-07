(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  cctxt : Client_context.wallet;
  smart_rollup_address : string;
  sidecar_endpoint : Uri.t;
  sequencer_key : Client_keys.sk_uri;
  maximum_number_of_chunks : int;
  keep_alive : bool;
  notify_no_preblock : unit -> unit;
}

module Types = struct
  type nonrec parameters = parameters

  type state = {
    cctxt : Client_context.wallet;
    smart_rollup_address : string;
    sidecar_endpoint : Uri.t;
    sequencer_key : Client_keys.sk_uri;
    preblocks_stream : Threshold_encryption_types.preblock Lwt_stream.t;
    maximum_number_of_chunks : int;
    keep_alive : bool;
    (* TODO: Make this fields mutable and avoid passing the reference around. *)
    pending_proposals : (Time.Protocol.t * bool) Queue.t;
    notify_no_preblock : unit -> unit;
  }
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base =
    [
      "evm_node";
      "dev";
      "threshold";
      "encryption";
      "blueprint";
      "producer";
      "worker";
    ]

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t =
    | Add_proposal_request : (Time.Protocol.t * bool) -> (unit, tztrace) t
    | Notify_proposal_processed : unit -> (Time.Protocol.t, tztrace) t

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Add_proposal_request"
          (obj3
             (req "request" (constant "add_proposal_request"))
             (req "timestamp" Time.Protocol.encoding)
             (req "force" bool))
          (function
            | View (Add_proposal_request (timestamp, force)) ->
                Some ((), timestamp, force)
            | View _ -> None)
          (fun ((), timestamp, force) ->
            View (Add_proposal_request (timestamp, force)));
        case
          (Tag 1)
          ~title:"Notify_proposal_processed"
          (obj1 (req "request" (constant "notify_proposal_processed")))
          (function
            | View (Notify_proposal_processed ()) -> Some () | View _ -> None)
          (fun () -> View (Notify_proposal_processed ()));
      ]

  let pp _ppf (View _) = ()
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

(*
   The legacy transactions are as follows:
  -----------------------------
  | Nonce    | Up to 32 bytes |
  -----------------------------
  | GasPrice | Up to 32 bytes |
  -----------------------------
  | GasLimit | Up to 32 bytes |
  -----------------------------
  | To       | 20 bytes addr  |
  -----------------------------
  | Value    | Up to 32 bytes |
  -----------------------------
  | Data     | 0 - unlimited  |
  -----------------------------
  | V        | 1 (usually)    |
  -----------------------------
  | R        | 32 bytes       |
  -----------------------------
  | S        | 32 bytes       |
  -----------------------------

   where `up to` start at 0, and encoded as the empty byte for the 0 value
   according to RLP specification.
*)

let minimum_ethereum_transaction_size =
  Rlp.(
    List
      [
        Value Bytes.empty;
        Value Bytes.empty;
        Value Bytes.empty;
        Value (Bytes.make 20 '\000');
        Value Bytes.empty;
        Value Bytes.empty;
        Value Bytes.empty;
        Value (Bytes.make 32 '\000');
        Value (Bytes.make 32 '\000');
      ]
    |> encode |> Bytes.length)

let take_delayed_transactions maximum_number_of_chunks =
  let open Lwt_result_syntax in
  let maximum_delayed_transaction_size = 4096 in
  let maximum_cumulative_size =
    Sequencer_blueprint.maximum_usable_space_in_blueprint
      maximum_number_of_chunks
  in
  let maximum_delayed_transactions =
    maximum_cumulative_size / maximum_delayed_transaction_size
  in
  let* delayed_transactions = Evm_context.delayed_inbox_hashes () in
  let delayed_transactions =
    List.take_n maximum_delayed_transactions delayed_transactions
  in
  let remaining_cumulative_size =
    maximum_cumulative_size - (List.length delayed_transactions * 4096)
  in
  return (delayed_transactions, remaining_cumulative_size)

(* This function should be called only when pending_proposals is not empty,
   otherwise it raises an exception.
   It peeks the least recent pending_proposals, builds a proposal out of it,
   and submits it to the sequencer sidecar. The proposal is not removed from
   the queue until the corresponding preblock has been received from the
   sequencer sidecar, and the blueprint originated from it is not applied on
   top of the EVM state. *)
let make_and_submit_proposal_exn ~maximum_number_of_chunks ~keep_alive
    ~sidecar_endpoint ~notify_no_preblock pending_proposals =
  let open Lwt_result_syntax in
  let timestamp, force = Queue.peek pending_proposals in
  let* is_locked = Tx_pool.is_locked () in
  if is_locked then
    (* In this case the proposal will not make it into a preblock. We can
       notify the preblock monitor that no proposal will be produced. *)
    let*! () = Block_producer_events.production_locked () in
    let () = notify_no_preblock () in
    return ()
  else
    let* delayed_transactions, remaining_cumulative_size =
      take_delayed_transactions maximum_number_of_chunks
    in
    let* transactions =
      (* Low key optimization to avoid even checking the txpool if there is not
         enough space for the smallest transaction. *)
      if remaining_cumulative_size <= minimum_ethereum_transaction_size then
        return []
      else
        Tx_pool.pop_transactions
          ~maximum_cumulative_size:remaining_cumulative_size
    in
    let n = List.length transactions + List.length delayed_transactions in
    if force || n > 0 then
      let*! head_info = Evm_context.head_info () in
      let transactions =
        List.map
          (fun (transaction, _hash) ->
            Threshold_encryption_types.Transaction.of_string transaction)
          transactions
      in
      let proposal : Threshold_encryption_types.proposal =
        Threshold_encryption_types.
          {
            transactions;
            delayed_transaction_hashes = delayed_transactions;
            previous_block_hash = head_info.current_block_hash;
            current_blueprint_number = head_info.next_blueprint_number;
            timestamp;
          }
      in
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/7189
         Handle reconnections, timeouts and stream closed on server side. *)
      let* () =
        Threshold_encryption_services.submit_proposal
          ~keep_alive
          ~sidecar_endpoint
          proposal
      in
      return ()
    else
      (* In this case the proposal will not make it into a preblock. We can
         notify the preblock monitor that a preblock will not be produced. *)
      let () = notify_no_preblock () in
      return ()

let add_proposal_request ~timestamp ~force state =
  let Types.
        {
          sidecar_endpoint;
          maximum_number_of_chunks;
          keep_alive;
          notify_no_preblock;
          pending_proposals;
          _;
        } =
    state
  in
  let open Lwt_result_syntax in
  (* If there are no pending proposals, we can trigger the proposal submission. *)
  let should_submit_proposal = Queue.is_empty pending_proposals in
  let () = Queue.add (timestamp, force) pending_proposals in

  (* If this is the only pending proposal, we are not waiting for a
     preblock to be received and the corresponding blueprint to
     be applied. In this case, it is safe to pull transactions from the
     Tx_pool and submit the proposal. Otherwise, we return and defer the
     submission  of the proposal until as many blueprints as pending proposals have
     been applied. *)
  (* It would be more convenient for the worker to send a message to itself.
     Check if that is possible. *)
  if should_submit_proposal then
    (* It is safe to call `make_and_submit_proposal_exn`, since we have
       just added a proposal request to the pending proposals. *)
    make_and_submit_proposal_exn
      ~sidecar_endpoint
      ~maximum_number_of_chunks
      ~keep_alive
      ~notify_no_preblock
      pending_proposals
  else return ()

let notify_proposal_processed state =
  let Types.
        {
          sidecar_endpoint;
          maximum_number_of_chunks;
          keep_alive;
          notify_no_preblock;
          pending_proposals;
          _;
        } =
    state
  in
  (* This function is executed once the EVM node has finished propcessing a
     proposal, either by applying the latest proposal, or if it determined
     that a proposal did not produce a preblock. *)
  let open Lwt_result_syntax in
  let timestamp, _force = Queue.take pending_proposals in
  (* If the proposal queue is not empty, the blueprint_producer can start
     publishing the next proposal. *)
  let* () =
    if not @@ Queue.is_empty pending_proposals then
      (* The queue pending_proposals is not empty, it is safe to call
         `make_and_submit_proposal_exn`. *)
      make_and_submit_proposal_exn
        ~sidecar_endpoint
        ~maximum_number_of_chunks
        ~keep_alive
        ~notify_no_preblock
        pending_proposals
    else return ()
  in
  return timestamp

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Add_proposal_request (timestamp, force) ->
        protect @@ fun () -> add_proposal_request ~timestamp ~force state
    | Request.Notify_proposal_processed () ->
        protect @@ fun () -> notify_proposal_processed state

  type launch_error = error trace

  let on_launch _w ()
      ({
         sequencer_key;
         cctxt;
         smart_rollup_address;
         sidecar_endpoint;
         keep_alive;
         maximum_number_of_chunks;
         notify_no_preblock;
       } :
        Types.parameters) =
    let open Lwt_result_syntax in
    let* preblocks_stream =
      Threshold_encryption_services.monitor_preblocks ~sidecar_endpoint ()
    in
    let state =
      Types.
        {
          sequencer_key;
          cctxt;
          smart_rollup_address;
          sidecar_endpoint;
          preblocks_stream;
          maximum_number_of_chunks;
          keep_alive;
          pending_proposals = Queue.create ();
          notify_no_preblock;
        }
    in
    return state

  let on_error (type a b) _w _st (_r : (a, b) Request.t) (_errs : b) :
      unit tzresult Lwt.t =
    Lwt_result_syntax.return_unit

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

type error += No_threshold_encryptioN_proposals_handler

type error += Threshold_encryption_proposals_handler_terminated

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail e -> Error (TzTrace.make @@ error_of_exn e)
    | Lwt.Sleep ->
        Error (TzTrace.make No_threshold_encryptioN_proposals_handler))

let handle_request_error rq =
  let open Lwt_syntax in
  let* rq in
  match rq with
  | Ok res -> return_ok res
  | Error (Worker.Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) ->
      Lwt.return_error [Threshold_encryption_proposals_handler_terminated]
  | Error (Closed (Some errs)) -> Lwt.return_error errs
  | Error (Any exn) -> Lwt.return_error [Exn exn]

let start parameters =
  let open Lwt_result_syntax in
  let*! () = Block_producer_events.started () in
  let+ worker = Worker.launch table () parameters (module Handlers) in
  Lwt.wakeup worker_waker worker

let shutdown () =
  let open Lwt_syntax in
  let w = Lazy.force worker in
  match w with
  | Error _ -> Lwt.return_unit
  | Ok w ->
      let* () = Block_producer_events.shutdown () in
      Worker.shutdown w

let add_proposal_request timestamp force =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    worker
    (Request.Add_proposal_request (timestamp, force))
  |> handle_request_error

let notify_proposal_processed () =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    worker
    (Request.Notify_proposal_processed ())
  |> handle_request_error
