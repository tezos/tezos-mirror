(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  sidecar_endpoint : Uri.t;
  maximum_number_of_chunks : int;
  keep_alive : bool;
  time_between_blocks : Configuration.time_between_blocks;
}

module Types = struct
  type nonrec parameters = parameters

  type state = {
    sidecar_endpoint : Uri.t;
    maximum_number_of_chunks : int;
    keep_alive : bool;
    time_between_blocks : Configuration.time_between_blocks;
    mutable last_proposed_timestamp : Time.Protocol.t option;
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
    | Submit_next_proposal :
        Time.Protocol.t
        -> (Threshold_encryption_types.proposal_submission_outcome, tztrace) t

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Submit_next_proposal"
          (obj2
             (req "request" (constant "submit_next_proposal"))
             (req "timestamp" Time.Protocol.encoding))
          (function
            | View (Submit_next_proposal timestamp) -> Some ((), timestamp))
          (fun ((), timestamp) -> View (Submit_next_proposal timestamp));
      ]

  let pp _ppf (View _) = ()
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type error += Threshold_encryption_proposals_handler_terminated

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

let should_submit_proposal (state : Types.state) timestamp =
  match (state.time_between_blocks, state.last_proposed_timestamp) with
  | Nothing, _ | _, None -> true
  | Time_between_blocks time_between_blocks, Some last_proposal_timestamp ->
      (* We force if the last produced block is older than [time_between_blocks]. *)
      let diff = Time.Protocol.(diff timestamp last_proposal_timestamp) in
      diff >= Int64.of_float time_between_blocks

let submit_proposal (state : Types.state) timestamp =
  let open Lwt_result_syntax in
  let*! () =
    Threshold_encryption_proposals_handler_events
    .received_proposal_submission_request
      ()
  in
  let* tx_pool_is_locked = Tx_pool.is_locked () in
  if tx_pool_is_locked then
    (* In this case the proposal will not make it into a preblock. *)
    let*! () = Block_producer_events.production_locked () in
    return Threshold_encryption_types.Tx_pool_is_locked
  else
    let* delayed_transactions, remaining_cumulative_size =
      take_delayed_transactions state.maximum_number_of_chunks
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
    if should_submit_proposal state timestamp || n > 0 then (
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
          ~keep_alive:state.keep_alive
          ~sidecar_endpoint:state.sidecar_endpoint
          proposal
      in
      state.Types.last_proposed_timestamp <- Some timestamp ;
      return Threshold_encryption_types.Proposal_submitted)
    else return Threshold_encryption_types.Proposal_is_early

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Submit_next_proposal proposal ->
        protect @@ fun () -> submit_proposal state proposal

  type launch_error = error trace

  let on_launch _w ()
      ({
         sidecar_endpoint;
         keep_alive;
         maximum_number_of_chunks;
         time_between_blocks;
       } :
        Types.parameters) =
    let open Lwt_result_syntax in
    let state =
      Types.
        {
          sidecar_endpoint;
          maximum_number_of_chunks;
          keep_alive;
          time_between_blocks;
          last_proposed_timestamp = None;
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

type error += No_threshold_encryption_proposals_handler

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail e -> Error (TzTrace.make @@ error_of_exn e)
    | Lwt.Sleep ->
        Error (TzTrace.make No_threshold_encryption_proposals_handler))

let start parameters =
  let open Lwt_result_syntax in
  let*! () = Threshold_encryption_proposals_handler_events.started () in
  let+ worker = Worker.launch table () parameters (module Handlers) in
  Lwt.wakeup worker_waker worker

let shutdown () =
  let open Lwt_syntax in
  let w = Lazy.force worker in
  match w with
  | Error _ -> Lwt.return_unit
  | Ok w ->
      let* () = Threshold_encryption_proposals_handler_events.shutdown () in
      Worker.shutdown w

let submit_next_proposal timestamp =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    worker
    (Request.Submit_next_proposal timestamp)
  |> handle_request_error
