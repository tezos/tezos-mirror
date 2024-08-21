(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  cctxt : Client_context.wallet;
  smart_rollup_address : string;
  sequencer_key : Client_keys.sk_uri;
}

module Types = struct
  type nonrec parameters = parameters

  type state = parameters
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base =
    ["evm_node"; "dev"; "theshold"; "encryption"; "block"; "producer"; "worker"]

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t =
    | Produce_block : Threshold_encryption_types.preblock -> (int, tztrace) t

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Produce_block"
          (obj2
             (req "request" (constant "produce_block"))
             (req "preblock" Threshold_encryption_types.preblock_encoding))
          (function View (Produce_block preblock) -> Some ((), preblock))
          (fun ((), preblock) -> View (Produce_block preblock));
      ]

  let pp _ppf (View _) = ()
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let get_hashes ~transactions ~delayed_transactions =
  let open Result_syntax in
  let hashes = List.map Ethereum_types.hash_raw_tx transactions in
  return (delayed_transactions @ hashes)

let produce_block ~sequencer_key ~cctxt ~smart_rollup_address preblock =
  let open Lwt_result_syntax in
  let Threshold_encryption_types.
        {
          transactions;
          delayed_transaction_hashes = delayed_transactions;
          timestamp;
          previous_block_hash;
          current_blueprint_number;
        } =
    preblock
  in
  let n = List.length transactions + List.length delayed_transactions in
  let transactions =
    List.map Threshold_encryption_types.Transaction.to_string transactions
  in
  let*? hashes = get_hashes ~transactions ~delayed_transactions in
  let* chunks =
    Sequencer_blueprint.prepare
      ~sequencer_key
      ~cctxt
      ~timestamp
      ~transactions
      ~delayed_transactions
      ~parent_hash:previous_block_hash
      ~number:current_blueprint_number
  in
  let payload = Sequencer_blueprint.create ~smart_rollup_address ~chunks in
  let (Qty number) = current_blueprint_number in
  let* () =
    Evm_context.apply_blueprint timestamp payload delayed_transactions
  in
  let* () =
    Blueprints_publisher.publish
      number
      (Blueprints_publisher_types.Request.Blueprint chunks)
  in
  let*! () =
    List.iter_p
      (fun hash -> Block_producer_events.transaction_selected ~hash)
      hashes
  in
  let* () = Tx_pool.clear_popped_transactions () in
  return n

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in

    match request with
    | Request.Produce_block preblock ->
        protect @@ fun () ->
        let {smart_rollup_address; cctxt; sequencer_key} = state in
        produce_block ~smart_rollup_address ~cctxt ~sequencer_key preblock

  type launch_error = error trace

  let on_launch _w () (parameters : Types.parameters) =
    Lwt_result_syntax.return parameters

  let on_error (type a b) _w _st (_r : (a, b) Request.t) (_errs : b) :
      unit tzresult Lwt.t =
    Lwt_result_syntax.return_unit

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

type error += No_block_producer

type error += Block_producer_terminated

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail e -> Error (TzTrace.make @@ error_of_exn e)
    | Lwt.Sleep -> Error (TzTrace.make No_block_producer))

let handle_request_error rq =
  let open Lwt_syntax in
  let* rq in
  match rq with
  | Ok res -> return_ok res
  | Error (Worker.Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) -> Lwt.return_error [Block_producer_terminated]
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

let produce_block preblock =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait worker (Request.Produce_block preblock)
  |> handle_request_error
