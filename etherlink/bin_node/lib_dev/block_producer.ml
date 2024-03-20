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

  let base = ["evm_node"; "dev"; "block"; "producer"; "worker"]

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t =
    | Produce_block : (Time.Protocol.t * bool) -> (int, tztrace) t

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Produce_block"
          (obj3
             (req "request" (constant "produce_block"))
             (req "timestamp" Time.Protocol.encoding)
             (req "force" bool))
          (function
            | View (Produce_block (timestamp, force)) ->
                Some ((), timestamp, force))
          (fun ((), timestamp, force) ->
            View (Produce_block (timestamp, force)));
      ]

  let pp _ppf (View _) = ()
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let get_hashes ~transactions ~delayed_transactions =
  let open Result_syntax in
  let delayed_hashes =
    List.map Ethereum_types.Delayed_transaction.hash delayed_transactions
  in
  let hashes =
    List.map
      (fun transaction ->
        let tx_hash_str = Ethereum_types.hash_raw_tx transaction in
        Ethereum_types.(
          Hash Hex.(of_string tx_hash_str |> show |> hex_of_string)))
      transactions
  in
  return (delayed_hashes @ hashes)

let produce_block ~cctxt ~smart_rollup_address ~sequencer_key ~force ~timestamp
    =
  let open Lwt_result_syntax in
  let* tx_pool_response = Tx_pool.pop_transactions () in
  match tx_pool_response with
  | Transactions (transactions, delayed_transactions) ->
      let n = List.length transactions + List.length delayed_transactions in
      if force || n > 0 then
        let* head_info = Evm_context.head_info () in
        Helpers.with_timing
          (Blueprint_events.blueprint_production
             head_info.next_blueprint_number)
        @@ fun () ->
        let*? hashes = get_hashes ~transactions ~delayed_transactions in
        let* blueprint =
          Helpers.with_timing
            (Blueprint_events.blueprint_proposal
               head_info.next_blueprint_number)
          @@ fun () ->
          Sequencer_blueprint.create
            ~sequencer_key
            ~cctxt
            ~timestamp
            ~smart_rollup_address
            ~transactions
            ~delayed_transactions
            ~parent_hash:head_info.current_block_hash
            ~number:head_info.next_blueprint_number
        in
        let* () = Evm_context.apply_sequencer_blueprint timestamp blueprint in
        let (Qty number) = head_info.next_blueprint_number in
        let* () = Blueprints_publisher.publish number blueprint.to_publish in
        let*! () =
          List.iter_p
            (fun hash -> Block_producer_events.transaction_selected ~hash)
            hashes
        in
        return n
      else return 0
  | Locked ->
      let*! () = Block_producer_events.production_locked () in
      return 0

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Produce_block (timestamp, force) ->
        protect @@ fun () ->
        let {cctxt; smart_rollup_address; sequencer_key} = state in
        produce_block
          ~cctxt
          ~smart_rollup_address
          ~sequencer_key
          ~force
          ~timestamp

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

let produce_block ~force ~timestamp =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    worker
    (Request.Produce_block (timestamp, force))
  |> handle_request_error
