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
  maximum_number_of_chunks : int;
}

(* The size of a delayed transaction is overapproximated to the maximum size
   of an inbox message, as chunks are not supported in the delayed bridge. *)
let maximum_delayed_transaction_size = 4096

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

let take_delayed_transactions maximum_number_of_chunks =
  let open Lwt_result_syntax in
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

let produce_block_with_transactions ~sequencer_key ~cctxt ~timestamp
    ~smart_rollup_address ~transactions_and_objects ~delayed_transactions
    head_info =
  let open Lwt_result_syntax in
  let transactions, tx_hashes =
    List.to_seq transactions_and_objects
    |> Seq.map (fun (raw, (obj : Ethereum_types.transaction_object)) ->
           (raw, obj.hash))
    |> Seq.split
    |> fun (l, r) -> (List.of_seq l, List.of_seq r)
  in
  Misc.with_timing
    (Blueprint_events.blueprint_production
       head_info.Evm_context.next_blueprint_number)
  @@ fun () ->
  let* blueprint =
    Misc.with_timing
      (Blueprint_events.blueprint_proposal
         head_info.Evm_context.next_blueprint_number)
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
  let* () =
    Evm_context.apply_blueprint timestamp blueprint delayed_transactions
  in
  let (Qty number) = head_info.next_blueprint_number in
  let* () = Blueprints_publisher.publish number blueprint in
  let*! () =
    List.iter_p
      (fun hash -> Block_producer_events.transaction_selected ~hash)
      (tx_hashes @ delayed_transactions)
  in
  return_unit

(** Produces a block if we find at least one valid transaction in the transaction
    pool or if [force] is true. *)
let produce_block_if_needed ~cctxt ~smart_rollup_address ~sequencer_key ~force
    ~timestamp ~maximum_number_of_chunks head_info =
  let open Lwt_result_syntax in
  let* delayed_transactions, remaining_cumulative_size =
    take_delayed_transactions maximum_number_of_chunks
  in
  let* transactions_and_objects =
    (* Low key optimization to avoid even checking the txpool if there is not
       enough space for the smallest transaction. *)
    if remaining_cumulative_size <= minimum_ethereum_transaction_size then
      return []
    else
      Tx_pool.pop_transactions
        ~maximum_cumulative_size:remaining_cumulative_size
  in
  let n =
    List.length transactions_and_objects + List.length delayed_transactions
  in
  if force || n > 0 then
    let* () =
      produce_block_with_transactions
        ~sequencer_key
        ~cctxt
        ~timestamp
        ~smart_rollup_address
        ~transactions_and_objects
        ~delayed_transactions
        head_info
    in
    let* () = Tx_pool.clear_popped_transactions () in
    return n
  else return 0

let produce_block ~cctxt ~smart_rollup_address ~sequencer_key ~force ~timestamp
    ~maximum_number_of_chunks =
  let open Lwt_result_syntax in
  let* is_locked = Tx_pool.is_locked () in
  if is_locked then
    let*! () = Block_producer_events.production_locked () in
    return 0
  else
    let*! head_info = Evm_context.head_info () in
    let is_going_to_upgrade =
      match head_info.pending_upgrade with
      | Some Evm_events.Upgrade.{hash = _; timestamp = upgrade_timestamp} ->
          timestamp >= upgrade_timestamp
      | None -> false
    in
    if is_going_to_upgrade then
      let* () =
        produce_block_with_transactions
          ~sequencer_key
          ~cctxt
          ~timestamp
          ~smart_rollup_address
          ~transactions_and_objects:[]
          ~delayed_transactions:[]
          head_info
      in
      return 0
    else
      produce_block_if_needed
        ~cctxt
        ~sequencer_key
        ~timestamp
        ~smart_rollup_address
        ~force
        ~maximum_number_of_chunks
        head_info

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
        let {
          cctxt;
          smart_rollup_address;
          sequencer_key;
          maximum_number_of_chunks;
        } =
          state
        in
        produce_block
          ~cctxt
          ~smart_rollup_address
          ~sequencer_key
          ~force
          ~timestamp
          ~maximum_number_of_chunks

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
