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
    | Produce_blueprint :
        (Time.Protocol.t * bool * string list * Ethereum_types.hash list)
        -> ( (Blueprint_types.t * Ethereum_types.hash trace * int) option,
             tztrace )
           t

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Produce_blueprint"
          (obj5
             (req "request" (constant "produce_blueprint"))
             (req "timestamp" Time.Protocol.encoding)
             (req "force" bool)
             (req "transactions"
             @@ Data_encoding.list (Data_encoding.string' Hex))
             (req "delayed_transactions"
             @@ Data_encoding.list Ethereum_types.hash_encoding))
          (function
            | View
                (Produce_blueprint
                  (timestamp, force, transactions, delayed_transactions)) ->
                Some ((), timestamp, force, transactions, delayed_transactions))
          (fun ((), timestamp, force, transactions, delayed_transactions) ->
            View
              (Produce_blueprint
                 (timestamp, force, transactions, delayed_transactions)));
      ]

  let pp _ppf (View _) = ()
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let produce_blueprint ~force ~timestamp ~sequencer_key ~cctxt
    ~smart_rollup_address transactions delayed_transactions =
  let open Lwt_result_syntax in
  let n = List.length transactions + List.length delayed_transactions in
  if force || n > 0 then
    let*! head_info = Evm_context.head_info () in
    Helpers.with_timing
      (Blueprint_events.blueprint_production head_info.next_blueprint_number)
    @@ fun () ->
    let* payload =
      Helpers.with_timing
        (Blueprint_events.blueprint_proposal head_info.next_blueprint_number)
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
    let blueprint =
      Blueprint_types.
        {payload; timestamp; number = head_info.next_blueprint_number}
    in
    return @@ Some (blueprint, delayed_transactions, n)
  else return None

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Produce_blueprint
        (timestamp, force, transactions, delayed_transactions) ->
        protect @@ fun () ->
        let {cctxt; smart_rollup_address; sequencer_key} = state in
        produce_blueprint
          ~cctxt
          ~smart_rollup_address
          ~sequencer_key
          ~force
          ~timestamp
          transactions
          delayed_transactions

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

let produce_blueprint ~force ~timestamp transactions delayed_transactions =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    worker
    (Request.Produce_blueprint
       (timestamp, force, transactions, delayed_transactions))
  |> handle_request_error
