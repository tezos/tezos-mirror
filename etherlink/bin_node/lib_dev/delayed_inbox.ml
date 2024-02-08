(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {rollup_node_endpoint : Uri.t; delayed_inbox_interval : int}

module StringSet = Set.Make (String)

module Types = struct
  type state = {
    rollup_node_endpoint : Uri.t;
    delayed_inbox_interval : int;
    mutable pending_transactions : StringSet.t;
  }

  type nonrec parameters = parameters
end

module Name = struct
  (* We only have a single delayed inbox in the evm node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node"; "delayed-inbox"; "dev"; "worker"]

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t = New_rollup_node_block : Int32.t -> (unit, error trace) t

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"New_rollup_node_block"
          (obj2
             (req "request" (constant "new_rollup_node_block"))
             (req "rollup_head" int32))
          (function
            | View (New_rollup_node_block rollup_head) -> Some ((), rollup_head))
          (fun ((), rollup_head) -> View (New_rollup_node_block rollup_head));
      ]

  let pp ppf (View r) =
    match r with
    | New_rollup_node_block rollup_head ->
        Format.fprintf ppf "New_rollup_node_block (level %ld)" rollup_head
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let subkeys_from_rollup_node path level rollup_node_endpoint =
  let open Rollup_node_services in
  call_service
    ~base:rollup_node_endpoint
    durable_state_subkeys
    ((), Block_id.Level level)
    {key = path}
    ()

let read_from_rollup_node path level rollup_node_endpoint =
  let open Rollup_node_services in
  call_service
    ~base:rollup_node_endpoint
    durable_state_value
    ((), Block_id.Level level)
    {key = path}
    ()

let delayed_transaction_hashes level rollup_node_endpoint =
  let open Lwt_result_syntax in
  let* keys =
    subkeys_from_rollup_node
      Durable_storage_path.Delayed_transaction.hashes
      level
      rollup_node_endpoint
  in
  match keys with
  | None -> return []
  | Some keys ->
      let hashes =
        (* Remove the empty, meta keys *)
        List.filter (fun key -> not (key = "" || key = "meta")) keys
      in
      return hashes

let fetch_delayed_inbox_hashes ~level ~rollup_node_endpoint =
  delayed_transaction_hashes level rollup_node_endpoint

let fetch_delayed_transactions ~hashes ~level ~rollup_node_endpoint =
  let open Lwt_syntax in
  List.filter_map_p
    (fun key ->
      let hash = Ethereum_types.hash_of_string key in
      let path = Durable_storage_path.Delayed_transaction.transaction hash in
      let* bytes = read_from_rollup_node path level rollup_node_endpoint in
      match bytes with
      | Ok (Some bytes) ->
          return (Ethereum_types.Delayed_transaction.of_bytes hash bytes)
      | _ ->
          let* () =
            Delayed_inbox_events.transaction_fetch_failed ~tx_hash:hash ~level
          in
          return_none)
    hashes

let include_delayed_transaction delayed_transaction =
  let open Lwt_syntax in
  let* () = Delayed_inbox_events.add_transaction ~delayed_transaction in
  let* _sent = Tx_pool.add_delayed delayed_transaction in
  return_unit

let on_new_head
    ({delayed_inbox_interval; rollup_node_endpoint; _} as state : Types.state)
    level =
  let open Lwt_syntax in
  (* Fetch the delayed inbox with the given interval *)
  if Int32.(rem level (of_int delayed_inbox_interval) <> 0l) then return_unit
  else
    (* Hashes in the delayed inbox *)
    let* delayed_transaction_hashes =
      fetch_delayed_inbox_hashes ~level ~rollup_node_endpoint
    in
    let* () =
      match delayed_transaction_hashes with
      | Error _err -> Delayed_inbox_events.fetch_failed ~level
      | Ok delayed_transaction_hashes ->
          (* Compute new hashes to avoid fetching transactions we already
             know about. *)
          let new_transaction_hashes =
            List.filter
              (fun tx_hash ->
                not (StringSet.mem tx_hash state.pending_transactions))
              delayed_transaction_hashes
          in
          let* () =
            Delayed_inbox_events.fetch_succeeded
              ~level
              ~nb_txs:(List.length new_transaction_hashes)
          in
          (* The new pending set is just the fetched delayed inbox.
             It's important to drop the transactions from the pending
             set that are no longer present in the delayed inbox.
             On the contrary, a transaction that is sent twice to the
             delayed inbox at different levels would only be added to the
             tx-pool once.
          *)
          let pending = StringSet.of_list delayed_transaction_hashes in
          (* Fetch transactions for new hashes *)
          let* new_delayed_transactions =
            fetch_delayed_transactions
              ~hashes:new_transaction_hashes
              ~level
              ~rollup_node_endpoint
          in
          state.pending_transactions <- pending ;
          (* Add new transactions to the tx-pool *)
          let* () =
            List.iter_p include_delayed_transaction new_delayed_transactions
          in
          return_unit
    in
    return_unit

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun worker request ->
    let open Lwt_result_syntax in
    match request with
    | Request.New_rollup_node_block rollup_block_lvl ->
        protect @@ fun () ->
        let*! () = on_new_head (Worker.state worker) rollup_block_lvl in
        return_unit

  type launch_error = error trace

  let on_launch _w ()
      ({rollup_node_endpoint; delayed_inbox_interval} : Types.parameters) =
    let state =
      Types.
        {
          rollup_node_endpoint;
          delayed_inbox_interval;
          pending_transactions = StringSet.empty;
        }
    in
    Lwt_result_syntax.return state

  let on_error (type a b) _w _st (_r : (a, b) Request.t) (_errs : b) :
      unit tzresult Lwt.t =
    Lwt_result_syntax.return_unit

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

type error += No_delayed_inbox

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail _ | Lwt.Sleep -> Error (TzTrace.make No_delayed_inbox))

let start parameters =
  let open Lwt_result_syntax in
  let*! () = Delayed_inbox_events.started () in
  let+ worker = Worker.launch table () parameters (module Handlers) in
  Lwt.wakeup worker_waker worker

let shutdown () =
  let w = Lazy.force worker in
  match w with
  | Error _ ->
      (* There is no delayed inbox, nothing to do *)
      Lwt.return_unit
  | Ok w -> Worker.shutdown w

let worker_add_request ~request : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  match Lazy.force worker with
  | Ok w ->
      let*! (_pushed : bool) = Worker.Queue.push_request w request in
      return_unit
  | Error [No_delayed_inbox] -> return_unit
  | Error e -> Lwt.return (Error e)

let new_rollup_block rollup_level =
  worker_add_request ~request:(New_rollup_node_block rollup_level)
