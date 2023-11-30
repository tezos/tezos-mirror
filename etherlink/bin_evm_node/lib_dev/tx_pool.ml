(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

module Pool = struct
  module Pkey_map = Map.Make (Ethereum_types.Address)
  module Nonce_map = Map.Make (Z)

  (** Transaction stored in the pool. *)
  type transaction = {
    index : int64; (* Global index of the transaction. *)
    raw_tx : string; (* Current transaction. *)
    gas_price : Z.t; (* The maximum price the user can pay for fees. *)
  }

  type t = {
    transactions : transaction Nonce_map.t Pkey_map.t;
    global_index : int64; (* Index to order the transactions. *)
  }

  let empty : t = {transactions = Pkey_map.empty; global_index = Int64.zero}

  (** Add a transacion to the pool.*)
  let add t pkey base_fee raw_tx =
    let open Result_syntax in
    let {transactions; global_index} = t in
    let* (Qty nonce) = Ethereum_types.transaction_nonce raw_tx in
    let* gas_price = Ethereum_types.transaction_gas_price base_fee raw_tx in
    let transaction = {index = global_index; raw_tx; gas_price} in
    (* Add the transaction to the user's transaction map *)
    let transactions =
      Pkey_map.update
        pkey
        (function
          | None ->
              (* User has no transactions in the pool *)
              Some (Nonce_map.singleton nonce transaction)
          | Some user_transactions ->
              Some
                (Nonce_map.update
                   nonce
                   (function
                     | None -> Some transaction
                     | Some user_transaction ->
                         if gas_price > user_transaction.gas_price then
                           Some transaction
                         else Some user_transaction)
                   user_transactions))
        transactions
    in
    return {transactions; global_index = Int64.(add global_index one)}

  (** Returns all the addresses of the pool *)
  let addresses {transactions; _} =
    Pkey_map.bindings transactions |> List.map fst

  (** Returns the transaction matching the predicate.
      And remove them from the pool. *)
  let partition pkey predicate {transactions; global_index} =
    (* Get the sequence of transaction *)
    let selected, remaining =
      transactions |> Pkey_map.find pkey
      |> Option.value ~default:Nonce_map.empty
      |> Nonce_map.partition predicate
    in
    (* Remove transactions from the public key map if empty *)
    let transactions =
      if Nonce_map.is_empty remaining then Pkey_map.remove pkey transactions
      else Pkey_map.add pkey remaining transactions
    in
    (* Convert the sequence to a list *)
    let selected = selected |> Nonce_map.bindings |> List.map snd in
    (selected, {transactions; global_index})

  (** Removes from the pool the transactions matching the predicate 
      for the given pkey. *)
  let remove pkey predicate t =
    let _txs, t = partition pkey predicate t in
    t

  (** Returns the next nonce for a given user.
      Returns the given nonce if the user does not have any transactions in the pool. *)
  let next_nonce pkey current_nonce (t : t) =
    let open Ethereum_types in
    let {transactions; global_index = _} = t in
    (* Retrieves the list of transactions for a given user. *)
    let user_transactions =
      Pkey_map.find pkey transactions
      |> Option.value ~default:Nonce_map.empty
      |> Nonce_map.bindings |> List.map fst
    in
    let rec aux current_nonce = function
      | [] -> current_nonce
      | nonce :: txs ->
          if current_nonce > nonce then aux current_nonce txs
          else if current_nonce = nonce then
            (aux [@tailcall]) Z.(add current_nonce one) txs
          else current_nonce
    in
    let (Qty current_nonce) = current_nonce in
    aux current_nonce user_transactions |> Ethereum_types.quantity_of_z
end

type mode =
  | Proxy of {rollup_node_endpoint : Uri.t}
  | Sequencer of {time_between_blocks : float}

type parameters = {
  rollup_node : (module Services_backend_sig.S);
  smart_rollup_address : string;
  mode : mode;
}

module Types = struct
  type state = {
    rollup_node : (module Services_backend_sig.S);
    smart_rollup_address : string;
    mutable pool : Pool.t;
    mode : mode;
  }

  type nonrec parameters = parameters
end

module Name = struct
  (* We only have a single tx-pool in the evm node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node"; "tx-pool"; "dev"; "worker"]

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t =
    | Add_transaction :
        string
        -> ((Ethereum_types.hash, string) result, tztrace) t
    | Inject_transactions : (unit, tztrace) t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Add_transaction"
          (obj2
             (req "request" (constant "add_transaction"))
             (req "transaction" string))
          (function
            | View (Add_transaction messages) -> Some ((), messages) | _ -> None)
          (fun ((), messages) -> View (Add_transaction messages));
        case
          (Tag 1)
          ~title:"Inject_transactions"
          (obj1 (req "request" (constant "new_l2_head")))
          (function View Inject_transactions -> Some () | _ -> None)
          (fun () -> View Inject_transactions);
      ]

  let pp ppf (View r) =
    match r with
    | Add_transaction transaction ->
        Format.fprintf
          ppf
          "Add [%s] tx to tx-pool"
          (Hex.of_string transaction |> Hex.show)
    | Inject_transactions -> Format.fprintf ppf "Inject transactions"
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let on_transaction state tx_raw =
  let open Lwt_result_syntax in
  let open Types in
  let {rollup_node = (module Rollup_node); pool; _} = state in
  Format.printf "[tx-pool] Incoming transaction.\n%!" ;
  let* is_valid = Rollup_node.is_tx_valid tx_raw in
  let* (Qty base_fee) = Rollup_node.base_fee_per_gas () in
  match is_valid with
  | Error err ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/6569*)
      Format.printf "[tx-pool] Transaction is not valid.\n%!" ;
      return (Error err)
  | Ok pkey ->
      (* Add the tx to the pool*)
      let*? pool = Pool.add pool pkey base_fee tx_raw in
      (* compute the hash *)
      let tx_hash = Ethereum_types.hash_raw_tx tx_raw in
      let hash =
        Ethereum_types.hash_of_string Hex.(of_string tx_hash |> show)
      in
      Format.printf
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6569*)
        "[tx-pool] Transaction %s added to the tx-pool.\n%!"
        (Ethereum_types.hash_to_string hash) ;
      state.pool <- pool ;
      return (Ok hash)

let inject_transactions ~smart_rollup_address rollup_node pool =
  let open Lwt_result_syntax in
  let (module Rollup_node : Services_backend_sig.S) = rollup_node in
  (* Get all the addresses in the tx-pool. *)
  let addresses = Pool.addresses pool in
  (* Get the nonce related to each address. *)
  let*! addr_with_nonces =
    Lwt_list.map_p
      (fun address ->
        let* nonce = Rollup_node.nonce address in
        let nonce = Option.value ~default:(Qty Z.zero) nonce in
        Lwt.return_ok (address, nonce))
      addresses
  in
  let addr_with_nonces = List.filter_ok addr_with_nonces in
  (* Remove transactions with too low nonce. *)
  let pool =
    addr_with_nonces
    |> List.fold_left
         (fun pool (pkey, current_nonce) ->
           Pool.remove
             pkey
             (fun nonce _tx ->
               let (Ethereum_types.Qty current_nonce) = current_nonce in
               Z.lt nonce current_nonce)
             pool)
         pool
  in
  (* Select transaction with nonce equal to user's nonce.
     Also removes the transactions from the pool. *)
  let txs, pool =
    addr_with_nonces
    |> List.fold_left
         (fun (txs, pool) (pkey, current_nonce) ->
           let selected, pool =
             Pool.partition
               pkey
               (fun nonce _tx ->
                 let (Ethereum_types.Qty current_nonce) = current_nonce in
                 Z.equal nonce current_nonce)
               pool
           in
           let txs = List.append txs selected in
           (txs, pool))
         ([], pool)
  in
  (* Sorting transactions by index.
     First tx in the pool is the first tx to be sent to the batcher. *)
  let txs =
    txs
    |> List.sort (fun Pool.{index = index_a; _} {index = index_b; _} ->
           Int64.compare index_a index_b)
    |> List.map (fun Pool.{raw_tx; _} -> raw_tx)
  in
  (* Send the txs to the rollup *)
  let*! hashes =
    Rollup_node.inject_raw_transactions ~smart_rollup_address ~transactions:txs
  in
  (match hashes with
  | Error _ ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/6569*)
      Format.printf "[tx-pool] Error when sending transaction.\n%!"
  | Ok hashes ->
      List.iter
        (fun hash ->
          Format.printf
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/6569*)
            "[tx-pool] Transaction %s sent to the rollup.\n%!"
            (Ethereum_types.hash_to_string hash))
        hashes) ;
  return pool

let on_head state =
  let open Lwt_result_syntax in
  let open Types in
  let {rollup_node; smart_rollup_address; pool; _} = state in
  let* pool = inject_transactions ~smart_rollup_address rollup_node pool in
  (* update the pool *)
  state.pool <- pool ;
  return_unit

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Add_transaction raw_tx ->
        protect @@ fun () -> on_transaction state raw_tx
    | Request.Inject_transactions -> protect @@ fun () -> on_head state

  type launch_error = error trace

  let on_launch _w ()
      ({rollup_node; smart_rollup_address; mode} : Types.parameters) =
    let state =
      Types.{rollup_node; smart_rollup_address; pool = Pool.empty; mode}
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

type error += No_tx_pool

type error += Tx_pool_terminated

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail _ | Lwt.Sleep -> Error (TzTrace.make No_tx_pool))

let handle_request_error rq =
  let open Lwt_syntax in
  let* rq in
  match rq with
  | Ok res -> return_ok res
  | Error (Worker.Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) -> Lwt.return_error [Tx_pool_terminated]
  | Error (Closed (Some errs)) -> Lwt.return_error errs
  | Error (Any exn) -> Lwt.return_error [Exn exn]

let make_streamed_call ~rollup_node_endpoint =
  let open Lwt_syntax in
  let stream, push = Lwt_stream.create () in
  let on_chunk _v = push (Some ()) and on_close () = push None in
  let* _spill_all =
    Tezos_rpc_http_client_unix.RPC_client_unix.call_streamed_service
      [Media_type.json]
      ~base:rollup_node_endpoint
      Rollup_node_services.global_block_watcher
      ~on_chunk
      ~on_close
      ()
      ()
      ()
  in
  return stream

let rec subscribe_l2_block ~stream_l2 worker =
  let open Lwt_syntax in
  let* new_head = Lwt_stream.get stream_l2 in
  match new_head with
  | Some () ->
      let* _pushed =
        Worker.Queue.push_request worker Request.Inject_transactions
      in
      subscribe_l2_block ~stream_l2 worker
  | None ->
      (* Kind of retry strategy *)
      Format.printf
        "Connection with the rollup node has been lost, retrying...\n" ;
      let* () = Lwt_unix.sleep 1. in
      subscribe_l2_block ~stream_l2 worker

let rec sequencer_produce_block ~time_between_blocks worker =
  let open Lwt_syntax in
  let* () = Lwt_unix.sleep time_between_blocks in
  let state = Worker.state worker in
  let (Types.{rollup_node = (module Rollup_node_rpc); _} : Types.state) =
    state
  in
  let* _pushed = Worker.Queue.push_request worker Request.Inject_transactions in
  sequencer_produce_block ~time_between_blocks worker

let start ({mode; _} as parameters) =
  let open Lwt_result_syntax in
  let+ worker = Worker.launch table () parameters (module Handlers) in
  let () =
    Lwt.dont_wait
      (fun () ->
        match mode with
        | Proxy {rollup_node_endpoint} ->
            let*! stream_l2 = make_streamed_call ~rollup_node_endpoint in
            subscribe_l2_block ~stream_l2 worker
        | Sequencer {time_between_blocks} ->
            sequencer_produce_block ~time_between_blocks worker)
      (fun _ ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6569*)
        Format.printf "[tx-pool] Pool has been stopped.\n%!")
  in
  Lwt.wakeup worker_waker worker

let shutdown () =
  let w = Lazy.force worker in
  match w with
  | Error _ ->
      (* There is no tx-pool, nothing to do *)
      Lwt.return_unit
  | Ok w -> Worker.shutdown w

let add raw_tx =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  Worker.Queue.push_request_and_wait w (Request.Add_transaction raw_tx)
  |> handle_request_error

let nonce pkey =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  let Types.{rollup_node = (module Rollup_node); pool; _} = Worker.state w in
  let* current_nonce = Rollup_node.nonce pkey in
  let next_nonce =
    match current_nonce with
    | None -> Ethereum_types.Qty Z.zero
    | Some current_nonce -> Pool.next_nonce pkey current_nonce pool
  in
  return next_nonce
