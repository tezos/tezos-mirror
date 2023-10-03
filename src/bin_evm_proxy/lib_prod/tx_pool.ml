(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

module Tx = struct
  type t = string

  let equal a b = a = b

  let hash t = Ethereum_types.hash_raw_tx t |> Hashtbl.hash
end

module Message_queue = Hash_queue.Make (Tx) (String)

module Types = struct
  type state = {
    rollup_node : (module Rollup_node.S);
    smart_rollup_address : string;
    mutable level : Ethereum_types.block_height;
    messages : Message_queue.t;
  }

  type parameters = (module Rollup_node.S) * string
end

module Name = struct
  (* We only have a single tx-pool in the evm proxy *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_proxy"; "tx-pool"; "prod"; "worker"]

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t =
    | Add_transaction :
        Ethereum_types.hex
        -> ((Ethereum_types.hash, string) result, tztrace) t
    | New_l2_head : Ethereum_types.block_height -> (unit, tztrace) t

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
             (req "transaction" Ethereum_types.hex_encoding))
          (function
            | View (Add_transaction messages) -> Some ((), messages) | _ -> None)
          (fun ((), messages) -> View (Add_transaction messages));
        case
          (Tag 1)
          ~title:"New_l2_head"
          (obj2
             (req "request" (constant "new_l2_head"))
             (req "block_height" Ethereum_types.block_height_encoding))
          (function View (New_l2_head b) -> Some ((), b) | _ -> None)
          (fun ((), b) -> View (New_l2_head b));
      ]

  let pp ppf (View r) =
    match r with
    | Add_transaction transaction ->
        let (Ethereum_types.Hex transaction) = transaction in
        Format.fprintf ppf "Add [%s] tx to tx-pool" transaction
    | New_l2_head block_height ->
        let (Ethereum_types.Block_height block_height) = block_height in
        Format.fprintf ppf "New L2 head: %s" (Z.to_string block_height)
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let on_transaction state tx_raw =
  let open Lwt_result_syntax in
  let open Types in
  let {rollup_node = (module Rollup_node); _} = state in
  let* is_valid = Rollup_node.is_tx_valid tx_raw in
  match is_valid with
  | Error err -> return (Error err)
  | Ok _ ->
      let (Ethereum_types.Hex raw_tx) = tx_raw in
      Message_queue.replace state.messages raw_tx raw_tx ;
      (* compute the hash *)
      let tx_raw = Ethereum_types.hex_to_bytes tx_raw in
      let tx_hash = Ethereum_types.hash_raw_tx tx_raw in
      let hash =
        Ethereum_types.hash_of_string Hex.(of_string tx_hash |> show)
      in
      return (Ok hash)

let on_head state block_height =
  let open Lwt_result_syntax in
  let open Types in
  let (module Rollup_node) = state.rollup_node in
  let smart_rollup_address = state.smart_rollup_address in
  state.level <- block_height ;
  (* Sends all transactions to the batcher *)
  let* () =
    Message_queue.fold_es
      (fun tx_hash raw_tx _ ->
        let raw_tx = Ethereum_types.Hex raw_tx in
        let*! result =
          Rollup_node.inject_raw_transaction ~smart_rollup_address raw_tx
        in
        match result with
        | Ok _hash ->
            (* Removes successfull transaction from the pool *)
            Message_queue.remove state.messages tx_hash ;
            return_unit
        | Error _ -> return_unit)
      state.messages
      ()
  in
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
    | Request.New_l2_head block_height ->
        protect @@ fun () -> on_head state block_height

  type launch_error = error trace

  let on_launch _w () (rollup_node, smart_rollup_address) =
    let state =
      Types.
        {
          rollup_node;
          smart_rollup_address;
          level = Block_height Z.zero;
          messages = Message_queue.create 100_000 (* ~ 400MB *);
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

(** Sends New_l2_level each time there is a new l2 level 
TODO: https://gitlab.com/tezos/tezos/-/issues/6079
listen to the node instead of pulling the level each 5s
*)
let rec subscribe_l2_block worker =
  let open Lwt_result_syntax in
  let*! () = Lwt_unix.sleep 5.0 in
  let state = Worker.state worker in
  let Types.{rollup_node = (module Rollup_node_rpc); _} = state in
  (* Get the current eth level.*)
  let*! res = Rollup_node_rpc.current_block_number () in
  match res with
  | Error _ ->
      (* Kind of retry strategy *)
      Format.printf
        "Connection with the rollup node has been lost, retrying...\n" ;
      subscribe_l2_block worker
  | Ok block_number ->
      if state.level != block_number then
        let*! _pushed =
          Worker.Queue.push_request worker (Request.New_l2_head block_number)
        in
        subscribe_l2_block worker
      else subscribe_l2_block worker

let start ((module Rollup_node_rpc : Rollup_node.S), smart_rollup_address) =
  let open Lwt_result_syntax in
  let+ worker =
    Worker.launch
      table
      ()
      ((module Rollup_node_rpc), smart_rollup_address)
      (module Handlers)
  in
  let () = Lwt.dont_wait (fun () -> subscribe_l2_block worker) (fun _ -> ()) in
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
