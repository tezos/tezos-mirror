(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

module Types = struct
  type state = {
    rollup_node : (module Rollup_node.S);
    smart_rollup_address : string;
  }

  type parameters = (module Rollup_node.S) * string
end

module Name = struct
  (* We only have a single tx-pool in the evm proxy *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_proxy"; "tx-pool"; "worker"]

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t =
    | Add_transaction : string -> (unit, error trace) t
    | New_l2_head : Ethereum_types.block_height -> (unit, error trace) t

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
        Format.fprintf ppf "Add [%s] to tx-pool" transaction
    | New_l2_head block_height ->
        let (Ethereum_types.Block_height block_height) = block_height in
        Format.fprintf ppf "New L2 head: %s" (Z.to_string block_height)
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let on_transaction _state _raw_tx = Lwt_result_syntax.return_unit

let on_head _state _block_height = Lwt_result_syntax.return_unit

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
    let state = Types.{rollup_node; smart_rollup_address} in
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

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail _ | Lwt.Sleep -> Error (TzTrace.make No_tx_pool))

let start ((module Rollup_node_rpc : Rollup_node.S), smart_rollup_address) =
  let open Lwt_result_syntax in
  let+ worker =
    Worker.launch
      table
      ()
      ((module Rollup_node_rpc), smart_rollup_address)
      (module Handlers)
  in
  Lwt.wakeup worker_waker worker

let shutdown () =
  let w = Lazy.force worker in
  match w with
  | Error _ ->
      (* There is no tx-pool, nothing to do *)
      Lwt.return_unit
  | Ok w -> Worker.shutdown w
