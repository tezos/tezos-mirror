(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type handler =
  quantity ->
  Blueprint_types.with_events ->
  [`Check_for_reorg of quantity | `Continue] tzresult Lwt.t

type parameters = {
  on_new_blueprint : handler;
  time_between_blocks : Configuration.time_between_blocks;
  evm_node_endpoint : Uri.t;
  ping_tx_pool : bool;
}

type error += Timeout

let timeout_from_tbb = function
  | Configuration.Nothing ->
      let p, _ = Lwt.task () in
      p
  | Time_between_blocks tbb ->
      let open Lwt_result_syntax in
      let*! _ = Lwt_unix.sleep (tbb +. 1.) in
      tzfail Timeout

let local_head_too_old ?remote_head ~evm_node_endpoint
    (Qty next_blueprint_number) =
  let open Lwt_result_syntax in
  let open Rpc_encodings in
  let is_too_old ~remote ~next = Z.Compare.(next <= remote) in
  let* (Qty remote_head_number) =
    match remote_head with
    | Some (Qty remote_head)
    (* If we are still behind the remote head, no need to fetch it again, we
       already know we have some work to do. On the other hand, if it appears
       that we have finally caught-up, we ctually don’t know how much time it
       took us. Worst-case scenario, the remote EVM node can now dozens of
       thousands of blocks or more. It is safer to call `eth_blockNumber` one
       last time in that case. *)
      when is_too_old ~remote:remote_head ~next:next_blueprint_number ->
        return (Qty remote_head)
    | None | Some _ ->
        (* See {Note keep_alive} *)
        Batch.call (module Block_number) ~keep_alive:true ~evm_node_endpoint ()
  in
  return
    ( is_too_old ~remote:remote_head_number ~next:next_blueprint_number,
      Qty remote_head_number )

let quantity_succ (Qty x) = Qty Z.(succ x)

let rec catchup ?remote_head ~next_blueprint_number ~first_connection params =
  let open Lwt_result_syntax in
  Metrics.start_bootstrapping () ;
  let* local_head_too_old, remote_head =
    local_head_too_old
      ?remote_head
      ~evm_node_endpoint:params.evm_node_endpoint
      next_blueprint_number
  in

  if local_head_too_old then
    let* blueprint =
      (* See {Note keep_alive} *)
      Evm_services.get_blueprint
        ~keep_alive:true
        ~evm_node_endpoint:params.evm_node_endpoint
        next_blueprint_number
    in
    let* r = params.on_new_blueprint next_blueprint_number blueprint in
    match r with
    | `Continue ->
        (catchup [@tailcall])
          ~next_blueprint_number:(quantity_succ next_blueprint_number)
          ~remote_head
          ~first_connection
          params
    | `Check_for_reorg level ->
        (catchup [@tailcall])
          ~next_blueprint_number:level
          ~first_connection
          params
  else
    let* () =
      when_ (not first_connection) @@ fun () ->
      let delay = Random.float 2. in
      let*! () =
        Events.retrying_connect ~endpoint:params.evm_node_endpoint ~delay
      in
      let*! () = Lwt_unix.sleep delay in
      return_unit
    in

    let*! call_result =
      Evm_services.monitor_blueprints
        ~evm_node_endpoint:params.evm_node_endpoint
        next_blueprint_number
    in

    match call_result with
    | Ok blueprints_stream ->
        (stream_loop [@tailcall]) next_blueprint_number params blueprints_stream
    | Error _ ->
        (catchup [@tailcall])
          ~next_blueprint_number
          ~first_connection:false
          params

and stream_loop (Qty next_blueprint_number) params stream =
  let open Lwt_result_syntax in
  Metrics.stop_bootstrapping () ;
  let*! candidate =
    Lwt.pick
      [
        (let*! res = Lwt_stream.get stream in
         return res);
        timeout_from_tbb params.time_between_blocks;
      ]
  in
  match candidate with
  | Ok (Some blueprint) -> (
      let* r = params.on_new_blueprint (Qty next_blueprint_number) blueprint in
      let* () =
        when_ params.ping_tx_pool @@ fun () ->
        Tx_pool.pop_and_inject_transactions ()
      in
      match r with
      | `Continue ->
          (stream_loop [@tailcall])
            (Qty (Z.succ next_blueprint_number))
            params
            stream
      | `Check_for_reorg level ->
          (catchup [@tailcall])
            ~next_blueprint_number:level
            ~first_connection:false
            params)
  | Ok None | Error [Timeout] ->
      (catchup [@tailcall])
        ~next_blueprint_number:(Qty next_blueprint_number)
        ~first_connection:false
        params
  | Error err -> fail err

let start ?(ping_tx_pool = true) ~time_between_blocks ~evm_node_endpoint
    ~next_blueprint_number on_new_blueprint =
  catchup
    ~next_blueprint_number
    ~first_connection:true
    {time_between_blocks; evm_node_endpoint; on_new_blueprint; ping_tx_pool}

(* {Note keep_alive}

   The observer is designed to be resilent to downtime from its EVM node
   endpoint. It would not make sense to break this logic here, so we force
   [keep_alive] to true. *)
