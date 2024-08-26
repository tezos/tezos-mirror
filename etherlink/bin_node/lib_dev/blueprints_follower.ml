(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type handler =
  Ethereum_types.quantity ->
  Blueprint_types.with_events ->
  (unit, tztrace) result Lwt.t

type parameters = {
  on_new_blueprint : handler;
  time_between_blocks : Configuration.time_between_blocks;
  evm_node_endpoint : Uri.t;
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
  let* (Qty remote_head_number) =
    match remote_head with
    | None ->
        (* The observer is designed to be resilent to downtime from its EVM
           node endpoint. It would not make sense to break this logic here, so
           we force [keep_alive] to true. *)
        Services.call
          (module Block_number)
          ~keep_alive:true
          ~evm_node_endpoint
          ()
    | Some remote_head -> return remote_head
  in
  return
    ( Z.Compare.(next_blueprint_number <= remote_head_number),
      Qty remote_head_number )

let quantity_succ (Qty x) = Qty Z.(succ x)

let[@tailrec] rec go ?remote_head ~next_blueprint_number ~first_connection
    params =
  let open Lwt_result_syntax in
  let* local_head_too_old, remote_head =
    local_head_too_old
      ?remote_head
      ~evm_node_endpoint:params.evm_node_endpoint
      next_blueprint_number
  in

  if local_head_too_old then
    let* blueprint =
      Evm_services.get_blueprint
        ~evm_node_endpoint:params.evm_node_endpoint
        next_blueprint_number
    in
    let* () = params.on_new_blueprint next_blueprint_number blueprint in
    (go [@tailcall])
      ~next_blueprint_number:(quantity_succ next_blueprint_number)
      ~remote_head
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
        (go [@tailcall]) ~next_blueprint_number ~first_connection:false params

and[@tailrec] stream_loop (Qty next_blueprint_number) params stream =
  let open Lwt_result_syntax in
  let*! candidate =
    Lwt.pick
      [
        (let*! res = Lwt_stream.get stream in
         return res);
        timeout_from_tbb params.time_between_blocks;
      ]
  in
  match candidate with
  | Ok (Some blueprint) ->
      let* () = params.on_new_blueprint (Qty next_blueprint_number) blueprint in
      let* () = Tx_pool.pop_and_inject_transactions () in
      (stream_loop [@tailcall])
        (Qty (Z.succ next_blueprint_number))
        params
        stream
  | Ok None | Error [Timeout] ->
      (go [@tailcall])
        ~next_blueprint_number:(Qty next_blueprint_number)
        ~first_connection:false
        params
  | Error err -> fail err

let start ~time_between_blocks ~evm_node_endpoint ~next_blueprint_number
    on_new_blueprint =
  go
    ~next_blueprint_number
    ~first_connection:true
    {time_between_blocks; evm_node_endpoint; on_new_blueprint}
