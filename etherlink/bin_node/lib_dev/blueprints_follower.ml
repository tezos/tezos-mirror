(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type on_new_blueprint_handler =
  quantity ->
  Blueprint_types.with_events ->
  [`Restart_from of quantity | `Continue] tzresult Lwt.t

type on_finalized_levels_handler =
  l1_level:int32 ->
  start_l2_level:Ethereum_types.quantity ->
  end_l2_level:Ethereum_types.quantity ->
  unit tzresult Lwt.t

type parameters = {
  on_new_blueprint : on_new_blueprint_handler;
  on_finalized_levels : on_finalized_levels_handler;
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

let local_head_too_old ?remote_head ~multichain ~evm_node_endpoint
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
        if multichain then
          Batch.call
            (module Generic_block_number)
            ~keep_alive:true
            ~evm_node_endpoint
            ()
        else
          Batch.call
            (module Block_number)
            ~keep_alive:true
            ~evm_node_endpoint
            ()
  in
  return
    ( is_too_old ~remote:remote_head_number ~next:next_blueprint_number,
      Qty remote_head_number )

let quantity_succ (Qty x) = Qty Z.(succ x)

let quantity_add (Qty x) y = Qty Z.(add x (of_int y))

module Blueprints_sequence = struct
  type t = (Blueprint_types.with_events, tztrace) Seq_es.t

  let legacy_rpc_fallback = ref false

  let rec fold f acc seq =
    let open Lwt_result_syntax in
    let* next_step = Seq_es.uncons seq in
    match next_step with
    | None -> return (`Completed acc)
    | Some (blueprint, seq) -> (
        let* acc = f acc blueprint in
        match acc with
        | `Continue acc -> (fold [@tailcall]) f acc seq
        | `Cut c -> return (`Cut c))

  let make_legacy_rpc ~multichain ~next_blueprint_number evm_node_endpoint : t =
    let open Lwt_result_syntax in
    Seq_es.ES.unfold
      (fun (remote_head, next_blueprint_number) ->
        let* is_too_old, remote_head =
          local_head_too_old
            ?remote_head
            ~multichain
            ~evm_node_endpoint
            next_blueprint_number
        in
        if is_too_old then
          let* blueprint =
            (* See {Note keep_alive} *)
            Evm_services.get_blueprint_with_events
              ~keep_alive:true
              ~evm_node_endpoint
              next_blueprint_number
          in
          return
            (Some
               ( blueprint,
                 (Some remote_head, quantity_succ next_blueprint_number) ))
        else return None)
      (None, next_blueprint_number)

  let rec make_with_chunks ?remote_head ~multichain ~next_blueprint_number
      evm_node_endpoint : t =
   fun () ->
    let open Lwt_result_syntax in
    let* is_too_old, remote_head =
      local_head_too_old
        ?remote_head
        ~multichain
        ~evm_node_endpoint
        next_blueprint_number
    in
    if is_too_old then
      let*! blueprints =
        (* See {Note keep_alive} *)
        Evm_services.get_blueprints_with_events
          ~keep_alive:true
          ~evm_node_endpoint
          ~count:500L
          next_blueprint_number
      in
      match blueprints with
      | Ok [] ->
          (* The node should never return an empty-list. It is expected that it
             will always return at least one element. *)
          assert false
      | Ok blueprints ->
          let next_chunks =
            make_with_chunks
              ~remote_head
              ~multichain
              ~next_blueprint_number:
                (quantity_add next_blueprint_number (List.length blueprints))
              evm_node_endpoint
          in
          Seq_es.append (Seq_es.of_seq (List.to_seq blueprints)) next_chunks
          @@ ()
      | Error
          (( Tezos_rpc.Context.Not_found _
           | RPC_client_errors.Request_failed
               {error = Unauthorized_uri | Forbidden; _} )
          :: _) ->
          legacy_rpc_fallback := true ;
          make_legacy_rpc
            ~multichain
            ~next_blueprint_number
            evm_node_endpoint
            ()
      | Error err -> fail err
    else return Seq_es.Nil

  let make ~multichain ~next_blueprint_number evm_node_endpoint : t =
    if !legacy_rpc_fallback then
      make_legacy_rpc ~multichain ~next_blueprint_number evm_node_endpoint
    else make_with_chunks ~multichain ~next_blueprint_number evm_node_endpoint
end

let rec catchup ~multichain ~next_blueprint_number ~first_connection params :
    Empty.t tzresult Lwt.t =
  let open Lwt_result_syntax in
  Metrics.start_bootstrapping () ;

  let* () =
    when_ (not first_connection) @@ fun () ->
    let delay = Random.float 2. in
    let*! () =
      Events.retrying_connect ~endpoint:params.evm_node_endpoint ~delay
    in
    let*! () = Lwt_unix.sleep delay in
    return_unit
  in

  let seq =
    Blueprints_sequence.make
      ~multichain
      ~next_blueprint_number
      params.evm_node_endpoint
  in

  let* fold_result =
    Blueprints_sequence.fold
      (fun next_blueprint_number blueprint ->
        let* result = params.on_new_blueprint next_blueprint_number blueprint in
        match result with
        | `Restart_from l -> return (`Cut l)
        | `Continue -> return (`Continue (quantity_succ next_blueprint_number)))
      next_blueprint_number
      seq
  in

  match fold_result with
  | `Cut level ->
      catchup ~multichain ~next_blueprint_number:level ~first_connection params
  | `Completed next_blueprint_number -> (
      let*! call_result =
        Evm_services.monitor_messages
          ~evm_node_endpoint:params.evm_node_endpoint
          next_blueprint_number
      in

      match call_result with
      | Ok blueprints_stream ->
          (stream_loop [@tailcall])
            ~multichain
            next_blueprint_number
            params
            blueprints_stream
      | Error _ ->
          (catchup [@tailcall])
            ~multichain
            ~next_blueprint_number
            ~first_connection:false
            params)

and stream_loop ~multichain (Qty next_blueprint_number) params stream =
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
  | Ok (Some (Finalized_levels {l1_level; start_l2_level; end_l2_level})) ->
      let* () =
        params.on_finalized_levels ~l1_level ~start_l2_level ~end_l2_level
      in
      (stream_loop [@tailcall])
        ~multichain
        (Qty next_blueprint_number)
        params
        stream
  | Ok (Some (Blueprint blueprint)) -> (
      let* r = params.on_new_blueprint (Qty next_blueprint_number) blueprint in
      let* () =
        when_ params.ping_tx_pool @@ fun () ->
        Tx_pool.pop_and_inject_transactions ()
      in
      match r with
      | `Continue ->
          (stream_loop [@tailcall])
            ~multichain
            (Qty (Z.succ next_blueprint_number))
            params
            stream
      | `Restart_from level ->
          (catchup [@tailcall])
            ~multichain
            ~next_blueprint_number:level
            ~first_connection:
              (* The connection was not interrupted, but we decided to restart
                 following blueprints from a different level. As a consequence,
                 no need to wait. *)
              true
            params)
  | Ok None | Error [Timeout] ->
      (catchup [@tailcall])
        ~multichain
        ~next_blueprint_number:(Qty next_blueprint_number)
        ~first_connection:false
        params
  | Error err -> fail err

let start ?(ping_tx_pool = true) ~multichain ~time_between_blocks
    ~evm_node_endpoint ~next_blueprint_number ~on_new_blueprint
    ~on_finalized_levels () =
  let open Lwt_result_syntax in
  let*! res =
    catchup
      ~multichain
      ~next_blueprint_number
      ~first_connection:true
      {
        time_between_blocks;
        evm_node_endpoint;
        on_new_blueprint;
        on_finalized_levels;
        ping_tx_pool;
      }
  in
  (* The blueprint follower should never fail. If it does, we better exit with
     an error. *)
  match res with
  | Ok _ -> .
  | Error err ->
      let*! () = Blueprint_events.follower_failed err in
      Lwt_exit.exit_and_raise
        Node_error.exit_code_when_error_blueprints_follower

(* {Note keep_alive}

   The observer is designed to be resilent to downtime from its EVM node
   endpoint. It would not make sense to break this logic here, so we force
   [keep_alive] to true. *)
