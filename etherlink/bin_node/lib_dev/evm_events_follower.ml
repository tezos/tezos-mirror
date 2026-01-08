(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  rollup_node_endpoint : Uri.t;
  filter_event : Evm_events.t -> bool;
  keep_alive : bool;
  rpc_timeout : float;
}

module StringSet = Set.Make (String)

module Types = struct
  type state = {
    rollup_node_endpoint : Uri.t;
    filter_event : Evm_events.t -> bool;
    keep_alive : bool;
    rpc_timeout : float;
    mutable last_l1_level : int32 option;
  }

  type nonrec parameters = parameters
end

module Name = struct
  (* We only have a single events follower in the evm node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node_worker"; "events_follower"]

  let pp _ _ = ()

  let equal () () = true
end

module Worker =
  Octez_telemetry.Worker.MakeSingle (Name) (Evm_events_follower_types.Request)
    (Types)

type worker = Worker.infinite Worker.queue Worker.t

let read_from_rollup_node ~keep_alive path level rollup_node_endpoint =
  let open Rollup_services in
  call_service
    ~keep_alive
    ~base:rollup_node_endpoint
    durable_state_value
    ((), Block_id.Level level)
    {key = path}
    ()

let fetch_event
    ({rollup_node_endpoint; keep_alive; rpc_timeout; _} : Types.state)
    rollup_block_lvl event_index =
  let open Lwt_result_syntax in
  let path = Durable_storage_path.Evm_events.nth_event event_index in
  let* bytes_opt =
    read_from_rollup_node
      ~keep_alive
      ~timeout:rpc_timeout
      path
      rollup_block_lvl
      rollup_node_endpoint
  in
  let*! event_opt =
    match bytes_opt with
    | Some bytes -> Evm_events.of_bytes bytes
    | None -> Lwt.return_none
  in
  let open Lwt_result_syntax in
  let*! () =
    if Option.is_none event_opt then
      Evm_events_follower_events.unreadable_event (event_index, rollup_block_lvl)
    else Lwt.return_unit
  in
  return event_opt

let fetch_events_one_by_one
    ({rollup_node_endpoint; keep_alive; rpc_timeout; filter_event; _} as state :
      Types.state) rollup_block_lvl =
  let open Lwt_result_syntax in
  let* nb_of_events_bytes =
    read_from_rollup_node
      ~keep_alive
      ~timeout:rpc_timeout
      Durable_storage_path.Evm_events.length
      rollup_block_lvl
      rollup_node_endpoint
  in
  match nb_of_events_bytes with
  | None -> return_nil
  | Some nb_of_events_bytes ->
      let (Qty nb_of_events) =
        Ethereum_types.decode_number_le nb_of_events_bytes
      in
      let nb_of_events = Z.to_int nb_of_events in
      let+ events =
        List.init_ep
          ~when_negative_length:
            (error_of_fmt
               "Internal error: the rollup node advertised a negative length \
                for the events stream")
          nb_of_events
          (fetch_event state rollup_block_lvl)
      in
      List.filter_map
        (function
          | Some event when filter_event event -> Some event | _ -> None)
        events

let fetch_events_at_once
    ({rollup_node_endpoint; keep_alive; rpc_timeout; filter_event; _} :
      Types.state) rollup_block_lvl =
  let open Lwt_result_syntax in
  let path = Durable_storage_path.Evm_events.events in
  let* bindings =
    let open Rollup_services in
    call_service
      ~keep_alive
      ~timeout:rpc_timeout
      ~base:rollup_node_endpoint
      durable_state_values
      ((), Block_id.Level rollup_block_lvl)
      {key = path}
      ()
  in
  let expected = ref 0 in
  let*! unsorted =
    List.filter_map_s
      (fun (key, value) ->
        let open Lwt_syntax in
        match key with
        | "length" ->
            let (Qty len) = Ethereum_types.decode_number_le value in
            expected := Z.to_int len ;
            return_none
        | index -> (
            match int_of_string_opt index with
            | None ->
                let*! () = Evm_events_follower_events.unexpected_key index in
                return_none
            | Some event_index -> (
                let* event = Evm_events.of_bytes value in
                match event with
                | None -> return_none
                | Some event -> return_some (event_index, event))))
      bindings
  in
  let*! () =
    let fetched = List.length unsorted in
    let expected = !expected in
    if fetched <> expected then
      Evm_events_follower_events.unexpected_number_of_events ~fetched ~expected
    else Lwt.return_unit
  in
  unsorted
  |> List.fast_sort (fun (i1, _) (i2, _) -> Compare.Int.compare i1 i2)
  |> List.filter_map (fun (_, e) -> if filter_event e then Some e else None)
  |> return

let fetch_events =
  let always_fallback = ref false in
  fun state rollup_block_lvl ->
    let open Lwt_result_syntax in
    if !always_fallback then fetch_events_one_by_one state rollup_block_lvl
    else
      let*! res =
        protect @@ fun () -> fetch_events_at_once state rollup_block_lvl
      in
      match res with
      | Ok events -> return events
      | Error e ->
          (match e with
          | Tezos_rpc.Context.Not_found _ :: _ ->
              (* 404, Rollup node is too old to support fetching all at
                 once. Always fallback in the future. *)
              always_fallback := true
          | RPC_client_errors.Request_failed
              {error = Unauthorized_uri | Forbidden; _}
            :: _ ->
              (* Rollup node forbids this RPC due to ACL. Always fallback in the
                 future. *)
              always_fallback := true
          | _ -> ()) ;
          let*! () = Evm_events_follower_events.fallback e in
          fetch_events_one_by_one state rollup_block_lvl

let apply_events state rollup_block_lvl =
  let open Lwt_result_syntax in
  let* events = fetch_events state rollup_block_lvl in
  Evm_context.apply_evm_events ~finalized_level:rollup_block_lvl events

type error += Evm_events_follower_is_closed

let () =
  register_error_kind
    `Permanent
    ~id:"evm_events_follower_is_closed"
    ~title:"Evm_events_follower_is_closed"
    ~description:
      "Tried to process evm events from the rollup node but the worker is \
       closed"
    Data_encoding.unit
    (function Evm_events_follower_is_closed -> Some () | _ -> None)
    (fun () -> Evm_events_follower_is_closed)

let new_rollup_block worker rollup_level =
  let open Lwt_result_syntax in
  let state = Worker.state worker in
  let add_request level =
    let*! (pushed : bool) =
      Worker.Queue.push_request worker (Apply_evm_events level)
    in
    if not pushed then
      (* this should not happen as we are called from within in the
         worker *)
      tzfail Evm_events_follower_is_closed
    else return_unit
  in
  (* add request for fetching evm events for rollup node block going
     from [from] to [to_] inclusive. *)
  let rec aux ~from ~to_ =
    if from > to_ then
      failwith
        "Internal error: The catchup of evm_event went too far, it should be \
         impossible. Catching up from %ld to %ld."
        from
        to_
    else
      let* () = add_request from in
      if from = to_ then (* we are catching up *) return_unit
      else (aux [@tailcall]) ~from:(Int32.succ from) ~to_
  in
  match state.last_l1_level with
  | Some last_l1_level
    when rollup_level = last_l1_level || rollup_level = Int32.pred last_l1_level
    ->
      let*! () =
        Evm_events_follower_events.rollup_level_is_already_processed
          rollup_level
      in
      return_unit
  | Some last_l1_level when rollup_level < last_l1_level ->
      failwith
        "Internal error: the received block from the rollup node is too old. \
         Received level: %ld, last_l1_level: %ld"
        rollup_level
        last_l1_level
  | None | Some _ ->
      let* from =
        match state.last_l1_level with
        | Some last_l1_level -> return @@ Int32.succ last_l1_level
        | None ->
            let*! () = Evm_store_events.no_l1_latest_level_to_catch_up () in
            return rollup_level
      in
      (* we apply all evm events from [current l1 + 1] to [rollup_head] *)
      let* () = aux ~from ~to_:rollup_level in
      state.last_l1_level <- Some rollup_level ;
      return_unit

module Handlers = struct
  open Evm_events_follower_types

  type self = worker

  let on_request : type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun worker request ->
    match request with
    | Request.New_rollup_node_block rollup_head ->
        protect @@ fun () -> new_rollup_block worker rollup_head
    | Apply_evm_events rollup_lvl ->
        protect @@ fun () -> apply_events (Worker.state worker) rollup_lvl

  type launch_error = error trace

  let on_launch _w ()
      ({rollup_node_endpoint; filter_event; keep_alive; rpc_timeout} :
        Types.parameters) =
    let open Lwt_result_syntax in
    let* last_l1_level = Evm_context.last_known_l1_level () in
    let state : Types.state =
      {
        rollup_node_endpoint;
        filter_event;
        keep_alive;
        rpc_timeout;
        last_l1_level;
      }
    in
    return state

  let on_error : type r request_error.
      worker ->
      Tezos_base.Worker_types.request_status ->
      (r, request_error) Request.t ->
      request_error ->
      [`Continue | `Shutdown] tzresult Lwt.t =
   fun _w _ req errs ->
    let open Lwt_result_syntax in
    match req with
    | Request.New_rollup_node_block _ ->
        let*! () =
          Evm_events_follower_events.worker_request_failed
            (Request.view req)
            errs
        in
        return `Continue
    | Request.Apply_evm_events _ ->
        let*! () =
          Evm_events_follower_events.worker_request_failed
            (Request.view req)
            errs
        in
        return `Continue

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

type error += No_worker

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail e -> Result_syntax.tzfail (error_of_exn e)
    | Lwt.Sleep -> Result_syntax.tzfail No_worker)

let bind_worker f =
  let open Lwt_result_syntax in
  let res = Lazy.force worker in
  match res with
  | Error [No_worker] ->
      (* There is no worker, nothing to do *)
      return_unit
  | Error errs -> fail errs
  | Ok w -> f w

let start parameters =
  let open Lwt_result_syntax in
  let*! () = Evm_events_follower_events.started () in
  let+ worker = Worker.launch table () parameters (module Handlers) in
  Lwt.wakeup worker_waker worker

let shutdown () =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! () = Evm_events_follower_events.shutdown () in
  let*! () = Worker.shutdown w in
  return_unit

let handle_request_error rq =
  let open Lwt_syntax in
  let* rq in
  match rq with
  | Ok res -> return_ok res
  | Error (Worker.Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) -> Lwt.return_error [Evm_events_follower_is_closed]
  | Error (Closed (Some errs)) -> Lwt.return_error errs
  | Error (Any exn) -> Lwt.return_error [Exn exn]

let worker_add_request ~request =
  bind_worker @@ fun w ->
  Worker.Queue.push_request_and_wait w request |> handle_request_error

let new_rollup_block rollup_level =
  worker_add_request ~request:(New_rollup_node_block rollup_level)

let status () =
  let open Result_syntax in
  let+ worker = Lazy.force worker in
  Worker.status worker

let available () =
  match status () with
  | Error _ | Ok (Closed _ | Closing _) -> false
  | Ok (Launching _) | Ok (Running _) -> true
