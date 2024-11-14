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
}

module StringSet = Set.Make (String)

module Types = struct
  type state = parameters

  type nonrec parameters = parameters
end

module Name = struct
  (* We only have a single events follower in the evm node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node"; "dev"; "events_follower"; "worker"]

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

let read_from_rollup_node ~keep_alive path level rollup_node_endpoint =
  let open Rollup_services in
  call_service
    ~keep_alive
    ~base:rollup_node_endpoint
    durable_state_value
    ((), Block_id.Level level)
    {key = path}
    ()

let fetch_event ({rollup_node_endpoint; keep_alive; _} : Types.state)
    rollup_block_lvl event_index =
  let open Lwt_result_syntax in
  let path = Durable_storage_path.Evm_events.nth_event event_index in
  let* bytes_opt =
    read_from_rollup_node ~keep_alive path rollup_block_lvl rollup_node_endpoint
  in
  let event_opt = Option.bind bytes_opt Evm_events.of_bytes in
  let*! () =
    if Option.is_none event_opt then
      Evm_events_follower_events.unreadable_event (event_index, rollup_block_lvl)
    else Lwt.return_unit
  in
  return event_opt

let on_new_head
    ({rollup_node_endpoint; keep_alive; filter_event} as state : Types.state)
    rollup_block_lvl =
  let open Lwt_result_syntax in
  let* last_known_l1_block = Evm_context.last_known_l1_level () in
  let needs_processing =
    match last_known_l1_block with
    | None -> `Process
    | Some last_known_l1_block ->
        let level_expected = Int32.succ last_known_l1_block in
        if Int32.equal rollup_block_lvl level_expected then `Process
        else if Compare.Int32.(rollup_block_lvl < level_expected) then `Ignore
        else
          `Exit
            (Node_error.Out_of_sync
               {level_received = rollup_block_lvl; level_expected})
  in

  match needs_processing with
  | `Ignore -> return_unit
  | `Process -> (
      let* nb_of_events_bytes =
        read_from_rollup_node
          ~keep_alive
          Durable_storage_path.Evm_events.length
          rollup_block_lvl
          rollup_node_endpoint
      in
      match nb_of_events_bytes with
      | None -> Evm_context.new_last_known_l1_level rollup_block_lvl
      | Some nb_of_events_bytes ->
          let (Qty nb_of_events) =
            Ethereum_types.decode_number_le nb_of_events_bytes
          in
          let nb_of_events = Z.to_int nb_of_events in
          let* events =
            List.init_ep
              ~when_negative_length:
                (error_of_fmt
                   "Internal error: the rollup node advertised a negative \
                    length for the events stream")
              nb_of_events
              (fetch_event state rollup_block_lvl)
          in
          let events =
            List.filter_map
              (function
                | Some event when filter_event event -> Some event | _ -> None)
              events
          in
          Evm_context.apply_evm_events ~finalized_level:rollup_block_lvl events)
  | `Exit err -> fail [err]

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
        let* () = on_new_head (Worker.state worker) rollup_block_lvl in
        return_unit

  type launch_error = error trace

  let on_launch _w () (parameters : Types.parameters) =
    let state = parameters in
    Lwt_result_syntax.return state

  let on_error :
      type r request_error.
      worker ->
      Tezos_base.Worker_types.request_status ->
      (r, request_error) Request.t ->
      request_error ->
      unit tzresult Lwt.t =
   fun _w _ req errs ->
    let open Lwt_result_syntax in
    match (req, errs) with
    | ( Request.New_rollup_node_block _,
        Node_error.Out_of_sync {level_expected; level_received} :: _ ) ->
        let*! () =
          Evm_events_follower_events.out_of_sync
            ~expected:level_expected
            ~received:level_received
        in
        Lwt_exit.exit_and_raise Node_error.exit_code_when_out_of_sync
    | _ -> return_unit

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

let worker_add_request ~request =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! (_pushed : bool) = Worker.Queue.push_request w request in
  return_unit

let new_rollup_block rollup_level =
  worker_add_request ~request:(New_rollup_node_block rollup_level)
