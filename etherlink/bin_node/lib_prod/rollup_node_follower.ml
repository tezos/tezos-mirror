(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {rollup_node_endpoint : Uri.t}

module Types = struct
  type state = Uri.t

  type nonrec parameters = parameters
end

module Name = struct
  (* We only have a single rollup node follower in the evm node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node"; "prod"; "l2_block"; "follower"; "worker"]

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t = Unit : (unit, tztrace) t

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding : view Data_encoding.t =
    let open Data_encoding in
    conv (fun (View _) -> ()) (fun () -> View Unit) unit

  let pp _ppf (View _) = ()
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun _w request ->
    match request with
    | Request.Unit -> protect @@ fun () -> Lwt_result_syntax.return_unit

  type launch_error = error trace

  let on_launch _w () ({rollup_node_endpoint; _} : Types.parameters) =
    let state = rollup_node_endpoint in
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

type error += No_l2_block_follower

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail e -> Error (TzTrace.make @@ error_of_exn e)
    | Lwt.Sleep -> Error (TzTrace.make No_l2_block_follower))

let read_from_rollup_node path level rollup_node_endpoint =
  let open Rollup_services in
  call_service
    ~base:rollup_node_endpoint
    durable_state_value
    ((), Block_id.Level level)
    {key = path}
    ()

let advertize_blueprints_publisher rollup_node_endpoint finalized_level =
  let open Lwt_syntax in
  let* finalized_current_number =
    read_from_rollup_node
      Durable_storage_path.Block.current_number
      finalized_level
      rollup_node_endpoint
  in
  match finalized_current_number with
  | Ok (Some bytes) ->
      let (Qty evm_block_number) = Ethereum_types.decode_number bytes in
      let* _ = Blueprints_publisher.new_l2_head evm_block_number in
      return_unit
  | _ -> return_unit

let process_new_block ~rollup_node_endpoint block =
  let open Lwt_syntax in
  let finalized_level = Sc_rollup_block.(Int32.(sub block.header.level 2l)) in
  let* _ = Delayed_inbox.new_rollup_block finalized_level in
  let* _ = Evm_events_follower.new_rollup_block finalized_level in
  let* () =
    advertize_blueprints_publisher rollup_node_endpoint finalized_level
  in
  return_unit

let rec process_rollup_node_stream ~stream worker =
  let open Lwt_syntax in
  let* new_head = Lwt_stream.get stream in
  match new_head with
  | None ->
      let* () = Rollup_node_follower_events.connection_lost () in
      Worker.shutdown worker
  | Some block ->
      let* () =
        Rollup_node_follower_events.new_block
          Sc_rollup_block.(block.header.level)
      in
      let* () =
        process_new_block ~rollup_node_endpoint:(Worker.state worker) block
      in
      process_rollup_node_stream ~stream worker

let start ({rollup_node_endpoint} as parameters) =
  let open Lwt_result_syntax in
  let*! () = Rollup_node_follower_events.started () in
  let+ worker = Worker.launch table () parameters (module Handlers) in
  let () =
    Lwt.dont_wait
      (fun () ->
        let*! stream =
          Rollup_services.make_streamed_call ~rollup_node_endpoint
        in
        process_rollup_node_stream ~stream worker)
      (fun _ -> ())
  in
  Lwt.wakeup worker_waker worker

let shutdown () =
  let open Lwt_syntax in
  let w = Lazy.force worker in
  match w with
  | Error _ -> Lwt.return_unit
  | Ok w ->
      let* () = Rollup_node_follower_events.shutdown () in
      Worker.shutdown w
