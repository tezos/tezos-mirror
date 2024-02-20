(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Baking_state

module Events = struct
  include Baking_events.Forge_worker
end

module Delegate_signing_queue = struct
  type t = {
    delegate : consensus_key_and_delegate;
    task_stream : (unit -> unit Lwt.t) Lwt_stream.t;
    push : (unit -> unit Lwt.t) option -> unit;
    worker : unit Lwt.t;
  }

  let start_delegate_worker_queue task_stream =
    let open Lwt_syntax in
    let rec loop () =
      let* task = Lwt_stream.get task_stream in
      match task with
      | None -> (* End of stream *) return_unit
      | Some task ->
          let* () = task () in
          loop ()
    in
    loop ()

  let create delegate =
    let task_stream, push = Lwt_stream.create () in
    let worker = start_delegate_worker_queue task_stream in
    {delegate; task_stream; push; worker}

  let cancel_pending_tasks state = Lwt_stream.junk_old state.task_stream

  let wait_all_tasks_and_close state =
    state.push None ;
    state.worker

  let cancel_all_tasks_and_close state =
    let open Lwt_syntax in
    let* () = cancel_pending_tasks state in
    wait_all_tasks_and_close state

  let push_task ~(on_error : tztrace -> unit Lwt.t)
      (f : unit -> unit tzresult Lwt.t) state =
    let open Lwt_result_syntax in
    let task () =
      let*! r =
        protect
          ~on_error:(fun trace ->
            let*! () = on_error trace in
            return_unit)
          (fun () -> f ())
      in
      match r with Error _err -> assert false | Ok () -> Lwt.return_unit
    in
    state.push (Some task)
end

type worker = {
  push_task : forge_request option -> unit;
  push_event : forge_event option -> unit;
  event_stream : forge_event Lwt_stream.t;
  delegate_signing_queues :
    Delegate_signing_queue.t Signature.Public_key_hash.Table.t;
}

type t = worker

let push_request state request = state.push_task (Some request)

let get_event_stream state = state.event_stream

let cancel_all_pending_tasks {delegate_signing_queues; _} =
  Lwt.dont_wait
    (fun () ->
      Signature.Public_key_hash.Table.iter_p
        (fun _ queue -> Delegate_signing_queue.cancel_pending_tasks queue)
        delegate_signing_queues)
    (fun _exn -> ())

let shutdown state =
  let open Lwt_syntax in
  let* () =
    Signature.Public_key_hash.Table.iter_p
      (fun _ queue -> Delegate_signing_queue.cancel_all_tasks_and_close queue)
      state.delegate_signing_queues
  in
  state.push_task None ;
  return_unit

let get_or_create_queue worker delegate =
  match
    Signature.Public_key_hash.Table.find_opt
      worker.delegate_signing_queues
      (fst delegate).public_key_hash
  with
  | None ->
      let queue = Delegate_signing_queue.create delegate in
      Signature.Public_key_hash.Table.add
        worker.delegate_signing_queues
        (fst delegate).public_key_hash
        queue ;
      queue
  | Some queue -> queue

let handle_forge_block worker baking_state (block_to_bake : block_to_bake) =
  let open Lwt_result_syntax in
  let task () =
    let* prepared_block =
      Baking_actions.prepare_block baking_state block_to_bake
    in
    worker.push_event (Some (Block_ready prepared_block)) ;
    return_unit
  in
  let queue = get_or_create_queue worker block_to_bake.delegate in
  Delegate_signing_queue.push_task
    ~on_error:(fun _err -> Lwt.return_unit)
    task
    queue

let start (baking_state : Baking_state.global_state) =
  let open Lwt_result_syntax in
  let task_stream, push_task = Lwt_stream.create () in
  let event_stream, push_event = Lwt_stream.create () in
  let delegate_signing_queues = Signature.Public_key_hash.Table.create 13 in
  let state : worker =
    {push_task; push_event; event_stream; delegate_signing_queues}
  in
  let rec worker_loop () =
    let*! (forge_request_opt : forge_request option) =
      Lwt_stream.get task_stream
    in
    let process_request = function
      | Forge_and_sign_block block_to_bake ->
          handle_forge_block state baking_state block_to_bake ;
          return_unit
    in
    match forge_request_opt with
    | None -> (* Shutdown called *) return_unit
    | Some request ->
        let*! result = process_request request in
        let*! () =
          match result with
          | Ok () -> Lwt.return_unit
          | Error errs ->
              let*! () =
                Events.(emit error_while_processing_forge_request errs)
              in
              Lwt.return_unit
        in
        worker_loop ()
  in
  Lwt.dont_wait
    (fun () ->
      Lwt.finalize
        (fun () ->
          let*! _r = worker_loop () in
          Lwt.return_unit)
        (fun () ->
          let () = Lwt.dont_wait (fun () -> shutdown state) (fun _exn -> ()) in
          Lwt.return_unit))
    (fun _exn -> ()) ;
  state
