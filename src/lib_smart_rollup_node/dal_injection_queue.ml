(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori     <contact@functori.com>          *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)
module Name = struct
  (* We only have a single batcher in the node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["dal-injection"; "worker"]

  let pp _ _ = ()

  let equal () () = true
end

module Dal_worker_types = struct
  module Request = struct
    type ('a, 'b) t =
      | Register : {
          slot_content : string;
          slot_index : int;
        }
          -> (unit, error trace) t
      | Produce_dal_slots : (unit, error trace) t

    type view = View : _ t -> view

    let view req = View req

    let encoding =
      let open Data_encoding in
      union
        [
          case
            (Tag 0)
            ~title:"Register"
            (obj3
               (req "request" (constant "register"))
               (req "slot_content" string)
               (req "slot_index" int31))
            (function
              | View (Register {slot_content; slot_index}) ->
                  Some ((), slot_content, slot_index)
              | _ -> None)
            (fun ((), slot_content, slot_index) ->
              View (Register {slot_content; slot_index}));
          case
            (Tag 1)
            ~title:"Produce_dal_slots"
            (obj1 (req "request" (constant "produce_dal_slots")))
            (function View Produce_dal_slots -> Some () | _ -> None)
            (fun () -> View Produce_dal_slots);
        ]

    let pp ppf (View r) =
      match r with
      | Register {slot_content = _; slot_index} ->
          Format.fprintf
            ppf
            "register slot injection request for index %d"
            slot_index
      | Produce_dal_slots -> Format.fprintf ppf "produce DAL slots"
  end
end

open Dal_worker_types

module Events = struct
  include Internal_event.Simple

  let section = "smart_rollup_node" :: Name.base

  let injected =
    declare_3
      ~section
      ~name:"injected"
      ~msg:
        "Injected a DAL slot {slot_commitment} at index {slot_index} with hash \
         {operation_hash}"
      ~level:Info
      ~pp1:Tezos_crypto_dal.Cryptobox.Commitment.pp
      ~pp3:Injector_sigs.Id.pp
      ("slot_commitment", Tezos_crypto_dal.Cryptobox.Commitment.encoding)
      ("slot_index", Data_encoding.uint8)
      ("operation_hash", Injector_sigs.Id.encoding)

  let request_failed =
    declare_3
      ~section
      ~name:"request_failed"
      ~msg:"Request {request} failed ({worker_status}): {errors}"
      ~level:Warning
      ("request", Request.encoding)
      ~pp1:Request.pp
      ("worker_status", Worker_types.request_status_encoding)
      ~pp2:Worker_types.pp_status
      ("errors", Error_monad.trace_encoding)
      ~pp3:Error_monad.pp_print_trace

  let request_completed =
    declare_2
      ~section
      ~name:"request_completed"
      ~msg:"{request} {worker_status}"
      ~level:Debug
      ("request", Request.encoding)
      ("worker_status", Worker_types.request_status_encoding)
      ~pp1:Request.pp
      ~pp2:Worker_types.pp_status
end

module Pending_chunks =
  Hash_queue.Make
    (struct
      type t = string (* slot content or chunk *)

      let equal = String.equal

      let hash = Hashtbl.hash
    end)
    (struct
      type t = int
      (* This is the slot index on which we inject. It is removed
         in the next MR. *)
    end)

module Recent_dal_injections =
  Hash_queue.Make
    (Hashed.Injector_operations_hash)
    (struct
      type t = unit
    end)

type state = {
  node_ctxt : Node_context.ro;
  recent_dal_injections : Recent_dal_injections.t;
  pending_chunks : Pending_chunks.t;
}

let inject_slot state ~slot_content ~slot_index =
  let open Lwt_result_syntax in
  let {Node_context.dal_cctxt; _} = state.node_ctxt in
  let* commitment, operation =
    match dal_cctxt with
    | Some dal_cctxt ->
        let* commitment, commitment_proof =
          Dal_node_client.post_slot dal_cctxt ~slot_index slot_content
        in
        return
          ( commitment,
            L1_operation.Publish_dal_commitment
              {slot_index; commitment; commitment_proof} )
    | None ->
        (* This should not be reachable, as we tested that some [dal_cctxt] is set
           at startup before launching the worker. *)
        assert false
  in
  let* l1_hash =
    Injector.check_and_add_pending_operation
      state.node_ctxt.config.mode
      operation
  in
  let* l1_hash =
    match l1_hash with
    | Some l1_hash -> return l1_hash
    | None ->
        let op = Injector.Inj_operation.make operation in
        return op.id
  in
  let*! () = Events.(emit injected) (commitment, slot_index, l1_hash) in
  Recent_dal_injections.replace state.recent_dal_injections l1_hash () ;
  return_unit

let on_register state ~slot_content ~slot_index : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let number_of_slots =
    (Reference.get state.node_ctxt.current_protocol).constants.dal
      .number_of_slots
  in
  let* () =
    fail_unless (slot_index >= 0 && slot_index < number_of_slots)
    @@ error_of_fmt "Slot index %d out of range" slot_index
  in
  Pending_chunks.replace state.pending_chunks slot_content slot_index ;
  return_unit

let on_produce_dal_slots state =
  let open Lwt_result_syntax in
  match Pending_chunks.take state.pending_chunks with
  | None -> return_unit
  | Some (slot_content, slot_index) ->
      inject_slot state ~slot_content ~slot_index

let init_dal_worker_state node_ctxt =
  let dal_constants =
    let open Node_context in
    (Reference.get node_ctxt.current_protocol).constants.dal
  in
  (* We initialize a bounded cache for [known_slots] large enough to contain
     [number_of_slots] entries during [attestation_lag] * 4 (sliding) levels. *)
  let max_retained_slots_info =
    dal_constants.number_of_slots * dal_constants.attestation_lag * 4
  in
  {
    node_ctxt;
    recent_dal_injections = Recent_dal_injections.create max_retained_slots_info;
    pending_chunks = Pending_chunks.create max_retained_slots_info;
  }

module Types = struct
  type nonrec state = state

  type parameters = {node_ctxt : Node_context.ro}
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Register {slot_content; slot_index} ->
        protect @@ fun () -> on_register state ~slot_content ~slot_index
    | Request.Produce_dal_slots ->
        protect @@ fun () -> on_produce_dal_slots state

  type launch_error = error trace

  let on_launch _w () Types.{node_ctxt} =
    let open Lwt_result_syntax in
    let state = init_dal_worker_state node_ctxt in
    return state

  let on_error (type a b) _w st (r : (a, b) Request.t) (errs : b) :
      unit tzresult Lwt.t =
    let open Lwt_result_syntax in
    match r with
    | Request.Register _ ->
        let*! () = Events.(emit request_failed) (Request.view r, st, errs) in
        return_unit
    | Request.Produce_dal_slots ->
        let*! () = Events.(emit request_failed) (Request.view r, st, errs) in
        return_unit

  let on_completion _w r _ st =
    match Request.view r with
    | Request.View (Register _ | Produce_dal_slots) ->
        Events.(emit request_completed) (Request.view r, st)

  let on_no_request _ = Lwt.return_unit

  let on_close _w = Lwt.return_unit
end

let table = Worker.create_table Queue

let (worker_promise : Worker.infinite Worker.queue Worker.t Lwt.t), worker_waker
    =
  Lwt.task ()

let start node_ctxt =
  let open Lwt_result_syntax in
  let node_ctxt = Node_context.readonly node_ctxt in
  let+ worker = Worker.launch table () {node_ctxt} (module Handlers) in
  Lwt.wakeup worker_waker worker

let start_in_mode mode =
  let open Configuration in
  match mode with
  | Batcher | Operator -> true
  | Observer | Accuser | Bailout | Maintenance -> false
  | Custom ops -> purpose_matches_mode (Custom ops) Batching

let init (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  match Lwt.state worker_promise with
  | Lwt.Return _ ->
      (* Worker already started, nothing to do. *)
      return_unit
  | Lwt.Fail exn ->
      (* Worker crashed, not recoverable. *)
      fail [Rollup_node_errors.No_dal_injector; Exn exn]
  | Lwt.Sleep ->
      (* Never started, start it. *)
      if
        Option.is_some node_ctxt.Node_context.dal_cctxt
        && start_in_mode node_ctxt.config.mode
      then start node_ctxt
      else return_unit

(* This is a DAL inection worker for a single scoru *)
let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail exn -> Error (Error_monad.error_of_exn exn)
    | Lwt.Sleep -> Error Rollup_node_errors.No_dal_injector)

let handle_request_error rq =
  let open Lwt_syntax in
  let* rq in
  match rq with
  | Ok res -> return_ok res
  | Error (Worker.Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) -> Lwt.return_error [Worker_types.Terminated]
  | Error (Closed (Some errs)) -> Lwt.return_error errs
  | Error (Any exn) -> Lwt.return_error [Exn exn]

let register_dal_slot ~slot_content ~slot_index =
  let open Lwt_result_syntax in
  let* w = lwt_map_error TzTrace.make (Lwt.return (Lazy.force worker)) in
  Worker.Queue.push_request_and_wait
    w
    (Request.Register {slot_content; slot_index})
  |> handle_request_error

let produce_dal_slots () =
  let open Lwt_result_syntax in
  match Lazy.force worker with
  | Error Rollup_node_errors.No_dal_injector ->
      (* There is no batcher, nothing to do *)
      return_unit
  | Error e -> tzfail e
  | Ok w ->
      Worker.Queue.push_request_and_wait w Request.Produce_dal_slots
      |> handle_request_error

let get_injection_ids () =
  let open Result_syntax in
  let+ w = Result.map_error TzTrace.make (Lazy.force worker) in
  let state = Worker.state w in
  Recent_dal_injections.keys state.recent_dal_injections

let forget_injection_id id =
  let open Result_syntax in
  let+ w = Result.map_error TzTrace.make (Lazy.force worker) in
  let state = Worker.state w in
  Recent_dal_injections.remove state.recent_dal_injections id
