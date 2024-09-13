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
      | Register : {message : string} -> (unit, error trace) t
      | Produce_dal_slots : (unit, error trace) t
      | Set_dal_slot_indices :
          Tezos_dal_node_services.Types.slot_index list
          -> (unit, error trace) t

    type view = View : _ t -> view

    let view req = View req

    let encoding =
      let open Data_encoding in
      union
        [
          case
            (Tag 0)
            ~title:"Register"
            (obj2
               (req "request" (constant "register"))
               (req "slot_content" string))
            (function
              | View (Register {message}) -> Some ((), message) | _ -> None)
            (fun ((), message) -> View (Register {message}));
          case
            (Tag 1)
            ~title:"Produce_dal_slots"
            (obj1 (req "request" (constant "produce_dal_slots")))
            (function View Produce_dal_slots -> Some () | _ -> None)
            (fun () -> View Produce_dal_slots);
          case
            (Tag 2)
            ~title:"Set_dal_slot_indices"
            (obj2
               (req "request" (constant "set_dal_slot_indices"))
               (req "indices" (list uint8)))
            (function
              | View (Set_dal_slot_indices ids) -> Some ((), ids) | _ -> None)
            (fun ((), ids) -> View (Set_dal_slot_indices ids));
        ]

    let pp ppf (View r) =
      match r with
      | Register _ -> Format.fprintf ppf "register slot injection request"
      | Produce_dal_slots -> Format.fprintf ppf "produce DAL slots"
      | Set_dal_slot_indices idx ->
          Format.fprintf
            ppf
            "set slot indices %a"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
               (fun fmt id -> Format.fprintf fmt "%d" id))
            idx
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

  let no_dal_slot_indices_set =
    declare_0
      ~section
      ~name:"no_dal_slot_index_set"
      ~msg:"Cannot inject slot as no DAL slot index is set"
      ~level:Error
      ()

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

module Pending_messages =
  Hash_queue.Make
    (struct
      type t = string

      let equal = String.equal

      let hash = Hashtbl.hash
    end)
    (struct
      type t = unit
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
  pending_messages : Pending_messages.t;
  dal_slot_indices : Tezos_dal_node_services.Types.slot_index Queue.t;
      (* We use a queue here to easily cycle through
         the slots and use different ones from a
         level to another (in case we have less data
         to publish than available slot indices). *)
}

let inject_slot state ~slot_content =
  let open Lwt_result_syntax in
  let {Node_context.dal_cctxt; _} = state.node_ctxt in
  if Queue.is_empty state.dal_slot_indices then
    (* No provided slot indices, no injection *)
    let*! () = Events.(emit no_dal_slot_indices_set) () in
    return_unit
  else
    (* The pop followed by a push is cycle through slot indices. *)
    let slot_index = Queue.pop state.dal_slot_indices in
    Queue.push slot_index state.dal_slot_indices ;
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

let on_register state ~message : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  Pending_messages.replace state.pending_messages message () ;
  return_unit

let on_produce_dal_slots state =
  let open Lwt_result_syntax in
  match Pending_messages.take state.pending_messages with
  | None -> return_unit
  | Some (message, ()) -> inject_slot state ~slot_content:message

let on_set_dal_slot_indices state idx =
  let open Lwt_result_syntax in
  let module SI = Set.Make (Int) in
  (* remove duplicates *)
  let idx = SI.of_list idx in
  let number_of_slots =
    (Reference.get state.node_ctxt.current_protocol).constants.dal
      .number_of_slots
  in
  let* () =
    SI.iter_es
      (fun slot_index ->
        fail_unless (slot_index >= 0 && slot_index < number_of_slots)
        @@ error_of_fmt "Slot index %d out of range" slot_index)
      idx
  in
  Queue.clear state.dal_slot_indices ;
  SI.iter (fun id -> Queue.push id state.dal_slot_indices) idx ;
  return_unit

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
    pending_messages = Pending_messages.create max_retained_slots_info;
    dal_slot_indices = Queue.create ();
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
    | Request.Register {message} ->
        protect @@ fun () -> on_register state ~message
    | Request.Produce_dal_slots ->
        protect @@ fun () -> on_produce_dal_slots state
    | Request.Set_dal_slot_indices idx ->
        protect @@ fun () -> on_set_dal_slot_indices state idx

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
    | Request.Set_dal_slot_indices _ ->
        let*! () = Events.(emit request_failed) (Request.view r, st, errs) in
        return_unit

  let on_completion _w r _ st =
    match Request.view r with
    | Request.View (Register _ | Produce_dal_slots | Set_dal_slot_indices _) ->
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
  | Custom ops -> purposes_matches_mode (Custom ops) [Batching]

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

(* This is a DAL injection worker for a single scoru *)
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

let register_dal_message w message =
  Worker.Queue.push_request_and_wait w (Request.Register {message})
  |> handle_request_error

let register_dal_messages ~messages =
  let open Lwt_result_syntax in
  let* w = lwt_map_error TzTrace.make (Lwt.return (Lazy.force worker)) in
  List.iter_es (register_dal_message w) messages

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

let set_dal_slot_indices idx =
  let open Lwt_result_syntax in
  let* w = lwt_map_error TzTrace.make (Lwt.return (Lazy.force worker)) in
  Worker.Queue.push_request_and_wait w Request.(Set_dal_slot_indices idx)
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
