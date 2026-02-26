(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  signer : Signer.map;
  smart_rollup_address : string;
  rollup_node_endpoint : Uri.t;
  rollup_node_endpoint_timeout : float;
}

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/7453 *)

let attestation_lag = 8

module Types = struct
  type nonrec parameters = parameters

  type state = {
    signer : Signer.map;
    smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
    rollup_node_endpoint : Uri.t;
    rollup_node_endpoint_timeout : float;
  }

  let of_parameters
      ({
         signer;
         smart_rollup_address;
         rollup_node_endpoint;
         rollup_node_endpoint_timeout;
       } :
        parameters) : state tzresult Lwt.t =
    let open Lwt_result_syntax in
    let*? smart_rollup_address =
      Tezos_crypto.Hashed.Smart_rollup_address.of_string smart_rollup_address
    in
    return
      {
        signer;
        smart_rollup_address;
        rollup_node_endpoint;
        rollup_node_endpoint_timeout;
      }
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node_worker"; "signal_publisher"]

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t =
    | New_rollup_node_block : {finalized_level : int32} -> (unit, tztrace) t

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"New_rollup_node_block"
          (obj1 (req "finalized_level" int32))
          (function
            | View (New_rollup_node_block {finalized_level}) ->
                Some finalized_level)
          (fun finalized_level ->
            View (New_rollup_node_block {finalized_level}));
      ]

  let pp _ppf (View _) = ()
end

module Worker = struct
  include Worker.MakeSingle (Name) (Request) (Types)

  (** [is_signal_injection_ready ~finalized_level (injection_id,
      status)] takes as input the current finalized level, and a
      manager operation injected by the rollup node identified by
      [injection_id] and whose injection status is [status].

      It returns [Some (injection_id, slot_id)] if the operation is
      the DAL publication of the slot identified by [slot_id] and is
      old enough for the slot to be importable by the rollup, if
      attested. We rely on the [finalized_level] in order to avoid any
      reorg that could occur during the attestation period.

      In all other cases (the operation is not a DAL publication, it
      failed, or is not yet old enough), the function returns
      [None]. *)
  let is_signal_injection_ready ~finalized_level (injection_id, status) =
    let is_after_attestation_period ~published_level =
      Int32.to_int published_level + attestation_lag
      <= Int32.to_int finalized_level
    in
    match status with
    | Rollup_node_services.Committed
        {
          finalized;
          l1_level = published_level;
          op = Publish_dal_commitment {slot_index; _};
          _;
        }
    | Included
        {
          finalized;
          l1_level = published_level;
          op = Publish_dal_commitment {slot_index; _};
          _;
        }
      when finalized && is_after_attestation_period ~published_level ->
        Some (injection_id, (slot_index, published_level))
    | Unknown | Pending_batch | Pending_injection _ | Injected _ | Included _
    | Committed _ ->
        None

  let new_rollup_block worker finalized_level =
    let open Lwt_result_syntax in
    let state = state worker in
    let* statuses =
      Rollup_services.get_injected_dal_operations_statuses
        ~timeout:state.rollup_node_endpoint_timeout
        ~rollup_node_endpoint:state.rollup_node_endpoint
    in
    let ready_injections =
      List.filter_map (is_signal_injection_ready ~finalized_level) statuses
    in
    let*! () =
      Signals_publisher_events.report_ready_operations
        ~operations:(List.length statuses)
        ~ready_operations:(List.length ready_injections)
    in
    unless (List.is_empty ready_injections) (fun () ->
        let* () =
          List.iter_es
            (fun (injection_id, (_slot_index, published_level)) ->
              let*! () =
                Signals_publisher_events.commited_or_included_injection_id
                  ~injector_op_hash:injection_id
                  ~published_level
              in
              let* () =
                Rollup_services.forget_dal_injection_id
                  ~rollup_node_endpoint:state.rollup_node_endpoint
                  ~timeout:state.rollup_node_endpoint_timeout
                  injection_id
              in
              let*! () =
                Signals_publisher_events.untracking
                  ~injector_op_hash:injection_id
              in
              return_unit)
            ready_injections
        in
        let signals = List.map snd ready_injections in
        let*! head_info = Evm_context.head_info () in
        let* expected_sequencer =
          Durable_storage.sequencer
            ~storage_version:head_info.storage_version
            (fun path ->
              let open Lwt_result_syntax in
              let*! res = Evm_state.inspect head_info.evm_state path in
              return res)
        in
        let*? signer = Signer.get_signer state.signer expected_sequencer in
        let* payload =
          Sequencer_signal.create
            ~signer
            ~smart_rollup_address:state.smart_rollup_address
            ~slot_ids:signals
        in
        let*! () =
          Signals_publisher_events.signal_signed
            ~signals
            ~smart_rollup_address:state.smart_rollup_address
        in
        let () = Metrics.record_signals_sent signals in
        Rollup_services.publish
          ~keep_alive:false
          ~rollup_node_endpoint:state.rollup_node_endpoint
          ~timeout:state.rollup_node_endpoint_timeout
          [payload])
end

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  type self = worker

  let on_request : type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    match request with
    | New_rollup_node_block {finalized_level} ->
        protect @@ fun () -> Worker.new_rollup_block w finalized_level

  type launch_error = error trace

  let on_launch _w () (parameters : Types.parameters) =
    Types.of_parameters parameters

  let on_error (type a b) _w _st (_r : (a, b) Request.t) (_errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    Lwt_result_syntax.return `Continue

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

type error += No_signals_publisher

let worker =
  let open Result_syntax in
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> return worker
    | Lwt.Fail e -> tzfail (error_of_exn e)
    | Lwt.Sleep -> tzfail No_signals_publisher)

let bind_worker f =
  let open Lwt_result_syntax in
  let res = Lazy.force worker in
  match res with
  | Error [No_signals_publisher] ->
      (* There is no worker, nothing to do *)
      return_unit
  | Error errs -> fail errs
  | Ok w -> f w

let worker_add_request ~request =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! (_pushed : bool) = Worker.Queue.push_request w request in
  return_unit

let start ~signer ~smart_rollup_address ~rollup_node_endpoint
    ~rollup_node_endpoint_timeout () =
  let open Lwt_result_syntax in
  let parameters =
    {
      signer;
      smart_rollup_address;
      rollup_node_endpoint;
      rollup_node_endpoint_timeout;
    }
  in
  let* worker = Worker.launch table () parameters (module Handlers) in
  let*! () = Signals_publisher_events.publisher_is_ready () in
  Lwt.wakeup worker_waker worker ;
  return_unit

let shutdown () =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! () = Signals_publisher_events.publisher_shutdown () in
  let*! () = Worker.shutdown w in
  return_unit

let new_rollup_block ~finalized_level =
  worker_add_request ~request:(New_rollup_node_block {finalized_level})
