(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  cctxt : Client_context.wallet;
  smart_rollup_address : string;
  sequencer_key : Client_keys.sk_uri;
  rollup_node_endpoint : Uri.t;
}

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/7453 *)

let attestation_lag = 8

module Types = struct
  type nonrec parameters = parameters

  type state = {
    cctxt : Client_context.wallet;
    smart_rollup_address : string;
    sequencer_key : Client_keys.sk_uri;
    rollup_node_endpoint : Uri.t;
  }

  let of_parameters
      ({cctxt; smart_rollup_address; sequencer_key; rollup_node_endpoint} :
        parameters) : state =
    {cctxt; smart_rollup_address; sequencer_key; rollup_node_endpoint}
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = Signals_publisher_events.section

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

  let new_rollup_block worker finalized_level =
    let open Lwt_result_syntax in
    let state = state worker in
    let* statuses =
      Rollup_services.get_injected_dal_operations_statuses
        ~rollup_node_endpoint:state.rollup_node_endpoint
    in
    statuses
    |> List.iter_es (fun (injection_id, status) ->
           (* [is_after_attestation_period ~published_level] is true if
              the published level is old enough, w.r.t to the last
              finalized level, to not be waiting for attestation
              anymore; this means that the DAL slot is either attested
              (and thus can be imported by the kernel) or unattested
              (in which case importing in the kernel does nothing). We
              rely on the [finalized_level] in order to avoid any
              reorg that could occur during the attestation period. *)
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
               let*! () =
                 Signals_publisher_events.commited_or_included_injection_id
                   ~injector_op_hash:injection_id
                   ~published_level
               in
               let* () =
                 Rollup_services.forget_dal_injection_id
                   ~rollup_node_endpoint:state.rollup_node_endpoint
                   injection_id
               in
               let*! () =
                 Signals_publisher_events.untracking
                   ~injector_op_hash:injection_id
               in
               let* payload =
                 Sequencer_signal.create
                   ~cctxt:state.cctxt
                   ~sequencer_key:state.sequencer_key
                   ~smart_rollup_address:state.smart_rollup_address
                   ~slot_ids:[(slot_index, published_level)]
               in
               let*! () =
                 Signals_publisher_events.signal_signed
                   ~injector_op_hash:injection_id
                   ~published_level
                   ~slot_index
                   ~smart_rollup_address:state.smart_rollup_address
               in
               Rollup_services.publish
                 ~keep_alive:false
                 ~rollup_node_endpoint:state.rollup_node_endpoint
                 [payload]
           | Unknown | Pending_batch | Pending_injection _ | Injected _
           | Included _ | Committed _ ->
               return_unit)
end

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    match request with
    | New_rollup_node_block {finalized_level} ->
        protect @@ fun () -> Worker.new_rollup_block w finalized_level

  type launch_error = error trace

  let on_launch _w () (parameters : Types.parameters) =
    Lwt_result_syntax.return (Types.of_parameters parameters)

  let on_error (type a b) _w _st (_r : (a, b) Request.t) (_errs : b) :
      unit tzresult Lwt.t =
    Lwt_result_syntax.return_unit

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

let start ~cctxt ~smart_rollup_address ~sequencer_key ~rollup_node_endpoint () =
  let open Lwt_result_syntax in
  let parameters =
    {cctxt; smart_rollup_address; sequencer_key; rollup_node_endpoint}
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
