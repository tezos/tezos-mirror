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
  max_blueprints_lag : int;
}

module Dal_injected_slots_tracker_queue = struct
  type value = {
    level : Z.t;
    slot_index : Tezos_dal_node_services.Types.slot_index;
  }

  include
    Hash_queue.Make
      (Tezos_crypto.Hashed.Injector_operations_hash)
      (struct
        type t = value
      end)
end

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/7453 *)

let attestation_lag = 8

let finalisation_delay = 2

let injection_lag = 3

let number_of_slots = 32

module Types = struct
  type nonrec parameters = parameters

  type state = {
    cctxt : Client_context.wallet;
    smart_rollup_address : string;
    sequencer_key : Client_keys.sk_uri;
    rollup_node_endpoint : Uri.t;
    dal_injected_slots_tracker_queue : Dal_injected_slots_tracker_queue.t;
        (** We remember in this bounded Hash_queue the hashes of DAL
            slot publications that we have asked the rollup node to
            inject. They are associated with their corresponding slot
            index and the blueprint's level. This lets us track the
            inclusion status of these operation which is needed to
            signal to the kernel when it's time to import the
            slots. *)
  }

  let of_parameters
      {
        cctxt;
        smart_rollup_address;
        sequencer_key;
        rollup_node_endpoint;
        max_blueprints_lag;
      } =
    {
      cctxt;
      smart_rollup_address;
      sequencer_key;
      rollup_node_endpoint;
      dal_injected_slots_tracker_queue =
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7386

           Decide of a suitable value of the cache below. *)
        Dal_injected_slots_tracker_queue.create
          ((injection_lag + attestation_lag + finalisation_delay)
          * number_of_slots * max_blueprints_lag);
      (* A size to track DAL slot publications in the structure for a few L1
         levels. *)
    }
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
    | Track : {
        injection_id : Tezos_crypto.Hashed.Injector_operations_hash.t;
        level : Z.t;
        slot_index : Tezos_dal_node_services.Types.slot_index;
      }
        -> (unit, tztrace) t

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Track"
          (obj3
             (req
                "injection_id"
                Tezos_crypto.Hashed.Injector_operations_hash.encoding)
             (req "level" z)
             (req "slot_index" int8))
          (function
            | View (Track {injection_id; level; slot_index}) ->
                Some (injection_id, level, slot_index)
            | View _ -> .)
          (fun (injection_id, level, slot_index) ->
            View (Track {injection_id; level; slot_index}));
      ]

  let pp _ppf (View _) = ()
end

module Worker = struct
  include Worker.MakeSingle (Name) (Request) (Types)

  let track _worker ~injection_id:_ ~level:_ ~slot_index:_ =
    let open Lwt_result_syntax in
    return_unit
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
    | Track {injection_id; level; slot_index} ->
        protect @@ fun () -> Worker.track w ~injection_id ~level ~slot_index

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

let start ~cctxt ~smart_rollup_address ~sequencer_key ~rollup_node_endpoint
    ~max_blueprints_lag () =
  let open Lwt_result_syntax in
  let parameters =
    {
      cctxt;
      smart_rollup_address;
      sequencer_key;
      rollup_node_endpoint;
      max_blueprints_lag;
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

let track ~injection_id ~level ~slot_index =
  worker_add_request ~request:(Track {injection_id; level; slot_index})
