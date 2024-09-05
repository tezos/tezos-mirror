(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  rollup_node_endpoint : Uri.t;
  latest_level_seen : Z.t;
  config : Configuration.blueprints_publisher_config;
  keep_alive : bool;
}

type state = {
  rollup_node_endpoint : Uri.t;
  max_blueprints_lag : Z.t;
  max_blueprints_ahead : Z.t;
  max_blueprints_catchup : Z.t;
  catchup_cooldown : int;
  keep_alive : bool;
  mutable latest_level_confirmed : Z.t;
      (** The current head of the EVM chain as seen by the rollup node *)
  mutable latest_level_seen : Z.t;
      (** The level of the latest blueprint the sequencer tried to inject back
          to layer 1 *)
  mutable cooldown : int;
      (** Do not try to catch-up if [cooldown] is not equal to 0 *)
  enable_dal : bool;
  dal_slots : int list option;
  mutable dal_last_used : Z.t;
}

module Types = struct
  type nonrec state = state

  type nonrec parameters = parameters
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = Blueprint_events.section

  let pp _fmt () = ()

  let equal () () = true
end

module Worker = struct
  include Worker.MakeSingle (Name) (Blueprints_publisher_types.Request) (Types)

  let rollup_node_endpoint worker = (state worker).rollup_node_endpoint

  let latest_level_seen worker = (state worker).latest_level_seen

  let latest_level_confirmed worker = (state worker).latest_level_confirmed

  let witness_level worker level =
    (* [witness_level] is called in [publish], which is used both when
       catching up and when publishing new blueprints. We only want to update
       the field on the latter case. *)
    if Z.Compare.(latest_level_seen worker < level) then
      (state worker).latest_level_seen <- level

  let set_latest_level_confirmed worker level =
    (state worker).latest_level_confirmed <- level

  let max_blueprints_lag worker = (state worker).max_blueprints_lag

  let max_level_ahead worker = (state worker).max_blueprints_ahead

  let max_blueprints_catchup worker = (state worker).max_blueprints_catchup

  type lag = No_lag | Needs_republish | Needs_lock

  let rollup_is_lagging_behind worker =
    let missing_levels =
      Z.sub (latest_level_seen worker) (latest_level_confirmed worker)
    in
    if Z.Compare.(missing_levels > max_level_ahead worker) then Needs_lock
    else if Z.(Compare.(missing_levels > max_blueprints_lag worker)) then
      Needs_republish
    else No_lag

  let set_cooldown worker cooldown = (state worker).cooldown <- cooldown

  let catchup_cooldown worker = (state worker).catchup_cooldown

  let keep_alive worker = (state worker).keep_alive

  let current_cooldown worker = (state worker).cooldown

  let on_cooldown worker = 0 < current_cooldown worker

  let decrement_cooldown worker =
    let current = current_cooldown worker in
    if on_cooldown worker then set_cooldown worker (current - 1) else ()

  let publish self chunks level ~use_dal_if_enabled =
    let open Lwt_result_syntax in
    let rollup_node_endpoint = rollup_node_endpoint self in
    (* We do not check if we succeed or not: this will be done when new L2
       heads come from the rollup node. *)
    witness_level self level ;
    let*! res =
      (* We do not check if we succeed or not: this will be done when new L2
         heads come from the rollup node. *)
      match (state self, chunks) with
      | ( {
            enable_dal = true;
            dal_slots = Some (slot_index :: _);
            dal_last_used;
            _;
          },
          Blueprints_publisher_types.Request.Blueprint
            {chunks = [chunk]; inbox_payload = _} )
        when use_dal_if_enabled && dal_last_used < level ->
          (state self).dal_last_used <- level ;
          let payload = Sequencer_blueprint.create_dal_payload chunk in
          let*! () = Blueprint_events.blueprint_injected_on_DAL level in
          let () =
            Prometheus.Counter.inc_one Metrics.BlueprintChunkSent.on_dal
          in
          Rollup_services.publish_on_dal
            ~rollup_node_endpoint
            ~slot_index
            payload
      | _ ->
          let payload =
            match chunks with
            | Blueprints_publisher_types.Request.Blueprint
                {chunks = _; inbox_payload} ->
                inbox_payload
            | Inbox payload -> payload
          in
          let*! () = Blueprint_events.blueprint_injected_on_inbox level in
          let () =
            Prometheus.Counter.inc_one Metrics.BlueprintChunkSent.on_inbox
          in
          Rollup_services.publish
            ~keep_alive:false
            ~rollup_node_endpoint
            payload
    in
    let*! () =
      match res with
      | Ok _ -> Blueprint_events.blueprint_injected level
      | Error trace ->
          (* We have failed to inject the blueprint. This is probably
             the sign that the rollup node is down. It will be injected again
             once the rollup node lag increases to [max_blueprints_lag]. *)
          Blueprint_events.blueprint_injection_failed level trace
    in
    match rollup_is_lagging_behind self with
    | No_lag | Needs_republish -> return_unit
    | Needs_lock -> Tx_pool.lock_transactions ()

  let catch_up worker =
    let open Lwt_result_syntax in
    let lower_bound = Z.succ (latest_level_confirmed worker) in
    (* We limit the maximum number of blueprints we send at once *)
    let upper_bound =
      Z.(
        min
          (add (latest_level_confirmed worker) (max_blueprints_catchup worker))
          (latest_level_seen worker))
    in

    let*! () = Blueprint_events.catching_up lower_bound upper_bound in

    let* blueprints =
      Evm_context.blueprints_range (Qty lower_bound) (Qty upper_bound)
    in

    let expected_count = Z.(to_int (sub upper_bound lower_bound)) + 1 in
    let actual_count = List.length blueprints in
    let* () =
      when_ (actual_count < expected_count) (fun () ->
          let*! () =
            Blueprint_events.missing_blueprints
              (expected_count - actual_count)
              (Qty lower_bound)
              (Qty upper_bound)
          in
          return_unit)
    in

    let* () =
      List.iter_es
        (fun (Ethereum_types.Qty current, payload) ->
          publish
            worker
            (Blueprints_publisher_types.Request.Inbox payload)
            current
            ~use_dal_if_enabled:false)
        blueprints
    in

    (* We give ourselves a cooldown window Tezos blocks to inject everything *)
    set_cooldown worker (catchup_cooldown worker) ;
    return_unit

  let retrieve_and_set_latest_level_confirmed self rollup_block_lvl =
    let open Lwt_result_syntax in
    let open Rollup_services in
    let* finalized_current_number =
      call_service
        ~keep_alive:(keep_alive self)
        ~base:(rollup_node_endpoint self)
        durable_state_value
        ((), Block_id.Level rollup_block_lvl)
        {key = Durable_storage_path.Block.current_number}
        ()
    in
    match finalized_current_number with
    | Some bytes ->
        let (Qty evm_block_number) = Ethereum_types.decode_number_le bytes in
        set_latest_level_confirmed self evm_block_number ;
        return_unit
    | None -> return_unit
end

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  open Blueprints_publisher_types

  type self = worker

  type launch_error = error trace

  let on_launch _self ()
      ({
         rollup_node_endpoint;
         config =
           {
             max_blueprints_lag;
             max_blueprints_ahead;
             max_blueprints_catchup;
             catchup_cooldown;
             dal_slots;
           };
         latest_level_seen;
         keep_alive;
       } :
        Types.parameters) =
    let open Lwt_result_syntax in
    let* () =
      match dal_slots with
      | None -> return_unit
      | Some dal_slots ->
          Rollup_services.set_dal_slot_indices
            ~rollup_node_endpoint
            ~slot_indices:dal_slots
    in
    return
      {
        latest_level_confirmed =
          (* Will be set at the correct value once the next L2 block is
             received from the rollup node *)
          Z.zero;
        latest_level_seen;
        cooldown = 0;
        rollup_node_endpoint;
        max_blueprints_lag = Z.of_int max_blueprints_lag;
        max_blueprints_ahead = Z.of_int max_blueprints_ahead;
        max_blueprints_catchup = Z.of_int max_blueprints_catchup;
        catchup_cooldown;
        keep_alive;
        enable_dal = Option.is_some dal_slots;
        dal_slots;
        dal_last_used = Z.zero;
      }

  let on_request :
      type r request_error.
      self -> (r, request_error) Request.t -> (r, request_error) result Lwt.t =
   fun self request ->
    let open Lwt_result_syntax in
    match request with
    | Publish {level; payload} ->
        let* () = Worker.publish self payload level ~use_dal_if_enabled:true in
        return_unit
    | New_rollup_node_block rollup_block_lvl -> (
        let* () =
          Worker.retrieve_and_set_latest_level_confirmed self rollup_block_lvl
        in
        match Worker.rollup_is_lagging_behind self with
        | (Needs_republish | Needs_lock) when not (Worker.on_cooldown self) ->
            (* The worker needs to republish, it's not in cooldown. *)
            Worker.catch_up self
        | Needs_lock ->
            (* If the worker still needs to stop, we idle and wait for the cooldown .*)
            Worker.decrement_cooldown self ;
            return_unit
        | No_lag | Needs_republish ->
            Worker.decrement_cooldown self ;
            (* If there is no lag or the worker just needs to republish we
               unlock the transaction pool in case it was locked. *)
            Tx_pool.unlock_transactions ())

  let on_completion (type a err) _self (_r : (a, err) Request.t) (_res : a) _st
      =
    Lwt_syntax.return_unit

  let on_no_request _self = Lwt.return_unit

  let on_close _self = Lwt.return_unit

  let on_error (type a b) _self _st (_r : (a, b) Request.t) (_errs : b) :
      unit tzresult Lwt.t =
    Lwt_result_syntax.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

let start ~rollup_node_endpoint ~config ~latest_level_seen ~keep_alive () =
  let open Lwt_result_syntax in
  let* worker =
    Worker.launch
      table
      ()
      {rollup_node_endpoint; config; latest_level_seen; keep_alive}
      (module Handlers)
  in
  let*! () = Blueprint_events.publisher_is_ready () in
  Lwt.wakeup worker_waker worker ;
  return_unit

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

let worker_add_request ~request =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! (_pushed : bool) = Worker.Queue.push_request w request in
  return_unit

let publish level payload =
  worker_add_request ~request:(Publish {level; payload})

let new_rollup_block rollup_level =
  worker_add_request ~request:(New_rollup_node_block rollup_level)

let shutdown () =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! () = Blueprint_events.publisher_shutdown () in
  let*! () = Worker.shutdown w in
  return_unit
