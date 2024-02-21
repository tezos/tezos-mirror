(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  rollup_node_endpoint : Uri.t;
  store : Store.t;
  max_blueprints_lag : int;
  max_blueprints_catchup : int;
  catchup_cooldown : int;
}

type state = {
  store : Store.t;
  rollup_node_endpoint : Uri.t;
  max_blueprints_lag : Z.t;
  max_blueprints_catchup : Z.t;
  catchup_cooldown : int;
  mutable latest_level_confirmed : Z.t;
      (** The current head of the EVM chain as seen by the rollup node *)
  mutable latest_level_seen : Z.t;
      (** The level of the latest blueprint the sequencer tried to inject back
          to layer 1 *)
  mutable cooldown : int;
      (** Do not try to catch-up if [cooldown] is not equal to 0 *)
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

  let blueprint_store worker = (state worker).store

  let max_blueprints_lag worker = (state worker).max_blueprints_lag

  let max_blueprints_catchup worker = (state worker).max_blueprints_catchup

  let rollup_is_lagging_behind worker =
    let missing_levels =
      Z.sub (latest_level_seen worker) (latest_level_confirmed worker)
    in
    Z.(Compare.(missing_levels > max_blueprints_lag worker))

  let set_cooldown worker cooldown = (state worker).cooldown <- cooldown

  let catchup_cooldown worker = (state worker).catchup_cooldown

  let current_cooldown worker = (state worker).cooldown

  let on_cooldown worker = 0 < current_cooldown worker

  let decrement_cooldown worker =
    let current = current_cooldown worker in
    if on_cooldown worker then set_cooldown worker (current - 1) else ()

  let publish self payload level =
    let open Lwt_syntax in
    let rollup_node_endpoint = rollup_node_endpoint self in
    (* We do not check if we succeed or not: this will be done when new L2
       heads come from the rollup node. *)
    witness_level self level ;
    let* res = Rollup_services.publish ~rollup_node_endpoint payload in
    match res with
    | Ok _ -> Blueprint_events.blueprint_injected level
    | Error _ ->
        (* We have failed to inject the blueprint. This is probably
           the sign that the rollup node is down. It will be injected again
           once the rollup node lag increases to [max_blueprints_lag]. *)
        Blueprint_events.blueprint_injection_failed level

  let catch_up worker =
    let open Lwt_result_syntax in
    (* We limit the maximum number of blueprints we send at once *)
    let upper_bound =
      Z.(
        min
          (add (latest_level_confirmed worker) (max_blueprints_catchup worker))
          (latest_level_seen worker))
    in

    let*! () =
      Blueprint_events.catching_up (latest_level_confirmed worker) upper_bound
    in

    let rec catching_up curr =
      if Z.Compare.(curr <= upper_bound) then
        let* payload =
          Store.Publishable_blueprints.find (blueprint_store worker) (Qty curr)
        in
        match payload with
        | Some payload ->
            let*! () = publish worker payload curr in
            catching_up Z.(succ curr)
        | None ->
            let*! () = Blueprint_events.missing_blueprint curr in
            Stdlib.failwith
              Format.(
                asprintf
                  "Blueprint for level %a missing from the store"
                  Z.pp_print
                  curr)
      else return_unit
    in

    let* () = catching_up (latest_level_confirmed worker) in

    (* We give ourselves a cooldown window Tezos blocks to inject everything *)
    set_cooldown worker (catchup_cooldown worker) ;
    return_unit
end

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  open Blueprints_publisher_types

  type self = worker

  type launch_error = error trace

  let on_launch _self ()
      ({
         rollup_node_endpoint;
         store;
         max_blueprints_lag;
         max_blueprints_catchup;
         catchup_cooldown;
       } :
        Types.parameters) =
    let open Lwt_result_syntax in
    return
      {
        store;
        latest_level_confirmed =
          (* Will be set at the correct value once the next L2 block is
             received from the rollup node *)
          Z.zero;
        latest_level_seen =
          (* Will be set at the correct value once the sequencer produces
             the next blueprint *)
          Z.zero;
        cooldown = 0;
        rollup_node_endpoint;
        max_blueprints_lag = Z.of_int max_blueprints_lag;
        max_blueprints_catchup = Z.of_int max_blueprints_catchup;
        catchup_cooldown;
      }

  let on_request :
      type r request_error.
      self -> (r, request_error) Request.t -> (r, request_error) result Lwt.t =
   fun self request ->
    let open Lwt_result_syntax in
    match request with
    | Publish {level; payload} ->
        let*! () = Worker.publish self payload level in
        return_unit
    | New_l2_head {rollup_head} ->
        Worker.set_latest_level_confirmed self rollup_head ;
        if Worker.rollup_is_lagging_behind self && not (Worker.on_cooldown self)
        then Worker.catch_up self
        else (
          Worker.decrement_cooldown self ;
          return_unit)

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

let start ~rollup_node_endpoint ~max_blueprints_lag ~max_blueprints_catchup
    ~catchup_cooldown store =
  let open Lwt_result_syntax in
  let* worker =
    Worker.launch
      table
      ()
      {
        rollup_node_endpoint;
        store;
        max_blueprints_lag;
        max_blueprints_catchup;
        catchup_cooldown;
      }
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
    | Lwt.Fail e -> Error (TzTrace.make @@ error_of_exn e)
    | Lwt.Sleep -> Error (TzTrace.make No_worker))

let worker_add_request ~request =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  let*! (_pushed : bool) = Worker.Queue.push_request w request in
  return_unit

let publish level payload =
  worker_add_request ~request:(Publish {level; payload})

let new_l2_head rollup_head =
  worker_add_request ~request:(New_l2_head {rollup_head})

let shutdown () =
  let open Lwt_syntax in
  match Lazy.force worker with
  | Error _ ->
      (* There is no publisher, nothing to do *)
      Lwt.return_unit
  | Ok w ->
      let* () = Blueprint_events.publisher_shutdown () in
      Worker.shutdown w
