(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Events = P2p_events.P2p_maintainance

type bounds = {
  min_threshold : int;
  min_target : int;
  max_target : int;
  max_threshold : int;
}

type config = {
  maintenance_idle_time : Time.System.Span.t;
  private_mode : bool;
  min_connections : int;
  max_connections : int;
  expected_connections : int;
  time_between_looking_for_peers : Ptime.span;
}

type test_config = {
  trigger_swap : bool;
  trigger_too_few_connections : bool;
  trigger_too_many_connections : bool;
}

type ('msg, 'meta, 'meta_conn) inner_state = {
  config : config;
  debug_config : test_config option;
  bounds : bounds;
  connect_handler : ('msg, 'meta, 'meta_conn) P2p_connect_handler.t;
  discovery : P2p_discovery.t option;
  just_maintained : unit Lwt_condition.t;
  please_maintain : unit Lwt_condition.t;
  triggers : P2p_trigger.t;
  log : P2p_connection.P2p_event.t -> unit;
  rng : Random.State.t;
}

let pool t = P2p_connect_handler.get_pool t.connect_handler

let broadcast_bootstrap_msg t =
  P2p_peer.Table.iter
    (fun peer_id peer_info ->
      match P2p_peer_state.get peer_info with
      | Running {data = conn; _} ->
          if not (P2p_conn.private_node conn) then (
            ignore (P2p_conn.write_bootstrap conn) ;
            t.log (Bootstrap_sent {source = peer_id}))
      | _ -> ())
    (P2p_pool.connected_peer_ids (pool t))

let send_swap_request t =
  match P2p_pool.Connection.propose_swap_request (pool t) with
  | None -> ()
  | Some (proposed_point, proposed_peer_id, recipient) ->
      let recipient_peer_id = (P2p_conn.info recipient).peer_id in
      t.log (Swap_request_sent {source = recipient_peer_id}) ;
      ignore
        (P2p_conn.write_swap_request recipient proposed_point proposed_peer_id)

let classify pool private_mode start_time seen_points point pi =
  let now = Time.System.now () in
  if
    P2p_point.Set.mem point seen_points
    || P2p_pool.Points.banned pool point
    || (private_mode && not (P2p_point_state.Info.trusted pi))
  then `Ignore
  else
    match P2p_point_state.get pi with
    | Disconnected -> (
        match P2p_point_state.Info.last_miss pi with
        | Some last
          when Time.System.(start_time < last)
               || P2p_point_state.Info.cannot_reconnect_yet ~now pi ->
            `Seen
        | last -> `Candidate last)
    | _ -> `Seen

(** [establish t contactable] tries to establish as many connection as possible
    with points in [contactable]. It returns the number of established
    connections *)
let establish t contactable =
  let open Lwt_syntax in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6140
     Allow to restrict how many connections are opened at a given time *)
  let try_to_connect point =
    let+ r =
      protect (fun () -> P2p_connect_handler.connect t.connect_handler point)
    in
    match r with Ok _ -> 1 | Error _ -> 0
  in
  let+ contacted = List.map_p try_to_connect contactable in
  List.fold_left Int.add 0 contacted

(* [connectable t start_time expected seen_points] selects at most
   [expected] connections candidates from the known points, not in [seen]
   points. *)
let connectable t start_time expected seen_points =
  let module Bounded_point_info = Bounded_heap.Make (struct
    type t = Time.System.t option * P2p_point.Id.t

    let compare (t1, _) (t2, _) =
      match (t1, t2) with
      | None, None -> 0
      | None, Some _ -> 1
      | Some _, None -> -1
      | Some t1, Some t2 -> Time.System.compare t2 t1
  end) in
  let acc = Bounded_point_info.create expected in
  let f point pi seen_points =
    match
      classify (pool t) t.config.private_mode start_time seen_points point pi
    with
    | `Ignore -> seen_points (* Ignored points can be retried again *)
    | `Candidate last ->
        Bounded_point_info.insert (last, point) acc ;
        P2p_point.Set.add point seen_points
    | `Seen -> P2p_point.Set.add point seen_points
  in
  let seen_points = P2p_pool.Points.fold_known (pool t) ~init:seen_points ~f in
  (List.map snd (Bounded_point_info.get acc), seen_points)

(* [try_to_contact_loop t start_time ~seen_points] is the main loop
    for contacting points. [start_time] is set when calling the function
    and remains constant in the loop. [seen_points] simply accumulates the
    points already seen, to avoid trying to contact them again.

    It repeats two operations until the number of connections is reached:
      - get [max_to_contact] points
      - connect to many of them as possible
*)
(* TODO: https://gitlab.com/tezos/tezos/-/issues/4601

   Why not the simpler implementation. Sort all candidates points, and try to
   connect to [n] of them. *)
let rec try_to_contact_loop t start_time ~seen_points min_to_contact
    max_to_contact =
  let open Lwt_syntax in
  if min_to_contact <= 0 then Lwt.return_true
  else
    let candidates, seen_points =
      connectable t start_time max_to_contact seen_points
    in
    if candidates = [] then
      let* () = Lwt.pause () in
      Lwt.return_false
    else
      let* established = establish t candidates in
      try_to_contact_loop
        t
        start_time
        ~seen_points
        (min_to_contact - established)
        (max_to_contact - established)

(** [try_to_contact t min_to_contact max_to_contact] tries to create
    between [min_to_contact] and [max_to_contact] new connections.

    It goes through all know points, and ignores points which are
    - greylisted,
    - banned,
    - for which a connection failed after the time this function is called
    - Non-trusted points if option --private-mode is set.

    It tries to favor points for which the last failed missed connection is old.

    Note that this function works as a sequence of lwt tasks that tries
    to incrementally reach the number of connections. The set of
    known points maybe be concurrently updated. *)
let try_to_contact t min_to_contact max_to_contact =
  let start_time = Time.System.now () in
  let seen_points = P2p_point.Set.empty in
  try_to_contact_loop t start_time min_to_contact max_to_contact ~seen_points

(** not enough contacts, ask the pals of our pals,
    discover the local network and then wait unless we are in private
    mode, in which case we just wait to prevent the maintenance to loop endlessly *)
let ask_for_more_contacts t =
  if t.config.private_mode then
    protect (fun () ->
        Lwt_result.ok
        @@ Lwt_unix.sleep
             (Ptime.Span.to_float_s t.config.time_between_looking_for_peers))
  else (
    broadcast_bootstrap_msg t ;
    Option.iter P2p_discovery.wakeup t.discovery ;
    protect (fun () ->
        Lwt_result.ok
        @@ Lwt.pick
             [
               P2p_trigger.wait_new_peer t.triggers;
               P2p_trigger.wait_new_point t.triggers;
               (* TODO: https://gitlab.com/tezos/tezos/-/issues/4602

                  Exponential back-off, or wait for the existence of a non
                  grey-listed peer? *)
               Lwt_unix.sleep
                 (Ptime.Span.to_float_s t.config.time_between_looking_for_peers);
             ]))

(** Selects [n] random connections. Ignore connections to
    nodes who are both private and trusted. *)
let random_connections ~rng pool n =
  let open P2p_conn in
  let f _ conn acc =
    if private_node conn && trusted_node conn then acc else conn :: acc
  in
  let candidates =
    P2p_pool.Connection.fold pool ~init:[] ~f |> List.shuffle ~rng
  in
  TzList.rev_take_n n candidates

(** Maintenance step.
    1. trigger greylist gc
    2. tries *forever* to achieve a number of connections
       between `min_threshold` and `max_threshold`. *)
let rec do_maintain t =
  let open Lwt_result_syntax in
  t.log P2p_connection.P2p_event.Maintenance_started ;
  let n_connected = P2p_pool.active_connections (pool t) in
  if n_connected < t.bounds.min_threshold then
    match t.debug_config with
    | Some {trigger_too_few_connections = false; _} -> return_unit
    | _ -> too_few_connections t n_connected
  else if t.bounds.max_threshold < n_connected then
    match t.debug_config with
    | Some {trigger_too_many_connections = false; _} -> return_unit
    | _ -> too_many_connections t n_connected
  else (
    (* end of maintenance when enough users have been reached *)
    Lwt_condition.broadcast t.just_maintained () ;
    return_unit)

and too_few_connections t n_connected =
  let open Lwt_result_syntax in
  (* try and contact new peers *)
  t.log Too_few_connections ;
  let*! () =
    Events.(emit too_few_connections)
      ( n_connected,
        t.bounds.min_target,
        t.config.expected_connections,
        t.config.min_connections )
  in
  let min_to_contact = t.bounds.min_target - n_connected in
  let max_to_contact = t.bounds.max_target - n_connected in
  let*! success = try_to_contact t min_to_contact max_to_contact in
  let* () = if success then return_unit else ask_for_more_contacts t in
  do_maintain t

and too_many_connections t n_connected =
  let open Lwt_syntax in
  (* kill random connections *)
  t.log Too_many_connections ;
  let n = n_connected - t.bounds.max_target in
  let* () =
    Events.(emit too_many_connections)
      ( n,
        t.bounds.max_target,
        t.config.expected_connections,
        t.config.max_connections )
  in
  let connections = random_connections ~rng:t.rng (pool t) n in
  let* () =
    List.iter_p (P2p_conn.disconnect ~reason:Maintenance_too_many) connections
  in
  do_maintain t

let rec maintain t motive =
  let open Lwt_result_syntax in
  let n_connected = P2p_pool.active_connections (pool t) in
  if
    n_connected < t.bounds.min_threshold || t.bounds.max_threshold < n_connected
  then (
    let*! () = Events.(emit maintenance_started) motive in
    let maintenance_start = Time.System.now () in
    let* () = do_maintain t in
    let maintenance_duration =
      Ptime.diff (Time.System.now ()) maintenance_start
    in
    let*! () = Events.(emit maintenance_ended) maintenance_duration in
    t.log P2p_connection.P2p_event.Maintenance_ended ;
    maintain t Events.Last_maintenance)
  else
    let () =
      if not t.config.private_mode then
        match t.debug_config with
        | Some {trigger_swap = false; _} -> ()
        | _ -> send_swap_request t
    in
    return_unit

let bounds ~min ~expected ~max =
  assert (min <= expected) ;
  assert (expected <= max) ;
  let step_min = (expected - min) / 3 and step_max = (max - expected) / 3 in
  {
    min_threshold = min + step_min;
    min_target = min + (2 * step_min);
    max_target = max - (2 * step_max);
    max_threshold = max - step_max;
  }

module Name = P2p_workers.Unique_name_maker (struct
  let base = ["lib_p2p"; "p2p_maintenance"]
end)

module Request = struct
  type ('response, 'error) t =
    | Maintain : Events.maintenance_trigger_motive -> (unit, tztrace) t

  type view = View : ('response, 'error) t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"maintain"
          (obj2
             (req "request" (constant "maintain"))
             (req "motive" Events.motive_encoding))
          (function View (Maintain motive) -> Some ((), motive))
          (fun ((), motive) -> View (Maintain motive));
      ]

  let pp fmt view =
    let open Format in
    match view with
    | View (Maintain motive) ->
        fprintf fmt "Maintain %a" Events.motive_pp motive

  (* This value is not used but has to be implemented to create the worker. *)
  let default_callback_value = View (Maintain Events.Activation)
end

module Types = struct
  type state = S : _ inner_state -> state

  type ('msg, 'meta, 'meta_conn) inner_parameters = {
    discovery : P2p_discovery.t option;
    config : config;
    debug_config : test_config option;
    connect_handler : ('msg, 'meta, 'meta_conn) P2p_connect_handler.t;
    triggers : P2p_trigger.t;
    log : P2p_connection.P2p_event.t -> unit;
    rng : Random.State.t;
    please_maintain : unit Lwt_condition.t;
  }

  type parameters =
    | P : ('msg, 'meta, 'meta_conn) inner_parameters -> parameters
end

module Worker = P2p_workers.Make (Name) (Request) (Types)

type worker = Worker.activated_worker

type ('msg, 'meta, 'meta_conn) t = worker

module Handlers = struct
  type self = Worker.callback Worker.t

  type launch_error = tztrace

  let on_request : type r request_error.
      self -> (r, request_error) Request.t -> (r, request_error) result Lwt.t =
   fun w request ->
    let (S state) = Worker.state w in
    match request with Request.Maintain motive -> maintain state motive

  let on_launch _w ()
      (Types.P
         {
           discovery;
           config;
           debug_config;
           connect_handler;
           triggers;
           log;
           rng;
           please_maintain;
         }) =
    let inner_state =
      let bounds =
        bounds
          ~min:config.min_connections
          ~expected:config.expected_connections
          ~max:config.max_connections
      in
      {
        config;
        debug_config;
        bounds;
        discovery;
        connect_handler;
        just_maintained = Lwt_condition.create ();
        please_maintain;
        triggers;
        log;
        rng;
      }
    in
    let state = Types.S inner_state in
    Lwt_result_syntax.return state

  let on_error (type a b) _w _st (req : (a, b) Request.t) (errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    let open Lwt_result_syntax in
    match (req, errs) with Request.Maintain _, _ -> return `Shutdown

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close w =
    let open Lwt_syntax in
    let (Types.S {discovery; just_maintained; _}) = Worker.state w in
    let* () = Option.iter_s P2p_discovery.shutdown discovery in
    Lwt_condition.broadcast just_maintained () ;
    return_unit
end

module Internal = struct
  type nonrec test_config = test_config = {
    trigger_swap : bool;
    trigger_too_few_connections : bool;
    trigger_too_many_connections : bool;
  }

  let create ?(rng = Random.State.make_self_init ()) ?discovery config
      ?debug_config connect_handler triggers ~log =
    let please_maintain = Lwt_condition.create () in
    let activated = ref false in
    let callback () =
      let open Lwt_syntax in
      let* motive =
        if !activated then
          let timer_promise =
            let idle_time = config.maintenance_idle_time in
            let* () = Systime_os.sleep idle_time in
            return (Events.Timer idle_time)
          in
          let external_event_promise =
            let* () = Lwt_condition.wait please_maintain in
            return Events.External
          in
          let too_few_connections_promise =
            let* () = P2p_trigger.wait_too_few_connections triggers in
            return Events.Too_few_connections
          in
          let too_many_connections_promise =
            let* () = P2p_trigger.wait_too_many_connections triggers in
            return Events.Too_many_connections
          in
          Lwt.pick
            [
              timer_promise;
              external_event_promise;
              too_few_connections_promise;
              too_many_connections_promise;
            ]
        else (
          activated := true ;
          return Events.Activation)
      in
      Lwt_syntax.return
      @@ Worker.Any_request (Request.Maintain motive, {scope = None})
    in
    Worker.create
      ~callback
      ()
      (Types.P
         {
           discovery;
           config;
           debug_config;
           connect_handler;
           triggers;
           log;
           rng;
           please_maintain;
         })
      (module Handlers)

  let activate w = Worker.activate w
end

let create ?discovery config connect_handler triggers ~log =
  Internal.create ?discovery config connect_handler triggers ~log

let activate t = Internal.activate t

let maintain (w : (_, _, _) t) =
  let (Types.S {just_maintained; please_maintain; _}) =
    Worker.state w.worker_state
  in
  let wait = Lwt_condition.wait just_maintained in
  Lwt_condition.broadcast please_maintain () ;
  wait

let shutdown w = Worker.shutdown w

module Internal_for_tests = Internal
