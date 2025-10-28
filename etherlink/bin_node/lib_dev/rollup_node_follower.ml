(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [process_new_block ?on_new_head ~finalized_level] call each worker
    that might have a logic dependent of the l1 level,
    {!Evm_events_follower.new_rollup_block},
    {!Blueprints_publisher.new_rollup_block}, and
    {!Signals_publisher.new_rollup_block}.

    Finally calls [on_new_head] if it is defined. *)
let process_new_block ?on_new_head ~finalized_level () =
  let open Lwt_result_syntax in
  let* () = Evm_events_follower.new_rollup_block finalized_level in
  let* () = Blueprints_publisher.new_rollup_block finalized_level in
  let* () = Signals_publisher.new_rollup_block ~finalized_level in
  match on_new_head with
  | None -> return_unit
  | Some on_new_head -> on_new_head ()

type error += Connection_lost | Connection_timeout

let () =
  register_error_kind
    `Temporary
    ~id:"rollup_node_follower_connection_lost"
    ~title:"Rollup_node_follower_connection_lost"
    ~description:"Connection to the rollup node was lost"
    Data_encoding.unit
    (function Connection_lost -> Some () | _ -> None)
    (fun () -> Connection_lost) ;
  register_error_kind
    `Temporary
    ~id:"rollup_node_follower_connection_timeout"
    ~title:"Rollup_node_follower_connection_timeout"
    ~description:
      "Connection to the rollup node was timeouted, e.g. the rollup node \
       stream was staling"
    Data_encoding.unit
    (function Connection_timeout -> Some () | _ -> None)
    (fun () -> Connection_timeout)

(** [process_finalized_level ?on_new_head
    ~oldest_rollup_node_known_l1_level ~finalized_level
    ~rollup_node_endpoint] process the rollup node block level
    [finalized_level] with {!process_new_block ?on_new_head} iff it's
    known by the rollup node (i.e. superior to
    [oldest_rollup_node_known_l1_level].

    This is necessary for the very beginning of the rollup life, when
    the evm node is started at the same moment at the origination of
    the rollup, and so `finalized_level` is < origination level. *)
let process_finalized_level ?on_new_head ~oldest_rollup_node_known_l1_level
    ~finalized_level () =
  let open Lwt_result_syntax in
  if oldest_rollup_node_known_l1_level <= finalized_level then
    process_new_block ?on_new_head ~finalized_level ()
  else return_unit

let reconnection_delay = 5.0

let min_timeout = 10.

let timeout_factor = 10.

type rollup_node_connection = {
  close : unit -> unit;  (** stream closing function *)
  finalized_levels : int32 Lwt_stream.t;
      (** current rollup node finalized levels stream *)
  rollup_node_endpoint : Uri.t;  (** endpoint used to reconnect to the node *)
  timeout : Float.t;
      (** expected time to receive a l2 block from the rollup node. is
          recalculated at each received block. *)
  rollup_node_endpoint_timeout : float;
      (** timeout for the initial connection to the node *)
}

(** [timeout] is updated to reflect reality of how long we should with
    the next block or assume the connection is failing/hanging. *)
let update_timeout ~elapsed ~connection =
  let new_timeout = elapsed *. timeout_factor in
  if new_timeout < min_timeout then connection
  else {connection with timeout = new_timeout}

let sleep_before_reconnection ~factor =
  let open Lwt_syntax in
  if factor = 0 then return_unit
  else
    (* randomised the sleep time to not DoS the rollup node if
       multiple evm node are connected to the same rollup node *)
    let fcount = float_of_int (factor - 1) in
    (* Randomized exponential backoff capped to 1.5h: 1.5^count * delay ± 50% *)
    let delay = reconnection_delay *. (1.5 ** fcount) in
    let delay = min delay 3600. in
    let randomization_factor =
      0.5
      (* 50% *)
    in
    let delay =
      delay
      +. Random.float (delay *. 2. *. randomization_factor)
      -. (delay *. randomization_factor)
    in
    let* () = Rollup_node_follower_events.trying_reconnection delay in
    Lwt_unix.sleep delay

(**[connect_to_stream ?count ~rollup_node_endpoint ()] try to connect
   to the stream of rollup node block. If [count] is superior to [0]
   then sleep some time with [sleep_before_reconnection] before trying
   to reconnect.

    [count] is the number of time we tried to reconnect in a row. *)
let rec connect_to_stream ?(count = 0) ~rollup_node_endpoint ~timeout () =
  let open Lwt_result_syntax in
  let*! () = sleep_before_reconnection ~factor:count in
  let*! res =
    Rollup_services.monitor_finalized_levels ~timeout rollup_node_endpoint
  in
  match res with
  | Ok (finalized_levels, close) ->
      let*! () = Rollup_node_follower_events.connection_acquired () in
      return
        {
          close;
          finalized_levels;
          rollup_node_endpoint;
          timeout = 300.;
          rollup_node_endpoint_timeout = timeout;
        }
  | Error errs ->
      let*! () = Rollup_node_follower_events.connection_failed errs in
      (connect_to_stream [@tailcall])
        ~count:(count + 1)
        ~rollup_node_endpoint
        ~timeout
        ()

(** [get_next_finalized_level ?on_new_head ~connection] returns the next level
    found in [connection.finalized_levels].

    - If the connection drops then it tries to reconnect the stream
    using [connect_to_stream].

    - If the connection timeout (takes more than [connection.timeout])
    or if the connection fails then reconnect with [connect_to_stream]
    and try to fetch [get_next_block] with that new stream.*)
let rec get_next_finalized_level ?on_new_head ~connection () =
  let open Lwt_result_syntax in
  let get_promise () =
    let*! res = Lwt_stream.get connection.finalized_levels in
    match res with None -> tzfail Connection_lost | Some block -> return block
  in
  let timeout_promise timeout =
    let*! () = Lwt_unix.sleep timeout in
    tzfail Connection_timeout
  in
  let*! get_or_timeout =
    Lwt.pick [get_promise (); timeout_promise connection.timeout]
  in
  match get_or_timeout with
  | Ok level -> return (level, connection)
  | Error ([(Connection_lost | Connection_timeout)] as errs) ->
      connection.close () ;
      let*! () = Rollup_node_follower_events.connection_failed errs in
      let* connection =
        connect_to_stream
          ~count:1
          ~rollup_node_endpoint:connection.rollup_node_endpoint
          ~timeout:connection.rollup_node_endpoint_timeout
          ()
      in
      (get_next_finalized_level [@tailcall]) ?on_new_head ~connection ()
  | Error errs ->
      let*! () = Rollup_node_follower_events.stream_failed errs in
      fail errs

(** [loop_on_rollup_node_stream ~keep_alive ?on_new_head
    ~oldest_rollup_node_known_l1_level ~connection] main loop to
    process the block.

    get the current rollup node block with [get_next_block], process it
    with [process_finalized_level] then loop over.

    [on_hew_head] is a process to be called on each new head that must
    be processed, i.e. {!process_new_block}. *)
let rec loop_on_rollup_node_stream ~keep_alive ?on_new_head
    ~oldest_rollup_node_known_l1_level ~connection () =
  let open Lwt_result_syntax in
  let start_time = Unix.gettimeofday () in
  let* finalized_level, connection =
    get_next_finalized_level ?on_new_head ~connection ()
  in
  let elapsed = Unix.gettimeofday () -. start_time in
  let connection = update_timeout ~elapsed ~connection in
  let* () =
    process_finalized_level
      ?on_new_head
      ~oldest_rollup_node_known_l1_level
      ~finalized_level
      ()
  in
  (loop_on_rollup_node_stream [@tailcall])
    ~keep_alive
    ?on_new_head
    ~oldest_rollup_node_known_l1_level
    ~connection
    ()

let start ~keep_alive ?on_new_head ~rollup_node_endpoint
    ~rollup_node_endpoint_timeout () =
  Lwt.async @@ fun () ->
  let open Lwt_syntax in
  let* () = Rollup_node_follower_events.started () in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* oldest_rollup_node_known_l1_level =
    Rollup_services.oldest_known_l1_level
      ~keep_alive
      ~timeout:rollup_node_endpoint_timeout
      rollup_node_endpoint
  in
  let* connection =
    connect_to_stream
      ~rollup_node_endpoint
      ~timeout:rollup_node_endpoint_timeout
      ()
  in
  loop_on_rollup_node_stream
    ~keep_alive
    ?on_new_head
    ~oldest_rollup_node_known_l1_level
    ~connection
    ()
