(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let process_new_block ~proxy ~finalized_level () =
  let open Lwt_result_syntax in
  let* () = Evm_events_follower.new_rollup_block finalized_level in
  let* () = Blueprints_publisher.new_rollup_block finalized_level in
  let* () = Signals_publisher.new_rollup_block ~finalized_level in
  if proxy then Tx_pool.pop_and_inject_transactions_lazy () else return_unit

type error += Connection_lost | Connection_timeout

(** [process_finalized_level ~oldest_rollup_node_known_l1_level
    ~finalized_level ~rollup_node_endpoint] process the rollup node
    block level [finalized_level] iff it's known by the rollup node
    (i.e. superior to [oldest_rollup_node_known_l1_level].

    This is necessary for the very beginning of the rollup life, when
    the evm node is started at the same moment at the origination of
    the rollup, and so `finalized_level` is < origination level. *)
let process_finalized_level ~proxy ~oldest_rollup_node_known_l1_level
    ~finalized_level () =
  let open Lwt_result_syntax in
  if oldest_rollup_node_known_l1_level <= finalized_level then
    process_new_block ~proxy ~finalized_level ()
  else return_unit

let reconnection_delay = 5.0

let min_timeout = 10.

let timeout_factor = 10.

type rollup_node_connection = {
  close : unit -> unit;  (** stream closing function *)
  stream : Sc_rollup_block.t Lwt_stream.t;
      (** current rollup node block stream *)
  rollup_node_endpoint : Uri.t;  (** endpoint used to reconnect to the node *)
  timeout : Float.t;
      (** expected time to receive a l2 block from the rollup node. is
          recalculated at each received block. *)
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
    let randomization_factor = 0.5 (* 50% *) in
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
let[@tailrec] rec connect_to_stream ?(count = 0) ~rollup_node_endpoint () =
  let open Lwt_result_syntax in
  let*! () = sleep_before_reconnection ~factor:count in
  let*! res = Rollup_services.make_streamed_call ~rollup_node_endpoint in
  match res with
  | Ok (stream, close) ->
      let*! () = Rollup_node_follower_events.connection_acquired () in
      return {close; stream; rollup_node_endpoint; timeout = 300.}
  | Error errs ->
      let*! () = Rollup_node_follower_events.connection_failed errs in
      (connect_to_stream [@tailcall])
        ~count:(count + 1)
        ~rollup_node_endpoint
        ()

(** [catchup_evm_event ~rollup_node_endpoint ~from ~to_] catchup on
    evm events from [from] to [to_] from the rollup node. *)
let[@tailrec] rec catchup_evm_event ~rollup_node_endpoint ~from ~to_ =
  let open Lwt_result_syntax in
  if from = to_ then (*we are catch up *) return_unit
  else if from > to_ then
    failwith
      "Internal error: The catchup of evm_event went too far, it should be \
       impossible."
  else
    (* reading event from [from] level then catching up from [from +
       1]. *)
    let next_l1_level = Int32.succ from in
    let* () = Evm_events_follower.new_rollup_block next_l1_level in
    catchup_evm_event ~rollup_node_endpoint ~from:next_l1_level ~to_

(** [catchup_and_next_block ~proxy ~catchup_event ~connection]
    returns the next block found in [connection.stream].

    - If the connection drops then it tries to reconnect the stream
    using [connect_to_stream].

    - If the connection timeout (takes more than [connection.timeout])
    or if the connection fails then reconnect with [connect_to_stream]
    and try to fetch [catchup_and_next_block] with that new stream.*)
let[@tailrec] rec catchup_and_next_block ~proxy ~catchup_event ~connection =
  let open Lwt_result_syntax in
  let get_promise () =
    let*! res = Lwt_stream.get connection.stream in
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
  | Ok block ->
      let* () =
        if catchup_event then
          let* latest_known_l1_level = Evm_context.last_known_l1_level () in
          match latest_known_l1_level with
          | None ->
              (* sequencer has no value to start from, it must be the
                 initial start. *)
              let*! () = Evm_store_events.no_l1_latest_level_to_catch_up () in
              return_unit
          | Some from ->
              let to_ = Sc_rollup_block.(Int32.(sub block.header.level 2l)) in
              catchup_evm_event
                ~rollup_node_endpoint:connection.rollup_node_endpoint
                ~from
                ~to_
        else return_unit
      in
      return (block, connection)
  | Error ([(Connection_lost | Connection_timeout)] as errs) ->
      connection.close () ;
      let*! () = Rollup_node_follower_events.connection_failed errs in
      let* connection =
        connect_to_stream
          ~count:1
          ~rollup_node_endpoint:connection.rollup_node_endpoint
          ()
      in
      (catchup_and_next_block [@tailcall])
        ~proxy
        ~catchup_event:(not proxy)
          (* catchup event if not in proxy mode, proxy does not have
             `evm_context` and would fail to fetch some data. Else
             catchup possible missed event.*)
        ~connection
  | Error errs ->
      let*! () = Rollup_node_follower_events.stream_failed errs in
      fail errs

(** [loop_on_rollup_node_stream ~proxy
    ~oldest_rollup_node_known_l1_level ~connection] main loop to
    process the block.

    get the current rollup node block with [catchup_and_next_block], process it
    with [process_finalized_level] then loop over. *)
let[@tailrec] rec loop_on_rollup_node_stream ~keep_alive ~catchup_event ~proxy
    ~oldest_rollup_node_known_l1_level ~connection () =
  let open Lwt_result_syntax in
  let start_time = Unix.gettimeofday () in
  let* block, connection =
    catchup_and_next_block ~proxy ~catchup_event ~connection
  in
  let elapsed = Unix.gettimeofday () -. start_time in
  let connection = update_timeout ~elapsed ~connection in
  let finalized_level = Sc_rollup_block.(Int32.(sub block.header.level 2l)) in
  let* () =
    process_finalized_level
      ~proxy
      ~oldest_rollup_node_known_l1_level
      ~finalized_level
      ()
  in
  (loop_on_rollup_node_stream [@tailcall])
    ~keep_alive
    ~catchup_event:false
    ~proxy
    ~oldest_rollup_node_known_l1_level
    ~connection
    ()

let start ~keep_alive ~proxy ~rollup_node_endpoint () =
  Lwt.async @@ fun () ->
  let open Lwt_syntax in
  let* () = Rollup_node_follower_events.started () in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* oldest_rollup_node_known_l1_level =
    Rollup_services.oldest_known_l1_level ~keep_alive rollup_node_endpoint
  in
  let* connection = connect_to_stream ~rollup_node_endpoint () in
  loop_on_rollup_node_stream
    ~keep_alive
      (* when fetching first block it should try to catchup if needed *)
    ~catchup_event:(not proxy)
    ~proxy
    ~oldest_rollup_node_known_l1_level
    ~connection
    ()
