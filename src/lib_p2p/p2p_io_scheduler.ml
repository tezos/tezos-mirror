(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

module Events = P2p_events.P2p_io_scheduler

let alpha = 0.2

module type IO = sig
  val name : string

  type in_param

  type data

  val length : data -> int

  val pop : in_param -> data tzresult Lwt.t

  type out_param

  val push : out_param -> data -> unit tzresult Lwt.t

  val close : out_param -> error list -> unit Lwt.t
end

module Scheduler (IO : IO) = struct
  (* The IO scheduler schedules the bandwidth usage of a unidirectional
     connection.

     It is a worker with two queues: a high priority queue and a low
     priority queue.

     A quota of IO is given to each attached connections for a time
     window (currently 1 sec.).
     The quota is the average quantity of IO done by the attached
     connections in the previous time window.
     Those which have not exhausted their quota are allowed to
     perform an IO first (they are placed in the high priority queue).

     If all connections have exhausted their quota, the low priority
     queue is served.

     A quota can also be set for the whole set of connections.

     The implementation of the scheduler is highly dependent on the
     implementation of the moving average worker. *)

  (* Two labels or constructors of the same name are defined in two mutually
     recursive types: fields canceler, counter and quota. *)
  [@@@ocaml.warning "-30"]

  type inner_state = {
    ma_state : Moving_average.state;
    mutable worker : unit Lwt.t;
    counter : Moving_average.t;
    max_speed : int option;
    mutable quota : int;
    quota_updated : unit Lwt_condition.t;
    readys : unit Lwt_condition.t;
    readys_high : (connection * IO.data tzresult) Queue.t;
    readys_low : (connection * IO.data tzresult) Queue.t;
  }

  and connection = {
    id : int;
    mutable closed : bool;
    canceler : Lwt_canceler.t;
    in_param : IO.in_param;
    out_param : IO.out_param;
    mutable current_pop : IO.data tzresult Lwt.t;
    mutable current_push : unit tzresult Lwt.t;
    counter : Moving_average.t;
    mutable quota : int;
    add_closing_reason : P2p_disconnection_reason.t -> unit;
  }

  [@@@ocaml.warning "+30"]

  let full_name = Format.sprintf "io_scheduler(%s)" IO.name

  (* if the connection is not already closed,
     - mark it closed
     - call the closer on the out_param
     - cancel its canceler *)
  let cancel (conn : connection) err =
    let open Lwt_syntax in
    if conn.closed then Lwt.return_unit
    else
      let* () =
        Events.(emit connection_closed) ("cancel", conn.id, full_name)
      in
      conn.closed <- true ;
      let* () = Unit.catch_s (fun () -> IO.close conn.out_param err) in
      Error_monad.cancel_with_exceptions conn.canceler

  (* [waiter] is an asynchronous thread that triggers an IO and then
     put back the [conn] in the queue for further IO treatment.

     Once an IO has been treated (current pop and push are resolved),
     the [conn] and its last pop result are pushed either in the high
     priority queue if [conn] has not exhausted its quota or in the
     low priority queue otherwise.

     If both scheduler's queues are empty, the scheduler is
     notified. *)
  let waiter st conn =
    assert (Lwt.state conn.current_pop <> Sleep) ;
    conn.current_pop <- IO.pop conn.in_param ;
    Lwt.dont_wait
      (fun () ->
        let open Lwt_syntax in
        (* To ensure that there is no concurrent calls to IO.pop, we
           wait for the promise to be fulfilled. *)
        let* res = conn.current_pop in
        let* _ = conn.current_push in
        let was_empty =
          Queue.is_empty st.readys_high && Queue.is_empty st.readys_low
        in
        if conn.quota > 0 then Queue.push (conn, res) st.readys_high
        else Queue.push (conn, res) st.readys_low ;
        if was_empty then Lwt_condition.broadcast st.readys () ;
        Lwt.return_unit)
      (fun exc ->
        Format.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string exc))

  (* Wait for a connection to be available, with data in one of the
     queues. *)
  let wait_data st =
    let is_empty =
      Queue.is_empty st.readys_high && Queue.is_empty st.readys_low
    in
    if is_empty then Lwt_condition.wait st.readys else Lwt.return_unit

  (* Check if the global quota has been reached.
     If so, wait until the moving average worker updates the quota.
     Quota is ignored if no max speed is set. *)
  let check_quota st =
    if st.max_speed <> None && st.quota < 0 then
      let open Lwt_syntax in
      let* () = Events.(emit wait_quota) full_name in
      Lwt_condition.wait st.quota_updated
    else Lwt.pause ()

  module Name = P2p_workers.Unique_name_maker (struct
    let base = ["p2p_io_scheduler"; IO.name]
  end)

  (* Note that we can only rely on a callback request for now and not a queue.
     The worker works with two queues: one for high priority requests, one for
     low priority. When the worker will eventually implement queues with
     priorities it will be useful to use it and not rely on a callback
     anymore with internal queues. *)
  module Request = P2p_workers.Loop_request

  module Types = struct
    type state = inner_state

    type parameters = {ma_state : Moving_average.state; max_speed : int option}
  end

  module Worker = Tezos_workers.Worker.MakeSingle (Name) (Request) (Types)

  type t = Worker.callback Worker.t

  (* Main worker loop:

     - Check that the global IO limit is not reach (or wait)

     - wait for available data

     - take the first connection [conn] ready in the queues (looking
       at high priority queue first). Connections comes with the chunk
       of bytes ready in their input.

     - push its chunk of bytes in its output pipe

     - add the bytes count to the global counter

     - remove the bytes count from the global quota

     - add the bytes count to the [conn]'s counter

     - remove the bytes count from the [conn]'s quota

     - Call [waiter] to trigger an asynchronous "perform IO-enqueue
       conn" on [conn]

     During the loop, if an IO result is an error, the associated
     connection is canceled

     The loop is stopped on cancellation of st.canceler

     Warning: It can be stuck, pending forever before reaching the
     canceler if the global quota has been surpassed and the
     moving_average worker is stopped beforehand.

     Implicit assumption: the quota of the scheduler and the
     connections are updated asynchronously by the moving_average
     worker. *)
  let loop worker =
    let open Lwt_syntax in
    let st = Worker.state worker in
    let* () = check_quota st in
    let* () = Events.(emit wait) full_name in
    let* () =
      Lwt.pick
        [Lwt_canceler.when_canceling (Worker.canceler worker); wait_data st]
    in
    if Lwt_canceler.canceled (Worker.canceler worker) then Lwt.return_unit
    else
      let prio, (conn, msg) =
        if not (Queue.is_empty st.readys_high) then
          (true, Queue.pop st.readys_high)
        else (false, Queue.pop st.readys_low)
      in
      match msg with
      | Error (Canceled :: _) -> return_unit
      | Error (P2p_errors.Connection_closed :: _ as err)
      | Error (Exn Lwt_pipe.Closed :: _ as err) ->
          let* () =
            Events.(emit connection_closed) ("pop", conn.id, full_name)
          in
          let* () = cancel conn err in
          return_unit
      | Error err ->
          let* () =
            Events.(emit unexpected_error) ("pop", conn.id, full_name, err)
          in
          conn.add_closing_reason (Scheduled_pop_unexpected_error err) ;
          let* () = cancel conn err in
          return_unit
      | Ok msg ->
          conn.current_push <-
            (let* r = IO.push conn.out_param msg in
             match r with
             | Ok () | Error (Canceled :: _) -> return_ok_unit
             | Error (P2p_errors.Connection_closed :: _ as err)
             | Error (Exn Lwt_pipe.Closed :: _ as err) ->
                 let* () =
                   Events.(emit connection_closed) ("push", conn.id, full_name)
                 in
                 let* () = cancel conn err in
                 return_ok_unit
             | Error err ->
                 let* () =
                   Events.(emit unexpected_error)
                     ("push", conn.id, full_name, err)
                 in
                 conn.add_closing_reason (Scheduled_push_unexpected_error err) ;
                 let* () = cancel conn err in
                 return_error err) ;
          let len = IO.length msg in
          let* () = Events.(emit handle_connection) (len, conn.id, full_name) in
          Moving_average.add st.counter len ;
          st.quota <- st.quota - len ;
          Moving_average.add conn.counter len ;
          if prio then conn.quota <- conn.quota - len ;
          waiter st conn ;
          return_unit

  module Handlers = struct
    type self = Worker.callback Worker.t

    type launch_error = tztrace

    let on_launch _self () Types.{ma_state; max_speed} =
      Lwt.return_ok
        {
          ma_state;
          worker = Lwt.return_unit;
          counter = Moving_average.create ma_state ~init:0 ~alpha;
          max_speed;
          (* if max_speed is None the quota will be ignored anyway *)
          quota = Option.value ~default:0 max_speed;
          quota_updated = Lwt_condition.create ();
          readys = Lwt_condition.create ();
          readys_high = Queue.create ();
          readys_low = Queue.create ();
        }

    let on_request : type resp err.
        self -> (resp, err) P2p_workers.loop -> (resp, err) result Lwt.t =
     fun self P2p_workers.Loop ->
      let open Lwt_result_syntax in
      let*! () = loop self in
      return_unit

    let on_no_request _ = Lwt.return_unit

    let on_close _ = Lwt.return_unit

    let on_error : type resp err.
        self ->
        Tezos_base.Worker_types.request_status ->
        (resp, err) P2p_workers.loop ->
        err ->
        [`Continue | `Shutdown] tzresult Lwt.t =
     fun _ _ _ _ -> Lwt.return_ok `Continue

    let on_completion : type resp err.
        self ->
        (resp, err) P2p_workers.loop ->
        resp ->
        Tezos_base.Worker_types.request_status ->
        unit Lwt.t =
     fun _ _ _ _ -> Lwt.return_unit
  end

  (* Create an IO scheduler over a moving average state and optional
     maximum speed. *)
  let create ma_state max_speed =
    let callback =
      Worker.(
        Callback
          (fun () ->
            Lwt.return (Any_request (P2p_workers.Loop, {scope = None}))))
    in
    Worker.(
      launch (create_table callback) () {ma_state; max_speed} (module Handlers))

  (* Scheduled connection. *)
  let create_connection st in_param out_param canceler id add_closing_reason =
    Events.(emit__dont_wait__use_with_care create_connection (id, full_name)) ;
    let conn =
      {
        id;
        closed = false;
        canceler;
        in_param;
        out_param;
        current_pop = Lwt.fail Not_found (* dummy *);
        current_push = Lwt_result_syntax.return_unit;
        counter = Moving_average.create st.ma_state ~init:0 ~alpha;
        quota = 0;
        add_closing_reason;
      }
    in
    waiter st conn ;
    conn

  (* Updating the global quota of the set of scheduled connections.
     If a max_speed is set, the new time frame's quota is max_speed
     unless previous window consumed more than its quota, in which
     case the excessive consumption is deducted to the max quota.

     The low priority queue is scanned for connections that deserve to
     be moved to the high priority queue. *)
  let update_quota st =
    Events.(emit__dont_wait__use_with_care update_quota full_name) ;
    Option.iter
      (fun max_speed ->
        st.quota <- min st.quota 0 + max_speed ;
        Lwt_condition.broadcast st.quota_updated ())
      st.max_speed ;
    if not (Queue.is_empty st.readys_low) then (
      let tmp = Queue.create () in
      Queue.iter
        (fun (((conn : connection), _) as msg) ->
          if conn.quota > 0 then Queue.push msg st.readys_high
          else Queue.push msg tmp)
        st.readys_low ;
      Queue.clear st.readys_low ;
      Queue.transfer tmp st.readys_low)

  (* On shutdown, cancel the scheduler canceler and wait for the
     worker to terminate.
     The canceler does not have attached callback. *)
  let shutdown st =
    let open Lwt_syntax in
    let* () = Worker.shutdown st in
    Events.(emit shutdown) full_name
end

module ReadIO = struct
  (* The ReaderScheduler schedules the connections reads.

     Data are popped for the fd by chunks of max_len and pushed into a
     pipe.

     As the quota update is set to 1 sec., the max_len should be
     lesser than what is expected to be used by one connection in such
     a time frame. Otherwise, the bandwidth usage will not be
     regular.*)

  let name = "read"

  type in_param = {
    fd : P2p_fd.t;
    (* File descriptor from which data are read *)
    maxlen : int;
    (* Length of data we want to read from the file descriptor *)
    read_buffer : Circular_buffer.t; (* Cache where data will be stored *)
  }

  type data = Circular_buffer.data

  let length = Circular_buffer.length

  (* [pop] at most [maxlen] data from the given [fd] and write them in
     the circular buffer [read_buffer].
     Invariant: Given a connection, there is not concurrent call to
     pop. *)
  let pop {fd; maxlen; read_buffer} =
    Error_monad.catch_es (fun () ->
        let open Lwt_result_syntax in
        let*! data_result =
          Circular_buffer.write ~maxlen ~fill_using:(P2p_fd.read fd) read_buffer
        in
        match data_result with
        | Ok data ->
            if Circular_buffer.length data = 0 then
              tzfail P2p_errors.Connection_closed
            else return data
        | Error
            ( `Connection_lost _ | `Connection_closed_by_peer
            | `Connection_locally_closed ) ->
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/5632
               Losing some information here... *)
            tzfail P2p_errors.Connection_closed
        | Error (`Unexpected_error _) ->
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/5632
               Losing some information here... *)
            tzfail P2p_errors.Connection_error)

  type out_param = Circular_buffer.data tzresult Lwt_pipe.Maybe_bounded.t

  (* [push] data to the pipe, feeding the application's data consumer. *)
  let push p msg =
    Error_monad.catch_s (fun () -> Lwt_pipe.Maybe_bounded.push p (Ok msg))

  (* on [close] we push the given [err] toward the data consumer. *)
  let close p err =
    Unit.catch_s (fun () -> Lwt_pipe.Maybe_bounded.push p (Error err))
end

module ReadScheduler = Scheduler (ReadIO)

module WriteIO = struct
  (* The WriteScheduler schedules the connections writes.

     Data are popped from a pipe fed by the application and pushed
     into the fd.

     Nothing here takes care of dividing the data by chunk.  The
     component user should take care of sending small enough data
     chunks to avoid irregular bandwidth usage. *)

  let name = "write"

  type in_param = Bytes.t Lwt_pipe.Maybe_bounded.t

  type data = Bytes.t

  let length = Bytes.length

  (* [pop] bytes to be sent from the queue. *)
  let pop p =
    Lwt.catch
      (fun () -> Lwt_result.ok @@ Lwt_pipe.Maybe_bounded.pop p)
      (function
        | Lwt_pipe.Closed -> fail_with_exn Lwt_pipe.Closed | _ -> assert false)

  type out_param = P2p_fd.t

  (* [push] bytes in the network. *)
  let push fd buf =
    let open Lwt_result_syntax in
    let*! r = P2p_fd.write fd buf in
    match r with
    | Ok () -> return_unit
    | Error
        ( `Connection_closed_by_peer | `Connection_lost _
        | `Connection_locally_closed
        | `Unexpected_error Lwt.Canceled ) ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5632
           Losing some information here... *)
        tzfail P2p_errors.Connection_closed
    | Error (`Unexpected_error ex) ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5632
           Losing some information here... *)
        fail_with_exn ex

  (* [close] does nothing, it will still be possible to push values to
     the network. *)
  let close _p _err = Lwt.return_unit
end

module WriteScheduler = Scheduler (WriteIO)

(* Type of a bidirectional scheduled connection *)
type connection = {
  fd : P2p_fd.t;
  canceler : Lwt_canceler.t;
  readable : P2p_buffer_reader.readable;
  read_conn : ReadScheduler.connection;
  write_conn : WriteScheduler.connection;
  write_queue : Bytes.t Lwt_pipe.Maybe_bounded.t;
  remove_from_connection_table : unit -> unit;
}

let to_readable connection = connection.readable

type t = {
  mutable closed : bool;
  ma_state : Moving_average.state;
  connected : connection P2p_fd.Table.t;
  read_scheduler : ReadScheduler.t;
  write_scheduler : WriteScheduler.t;
  max_upload_speed : int option;
  (* bytes per second. *)
  max_download_speed : int option;
  read_buffer_size : int;
  read_queue_size : int option;
  write_queue_size : int option;
}

(* updating quota for schedulers and connections on each
   Moving_average update (approx one time per sec.).

   Each connection's quota is the average bandwidth consumption
   divided by the number of connections minus the over consumption of
   the previous round. *)
let reset_quota st =
  Events.(emit__dont_wait__use_with_care reset_quota ()) ;
  let {Moving_average.average = current_inflow; _} =
    Moving_average.stat (ReadScheduler.Worker.state st.read_scheduler).counter
  and {Moving_average.average = current_outflow; _} =
    Moving_average.stat (WriteScheduler.Worker.state st.write_scheduler).counter
  in
  let nb_conn = P2p_fd.Table.length st.connected in
  (if nb_conn > 0 then
     let fair_read_quota = current_inflow / nb_conn
     and fair_write_quota = current_outflow / nb_conn in
     P2p_fd.Table.iter
       (fun _id conn ->
         conn.read_conn.quota <- min conn.read_conn.quota 0 + fair_read_quota ;
         conn.write_conn.quota <- min conn.write_conn.quota 0 + fair_write_quota)
       st.connected) ;
  ReadScheduler.update_quota (ReadScheduler.Worker.state st.read_scheduler) ;
  WriteScheduler.update_quota (WriteScheduler.Worker.state st.write_scheduler)

(* [create] a scheduler for reading and writing on registered
   connections and starting the associated moving average worker.

   The worker will call [reset_quota] at each update.
*)
let create ?max_upload_speed ?max_download_speed ?read_queue_size
    ?write_queue_size ~read_buffer_size () =
  let open Lwt_result_syntax in
  Events.(emit__dont_wait__use_with_care create ()) ;
  let ma_state =
    Moving_average.fresh_state ~id:"p2p-io-sched" ~refresh_interval:1.0
  in
  let* read_scheduler = ReadScheduler.create ma_state max_download_speed in
  let* write_scheduler = WriteScheduler.create ma_state max_upload_speed in
  let st =
    {
      closed = false;
      ma_state;
      connected = P2p_fd.Table.create 53;
      read_scheduler;
      write_scheduler;
      max_upload_speed;
      max_download_speed;
      read_buffer_size;
      read_queue_size;
      write_queue_size;
    }
  in
  Moving_average.on_update ma_state (fun () -> reset_quota st) ;
  return st

let ma_state {ma_state; _} = ma_state

exception Closed

let read_size = function
  | Ok data ->
      (Sys.word_size / 8 * 8)
      + Circular_buffer.length data
      + Lwt_pipe.Maybe_bounded.push_overhead
  | Error _ -> 0

(* we push Error only when we close the socket, we don't fear memory
   leaks in that case... *)

let write_size bytes =
  (Sys.word_size / 8 * 6)
  + Bytes.length bytes + Lwt_pipe.Maybe_bounded.push_overhead

(* [register] a socket for scheduling by [st].

   Creating read/write pipes and attaching them to their respective
   scheduler.

   Attaching to the freshly created canceler of the connection :
   - removal from the set of scheduled fd
   - destruction of moving average counters
   - closing read/write pipes
   - closing underlying socket (p2p_fd) *)
let register st fd =
  if st.closed then (
    Lwt.dont_wait
      (fun () -> P2p_fd.close ~reason:IO_scheduler_closed fd)
      (fun exc ->
        Format.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string exc)) ;
    raise Closed)
  else
    let id = P2p_fd.id fd in
    let canceler = Lwt_canceler.create () in
    let read_size = Option.map (fun v -> (v, read_size)) st.read_queue_size in
    let write_size =
      Option.map (fun v -> (v, write_size)) st.write_queue_size
    in
    let read_queue = Lwt_pipe.Maybe_bounded.create ?bound:read_size () in
    let write_queue = Lwt_pipe.Maybe_bounded.create ?bound:write_size () in
    (* This buffer is allocated once and is reused every time we read
       a message from the corresponding file descriptor. *)
    let read_buffer =
      Circular_buffer.create ~maxlength:(st.read_buffer_size * 2) ()
    in
    let add_closing_reason reason = P2p_fd.add_closing_reason ~reason fd in
    let read_conn =
      ReadScheduler.create_connection
        (ReadScheduler.Worker.state st.read_scheduler)
        {fd; maxlen = st.read_buffer_size; read_buffer}
        read_queue
        canceler
        id
        add_closing_reason
    and write_conn =
      WriteScheduler.create_connection
        (WriteScheduler.Worker.state st.write_scheduler)
        write_queue
        fd
        canceler
        id
        add_closing_reason
    in
    Lwt_canceler.on_cancel canceler (fun () ->
        P2p_fd.Table.remove st.connected fd ;
        Moving_average.destroy st.ma_state read_conn.counter ;
        Moving_average.destroy st.ma_state write_conn.counter ;
        Lwt_pipe.Maybe_bounded.close write_queue ;
        Lwt_pipe.Maybe_bounded.close read_queue ;
        P2p_fd.close fd) ;
    let readable = P2p_buffer_reader.mk_readable ~read_buffer ~read_queue in
    let conn =
      {
        fd;
        canceler;
        readable;
        read_conn;
        write_queue;
        write_conn;
        remove_from_connection_table =
          (fun () -> P2p_fd.Table.remove st.connected fd);
      }
    in
    P2p_fd.Table.add st.connected conn.fd conn ;
    (* Events.(emit register) id) *)
    conn

(* pushing bytes in the pipe *)
let write ?canceler {write_queue; _} msg =
  trace P2p_errors.Connection_closed
  @@ protect ?canceler (fun () ->
         Lwt_result.ok @@ Lwt_pipe.Maybe_bounded.push write_queue msg)

(* pushing bytes in the pipe or return false if it is bounded and full *)
let write_now {write_queue; _} msg =
  Lwt_pipe.Maybe_bounded.push_now write_queue msg

let set_peer_id ~peer_id conn = P2p_fd.set_peer_id ~peer_id conn.fd

let convert ~ws ~rs =
  {
    P2p_stat.total_sent = ws.Moving_average.total;
    total_recv = rs.Moving_average.total;
    current_outflow = ws.average;
    current_inflow = rs.average;
  }

let global_stat {read_scheduler; write_scheduler; _} =
  let rs =
    Moving_average.stat (ReadScheduler.Worker.state read_scheduler).counter
  and ws =
    Moving_average.stat (WriteScheduler.Worker.state write_scheduler).counter
  in
  convert ~rs ~ws

let stat {read_conn; write_conn; _} =
  let rs = Moving_average.stat read_conn.counter
  and ws = Moving_average.stat write_conn.counter in
  convert ~rs ~ws

let add_closing_reason ~reason t = P2p_fd.add_closing_reason ~reason t.fd

(* [close conn] prevents further data to be pushed to the remote peer
   and start a cascade of effects that should close the connection.

   NOTE:

   - This function may be called after the underlying socket was
   closed or not. In case an error happens while writing to/reading
   from the socket, likely the [P2p_fd] module will already stop data
   to be pushed to the remote peer

   - It may happen that this function is closed because we were
   "nacked" too. Likely this function may be called twice in this
   case: 1 for the nack, a second time by [P2p_socket] when closing.
   Hence, it is important that this function to have some kind of
   idempotency property.
*)
let close ?timeout ?reason conn =
  let open Lwt_result_syntax in
  let id = P2p_fd.id conn.fd in
  Option.iter (fun reason -> P2p_fd.add_closing_reason ~reason conn.fd) reason ;
  conn.remove_from_connection_table () ;
  Lwt_pipe.Maybe_bounded.close conn.write_queue ;
  (* Here, the WriteScheduler will drain the write_queue, then get a
     [Exn Lwt_pipe.Closed;...] trace and thus cancel the
     [write_conn.canceler] which is the connections canceler (by
     connection construction).

     And remember, the canceler has the following callback attached:
     - removal from the set of scheduled fd
     - destruction of moving average counters
     - closing read/write pipes
     - closing underlying socket (p2p_fd)

     We wait the cancellation to be finished.*)
  let* () =
    match timeout with
    | None ->
        let*! r = Lwt_canceler.when_canceled conn.canceler in
        let*! () =
          match r with
          | Ok () | Error [] -> Lwt.return_unit
          | Error excs ->
              (* Do not prevent the closing if an exception is raised *)
              List.iter_p
                (fun exc -> Events.(emit close_error) (id, error_of_exn exc))
                excs
        in
        return_unit
    | Some timeout ->
        with_timeout
          ~canceler:conn.canceler
          (Lwt_unix.sleep timeout)
          (fun canceler ->
            let*! r = Lwt_canceler.when_canceled canceler in
            match r with
            | Ok () | Error [] -> return_unit
            | Error (exn :: _) ->
                (* Do not prevent the closing if an exception is raised *)
                let*! () = Events.(emit close_error) (id, error_of_exn exn) in
                return_unit)
  in
  (* and here we wait for one push in the socket, not for all the
     values in the pipe to be pushed. *)
  let*! res = conn.write_conn.current_push in
  let*! () = Events.(emit close) id in
  Lwt.return res

let iter_connection {connected; _} f =
  P2p_fd.Table.iter (fun _ conn -> f conn) connected

let shutdown ?timeout st =
  let open Lwt_syntax in
  st.closed <- true ;
  (* stop the reader loop if it's not stuck due to (max_speed+dead
     moving average worker). *)
  let* () = ReadScheduler.shutdown st.read_scheduler in
  (* trigger the connections closing and wait for the start of the
     cancellation of every connection. *)
  let* () =
    P2p_fd.Table.iter_p
      (fun _peer_id conn ->
        let* _ = close ?timeout ~reason:IO_scheduler_shutdown conn in
        Lwt.return_unit)
      st.connected
  in
  (* stop the writer loop if it's not stuck due to (max_speed+dead
     moving average worker).*)
  let* () = WriteScheduler.shutdown st.write_scheduler in
  Events.(emit shutdown_scheduler) ()

let id conn = P2p_fd.id conn.fd
