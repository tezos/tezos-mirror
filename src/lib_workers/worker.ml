(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** An error returned when trying to communicate with a worker that
    has been closed.*)
type worker_name = {base : string; name : string}

type Error_monad.error += Closed of worker_name

let () =
  register_error_kind
    `Permanent
    ~id:"worker.closed"
    ~title:"Worker closed"
    ~description:
      "An operation on a worker could not complete before it was shut down."
    ~pp:(fun ppf w ->
      Format.fprintf ppf "Worker %s[%s] has been shut down." w.base w.name)
    Data_encoding.(
      conv
        (fun {base; name} -> (base, name))
        (fun (name, base) -> {base; name})
        (obj1 (req "worker" (tup2 string string))))
    (function Closed w -> Some w | _ -> None)
    (fun w -> Closed w)

module type T = sig
  module Name : Worker_intf.NAME

  module Event : Worker_intf.EVENT

  module Request : Worker_intf.REQUEST

  module Types : Worker_intf.TYPES

  (** A handle to a specific worker, parameterized by the type of
      internal message buffer. *)
  type 'kind t

  (** A handle to a table of workers. *)
  type 'kind table

  (** Internal buffer kinds used as parameters to {!t}. *)
  type 'a queue

  and bounded

  and infinite

  type dropbox

  (** Supported kinds of internal buffers. *)
  type _ buffer_kind =
    | Queue : infinite queue buffer_kind
    | Bounded : {size : int} -> bounded queue buffer_kind
    | Dropbox : {
        merge :
          dropbox t -> any_request -> any_request option -> any_request option;
      }
        -> dropbox buffer_kind

  and any_request = Any_request : _ Request.t -> any_request

  (** Create a table of workers. *)
  val create_table : 'kind buffer_kind -> 'kind table

  (** The callback handlers specific to each worker instance. *)
  module type HANDLERS = sig
    (** Placeholder replaced with {!t} with the right parameters
        provided by the type of buffer chosen at {!launch}.*)
    type self

    (** Builds the initial internal state of a worker at launch.
        It is possible to initialize the message queue.
        Of course calling {!state} will fail at that point. *)
    val on_launch :
      self -> Name.t -> Types.parameters -> Types.state tzresult Lwt.t

    (** The main request processor, i.e. the body of the event loop. *)
    val on_request : self -> 'a Request.t -> 'a tzresult Lwt.t

    (** Called when no request has been made before the timeout, if
        the parameter has been passed to {!launch}. *)
    val on_no_request : self -> unit tzresult Lwt.t

    (** A function called when terminating a worker. *)
    val on_close : self -> unit Lwt.t

    (** A function called at the end of the worker loop in case of an
        abnormal error. This function can handle the error by
        returning [Ok ()], or leave the default unexpected error
        behaviour by returning its parameter. A possibility is to
        handle the error for ad-hoc logging, and still use
        {!trigger_shutdown} to kill the worker. *)
    val on_error :
      self ->
      Request.view ->
      Worker_types.request_status ->
      error list ->
      unit tzresult Lwt.t

    (** A function called at the end of the worker loop in case of a
        successful treatment of the current request. *)
    val on_completion :
      self -> 'a Request.t -> 'a -> Worker_types.request_status -> unit Lwt.t
  end

  (** Creates a new worker instance.
      Parameter [queue_size] not passed means unlimited queue. *)
  val launch :
    'kind table ->
    ?timeout:Time.System.Span.t ->
    Name.t ->
    Types.parameters ->
    (module HANDLERS with type self = 'kind t) ->
    'kind t tzresult Lwt.t

  (** Triggers a worker termination and waits for its completion.
      Cannot be called from within the handlers.  *)
  val shutdown : _ t -> unit Lwt.t

  module type BOX = sig
    type t

    val put_request : t -> 'a Request.t -> unit

    val put_request_and_wait : t -> 'a Request.t -> 'a tzresult Lwt.t
  end

  module type QUEUE = sig
    type 'a t

    val push_request_and_wait : 'q t -> 'a Request.t -> 'a tzresult Lwt.t

    val push_request : 'q t -> 'a Request.t -> unit Lwt.t

    val pending_requests : 'a t -> (Time.System.t * Request.view) list

    val pending_requests_length : 'a t -> int
  end

  module Dropbox : sig
    include BOX with type t := dropbox t
  end

  module Queue : sig
    include QUEUE with type 'a t := 'a queue t

    (** Adds a message to the queue immediately. *)
    val push_request_now : infinite queue t -> 'a Request.t -> unit
  end

  (** Detects cancellation from within the request handler to stop
      asynchronous operations. *)
  val protect :
    _ t ->
    ?on_error:(error list -> 'b tzresult Lwt.t) ->
    (unit -> 'b tzresult Lwt.t) ->
    'b tzresult Lwt.t

  (** Exports the canceler to allow cancellation of other tasks when this
      worker is shut down or when it dies. *)
  val canceler : _ t -> Lwt_canceler.t

  (** Triggers a worker termination. *)
  val trigger_shutdown : _ t -> unit

  (** Record an event in the backlog. *)
  val record_event : _ t -> Event.t -> unit

  (** Record an event and make sure it is logged. *)
  val log_event : _ t -> Event.t -> unit Lwt.t

  (** Access the internal state, once initialized. *)
  val state : _ t -> Types.state

  (** Introspect the message queue, gives the times requests were pushed. *)
  val pending_requests : _ queue t -> (Time.System.t * Request.view) list

  (** Get the running status of a worker. *)
  val status : _ t -> Worker_types.worker_status

  (** Get the request being treated by a worker.
      Gives the time the request was pushed, and the time its
      treatment started. *)
  val current_request :
    _ t -> (Time.System.t * Time.System.t * Request.view) option

  val information : _ t -> Worker_types.worker_information

  (** Lists the running workers in this group. *)
  val list : 'a table -> (Name.t * 'a t) list

  (** [find_opt table n] is [Some worker] if the [worker] is in the [table] and
      has name [n]. *)
  val find_opt : 'a table -> Name.t -> 'a t option
end

module Make
    (Name : Worker_intf.NAME)
    (Event : Worker_intf.EVENT)
    (Request : Worker_intf.REQUEST)
    (Types : Worker_intf.TYPES)
    (Logger : Worker_intf.LOGGER
                with module Event = Event
                 and type Request.view = Request.view) =
struct
  module Name = Name
  module Event = Event
  module Request = Request
  module Types = Types
  module Logger = Logger

  module Nametbl = Hashtbl.MakeSeeded (struct
    type t = Name.t

    let hash = Hashtbl.seeded_hash

    let equal = Name.equal
  end)

  let base_name = String.concat "-" Name.base

  type message = Message : 'a Request.t * 'a tzresult Lwt.u option -> message

  type 'a queue

  and bounded

  and infinite

  type dropbox

  type _ buffer_kind =
    | Queue : infinite queue buffer_kind
    | Bounded : {size : int} -> bounded queue buffer_kind
    | Dropbox : {
        merge :
          dropbox t -> any_request -> any_request option -> any_request option;
      }
        -> dropbox buffer_kind

  and any_request = Any_request : _ Request.t -> any_request

  and _ buffer =
    | Queue_buffer :
        (Time.System.t * message) Lwt_pipe.Unbounded.t
        -> infinite queue buffer
    | Bounded_buffer :
        (Time.System.t * message) Lwt_pipe.Bounded.t
        -> bounded queue buffer
    | Dropbox_buffer : (Time.System.t * message) Lwt_dropbox.t -> dropbox buffer

  and 'kind t = {
    timeout : Time.System.Span.t option;
    parameters : Types.parameters;
    mutable (* only for init *) worker : unit Lwt.t;
    mutable (* only for init *) state : Types.state option;
    buffer : 'kind buffer;
    canceler : Lwt_canceler.t;
    name : Name.t;
    id : int;
    mutable status : Worker_types.worker_status;
    mutable current_request :
      (Time.System.t * Time.System.t * Request.view) option;
    logEvent : (module Internal_event.EVENT with type t = Logger.t);
    table : 'kind table;
  }

  and 'kind table = {
    buffer_kind : 'kind buffer_kind;
    mutable last_id : int;
    instances : 'kind t Nametbl.t;
  }

  let queue_item ?u r = (Time.System.now (), Message (r, u))

  let drop_request w merge message_box request =
    try
      match
        match Lwt_dropbox.peek message_box with
        | None -> merge w (Any_request request) None
        | Some (_, Message (old, _)) ->
            Lwt.ignore_result (Lwt_dropbox.take message_box) ;
            merge w (Any_request request) (Some (Any_request old))
      with
      | None -> ()
      | Some (Any_request neu) ->
          Lwt_dropbox.put message_box (Time.System.now (), Message (neu, None))
    with Lwt_dropbox.Closed -> ()

  let drop_request_and_wait w message_box request =
    let (t, u) = Lwt.wait () in
    Lwt.catch
      (fun () ->
        Lwt_dropbox.put message_box (queue_item ~u request) ;
        t)
      (function
        | Lwt_dropbox.Closed ->
            let name = Format.asprintf "%a" Name.pp w.name in
            Lwt_result_syntax.tzfail (Closed {base = base_name; name})
        | exn ->
            (* [Lwt_dropbox.put] can only raise [Closed] which is caught above.
               We don't want to catch any other exception but we cannot use an
               incomplete pattern like we would in a [try]-[with] construct so
               we must explicitly match and re-raise [exn]. *)
            raise exn)

  module type BOX = sig
    type t

    val put_request : t -> 'a Request.t -> unit

    val put_request_and_wait : t -> 'a Request.t -> 'a tzresult Lwt.t
  end

  module type QUEUE = sig
    type 'a t

    val push_request_and_wait : 'q t -> 'a Request.t -> 'a tzresult Lwt.t

    val push_request : 'q t -> 'a Request.t -> unit Lwt.t

    val pending_requests : 'a t -> (Time.System.t * Request.view) list

    val pending_requests_length : 'a t -> int
  end

  module Dropbox = struct
    let put_request (w : dropbox t) request =
      let (Dropbox {merge}) = w.table.buffer_kind in
      let (Dropbox_buffer message_box) = w.buffer in
      drop_request w merge message_box request

    let put_request_and_wait (w : dropbox t) request =
      let (Dropbox_buffer message_box) = w.buffer in
      drop_request_and_wait w message_box request
  end

  module Queue = struct
    let push_request (type a) (w : a queue t) request =
      match w.buffer with
      | Queue_buffer message_queue ->
          Lwt_pipe.Unbounded.push message_queue (queue_item request) ;
          (* because pushing on an unbounded pipe is immediate, we return within
             Lwt explicitly for compatibility with the other case *)
          Lwt.return_unit
      | Bounded_buffer message_queue ->
          Lwt_pipe.Bounded.push message_queue (queue_item request)

    let push_request_now (w : infinite queue t) request =
      let (Queue_buffer message_queue) = w.buffer in
      if Lwt_pipe.Unbounded.is_closed message_queue then ()
      else Lwt_pipe.Unbounded.push message_queue (queue_item request)

    let push_request_and_wait (type a) (w : a queue t) request =
      match w.buffer with
      | Queue_buffer message_queue -> (
          try
            let (t, u) = Lwt.wait () in
            Lwt_pipe.Unbounded.push message_queue (queue_item ~u request) ;
            t
          with Lwt_pipe.Closed ->
            let name = Format.asprintf "%a" Name.pp w.name in
            Lwt_result_syntax.tzfail (Closed {base = base_name; name}))
      | Bounded_buffer message_queue ->
          let (t, u) = Lwt.wait () in
          Lwt.try_bind
            (fun () ->
              Lwt_pipe.Bounded.push message_queue (queue_item ~u request))
            (fun () -> t)
            (function
              | Lwt_pipe.Closed ->
                  let name = Format.asprintf "%a" Name.pp w.name in
                  Lwt_result_syntax.tzfail (Closed {base = base_name; name})
              | exn -> raise exn)

    let pending_requests (type a) (w : a queue t) =
      let peeked =
        try
          match w.buffer with
          | Queue_buffer message_queue ->
              Lwt_pipe.Unbounded.peek_all_now message_queue
          | Bounded_buffer message_queue ->
              Lwt_pipe.Bounded.peek_all_now message_queue
        with Lwt_pipe.Closed -> []
      in
      List.map
        (function (t, Message (req, _)) -> (t, Request.view req))
        peeked

    let pending_requests_length (type a) (w : a queue t) =
      let pipe_length (type a) (q : a buffer) =
        match q with
        | Queue_buffer queue -> Lwt_pipe.Unbounded.length queue
        | Bounded_buffer queue -> Lwt_pipe.Bounded.length queue
        | Dropbox_buffer _ -> 1
      in
      pipe_length w.buffer
  end

  let close (type a) (w : a t) =
    let wakeup = function
      | (_, Message (_, Some u)) ->
          let name = Format.asprintf "%a" Name.pp w.name in
          Lwt.wakeup_later
            u
            (Result_syntax.tzfail (Closed {base = base_name; name}))
      | (_, Message (_, None)) -> ()
    in
    let close_queue message_queue =
      let messages = Lwt_pipe.Bounded.pop_all_now message_queue in
      List.iter wakeup messages ;
      Lwt_pipe.Bounded.close message_queue
    in
    let close_unbounded_queue message_queue =
      let messages = Lwt_pipe.Unbounded.pop_all_now message_queue in
      List.iter wakeup messages ;
      Lwt_pipe.Unbounded.close message_queue
    in
    match w.buffer with
    | Queue_buffer message_queue -> close_unbounded_queue message_queue
    | Bounded_buffer message_queue -> close_queue message_queue
    | Dropbox_buffer message_box ->
        (try Option.iter wakeup (Lwt_dropbox.peek message_box)
         with Lwt_dropbox.Closed -> ()) ;
        Lwt_dropbox.close message_box

  let pop (type a) (w : a t) =
    let open Lwt_syntax in
    let pop_queue message_queue =
      match w.timeout with
      | None ->
          let* m = Lwt_pipe.Bounded.pop message_queue in
          return_some m
      | Some timeout ->
          Lwt_pipe.Bounded.pop_with_timeout
            (Systime_os.sleep timeout)
            message_queue
    in
    let pop_unbounded_queue message_queue =
      match w.timeout with
      | None ->
          let* m = Lwt_pipe.Unbounded.pop message_queue in
          return_some m
      | Some timeout ->
          Lwt_pipe.Unbounded.pop_with_timeout
            (Systime_os.sleep timeout)
            message_queue
    in
    match w.buffer with
    | Queue_buffer message_queue -> pop_unbounded_queue message_queue
    | Bounded_buffer message_queue -> pop_queue message_queue
    | Dropbox_buffer message_box -> (
        match w.timeout with
        | None ->
            let* m = Lwt_dropbox.take message_box in
            return_some m
        | Some timeout ->
            Lwt_dropbox.take_with_timeout (Systime_os.sleep timeout) message_box
        )

  let trigger_shutdown w = Lwt.ignore_result (Lwt_canceler.cancel w.canceler)

  let canceler {canceler; _} = canceler

  let lwt_emit w (status : Logger.status) =
    let (module LogEvent) = w.logEvent in
    let time = Time.System.now () in
    Lwt.bind
      (LogEvent.emit
         ~section:(Internal_event.Section.make_sanitized Name.base)
         (fun () -> Time.System.stamp ~time status))
      (function
        | Ok () -> Lwt.return_unit
        | Error el ->
            Format.kasprintf
              Lwt.fail_with
              "Worker_event.emit: %a"
              pp_print_trace
              el)

  let log_event w evt = lwt_emit w (Logger.WorkerEvent (evt, Event.level evt))

  let record_event w evt = Lwt.ignore_result (log_event w evt)

  module type HANDLERS = sig
    type self

    val on_launch :
      self -> Name.t -> Types.parameters -> Types.state tzresult Lwt.t

    val on_request : self -> 'a Request.t -> 'a tzresult Lwt.t

    val on_no_request : self -> unit tzresult Lwt.t

    val on_close : self -> unit Lwt.t

    val on_error :
      self ->
      Request.view ->
      Worker_types.request_status ->
      error list ->
      unit tzresult Lwt.t

    val on_completion :
      self -> 'a Request.t -> 'a -> Worker_types.request_status -> unit Lwt.t
  end

  let create_table buffer_kind =
    {buffer_kind; last_id = 0; instances = Nametbl.create ~random:true 10}

  let worker_loop (type kind) handlers (w : kind t) =
    let (module Handlers : HANDLERS with type self = kind t) = handlers in
    let do_close errs =
      let open Lwt_syntax in
      let t0 =
        match w.status with
        | Running t0 -> t0
        | Launching _ | Closing _ | Closed _ -> assert false
      in
      w.status <- Closing (t0, Time.System.now ()) ;
      close w ;
      let* () = Error_monad.cancel_with_exceptions w.canceler in
      w.status <- Closed (t0, Time.System.now (), errs) ;
      let* () = Handlers.on_close w in
      Nametbl.remove w.table.instances w.name ;
      w.state <- None ;
      return_unit
    in
    let rec loop () =
      (* The call to [protect] here allows the call to [pop] (responsible
         for fetching the next request) to be canceled by the use of the
         [canceler].

         These cancellations cannot affect the processing of ongoing requests.
         This is due to the limited scope of the argument of [protect]. As a
         result, ongoing requests are never canceled by this mechanism.

         In the case when the [canceler] is canceled whilst a request is being
         processed, the processing eventually resolves, at which point a
         recursive call to this [loop] at which point this call to [protect]
         fails immediately with [Canceled]. *)
      Lwt.bind
        Lwt_result_syntax.(
          let* popped =
            protect ~canceler:w.canceler (fun () -> Lwt_result.ok @@ pop w)
          in
          match popped with
          | None -> Handlers.on_no_request w
          | Some (pushed, Message (request, u)) -> (
              let current_request = Request.view request in
              let treated = Time.System.now () in
              w.current_request <- Some (pushed, treated, current_request) ;
              match u with
              | None ->
                  let* res = Handlers.on_request w request in
                  let completed = Time.System.now () in
                  w.current_request <- None ;
                  let status = Worker_types.{pushed; treated; completed} in
                  let*! () = Handlers.on_completion w request res status in
                  let*! () =
                    lwt_emit w (Request (current_request, status, None))
                  in
                  return_unit
              | Some u ->
                  (* [res] is a result. But the side effect [wakeup]
                     needs to happen regardless of success (Ok) or failure
                     (Error). To that end, we treat it locally like a regular
                     promise (which happens to carry a [result]) within the Lwt
                     monad. *)
                  let*! res = Handlers.on_request w request in
                  Lwt.wakeup_later u res ;
                  let*? res = res in
                  let completed = Time.System.now () in
                  let status = Worker_types.{pushed; treated; completed} in
                  w.current_request <- None ;
                  let*! () = Handlers.on_completion w request res status in
                  let*! () =
                    lwt_emit w (Request (current_request, status, None))
                  in
                  return_unit))
        Lwt_syntax.(
          function
          | Ok () -> loop ()
          | Error (Canceled :: _)
          | Error (Exn Lwt.Canceled :: _)
          | Error (Exn Lwt_pipe.Closed :: _)
          | Error (Exn Lwt_dropbox.Closed :: _) ->
              let* () = lwt_emit w Terminated in
              do_close None
          | Error errs -> (
              let* r =
                match w.current_request with
                | Some (pushed, treated, request) ->
                    let completed = Time.System.now () in
                    w.current_request <- None ;
                    Handlers.on_error
                      w
                      request
                      Worker_types.{pushed; treated; completed}
                      errs
                | None -> assert false
              in
              match r with
              | Ok () -> loop ()
              | Error (Timeout :: _ as errs) ->
                  let* () = lwt_emit w Terminated in
                  do_close (Some errs)
              | Error errs ->
                  let* () = lwt_emit w (Crashed errs) in
                  do_close (Some errs)))
    in
    loop ()

  let launch :
      type kind.
      kind table ->
      ?timeout:Time.System.Span.t ->
      Name.t ->
      Types.parameters ->
      (module HANDLERS with type self = kind t) ->
      kind t tzresult Lwt.t =
   fun table ?timeout name parameters (module Handlers) ->
    let name_s = Format.asprintf "%a" Name.pp name in
    let full_name =
      if name_s = "" then base_name
      else Format.asprintf "%s_%s" base_name name_s
    in
    if Nametbl.mem table.instances name then
      invalid_arg
        (Format.asprintf "Worker.launch: duplicate worker %s" full_name)
    else
      let id =
        table.last_id <- table.last_id + 1 ;
        table.last_id
      in
      let id_name =
        if name_s = "" then base_name else Format.asprintf "%s_%d" base_name id
      in
      let canceler = Lwt_canceler.create () in
      let buffer : kind buffer =
        match table.buffer_kind with
        | Queue -> Queue_buffer (Lwt_pipe.Unbounded.create ())
        | Bounded {size} ->
            Bounded_buffer
              (Lwt_pipe.Bounded.create
                 ~max_size:size
                 ~compute_size:(fun _ -> 1)
                 ())
        | Dropbox _ -> Dropbox_buffer (Lwt_dropbox.create ())
      in
      let w =
        {
          parameters;
          name;
          canceler;
          table;
          buffer;
          state = None;
          id;
          worker = Lwt.return_unit;
          timeout;
          current_request = None;
          logEvent = (module Logger.LogEvent);
          status = Launching (Time.System.now ());
        }
      in
      Nametbl.add table.instances name w ;
      let open Lwt_result_syntax in
      let started = if id_name = base_name then None else Some name_s in
      let*! () = lwt_emit w (Started started) in
      let* state = Handlers.on_launch w name parameters in
      w.status <- Running (Time.System.now ()) ;
      w.state <- Some state ;
      w.worker <-
        Lwt_utils.worker
          full_name
          ~on_event:Internal_event.Lwt_worker_event.on_event
          ~run:(fun () -> worker_loop (module Handlers) w)
          ~cancel:(fun () -> Error_monad.cancel_with_exceptions w.canceler) ;
      return w

  let shutdown w =
    (* The actual cancellation ([Lwt_canceler.cancel w.canceler]) resolves
       immediately because no hooks are registered on the canceler. However, the
       worker ([w.worker]) resolves only once the ongoing request has resolved
       (if any) and some clean-up operations have completed. *)
    let open Lwt_syntax in
    let* () = lwt_emit w Triggering_shutdown in
    let* () = Error_monad.cancel_with_exceptions w.canceler in
    w.worker

  let state w =
    match (w.state, w.status) with
    | (None, Launching _) ->
        invalid_arg
          (Format.asprintf
             "Worker.state (%s[%a]): state called before worker was initialized"
             base_name
             Name.pp
             w.name)
    | (None, (Closing _ | Closed _)) ->
        invalid_arg
          (Format.asprintf
             "Worker.state (%s[%a]): state called after worker was terminated"
             base_name
             Name.pp
             w.name)
    | (None, _) -> assert false
    | (Some state, _) -> state

  let pending_requests q = Queue.pending_requests q

  let status {status; _} = status

  let current_request {current_request; _} = current_request

  let information (type a) (w : a t) =
    {
      Worker_types.instances_number = Nametbl.length w.table.instances;
      wstatus = w.status;
      queue_length =
        (match w.buffer with
        | Queue_buffer pipe -> Lwt_pipe.Unbounded.length pipe
        | Bounded_buffer pipe -> Lwt_pipe.Bounded.length pipe
        | Dropbox_buffer _ -> 1);
    }

  let list {instances; _} =
    Nametbl.fold (fun n w acc -> (n, w) :: acc) instances []

  let find_opt {instances; _} = Nametbl.find instances

  (* TODO? add a list of cancelers for nested protection ? *)
  let protect {canceler; _} ?on_error f = protect ?on_error ~canceler f

  let () =
    Internal_event.register_section
      (Internal_event.Section.make_sanitized Name.base)
end
