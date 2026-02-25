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

module type T = sig
  module Name : Worker_intf.NAME

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

  type callback

  type scope

  type metadata = {scope : scope option}

  type 'a message_error =
    | Closed of error list option
    | Request_error of 'a
    | Any of exn

  (** Supported kinds of internal buffers. *)
  type _ buffer_kind =
    | Queue : infinite queue buffer_kind
    | Bounded : {size : int} -> bounded queue buffer_kind
    | Dropbox : {
        merge :
          dropbox t -> any_request -> any_request option -> any_request option;
      }
        -> dropbox buffer_kind
    | Callback : (unit -> any_request Lwt.t) -> callback buffer_kind

  and any_request = Any_request : _ Request.t * metadata -> any_request

  (** Create a table of workers. *)
  val create_table : 'kind buffer_kind -> 'kind table

  (** The callback handlers specific to each worker instance. *)
  module type HANDLERS = sig
    (** Placeholder replaced with {!t} with the right parameters
        provided by the type of buffer chosen at {!launch}.*)
    type self

    type launch_error

    (** Builds the initial internal state of a worker at launch.
        It is possible to initialize the message queue.
        Of course calling {!state} will fail at that point. *)
    val on_launch :
      self ->
      Name.t ->
      Types.parameters ->
      (Types.state, launch_error) result Lwt.t

    (** The main request processor, i.e. the body of the event loop. *)
    val on_request :
      self ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error) result Lwt.t

    (** Called when no request has been made before the timeout, if
        the parameter has been passed to {!launch}. *)
    val on_no_request : self -> unit Lwt.t

    (** A function called when terminating a worker. *)
    val on_close : self -> unit Lwt.t

    (** A function called at the end of the worker loop in case of an abnormal
        error. This function can handle the error by returning [Ok `Continue]
        for the worker loop to continue and take the next request, [Ok
        `Shutdown] and stop the worker loop without crashing, or leave the
        default unexpected error behaviour by returning its parameter. A
        possibility is to handle the error for ad-hoc logging, and still use
        {!trigger_shutdown} to kill the worker. *)
    val on_error :
      self ->
      Worker_types.request_status ->
      ('a, 'request_error) Request.t ->
      'request_error ->
      [`Continue | `Shutdown] tzresult Lwt.t

    (** A function called at the end of the worker loop in case of a
        successful treatment of the current request. *)
    val on_completion :
      self ->
      ('a, 'request_error) Request.t ->
      'a ->
      Worker_types.request_status ->
      unit Lwt.t
  end

  (** Creates a new worker instance.
      Parameter [queue_size] not passed means unlimited queue. *)
  val launch :
    'kind table ->
    ?timeout:Time.System.Span.t ->
    Name.t ->
    Types.parameters ->
    (module HANDLERS
       with type self = 'kind t
        and type launch_error = 'launch_error) ->
    ('kind t, 'launch_error) result Lwt.t

  (** Triggers a worker termination and waits for its completion.
      Cannot be called from within the handlers.  *)
  val shutdown : _ t -> unit Lwt.t

  (** Waits for completion, but doesn't trigger the shutdown. *)
  val wait_for_completion : _ t -> unit Lwt.t

  module type BOX = sig
    type t

    val put_request : t -> ('a, 'request_error) Request.t -> unit

    val put_request_and_wait :
      t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Lwt.t
  end

  module type QUEUE = sig
    type 'a t

    val push_request_and_wait :
      'q t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Lwt.t

    val push_request : 'q t -> ('a, 'request_error) Request.t -> bool Lwt.t

    val pending_requests : 'a t -> (Time.System.t * Request.view) list

    val pending_requests_length : 'a t -> int
  end

  module Dropbox : sig
    include BOX with type t := dropbox t
  end

  module Queue : sig
    include QUEUE with type 'a t := 'a queue t

    (** Adds a message to the queue immediately. *)
    val push_request_now :
      infinite queue t -> ('a, 'request_error) Request.t -> unit
  end

  (** Exports the canceler to allow cancellation of other tasks when this
      worker is shut down or when it dies. *)
  val canceler : _ t -> Lwt_canceler.t

  (** Triggers a worker termination. *)
  val trigger_shutdown : _ t -> unit

  (** Returns true if the worker is shutting down. *)
  val is_shutting_down : _ t -> bool

  (** Access the internal state, once initialized. *)
  val state : _ t -> Types.state

  (** Access the internal state if available. *)
  val state_opt : _ t -> Types.state option

  val with_state :
    _ t -> (Types.state -> (unit, 'b) result Lwt.t) -> (unit, 'b) result Lwt.t

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

module Make_internal
    (Name : Worker_intf.NAME)
    (Request : Worker_intf.REQUEST)
    (Types : Worker_intf.TYPES)
    (Worker_events :
      Worker_events.S
        with type view = Request.view
         and type critical_error = tztrace) =
struct
  module Name = Name
  module Request = Request
  module Types = Types

  module Nametbl = Hashtbl.MakeSeeded (struct
    type t = Name.t

    (* See [src/lib_base/tzPervasives.ml] for an explanation *)
    [@@@ocaml.warning "-32"]

    let hash = Hashtbl.seeded_hash

    let seeded_hash = Hashtbl.seeded_hash

    [@@@ocaml.warning "+32"]

    let equal = Name.equal
  end)

  let base_name = String.concat "-" Name.base

  type scope = Opentelemetry.Scope.t

  type metadata = {scope : scope option}

  type 'a message_error =
    | Closed of error list option
    | Request_error of 'a
    | Any of exn

  type message =
    | Message :
        ('a, 'b) Request.t
        * metadata
        * ('a, 'b message_error) result Lwt.u option
        -> message

  type 'a queue

  and bounded

  and infinite

  type dropbox

  type callback

  type _ buffer_kind =
    | Queue : infinite queue buffer_kind
    | Bounded : {size : int} -> bounded queue buffer_kind
    | Dropbox : {
        merge :
          dropbox t -> any_request -> any_request option -> any_request option;
      }
        -> dropbox buffer_kind
    | Callback : (unit -> any_request Lwt.t) -> callback buffer_kind

  and any_request = Any_request : _ Request.t * metadata -> any_request

  and _ buffer =
    | Queue_buffer :
        (Time.System.t * message) Lwt_pipe.Unbounded.t
        -> infinite queue buffer
    | Bounded_buffer :
        (Time.System.t * message) Lwt_pipe.Bounded.t
        -> bounded queue buffer
    | Dropbox_buffer : (Time.System.t * message) Lwt_dropbox.t -> dropbox buffer
    | Callback_buffer : (unit -> any_request Lwt.t) -> callback buffer

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
    table : 'kind table;
  }

  and 'kind table = {
    buffer_kind : 'kind buffer_kind;
    mutable last_id : int;
    instances : 'kind t Nametbl.t;
  }

  let extract_status_errors w =
    match w.status with
    | Worker_types.Launching _ | Running _ | Closing _ -> None
    | Closed (_, _, errs) -> errs

  let queue_item ~metadata ?u r = (Time.System.now (), Message (r, metadata, u))

  let drop_request w merge message_box request metadata =
    try
      match
        match Lwt_dropbox.peek message_box with
        | None -> merge w (Any_request (request, metadata)) None
        | Some (_, Message (old, old_metadata, _)) ->
            Lwt.ignore_result (Lwt_dropbox.take message_box) ;
            merge
              w
              (Any_request (request, metadata))
              (Some (Any_request (old, old_metadata)))
      with
      | None -> ()
      | Some (Any_request (neu, neu_metadata)) ->
          Lwt_dropbox.put
            message_box
            (Time.System.now (), Message (neu, neu_metadata, None))
    with Lwt_dropbox.Closed -> ()

  let drop_request_and_wait w message_box request metadata =
    let t, u = Lwt.wait () in
    Lwt.catch
      (fun () ->
        Lwt_dropbox.put message_box (queue_item ~metadata ~u request) ;
        t)
      (function
        | Lwt_dropbox.Closed ->
            Lwt.return_error (Closed (extract_status_errors w))
        | exn ->
            (* [Lwt_dropbox.put] can only raise [Closed] which is caught above. *)
            Lwt.return_error (Any exn))

  module type BOX = sig
    type t

    val put_request : t -> ('a, 'request_error) Request.t -> unit

    val put_request_and_wait :
      t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Lwt.t
  end

  module type QUEUE = sig
    type 'a t

    val push_request_and_wait :
      'q t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Lwt.t

    val push_request : 'q t -> ('a, 'request_error) Request.t -> bool Lwt.t

    val pending_requests : 'a t -> (Time.System.t * Request.view) list

    val pending_requests_length : 'a t -> int
  end

  let make_metadata () =
    (* This pattern is equivalent to:
       ```
       let scope =
         if "ppx is enabled" then
           Opentelemetry.Scope.get_ambient_scope ()
         else
           None
       ```
    *)
    let scope =
      (None
      [@profiler.overwrite
        {driver_ids = [Opentelemetry]}
          (Opentelemetry.Scope.get_ambient_scope ())])
    in
    {scope}

  module Dropbox = struct
    let put_request (w : dropbox t) request =
      let (Dropbox {merge}) = w.table.buffer_kind in
      let (Dropbox_buffer message_box) = w.buffer in
      let metadata = make_metadata () in
      drop_request w merge message_box request metadata

    let put_request_and_wait (w : dropbox t) request =
      let (Dropbox_buffer message_box) = w.buffer in
      let metadata = make_metadata () in
      drop_request_and_wait w message_box request metadata
  end

  module Queue = struct
    let push_request (type a) (w : a queue t) request =
      let metadata = make_metadata () in
      match w.buffer with
      | Queue_buffer message_queue ->
          if Lwt_pipe.Unbounded.is_closed message_queue then Lwt.return_false
          else (
            Lwt_pipe.Unbounded.push message_queue (queue_item ~metadata request) ;
            (* because pushing on an unbounded pipe is immediate, we return within
               Lwt explicitly for compatibility with the other case *)
            Lwt.return_true)
      | Bounded_buffer message_queue ->
          if Lwt_pipe.Bounded.is_closed message_queue then Lwt.return_false
          else
            let open Lwt_syntax in
            let* () =
              Lwt_pipe.Bounded.push message_queue (queue_item ~metadata request)
            in
            Lwt.return_true

    let push_request_now (w : infinite queue t) request =
      let metadata = make_metadata () in
      let (Queue_buffer message_queue) = w.buffer in
      if Lwt_pipe.Unbounded.is_closed message_queue then ()
      else Lwt_pipe.Unbounded.push message_queue (queue_item ~metadata request)

    let push_request_and_wait (type a) (w : a queue t) request =
      let metadata = make_metadata () in
      match w.buffer with
      | Queue_buffer message_queue -> (
          try
            let t, u = Lwt.wait () in
            Lwt_pipe.Unbounded.push
              message_queue
              (queue_item ~metadata ~u request) ;
            t
          with Lwt_pipe.Closed ->
            Lwt.return_error (Closed (extract_status_errors w)))
      | Bounded_buffer message_queue ->
          let t, u = Lwt.wait () in
          Lwt.try_bind
            (fun () ->
              Lwt_pipe.Bounded.push
                message_queue
                (queue_item ~metadata ~u request))
            (fun () -> t)
            (function
              | Lwt_pipe.Closed ->
                  Lwt.return_error (Closed (extract_status_errors w))
              | exn -> Lwt.return_error (Any exn))

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
        (function t, Message (req, _, _) -> (t, Request.view req))
        peeked

    let pending_requests_length (type a) (w : a queue t) =
      let pipe_length (type a) (q : a buffer) =
        match q with
        | Queue_buffer queue -> Lwt_pipe.Unbounded.length queue
        | Bounded_buffer queue -> Lwt_pipe.Bounded.length queue
        | Dropbox_buffer _ -> 1
        | Callback_buffer _ -> 1
      in
      pipe_length w.buffer
  end

  let close (type a) (w : a t) =
    let wakeup = function
      | _, Message (_, _, Some u) ->
          Lwt.wakeup_later u (Error (Closed (extract_status_errors w)))
      | _, Message (_, _, None) -> ()
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
    | Callback_buffer _ -> ()

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
    let pop_from_callback (read : unit -> any_request Lwt.t) =
      let take () =
        let* (Any_request (request, metadata)) = read () in
        let time = Time.System.now () in
        return_some (time, Message (request, metadata, None))
      in
      match w.timeout with
      | None -> take ()
      | Some timeout ->
          let sleeper =
            let* () = Systime_os.sleep timeout in
            Lwt.return_none
          in
          Lwt.pick [sleeper; take ()]
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
    | Callback_buffer read -> pop_from_callback read

  let trigger_shutdown w = Lwt.ignore_result (Lwt_canceler.cancel w.canceler)

  let canceler {canceler; _} = canceler

  let is_shutting_down w = Lwt_canceler.canceling w.canceler

  module type HANDLERS = sig
    type self

    type launch_error

    val on_launch :
      self ->
      Name.t ->
      Types.parameters ->
      (Types.state, launch_error) result Lwt.t

    val on_request :
      self ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error) result Lwt.t

    val on_no_request : self -> unit Lwt.t

    val on_close : self -> unit Lwt.t

    val on_error :
      self ->
      Worker_types.request_status ->
      ('a, 'request_error) Request.t ->
      'request_error ->
      [`Shutdown | `Continue] tzresult Lwt.t

    val on_completion :
      self ->
      ('a, 'request_error) Request.t ->
      'a ->
      Worker_types.request_status ->
      unit Lwt.t
  end

  let create_table buffer_kind =
    {buffer_kind; last_id = 0; instances = Nametbl.create ~random:true 10}

  let close (type kind) handlers (w : kind t) errs =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3264
              close should be called only once in a worker lifetime *)
    let (module Handlers : HANDLERS with type self = kind t) = handlers in
    let open Lwt_syntax in
    match w.status with
    (* Launching is not accessible from here, as the only occurrence
       in form [launch] and happens before the call to [worker_loop] *)
    | Closed _ | Closing _ | Launching _ -> Lwt.return_unit
    | Running t0 ->
        w.status <- Closing (t0, Time.System.now ()) ;
        close w ;
        let* () = Error_monad.cancel_with_exceptions w.canceler in
        w.status <- Closed (t0, Time.System.now (), errs) ;
        let* () = Handlers.on_close w in
        Nametbl.remove w.table.instances w.name ;
        w.state <- None ;
        return_unit

  let worker_loop (type kind) handlers (w : kind t) =
    let (module Handlers : HANDLERS with type self = kind t) = handlers in
    let open Lwt_syntax in
    let rec loop () =
      let* popped = protect_result (fun () -> pop w) in
      match popped with
      | Error exn -> raise exn
      | Ok None ->
          let* () = Handlers.on_no_request w in
          loop ()
      | Ok (Some (pushed, Message (request, {scope = _scope}, u))) -> (
          (let current_request = Request.view request in
           let treated = Time.System.now () in
           w.current_request <- Some (pushed, treated, current_request) ;
           let* r =
             match u with
             | None -> (
                 let open Lwt_result_syntax in
                 let*! res = Handlers.on_request w request in
                 match res with
                 | Error err -> Lwt.return_error err
                 | Ok res ->
                     let completed = Time.System.now () in
                     w.current_request <- None ;
                     let status = Worker_types.{pushed; treated; completed} in
                     let*! () = Handlers.on_completion w request res status in
                     let*! () =
                       Worker_events.(emit request_no_errors)
                         (current_request, status)
                     in
                     return_unit)
             | Some u -> (
                 (* [res] is a result. But the side effect [wakeup]
                    needs to happen regardless of success (Ok) or failure
                    (Error). To that end, we treat it locally like a regular
                    promise (which happens to carry a [result]) within the Lwt
                    monad. *)
                 let* res = Handlers.on_request w request in
                 match res with
                 | Error err ->
                     Lwt.wakeup_later u (Error (Request_error err)) ;
                     Lwt.return (Error err)
                 | Ok res ->
                     Lwt.wakeup_later u (Ok res) ;
                     let completed = Time.System.now () in
                     let status = Worker_types.{pushed; treated; completed} in
                     w.current_request <- None ;
                     let* () = Handlers.on_completion w request res status in
                     let* () =
                       Worker_events.(emit request_no_errors)
                         (current_request, status)
                     in
                     return (Ok ()))
           in
           match r with
           | Ok () -> loop ()
           | Error err -> (
               let* r =
                 match w.current_request with
                 | Some (pushed, treated, _request_view) ->
                     let completed = Time.System.now () in
                     w.current_request <- None ;
                     Handlers.on_error
                       w
                       Worker_types.{pushed; treated; completed}
                       request
                       err
                 | None -> assert false
               in
               match r with
               | Ok `Continue -> loop ()
               | Ok `Shutdown ->
                   let* () = Worker_events.(emit terminated) () in
                   close handlers w None
               | Error errs ->
                   let* () = Worker_events.(emit crashed) errs in
                   close handlers w (Some errs)))
          [@profiler.wrap_f
            {driver_ids = [Opentelemetry]}
              (Tezos_profiler_complex_backends.Opentelemetry_profiler
               .update_scope
                 _scope)])
    in
    let* r = protect_result ~canceler:w.canceler (fun () -> loop ()) in
    match r with
    | Ok () -> Lwt.return_unit
    | Error (Lwt.Canceled | Lwt_pipe.Closed | Lwt_dropbox.Closed) ->
        (* Explicit cancellation or closed buffer *)
        let* () = Worker_events.(emit terminated) () in
        close handlers w None
    | Error
        (Unix.Unix_error
           ((Unix.EPIPE | Unix.ECONNRESET | Unix.ECONNREFUSED), _, _))
      when is_shutting_down w ->
        (* Only suppress socket errors during shutdown *)
        let* () = Worker_events.(emit terminated) () in
        close handlers w None
    | Error exn ->
        let* () = Worker_events.(emit crashed) [Exn exn] in
        raise exn

  let launch : type kind launch_error.
      kind table ->
      ?timeout:Time.System.Span.t ->
      Name.t ->
      Types.parameters ->
      (module HANDLERS
         with type self = kind t
          and type launch_error = launch_error) ->
      (kind t, launch_error) result Lwt.t =
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
        | Callback read -> Callback_buffer read
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
          status = Launching (Time.System.now ());
        }
      in
      Nametbl.add table.instances name w ;
      let open Lwt_result_syntax in
      let*! () =
        if id_name = base_name then Worker_events.(emit started) ()
        else Worker_events.(emit started_for) name_s
      in
      let* state = Handlers.on_launch w name parameters in
      w.status <- Running (Time.System.now ()) ;
      w.state <- Some state ;
      w.worker <-
        Lwt_utils.worker
          full_name
          ~on_event:Internal_event.Lwt_worker_logger.on_event
          ~run:(fun () -> worker_loop (module Handlers) w)
          ~cancel:(fun () -> Error_monad.cancel_with_exceptions w.canceler) ;
      return w

  let shutdown w =
    (* The actual cancellation ([Lwt_canceler.cancel w.canceler]) resolves
       immediately because no hooks are registered on the canceler. *)
    let open Lwt_syntax in
    let* () = Worker_events.(emit triggering_shutdown) () in
    let* () = Error_monad.cancel_with_exceptions w.canceler in
    w.worker

  let wait_for_completion w = w.worker

  let state w =
    match (w.state, w.status) with
    | None, Launching _ ->
        invalid_arg
          (Format.asprintf
             "Worker.state (%s[%a]): state called before worker was initialized"
             base_name
             Name.pp
             w.name)
    | None, (Closing _ | Closed _) ->
        invalid_arg
          (Format.asprintf
             "Worker.state (%s[%a]): state called after worker was terminated"
             base_name
             Name.pp
             w.name)
    | None, _ -> assert false
    | Some state, _ -> state

  let state_opt w = w.state

  let with_state :
      _ t -> (Types.state -> (unit, 'b) result Lwt.t) -> (unit, 'b) result Lwt.t
      =
   fun w f ->
    match w.state with
    | Some state -> f state
    | None -> Lwt_result_syntax.return_unit

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
        | Dropbox_buffer _ -> 1
        | Callback_buffer _ -> 1);
    }

  let list {instances; _} =
    Nametbl.fold (fun n w acc -> (n, w) :: acc) instances []

  let find_opt {instances; _} = Nametbl.find instances

  let () =
    Internal_event.register_section
      (Internal_event.Section.make_sanitized Name.base)
end

module MakeGroup (Name : Worker_intf.NAME) (Request : Worker_intf.REQUEST) =
struct
  module Events =
    Worker_events.Make (Name) (Request)
      (struct
        type t = tztrace

        let encoding = Error_monad.trace_encoding

        let pp = Error_monad.pp_print_trace
      end)

  module MakeWorker (Types : Worker_intf.TYPES) = struct
    include Make_internal (Name) (Request) (Types) (Events)
  end
end

module MakeSingle
    (Name : Worker_intf.NAME)
    (Request : Worker_intf.REQUEST)
    (Types : Worker_intf.TYPES) =
struct
  module WG = MakeGroup (Name) (Request)
  include WG.MakeWorker (Types)
end
