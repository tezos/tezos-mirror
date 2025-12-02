(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2018 Dynamic Ledger Solutions <contact@tezos.com> *)
(* SPDX-FileCopyrightText: 2018-2025 Nomadic Labs <contact@nomadic-labs.com> *)
(*                                                                           *)
(*****************************************************************************)

module Eio = struct
  include Eio

  module type MutexS = sig
    type t = Mutex.t

    exception Poisoned of exn

    val create : unit -> t [@@ocaml.warning "-32"]

    val with_lock_rw : protect:bool -> t -> (unit -> 'a) -> 'a
    [@@ocaml.warning "-32"]
  end

  module Mutex : MutexS = struct
    include Mutex

    let with_lock_rw = use_rw
  end
end

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

  and any_request = Any_request : _ Request.t * metadata -> any_request

  (** Create a table of workers. *)
  val create_table : 'kind buffer_kind -> 'kind table

  (** The callback handlers specific to each worker instance. *)
  module type HANDLERS = sig
    (** Placeholder replaced with {!t} with the right parameters
        provided by the type of buffer chosen at {!launch}.*)
    type self

    (** The type of errors happening when launching an instance of a worker.  *)
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

    (** A function called at the end of the worker loop in case of an error. One
        can first log the incoming error. Then, the error can be filtered out by
        returning [return_unit] and the worker execution continues, or the error
        can be propagated through a [tzresult], making the worker crash. *)
    val on_error :
      self ->
      Worker_types.request_status ->
      ('a, 'request_error) Request.t ->
      'request_error ->
      unit tzresult Lwt.t

    (** A function called at the end of the worker loop in case of a
        successful treatment of the current request. *)
    val on_completion :
      self ->
      ('a, 'request_error) Request.t ->
      'a ->
      Worker_types.request_status ->
      unit Lwt.t
  end

  (** Creates a new worker instance. *)
  val launch :
    'kind table ->
    ?timeout:Time.System.Span.t ->
    ?domains:int ->
    Name.t ->
    Types.parameters ->
    (module HANDLERS
       with type self = 'kind t
        and type launch_error = 'launch_error) ->
    ('kind t, 'launch_error) result Lwt.t

  (** Triggers a worker termination and waits for its completion.
      Cannot be called from within the handlers.  *)
  val shutdown : _ t -> unit Lwt.t

  val shutdown_eio : _ t -> unit

  module type BOX = sig
    type t

    val put_request : t -> ('a, 'request_error) Request.t -> unit

    val put_request_eio : t -> ('a, 'request_error) Request.t -> unit

    val put_request_and_wait :
      t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Lwt.t

    val put_request_and_wait_eio :
      t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Eio.Promise.t
  end

  module type QUEUE = sig
    type 'a t

    val push_request_and_wait :
      'q t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Lwt.t

    val push_request_and_wait_eio :
      'q t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Eio.Promise.t

    val push_request : 'q t -> ('a, 'request_error) Request.t -> bool Lwt.t

    val push_request_eio : 'q t -> ('a, 'request_error) Request.t -> bool

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

    val push_request_now_eio :
      infinite queue t -> ('a, 'request_error) Request.t -> unit
  end

  (** Exports the canceler to allow cancellation of other tasks when this
      worker is shutdown or when it dies. *)
  val canceler : _ t -> Lwt_canceler.t

  (** Triggers a worker termination. *)
  val trigger_shutdown : _ t -> unit

  (** Access the internal state, once initialized. *)
  val state : _ t -> Types.state

  val with_state :
    _ t ->
    (Types.state -> (unit, 'request_error) result Lwt.t) ->
    (unit, 'request_error) result Lwt.t

  val with_state_eio :
    _ t ->
    (Types.state -> (unit, 'request_error) result) ->
    (unit, 'request_error) result

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

  module type EIO_HANDLERS = sig
    type self

    type launch_error

    val on_launch :
      self -> Name.t -> Types.parameters -> (Types.state, launch_error) result

    val on_request :
      self -> ('a, 'request_error) Request.t -> ('a, 'request_error) result

    val on_no_request : self -> unit

    val on_close : self -> unit

    val on_error :
      self ->
      Worker_types.request_status ->
      ('a, 'request_error) Request.t ->
      'request_error ->
      unit tzresult

    val on_completion :
      self ->
      ('a, 'request_error) Request.t ->
      'a ->
      Worker_types.request_status ->
      unit
  end

  module MakeEIO (H : HANDLERS) :
    EIO_HANDLERS with type self = H.self and type launch_error = H.launch_error

  val launch_eio :
    'kind table ->
    ?timeout:Time.System.Span.t ->
    ?domains:int ->
    name:Name.t ->
    Types.parameters ->
    (module EIO_HANDLERS
       with type self = 'kind t
        and type launch_error = 'launch_error) ->
    ('kind t, 'launch_error) result
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
    | Message : {
        request : ('a, 'request_error) Request.t;
        resolver :
          ('a, 'request_error message_error) Result.t Eio.Promise.u option;
        metadata : metadata;
        timestamp : Time.System.t;
      }
        -> message
    | Shutdown : message (* Appears when a worker shutdown is happening *)

  type 'a queue

  and bounded

  and infinite

  type dropbox

  type any_request = Any_request : _ Request.t * metadata -> any_request

  type _ buffer_kind =
    | Queue : infinite queue buffer_kind
    | Bounded : {size : int} -> bounded queue buffer_kind
    | Dropbox : {
        merge :
          dropbox t -> any_request -> any_request option -> any_request option;
      }
        -> dropbox buffer_kind

  and _ buffer =
    | Queue_buffer : message Eio.Stream.t -> infinite queue buffer
    | Bounded_buffer : message Eio.Stream.t -> bounded queue buffer
    | Dropbox_buffer : message Eio.Stream.t -> dropbox buffer

  and 'kind t = {
    mutex : Eio.Mutex.t;
    timeout : Time.System.Span.t option;
    parameters : Types.parameters;
    buffer : 'kind buffer;
    mutable stream_explorer : (Ptime.t * Request.view) list;
    (* Gives an approximate view of the requests in the buffer. Use with
       care. *)
    mutable state : Types.state option;
    mutable status : Worker_types.worker_status;
    mutable current_request :
      (Time.System.t * Time.System.t * Request.view) option;
    canceler : Lwt_canceler.t; (* Used for compatibility with Lwt. *)
    name : Name.t;
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

  let resolve_error : ('a, 'b) result Eio.Promise.u option -> 'b -> unit =
   fun resolver error ->
    match resolver with
    | Some resolver -> Eio.Promise.resolve_error resolver error
    | None -> ()

  let resolve : ('a, 'b) result Eio.Promise.u option -> ('a, 'b) result -> unit
      =
   fun resolver res ->
    match resolver with
    | Some resolver -> Eio.Promise.resolve resolver res
    | None -> ()

  let take (type a) (t : a t) =
    match t.buffer with
    | Queue_buffer q -> Eio.Stream.take q
    | Bounded_buffer q -> Eio.Stream.take q
    | Dropbox_buffer q -> Eio.Stream.take q

  (** [take t] returns the next item from the queue of the worker.
     If no item is available, it waits until there is one. If some timeout
     has been specified for the worker, returns [None] if no message
     arrived. *)
  let take t =
    match t.timeout with
    | None -> Some (take t)
    | Some timeout -> (
        match
          Eio.Time.with_timeout
            (Tezos_base_unix.Event_loop.env_exn ())#clock
            (Ptime.Span.to_float_s timeout)
            (fun () -> Ok (take t))
        with
        | Error `Timeout -> None
        | Ok m -> Some m)

  (** [take_nonblocking t] is like [take], but returns [None] instead of
      waiting if no message is available. *)
  let take_nonblocking (type a) (t : a t) =
    match t.buffer with
    | Queue_buffer q -> Eio.Stream.take_nonblocking q
    | Bounded_buffer q -> Eio.Stream.take_nonblocking q
    | Dropbox_buffer q -> Eio.Stream.take_nonblocking q

  let add (type a) (t : a t) (m : message) =
    match t.buffer with
    | Queue_buffer q -> Eio.Stream.add q m
    | Bounded_buffer q -> Eio.Stream.add q m
    | Dropbox_buffer q -> Eio.Stream.add q m

  type exn += Request_exn

  let state w =
    match (w.state, w.status) with
    | None, Launching _ ->
        invalid_arg
          (Format.asprintf
             "Worker.state ([%a]): state called before worker was initialized"
             Name.pp
             w.name)
    | None, (Closing _ | Closed _) ->
        invalid_arg
          (Format.asprintf
             "Worker.state ([%a]): state called after worker was terminated"
             Name.pp
             w.name)
    | None, _ -> assert false
    | Some state, _ -> state

  let with_state : type a.
      a t ->
      (Types.state -> (unit, 'request_error) result Lwt.t) ->
      (unit, 'request_error) result Lwt.t =
   fun w f ->
    match w.state with None -> Lwt.return (Ok ()) | Some state -> f state

  let with_state_eio : type a.
      a t ->
      (Types.state -> (unit, 'request_error) result) ->
      (unit, 'request_error) result =
   fun w f -> match w.state with None -> Ok () | Some state -> f state

  let pending_requests w = w.stream_explorer

  let status w = w.status

  let current_request w = w.current_request

  let information (type a) (w : a t) =
    {
      Worker_types.instances_number = Nametbl.length w.table.instances;
      wstatus = w.status;
      queue_length =
        (match w.buffer with
        | Queue_buffer q -> Eio.Stream.length q
        | Bounded_buffer q -> Eio.Stream.length q
        | Dropbox_buffer q -> Eio.Stream.length q);
    }

  let list {instances; _} =
    Nametbl.fold (fun k v acc -> (k, v) :: acc) instances []

  let find_opt {instances; _} name = Nametbl.find_opt instances name

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
      unit tzresult Lwt.t

    val on_completion :
      self ->
      ('a, 'request_error) Request.t ->
      'a ->
      Worker_types.request_status ->
      unit Lwt.t
  end

  module type EIO_HANDLERS = sig
    type self

    type launch_error

    val on_launch :
      self -> Name.t -> Types.parameters -> (Types.state, launch_error) result

    val on_request :
      self -> ('a, 'request_error) Request.t -> ('a, 'request_error) result

    val on_no_request : self -> unit

    val on_close : self -> unit

    val on_error :
      self ->
      Worker_types.request_status ->
      ('a, 'request_error) Request.t ->
      'request_error ->
      unit tzresult

    val on_completion :
      self ->
      ('a, 'request_error) Request.t ->
      'a ->
      Worker_types.request_status ->
      unit
  end

  module MakeEIO (H : HANDLERS) :
    EIO_HANDLERS with type self = H.self and type launch_error = H.launch_error =
  struct
    type self = H.self

    type launch_error = H.launch_error

    let on_launch self name params =
      Lwt_eio.run_lwt_in_main @@ fun () -> H.on_launch self name params

    let on_close self = Lwt_eio.run_lwt_in_main @@ fun () -> H.on_close self

    let on_request self request =
      Lwt_eio.run_lwt_in_main @@ fun () -> H.on_request self request

    let on_no_request self =
      Lwt_eio.run_lwt_in_main @@ fun () -> H.on_no_request self

    let on_error self status request error =
      Lwt_eio.run_lwt_in_main @@ fun () -> H.on_error self status request error

    let on_completion self request params status =
      Lwt_eio.run_lwt_in_main @@ fun () ->
      H.on_completion self request params status
  end

  (** Clear the queue canceling promises until it is empty
      and add a [Shutdown] message for notifying others if needed. *)
  let cancel_promises_and_notify_shutdown w =
    let rec cancel_promises () =
      match take_nonblocking w with
      | None -> ()
      | Some Shutdown -> cancel_promises ()
      | Some (Message {request = _; metadata = _; resolver; timestamp = _}) ->
          ignore
          @@ Option.map
               (fun resolver ->
                 Eio.Promise.try_resolve resolver @@ Error (Any Request_exn))
               resolver ;
          cancel_promises ()
    in
    cancel_promises () ;
    add w Shutdown

  (** On shutdown:
      - set the worker in a [Closing] status
      - cancel all the pending promises and notify shutdown via
        [cancel_promises_and_notify_shutdown]
      - set the worker in a [Closed] status

      There are few steps to perform after this shutdown, which will
      be handheld by the [worker_loop] when it encounter a [Closed]
      worker status.
  *)
  let rec shutdown_eio ?birth ?emit w =
    match w.status with
    | Launching _ ->
        Eio.Fiber.yield () ;
        shutdown_eio ?birth ?emit w
    | Closed _ | Closing _ ->
        add w Shutdown ;
        Eio.Fiber.yield ()
    | Running _ ->
        let now = Time.System.now () in
        let birth = Option.value ~default:now birth in
        w.status <- Closing (birth, now) ;
        w.state <- None ;
        Option.iter Lwt_eio.run_lwt_in_main emit ;
        cancel_promises_and_notify_shutdown w ;
        w.stream_explorer <- [] ;
        w.status <- Closed (birth, Time.System.now (), None) ;
        Eio.Fiber.yield ()

  let shutdown ?birth ?emit w =
    Lwt_eio.run_eio @@ fun () -> shutdown_eio ?birth ?emit w

  let worker_loop (type kind) handlers _domain (worker : kind t) : [`Stop_daemon]
      =
    let (module Handlers : EIO_HANDLERS with type self = kind t) = handlers in
    let rec loop () : [`Stop_daemon] =
      (* Make sure that recursive calls do yield *)
      Eio.Fiber.yield () ;
      match worker.status with
      | Launching _ -> assert false
      | Closing _ -> loop ()
      | Closed _ ->
          (* The loop that triggered the Closed state should already
             have handled this, but it cost nothing to add it here as a safety
             net *)
          cancel_promises_and_notify_shutdown worker ;
          (* TODO: only call this one once *)
          Handlers.on_close worker ;
          (* TODO: check whether the canceler is used with the legacy
             workers except when shutdowning the worker, as it
             introduces a dependency to Lwt's event loop. *)
          (* [Error_monad.cancel_with_exceptions] relies on
             [Lwt_canceler.cancel] which means that it can be called multiple
             times and still be executed only once. *)
          ( Lwt_eio.run_lwt_in_main @@ fun () ->
            Error_monad.cancel_with_exceptions worker.canceler ) ;
          `Stop_daemon
      | Running birth -> (
          match take worker with
          | None ->
              Handlers.on_no_request worker ;
              loop ()
          | Some (Message {request; metadata = _; resolver; timestamp = pushed})
            -> (
              worker.stream_explorer <-
                List.tl worker.stream_explorer
                |> Stdlib.Option.value ~default:[] ;
              let treated = Time.System.now () in
              (* Retrieves the last possible error from the hive and returns it.
                 See `Eio.Executor_pool`. *)
              Option.iter
                (fun x -> resolve_error resolver (Any x))
                (Hive.get_error (Format.asprintf "%a" Name.pp worker.name)) ;
              match Handlers.on_request worker request with
              | Ok v ->
                  let completed = Time.System.now () in
                  let status = Worker_types.{pushed; treated; completed} in
                  (* We call the [on_completion] callbacks before resolving
                     the promise in order to make sure that the user code can
                     rely on the fact that these callbacks have been called
                     once the promise is resolved. *)
                  Handlers.on_completion worker request v status ;
                  resolve resolver (Ok v) ;
                  ( Hive.async_lwt @@ fun () ->
                    Worker_events.(emit request_no_errors)
                      (Request.view request, status) ) ;
                  loop ()
              | Error e -> (
                  resolve resolver (Error (Request_error e)) ;
                  let r =
                    let status =
                      Worker_types.
                        {pushed; treated; completed = Time.System.now ()}
                    in
                    Handlers.on_error worker status request e
                  in
                  match r with
                  | Ok () -> loop ()
                  | Error errs ->
                      (* On unrecoverable error, launch the shutdown process *)
                      let emit () = Worker_events.(emit crashed) errs in
                      shutdown_eio ~birth worker ~emit ;
                      loop ())
              | exception e ->
                  resolve resolver (Error (Any e)) ;
                  raise e)
          | Some Shutdown ->
              (* If [Shutdown] is read, shutdown is already happening.
                 Propagate the info and loop, so that we will fall into the
                 [Closing _ | Closed _] case and handle the termination there. *)
              add worker Shutdown ;
              loop ())
    in
    loop ()

  let create_table buffer_kind =
    {buffer_kind; last_id = 0; instances = Nametbl.create ~random:true 10}

  let init (type kind) (table : kind table) ?timeout name parameters =
    let buffer : kind buffer =
      match table.buffer_kind with
      | Queue -> Queue_buffer (Eio.Stream.create max_int)
      | Bounded {size} -> Bounded_buffer (Eio.Stream.create size)
      | Dropbox _ -> Dropbox_buffer (Eio.Stream.create 1)
    in
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
      ( {
          mutex = Eio.Mutex.create ();
          timeout;
          name;
          parameters;
          buffer;
          canceler = Lwt_canceler.create ();
          stream_explorer = [];
          current_request = None;
          state = None;
          status = Launching (Time.System.now ());
          table;
        },
        id_name )

  let start (type kind launch_error) worker ?(domains = 1) ~name ~id_name
      parameters
      (module Handlers : EIO_HANDLERS
        with type self = kind t
         and type launch_error = launch_error) =
    try
      let state =
        Hive.run_on_main (fun () -> Handlers.on_launch worker name parameters)
      in
      if id_name = base_name then
        Hive.async_lwt (fun () -> Worker_events.(emit started) ())
      else
        Hive.async_lwt (fun () ->
            Worker_events.(emit started_for) (Format.asprintf "%a" Name.pp name)) ;
      match state with
      | Ok state ->
          worker.state <- Some state ;
          worker.status <- Worker_types.Running (Time.System.now ()) ;
          Hive.run_on_main (fun () ->
              Hive.launch_worker
                worker
                ~bee_name:(Format.asprintf "%a" Name.pp name)
                ~domains
                (worker_loop (module Handlers))) ;
          Ok worker
      | Error e -> Error e
    with exn ->
      (* Ensure any partially started worker is shut down before propagating. *)
      (try shutdown_eio worker with _ -> ()) ;
      raise exn

  (** [launch table name parameters handlers] creates a single worker
      instance, using exclusively Lwt handlers. *)
  let launch (type kind launch_error) (table : kind table) ?timeout ?domains
      name parameters
      (module Lwt_handlers : HANDLERS
        with type self = kind t
         and type launch_error = launch_error) :
      (kind t, launch_error) result Lwt.t =
    let module Handlers = MakeEIO (Lwt_handlers) in
    let worker, id_name = init table ?timeout name parameters in
    Lwt_eio.run_eio @@ fun () ->
    start
      worker
      ?domains
      ~name
      ~id_name
      parameters
      (module Handlers : EIO_HANDLERS
        with type self = kind t
         and type launch_error = launch_error)

  (** [launch table name parameters handlers] creates a swarm of bees
      (workers), each running on a separate domain. By default only a single
      domain (then worker) is used to handle the requests. *)
  let launch_eio (type kind launch_error) (table : kind table) ?timeout ?domains
      ~name parameters
      (module Handlers : EIO_HANDLERS
        with type self = kind t
         and type launch_error = launch_error) : (kind t, launch_error) result =
    let worker, id_name = init table ?timeout name parameters in
    start worker ?domains ~name ~id_name parameters (module Handlers)

  let drop_request ?(_priority = 0) worker merge message_box request metadata =
    match worker.status with
    | Running _ -> (
        Eio.Mutex.with_lock_rw ~protect:true worker.mutex @@ fun () ->
        let merge_res =
          match Eio.Stream.take_nonblocking message_box with
          | None -> merge worker (Any_request (request, metadata)) None
          | Some (Message {request = old; metadata = old_metadata; _}) ->
              merge
                worker
                (Any_request (request, metadata))
                (Some (Any_request (old, old_metadata)))
          | Some Shutdown ->
              Eio.Stream.add message_box Shutdown ;
              None
        in
        match merge_res with
        | None -> ()
        | Some (Any_request (request, metadata)) ->
            Eio.Stream.add
              message_box
              (Message
                 {
                   request;
                   metadata;
                   timestamp = Time.System.now ();
                   resolver = None;
                 }))
    | Launching _ | Closing _ | Closed _ -> ()

  let drop_request_and_wait ?(_priority = 0) worker message_box request metadata
      =
    match worker.status with
    | Running _ ->
        let promise, resolver =
          Eio.Promise.create (* should we use label? *) ()
        in
        Eio.Mutex.with_lock_rw ~protect:true worker.mutex @@ fun () ->
        ignore @@ Eio.Stream.take_nonblocking message_box ;
        Eio.Stream.add
          message_box
          (Message
             {
               request;
               metadata;
               timestamp = Time.System.now ();
               resolver = Some resolver;
             }) ;
        promise
    | Launching _ | Closing _ | Closed _ ->
        Eio.Promise.create_resolved (Error (Closed None))

  let shutdown_eio w = shutdown_eio w

  let shutdown w =
    Lwt.bind (shutdown w) @@ fun () ->
    Lwt.map ignore (Lwt_canceler.when_canceled w.canceler)

  module type BOX = sig
    type t

    val put_request : t -> ('a, 'request_error) Request.t -> unit

    val put_request_eio : t -> ('a, 'request_error) Request.t -> unit

    val put_request_and_wait :
      t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Lwt.t

    val put_request_and_wait_eio :
      t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Eio.Promise.t
  end

  module type QUEUE = sig
    type 'a t

    val push_request_and_wait :
      'q t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Lwt.t

    val push_request_and_wait_eio :
      'q t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Eio.Promise.t

    val push_request : 'q t -> ('a, 'request_error) Request.t -> bool Lwt.t

    val push_request_eio : 'q t -> ('a, 'request_error) Request.t -> bool

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
    let put_request_eio worker request =
      let (Dropbox {merge}) = worker.table.buffer_kind in
      let (Dropbox_buffer message_box) = worker.buffer in
      let metadata = make_metadata () in
      drop_request worker merge message_box request metadata

    let put_request worker request = put_request_eio worker request

    let put_request_and_wait_eio worker request =
      let (Dropbox_buffer message_box) = worker.buffer in
      let metadata = make_metadata () in
      drop_request_and_wait worker message_box request metadata

    let put_request_and_wait worker request =
      let (Dropbox_buffer message_box) = worker.buffer in
      let metadata = make_metadata () in
      Lwt_eio.Promise.await_eio
      @@ drop_request_and_wait worker message_box request metadata
  end

  module Queue = struct
    let push_request ?(_priority = 0) worker request metadata =
      match worker.status with
      | Closing _ | Closed _ | Launching _ -> false
      | Running _ ->
          let timestamp = Time.System.now () in
          let message =
            Message {request; metadata; resolver = None; timestamp}
          in
          add worker message ;
          worker.stream_explorer <-
            (timestamp, Request.view request) :: worker.stream_explorer ;
          true

    let push_request_now ?(_priority = 0) (worker : infinite queue t) request
        metadata =
      match worker.status with
      | Closing _ | Closed _ | Launching _ -> ()
      | Running _ ->
          let (Queue_buffer message_queue) = worker.buffer in
          let timestamp = Time.System.now () in
          let message =
            Message {request; metadata; timestamp; resolver = None}
          in
          Eio.Stream.add message_queue message ;
          worker.stream_explorer <-
            (timestamp, Request.view request) :: worker.stream_explorer

    let push_request_and_wait ?(_priority = 0) worker request metadata =
      match worker.status with
      | Running _ ->
          let promise, resolver =
            Eio.Promise.create (* should we use ~label?*) ()
          in
          let timestamp = Time.System.now () in
          let message =
            Message {request; metadata; resolver = Some resolver; timestamp}
          in
          add worker message ;
          worker.stream_explorer <-
            (timestamp, Request.view request) :: worker.stream_explorer ;
          promise
      | _ ->
          Eio.Promise.create_resolved
            (Error (Closed (extract_status_errors worker)))

    let push_request_eio worker request =
      let metadata = make_metadata () in
      push_request worker request metadata

    let push_request worker request =
      let metadata = make_metadata () in
      Lwt_eio.run_eio @@ fun () -> push_request worker request metadata

    let push_request_and_wait_eio worker request =
      let metadata = make_metadata () in
      push_request_and_wait worker request metadata

    let push_request_and_wait worker request =
      let metadata = make_metadata () in
      Lwt_eio.Promise.await_eio @@ push_request_and_wait worker request metadata

    let push_request_now_eio worker request =
      let metadata = make_metadata () in
      push_request_now worker request metadata

    let push_request_now worker request =
      let metadata = make_metadata () in
      push_request_now worker request metadata

    let pending_requests (type a) (worker : a queue t) = worker.stream_explorer

    let pending_requests_length (type a) (worker : a queue t) =
      List.length worker.stream_explorer
  end

  let trigger_shutdown (type a) (w : a t) = shutdown_eio w

  let canceler {canceler; _} = canceler
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
