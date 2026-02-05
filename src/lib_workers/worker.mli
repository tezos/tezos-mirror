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

(** Workers, their types and signatures, and a functor to make them. *)

(** {2 Worker group maker} *)

type worker_name = {base : string; name : string}

(** Functor to build a group of workers.
    At that point, all the types are fixed and introspectable,
    but the actual parameters and event handlers can be tweaked
    for each individual worker. *)
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

  (** An error returned when waiting for a message pushed to the worker.
      [Closed errs] is returned if the worker is terminated or has crashed. If the
      worker is terminated, [errs] is an empty list.
      [Request_error err] is returned if the request failed with an error. [Any
      exn] is returned if the request failed with an exception. *)
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
        returning [`Continue] and the worker execution continues, [`Shutdown]
        and the worker shutdowns without crashing, or the error can be propagated
        through a [tzresult], making the worker crash. *)
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

  (** The following interface are common elements of multiple modules below.
      They are used to minimize repetition. *)
  module type BOX = sig
    (** With [BOX]es, you can put a request right at the front *)
    type t

    (** [put_request worker request] sends the [request] to the [worker]. If the
        [worker] dropbox is closed, then it is a no-op. *)
    val put_request : t -> ('a, 'request_error) Request.t -> unit

    (** [put_request_and_wait worker request] sends the [request] to the
        [worker] and waits for its completion. If the worker dropbox is closed,
        then it returns [Error Closed]. *)
    val put_request_and_wait :
      t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Lwt.t
  end

  module type QUEUE = sig
    (** With [QUEUE]s, you can push requests in the queue *)
    type 'a t

    (** [push_request_and_wait worker request] sends the [request] to the
        [worker] and waits for its completion. If the [worker] queue is closed,
        then it returns [Error Closed]. If the buffer is a bounded queue and the
        underlying queue is full, the call is blocking. *)
    val push_request_and_wait :
      'q t ->
      ('a, 'request_error) Request.t ->
      ('a, 'request_error message_error) result Lwt.t

    (** [push_request worker request] sends the [request] to the [worker]. The
        promise returned is [true] if the request was pushed successfuly or
        [false] if the worker queue is closed. If the buffer is a bounded queue
        and the underlying queue is full, the call is blocking. *)
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
      worker is shutdown or when it dies. *)
  val canceler : _ t -> Lwt_canceler.t

  (** Triggers a worker termination. *)
  val trigger_shutdown : _ t -> unit

  (** Returns true if the worker is shutting down. *)
  val is_shutting_down : _ t -> bool

  (** Access the internal state, once initialized. *)
  val state : _ t -> Types.state

  (** Access the internal state if available. *)
  val state_opt : _ t -> Types.state option

  (** [with_state w f] calls [f] on the current state of worker [w] if
      it was intialized and not closed or crashed, otherwise
      returns immediately. *)
  val with_state :
    _ t ->
    (Types.state -> (unit, 'request_error) result Lwt.t) ->
    (unit, 'request_error) result Lwt.t

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

(** [module WG = MakeGroup (Name) (Request)] defines a {e worker group} all
    using the same [Name], [Event], etc. To instantiate a worker from a group,
    you must give the [Types] parameter: [WG.MakeWorker(Types)]. This defines a
    [Worker] module of type [T]. This last instantiation can be safely used as
    first class module.

    The delayed application is there to prevent multiple side-effect executions
    in case of multiple instantiation. (Inner events trigger side effects.)
*)
module MakeGroup (Name : Worker_intf.NAME) (Request : Worker_intf.REQUEST) : sig
  module MakeWorker (Types : Worker_intf.TYPES) :
    T
      with module Name = Name
       and module Request = Request
       and module Types = Types
end

(** [MakeSingle (Name) (Request) (Types)] is the same as using
    [MakeGroup] and then [MakeWorker]. It's a special case which you can
    use if you only ever need a single instantiation.
*)
module MakeSingle
    (Name : Worker_intf.NAME)
    (Request : Worker_intf.REQUEST)
    (Types : Worker_intf.TYPES) :
  T
    with module Name = Name
     and module Request = Request
     and module Types = Types
