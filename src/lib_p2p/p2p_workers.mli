(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module defines a simple interface that help writing generic workers
    used in lib_p2p, and avoid redundancy in their implementation.

    In general, there exists a single worker of a kind that is devoted to a
    specific task, and is basically an infinite loop. The loop is handled
    through a [Callback] kind that returns instantaneously, and the handler is
    responsible for sleeping and resume.

    P2P workers are generally created (for their state to exist) and activated
    (starting their worker loop) in two phases, which slightly differs from
    workers that are launched directly. These workers emulate this semantics on
    top of [Tezos_workers].
*)

module type COMPONENT = sig
  val base : string list
end

(** This functor generates a [Name] module out of a base, and generates a name
    for unique workers: once a worker has been spawned from a table, no other
    worker can be spawned again. *)
module UniqueNameMaker : functor (Component : COMPONENT) ->
  Tezos_base.Worker_intf.NAME with type t = unit

type ('response, 'error) loop = Loop : (unit, tztrace) loop

(** The request for P2P workers is basically not useful, and serves only to loop
    again and return its result by a callback. *)
module LoopRequest :
  Tezos_base.Worker_intf.REQUEST
    with type ('response, 'error) t = ('response, 'error) loop

(** The functor generates a valid [Worker], using [LoopRequest] for requests.
    However, it is recommended to use the [activated_worker] kind as it enables
    decoupling creation of the state and activation of the loop. *)
module Make : functor
  (Name : Tezos_base.Worker_intf.NAME)
  (Types : Tezos_base.Worker_intf.TYPES)
  -> sig
  include
    Tezos_workers.Worker.T
      with module Name = Name
       and module Request = LoopRequest
       and module Types = Types

  type activator

  type nonrec activated_worker = {
    worker_state : callback t;
    activate : activator;
  }

  val activated_callback :
    ?callback:(unit -> any_request Lwt.t) ->
    unit ->
    callback buffer_kind * unit Lwt.u

  (** [create ?timeout ?callback name parameters handlers] creates a worker
      whose table is a callback that returns [Loop]. The worker loop isn't
      launched yet, and needs to be started with [activate].

      If [callback] is not given, it will by default simply return
      [LoopRequest.Loop]. *)
  val create :
    ?timeout:Tezos_base.Time.System.Span.t ->
    ?callback:(unit -> any_request Lwt.t) ->
    Name.t ->
    Types.parameters ->
    (module HANDLERS with type launch_error = 'a and type self = callback t) ->
    (activated_worker, 'a) result Lwt.t

  (** [activate worker] starts the worker loop. *)
  val activate : activated_worker -> unit
end
