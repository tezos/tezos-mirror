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
module Unique_name_maker : functor (Component : COMPONENT) ->
  Tezos_base.Worker_intf.NAME with type t = unit

(** Module signature that is equivalent to the {!Tezos_base.Worker_intf.REQUEST} with
    [view] being explicit. *)
module type P2P_REQUEST = sig
  (* Both these types need to be defined to be replaced in the [REQUEST] inclusion *)
  type ('response, 'error) t

  type view = View : ('reponse, 'error) t -> view

  include
    Tezos_base.Worker_intf.REQUEST
      with type ('response, 'error) t := ('response, 'error) t
       and type view := view

  val default_callback_value : view
end

type ('response, 'error) loop = Loop : (unit, tztrace) loop

(** The request for P2P workers is generally not useful.
    This module can be used as a [P2p_request] that serve only to loop again
    and return its result by a callback. *)
module Loop_request :
  P2P_REQUEST with type ('response, 'error) t = ('response, 'error) loop

(** The functor generates a valid [Worker], using [P2p_request] for requests.
    However, it is recommended to use the [activated_worker] kind as it enables
    decoupling creation of the state and activation of the loop. *)
module Make : functor
  (Name : Tezos_base.Worker_intf.NAME)
  (P2p_request : P2P_REQUEST)
  (Types : Tezos_base.Worker_intf.TYPES)
  -> sig
  include
    Tezos_workers.Worker.T
      with module Name = Name
       and type ('response, 'error) Request.t =
        ('response, 'error) P2p_request.t
       and type Request.view = P2p_request.view
       and module Types = Types

  type activator

  type activated_worker = {worker_state : callback t; activate : activator}

  val activated_callback :
    ?callback:(unit -> any_request Lwt.t) ->
    unit ->
    callback buffer_kind * unit Lwt.u

  (** [create ?timeout ?callback name parameters handlers] creates a worker
      whose table is a callback that returns [Loop]. The worker loop isn't
      launched yet, and needs to be started with [activate].

      If [callback] is not given, it will by default simply return
      [P2p_request.default_callback_value]. *)
  val create :
    ?timeout:Tezos_base.Time.System.Span.t ->
    ?callback:(unit -> any_request Lwt.t) ->
    Name.t ->
    Types.parameters ->
    (module HANDLERS with type launch_error = 'a and type self = callback t) ->
    (activated_worker, 'a) result Lwt.t

  (** [activate worker] starts the worker loop. *)
  val activate : activated_worker -> unit

  (** [shutdown worker] triggers a worker termination and waits for its
      completion. *)
  val shutdown : activated_worker -> unit Lwt.t
end
