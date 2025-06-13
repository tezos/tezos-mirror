(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type REQUEST = sig
  include Tezos_base.Worker_intf.REQUEST

  (** [name t] is used to make the span name associated to [t] *)
  val name : (_, _) t -> string
end

(** Drop-in replacement of {!Tezos_workers.Worker.MakeSingle}, where each
    request processing is wrapped in an OpenTelemetry span. *)
module MakeSingle : functor
  (Name : Tezos_base.Worker_intf.NAME)
  (Raw_request : REQUEST)
  (Types : Tezos_base.Worker_intf.TYPES)
  -> sig
  module Raw_worker :
    Tezos_workers.Worker.T
      with type Name.t = Name.t
      with type ('a, 'b) Request.t = ('a, 'b) Raw_request.t
       and module Types = Types

  include
    Tezos_workers.Worker.T with module Name = Name and module Types = Types

  (** Instrumented queue tracing a request handling from the moment it is being
      pushed to a worker’s pending queue. *)
  module Queue : sig
    val push_request_and_wait :
      'a queue t ->
      ('b, 'c) Raw_request.t ->
      ('b, 'c message_error) result Lwt.t

    val push_request : 'a queue t -> ('b, 'c) Raw_request.t -> bool Lwt.t
  end

  (** [launch] starts an instrumented worker based on handlers implemented for
      the “raw” (non-instrumented) requests. *)
  val launch :
    'kind table ->
    Name.t ->
    Types.parameters ->
    (module Raw_worker.HANDLERS
       with type launch_error = 'launch_error
        and type self = 'kind t) ->
    ('kind t, 'launch_error) result Lwt.t
end
