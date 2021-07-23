(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** {2 Parameters to build a worker group} *)

open Error_monad

(** The name of the group of workers corresponding to an instantiation
    of {!Make}, as well as the name of each worker in that group. *)
module type NAME = sig
  (** The name/path of the worker group *)
  val base : string list

  (** The abstract name of a single worker *)
  type t

  (** Serializer for the introspection RPCs *)
  val encoding : t Data_encoding.t

  (** Pretty printer for displaying the worker name *)
  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end

module type VIEW = sig
  type view

  val encoding : view Data_encoding.t

  val pp : Format.formatter -> view -> unit
end

(** Events that are used for logging and introspection.
    Events are pretty printed immediately in the log, and stored in
    the worker's event backlog for introspection. *)
module type EVENT = sig
  (** The type of an event. *)
  type t

  include VIEW with type view := t

  (** Assigns a logging level to each event.
      Events can be ignored for logging w.r.t. the global node configuration.
      Events can be ignored for introspection w.r.t. to the worker's
      {!Worker_types.limits}. *)
  val level : t -> Internal_event.level
end

(** The type of messages that are fed to the worker's event loop. *)
module type REQUEST = sig
  (** The type of events.
      It is possible to wait for an event to be processed from outside
      the worker using {!push_request_and_wait}. In this case, the
      handler for this event can return a value. The parameter is the
      type of this value. *)
  type 'a t

  include VIEW

  (** The projection function from full request to simple views. *)
  val view : 'a t -> view
end

(** The (imperative) state of the event loop. *)
module type TYPES = sig
  (** The internal state that is passed to the event handlers. *)
  type state

  (** The parameters provided when launching a new worker. *)
  type parameters
end

module type LOGGER = sig
  module Event : EVENT

  module Request : VIEW

  type status =
    | WorkerEvent of (Event.t * Internal_event.level)
    | Request of
        (Request.view * Worker_types.request_status * error list option)
    | Terminated
    | Timeout
    | Crashed of error list
    | Started of string option
    | Triggering_shutdown
    | Duplicate of string

  type t = status Time.System.stamped

  module LogEvent : Internal_event.EVENT with type t = t
end
