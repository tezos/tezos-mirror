(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Generic cache / request scheduling service.

    This module defines a generic key-value cache service [Cache].
    It is parameterized by abstract services [Disk], [Scheduler], [Memory_table]
    and [Precheck].

    Values are looked up in [Disk] and cached in [Memory_table]. Using its
    [Scheduler], the service can also schedule requests for missing keys
    to an external source (e.g. network). The key is then *pending* and
    the service waits synchronously for the value to be available.

    The [Scheduler] is also a generic service, parameterized by
    the [Memory_table] and [Request] modules. Importantly, the [Memory_table]
    must be shared between the [Scheduler] and the [Cache] as it
    used to store both pending requests and found values.

    TODO: this can maybe statically enforced by reviewing the set of exported
          functors. We may not need to export the scheduler interface to the
          client

    The cache is "semi"-readthrough. It sends a request via
    [Request.send] to query a value to the network, but it is the
    responsibility of the client to *notify the cache* with
    [Cache.notify] when the requested value is available.

    Notified values are validated before being inserted in the cache,
    using the [Precheck] module. *)

module type MEMORY_TABLE = sig
  (** subtypes Hashtbl.S *)
  type 'a t

  type key

  val create : int -> 'a t

  val find : 'a t -> key -> 'a

  val find_opt : 'a t -> key -> 'a option

  val add : 'a t -> key -> 'a -> unit

  val replace : 'a t -> key -> 'a -> unit

  val remove : 'a t -> key -> unit

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val length : 'a t -> int
end

module type REQUEST = sig
  type key

  type param

  val initial_delay : Time.System.Span.t

  val active : param -> P2p_peer.Set.t

  val send : param -> P2p_peer.Id.t -> key list -> unit
end

(** This defines the scheduler interface to be used by the cache. Only
    [memory_table_length] is used outside this module.
    TODO: enforce this statically. *)
module type SCHEDULER_EVENTS = sig
  type t

  type key

  val request : t -> P2p_peer.Id.t option -> key -> unit

  val notify : t -> P2p_peer.Id.t -> key -> unit

  val notify_cancellation : t -> key -> unit

  val notify_unrequested : t -> P2p_peer.Id.t -> key -> unit

  val notify_duplicate : t -> P2p_peer.Id.t -> key -> unit

  val notify_invalid : t -> P2p_peer.Id.t -> key -> unit

  (** Returns the number of requests currently pending *)
  val memory_table_length : t -> int
end

(** Creates a request scheduler to be used by the cache. The request scheduler
    relies on the worker. [create] / [shutdown] extends the [SCHEDULER_EVENTS]
    signature to start and cleanup the worker. They must be called resp.
    before first use / after last use of the cache. The client shouldn't
    use directly other functions of this module. TODO enforce this statically *)
module Make_request_scheduler (Hash : sig
  type t

  val name : string

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  module Logging : sig
    val tag : t Tag.def
  end
end)
(Table : MEMORY_TABLE with type key := Hash.t)
(Request : REQUEST with type key := Hash.t) : sig
  type t

  val create : Request.param -> t

  val shutdown : t -> unit Lwt.t

  include SCHEDULER_EVENTS with type t := t and type key := Hash.t
end

module type CACHE = sig
  type t

  (** The index key *)
  type key

  (** The indexed data *)
  type value

  (** An extra parameter for the network lookup, usually
      used for prevalidating data. *)
  type param

  (** [know t k] returns true iff the key is present in the memory table or
      the disk. *)
  val known : t -> key -> bool Lwt.t

  type error += Missing_data of key

  type error += Canceled of key

  type error += Timeout of key

  (** Return value if it is found in-memory, or else on disk. Otherwise fail
      with error [Missing_data]. *)
  val read : t -> key -> value tzresult Lwt.t

  (** Same as [read] but returns [None] if not found. *)
  val read_opt : t -> key -> value option Lwt.t

  (** Same as [fetch] but the call is non-blocking: the data will be
      stored in the memory table when received. *)
  val prefetch :
    t ->
    ?peer:P2p_peer.Id.t ->
    ?timeout:Time.System.Span.t ->
    key ->
    param ->
    unit

  (** [fetch t ?peer ?timeout k param] returns the value when it is known.
      It can fail with [Timeout k] if [timeout] is provided and the value
      isn't know before the timeout expires. It can fail with [Cancel] if
      the request is canceled.

      The key is first looked up in memory, then on disk. If not present and
      not already requested, it schedules a request for this key using
      [Scheduler.request] with optional [peer] parameter and blocks until
      the cache is notified with [notify]. [param] is used to validate the
      notified value using [notify]. (see [PRECHECK] and [notify] doc). *)
  val fetch :
    t ->
    ?peer:P2p_peer.Id.t ->
    ?timeout:Time.System.Span.t ->
    key ->
    param ->
    value tzresult Lwt.t

  (** Remove the data from the local index or cancel all pending
      request. Any pending [fetch] promises are resolved with the
      error [Canceled]. *)
  val clear_or_cancel : t -> key -> unit

  (* [inject t k v] returns [false] if [k] is already present in the memory table
     or in the disk, or has already been request.  Otherwise it updates the
     memory table and return [true] *)
  val inject : t -> key -> value -> bool Lwt.t

  (** Monitor all the fetched data. A given data will appear only
      once. *)
  val watch : t -> (key * value) Lwt_stream.t * Lwt_watcher.stopper

  (** [pending t k] returns [true] iff a the key status is pending *)
  val pending : t -> key -> bool
end

module type DISK_TABLE = sig
  type store

  type key

  type value

  val known : store -> key -> bool Lwt.t

  val read : store -> key -> value tzresult Lwt.t

  val read_opt : store -> key -> value option Lwt.t
end

(** When a requested value is received, it goes to a validation phase.

    At fetching time, the client gives a [param].
    At notification time, the client provides a [notified_value].
    [precheck] tries to construct a [value] from [param] and [notified_value].

    In the simplest case, [precheck] is defined as

    let precheck k () v -> Some v

    And no validation takes places. *)
module type PRECHECK = sig
  type key

  type param

  type notified_value

  type value

  val precheck : key -> param -> notified_value -> value option
end

module Make_table (Hash : sig
  type t

  val name : string

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end)
(Disk_table : DISK_TABLE with type key := Hash.t)
(Memory_table : MEMORY_TABLE with type key := Hash.t)
(Scheduler : SCHEDULER_EVENTS with type key := Hash.t)
(Precheck : PRECHECK with type key := Hash.t and type value := Disk_table.value) : sig
  include
    CACHE
      with type key = Hash.t
       and type value = Disk_table.value
       and type param = Precheck.param

  val create :
    ?global_input:(key * value) Lwt_watcher.input ->
    Scheduler.t ->
    Disk_table.store ->
    t

  (** [notify t peer k p] notifies the cache that a value has been received
      for key [k], from peer [peer].

      If the key is not pending (e.g. the request has been resolved already),
      the notification is essentially ignored (the scheduler is notified which
      simply generate log events).

      If the key is pending, the notified value is validated using
      [Precheck.precheck] against the [param] provided at fetching time,
      and all threads waiting on this key are woken up. *)
  val notify :
    t -> P2p_peer.Id.t -> key -> Precheck.notified_value -> unit Lwt.t

  (** [memory_table_length t] returns the number of keys either known or
      pending in the memory table of [t] *)
  val memory_table_length : t -> int
end
