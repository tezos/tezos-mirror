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
    It is parameterized by abstract services [Disk], [Request], [Memory_table]
    and [Precheck].

    Values are looked up in [Disk.store] and are cached in [Memory_table.t].
    Internally, the service schedules requests to an abstract external
    sources (e.g. network) for missing keys. The key is then *pending* and
    while the caller waits the value to be available.

    When a value is missing, the cache sends a request via
    [Request.send] to query a value to the network, but it is the
    responsibility of the client to *notify* the cache with
    [Cache.notify] when the requested value is available.

    Notified values are validated before being inserted in the cache,
    using the [Precheck] module.

    The cache service offers two interface. [FULL_CACHE] is the full view, which
    includes the creation, shutdown, and reception notification functions.
    [CACHE] is a restricted view which essentially offers a *read-through*
    cache interface. *)

module type CACHE = sig
  type t

  (** The index key *)
  type key

  (** The indexed data *)
  type value

  (** validation clue *)
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

  (** [fetch t ?peer ?timeout k param] returns the value when it is known.
      It can fail with [Timeout k] if [timeout] is provided and the value
      isn't know before the timeout expires. It can fail with [Cancel] if
      the request is canceled.

      The key is first looked up in memory, then on disk. If not present and
      not already requested, it schedules a request, and blocks until
      the cache is notified with [notify]. [param] is used to validate the
      notified value once it is received. (see also [PRECHECK] and [notify].

      Requests are re-sent via a 1.5 exponential back-off, with initial
      delay set to [Request.initial_delay]. If the function
      is called multiple time with the same key but with distinct
      peers, the internal scheduler randomly chooses the requested
      peer (at each retry). *)
  val fetch :
    t ->
    ?peer:P2p_peer.Id.t ->
    ?timeout:Time.System.Span.t ->
    key ->
    param ->
    value tzresult Lwt.t

  (** Remove the data from the memory table if present. Otherwise cancel all
      pending requests. Any pending [fetch] promises are resolved with the
      error [Canceled]. *)
  val clear_or_cancel : t -> key -> unit
end

module type FULL_CACHE = sig
  include CACHE

  (** The "disk" storage *)
  type store

  (** Configuration parameter to the [Request] service *)
  type request_param

  (** type of values notified to the cache *)
  type notified_value

  (** [pending t k] returns [true] iff a the key status is pending *)
  val pending : t -> key -> bool

  (** Monitor all the fetched data. A given data will appear only
      once. *)
  val watch : t -> (key * value) Lwt_stream.t * Lwt_watcher.stopper

  (** [inject t k v] returns [false] if [k] is already present in the memory table
      or in the disk, or has already been request.  Otherwise it updates the
      memory table and return [true] *)
  val inject : t -> key -> value -> bool Lwt.t

  (** [notify t peer k nv] notifies the cache that a value has been received
      for key [k], from peer [peer]. [nv] is a *notified value*. The
      notified value is validated using [Precheck.precheck], and the
      [param] provided at fetching time (see [PRECHECK]). If valid,
      the memory table is updated and all promises waiting on this key are
      resolved.

      If the key is not pending the notification is ignored. *)
  val notify : t -> P2p_peer.Id.t -> key -> notified_value -> unit Lwt.t

  (** [memory_table_length t] returns the number of keys either known or
      pending in the memory table of [t] *)
  val memory_table_length : t -> int

  (** Returns the number of requests currently pending *)
  val pending_requests : t -> int

  (** [create ?global_input r s] creates a cache. [r] is the
      configuration parameter passed to [Request] functions.  *)
  val create :
    ?global_input:(key * value) Lwt_watcher.input ->
    request_param ->
    store ->
    t

  val shutdown : t -> unit Lwt.t
end

module type DISK_TABLE = sig
  type store

  type key

  type value

  val known : store -> key -> bool Lwt.t

  val read : store -> key -> value tzresult Lwt.t

  val read_opt : store -> key -> value option Lwt.t
end

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

(** [Requests] abstracts a service used to sends asynchronous key requests
    to peers. *)
module type REQUEST = sig
  type key

  (** [param] represents the state/configuration of the service. *)
  type param

  (** [initial_delay] is a service configuration time constant. Typically, use
      to set up a retry time interval. *)
  val initial_delay : Time.System.Span.t

  (** [active param] returns a set of active peers. For instance in order
      to broadcast a query to a set of peers. *)
  val active : param -> P2p_peer.Set.t

  val send : param -> P2p_peer.Id.t -> key list -> unit
end

(** When a requested value is received, it goes to a validation phase.

    At fetching time, the client gives a [param].
    At notification time, the client provides a [notified_value].
    [precheck] tries to construct a [value] from [param] and [notified_value].

    If no validation is needed. [precheck] is defined as

    let precheck k () v -> Some v  *)
module type PRECHECK = sig
  type key

  type param

  type notified_value

  type value

  val precheck : key -> param -> notified_value -> value option
end

module Make (Hash : sig
  type t

  val name : string

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  module Logging : sig
    val tag : t Tag.def
  end
end)
(Disk_table : DISK_TABLE with type key := Hash.t)
(Memory_table : MEMORY_TABLE with type key := Hash.t)
(Request : REQUEST with type key := Hash.t)
(Precheck : PRECHECK with type key := Hash.t and type value := Disk_table.value) : sig
  include
    FULL_CACHE
      with type key = Hash.t
       and type value = Disk_table.value
       and type param = Precheck.param
       and type request_param = Request.param
       and type notified_value = Precheck.notified_value
       and type store = Disk_table.store
end
