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

(** Generic resource fetching/requesting service.

    This module defines a generic resource fetching service [Requester].
    It is parameterized by abstract services [Disk], [Request], [Memory_table]
    and [Probe].

    It offers a key/value store interface. Values are looked up successively
    in [Memory_table.t], then in [Disk.store]. If not found, they are
    *requested* using the [Request] service. Ultimately, values are *cached* in
    the [Memory_table] for faster retrieval on subsequent queries. This is
    similar to a *read-through* cache except that values are never evicted
    from the [Memory_table].

    Internally, the service schedules requests to an abstract external
    source (e.g. network) for missing keys. The key is then *pending*
    and the caller waits for the value to be available.

    When a value is missing, the requester sends a request via
    [Request.send] to query a value to the network. The requester must be
    *notified* by an external component when the requested value is available
    using [Requester.notify].

    Notified values are validated before being inserted in the requester,
    using the [Probe] module.

    The full resource fetching service is realized by the conjunction of
    two components. The requester component, defined by this library, and
    a notifying component (for instance, a worker thread waiting for network
    messages).

    The requester offers two interfaces. [FULL_REQUESTER] is the full
    view, which includes the creation, shutdown, and notification
    functions. It is to be used by the controller setting up the service,
    and the notifying component. [REQUESTER] is the restricted view, exported
    to the service final user. *)

module type REQUESTER = sig
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

  (** [inject t k v] returns [false] if [k] is already present in the memory
      table or in the disk, or has already been requested. Otherwise it
      updates the memory table and return [true] *)
  val inject : t -> key -> value -> bool Lwt.t

  (** [fetch t ?peer ?timeout k param] returns the value when it is known.
      It can fail with [Timeout k] if [timeout] is provided and the value
      isn't know before the timeout expires. It can fail with [Cancel] if
      the request is canceled.

      The key is first looked up in memory, then on disk. If not present and
      not already requested, it schedules a request, and blocks until
      the requester is notified with [notify]. [param] is used to validate the
      notified value once it is received. (see also [PROBE] and [notify]).

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

module type FULL_REQUESTER = sig
  include REQUESTER

  (** The "disk" storage *)
  type store

  (** Configuration parameter to the [Request] service *)
  type request_param

  (** type of values notified to the requester *)
  type notified_value

  (** [pending t k] returns [true] iff the key status is pending *)
  val pending : t -> key -> bool

  (** Monitor all the fetched data. A given data will appear only
      once. *)
  val watch : t -> (key * value) Lwt_stream.t * Lwt_watcher.stopper

  (** [notify t peer k nv] notifies the requester that a value has been
      received for key [k], from peer [peer]. [nv] is a *notified value*. The
      notified value is validated using [Probe.probe], and the
      [param] provided at fetching time (see [PROBE]). If valid,
      the memory table is updated and all promises waiting on this key are
      resolved.

      If the key is not pending the notification is ignored. *)
  val notify : t -> P2p_peer.Id.t -> key -> notified_value -> unit Lwt.t

  (** [memory_table_length t] returns the number of keys either known or
      pending in the memory table of [t] *)
  val memory_table_length : t -> int

  (** Returns the number of requests currently pending *)
  val pending_requests : t -> int

  (** [create ?random_table ?global_input r s] creates a requester. [r] is the
      configuration parameter passed to [Request] functions.

      The value for [random_table] determines whether the underlying hashtable
      randomises its hash (see {!Stdlib.Hashtbl.create} and specifically the
      documentation of the [random] parameter). The default depends on
      environment variables and {!Stdlib.Hashtbl.randomize} has been called. *)
  val create :
    ?random_table:bool ->
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
  (** subtypes {!Hashtbl.SeededS} *)
  type 'a t

  type key

  val create : entry_type:string -> ?random:bool -> int -> 'a t

  val find : 'a t -> key -> 'a option

  val add : 'a t -> key -> 'a -> unit

  val replace : 'a t -> key -> 'a -> unit

  val remove : 'a t -> key -> unit

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val length : 'a t -> int
end

(** [Requests] abstracts a service used to send asynchronous key requests
    to peers. *)
module type REQUEST = sig
  type key

  (** [param] represents the state/configuration of the service. *)
  type param

  (** [initial_delay] is a service configuration time constant. Typically, used
      to set up a retry time interval. *)
  val initial_delay : Time.System.Span.t

  (** [active param] returns a set of active peers. For instance in order
      to broadcast a query to a set of peers. *)
  val active : param -> P2p_peer.Set.t

  (** [send param peer_id kl] queries peer with id [peer_id] for the values
      associated with the keys in [kl]. *)
  val send : param -> P2p_peer.Id.t -> key list -> unit
end

(** When a requested value is received, it goes to a validation phase.

    At fetching time, the client gives a [param].
    At notification time, the client provides a [notified_value].
    [probe] tries to construct a [value] from [param] and [notified_value].

    If no validation is needed. [probe] is defined as

    let probe k () v -> Some v  *)
module type PROBE = sig
  type key

  type param

  type notified_value

  type value

  val probe : key -> param -> notified_value -> value option
end

(** An input module of {!Make} *)
module type HASH = sig
  type t

  val name : string

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end

module Make
    (Hash : HASH)
    (Disk_table : DISK_TABLE with type key := Hash.t)
    (Memory_table : MEMORY_TABLE with type key := Hash.t)
    (Request : REQUEST with type key := Hash.t)
    (Probe : PROBE with type key := Hash.t and type value := Disk_table.value) :
  FULL_REQUESTER
    with type key = Hash.t
     and type value = Disk_table.value
     and type param = Probe.param
     and type request_param = Request.param
     and type notified_value = Probe.notified_value
     and type store = Disk_table.store
