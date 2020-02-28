(* The MIT License

Copyright (c) 2019 Craig Ferguson <craig@tarides.com>
                   Thomas Gazagnaire <thomas@tarides.com>
                   Ioana Cristescu <ioana@tarides.com>
                   Clément Pascutto <clement@tarides.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software. *)

(** Index

    [Index] is a scalable implementation of persistent indices in OCaml.

    [Index] provides the standard key-value interface: [find], [mem] and
    [replace]. It requires three IO instances:

    - A `log` IO containing all of the recently-added bindings; this is also
      kept in memory.

    - When the `log` IO is full, it is merged into the `index` IO. Search is
      done first in `log` then in `index`, which makes recently added bindings
      search faster.

    - A `lock` IO to ensure safe concurrent access. *)

(** The input of [Make] for keys. *)
module type Key = sig
  type t
  (** The type for keys. *)

  val equal : t -> t -> bool
  (** The equality function for keys. *)

  val hash : t -> int
  (** Note: Unevenly distributed hash functions may result in performance drops. *)

  val hash_size : int
  (** The number of bits necessary to encode the maximum output value of
      {!hash}. `Hashtbl.hash` uses 30 bits.

      Overestimating the [hash_size] will result in performance drops;
      underestimation will result in undefined behavior. *)

  val encode : t -> string
  (** [encode] is an encoding function. The resultant encoded values must have
      size {!encoded_size}. *)

  val encoded_size : int
  (** [encoded_size] is the size of the result of {!encode}, expressed in number
      of bytes. *)

  val decode : string -> int -> t
  (** [decode s off] is the decoded form of the encoded value at the offset
      [off] of string [s]. Must satisfy [decode (encode t) 0 = t]. *)

  val pp : t Fmt.t
  (** Formatter for keys *)
end

module Stats = Stats

(** The input of [Make] for values. The same requirements as for [Key] apply. *)
module type Value = sig
  type t

  val encode : t -> string

  val encoded_size : int

  val decode : string -> int -> t

  val pp : t Fmt.t
end

module type IO = Io.S

module type MUTEX = sig
  (** Locks for mutual exclusion *)

  type t
  (** The type of mutual-exclusion locks. *)

  val create : unit -> t
  (** Return a fresh mutex. *)

  val lock : t -> unit
  (** Lock the given mutex. Locks are not assumed to be re-entrant. *)

  val unlock : t -> unit
  (** Unlock the mutex. If any threads are attempting to lock the mutex, exactly
      one of them will gain access to the lock. *)

  val with_lock : t -> (unit -> 'a) -> 'a
  (** [with_lock t f] first obtains [t], then computes [f ()], and finally
      unlocks [t]. *)
end

module type THREAD = sig
  (** Cooperative threads. *)

  type t
  (** The type of thread handles. *)

  val async : (unit -> 'a) -> t
  (** [async f] creates a new thread of control which executes [f ()] and
      returns the corresponding thread handle. The thread terminates whenever
      [f ()] returns a value or raises an exception. *)

  val await : t -> unit
  (** [await t] blocks on the termination of [t]. *)

  val return : unit -> t
  (** [return ()] is a pre-terminated thread handle. *)

  val yield : unit -> unit
  (** Re-schedule the calling thread without suspending it. *)
end

exception RO_not_allowed
(** The exception raised when a write operation is attempted on a read_only
    index. *)

exception Closed
(** The exception raised when any operation is attempted on a closed index,
    except for [close], which is idempotent. *)

(** Index module signature. *)
module type S = sig
  type t
  (** The type for indexes. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for values. *)

  val v : ?fresh:bool -> ?readonly:bool -> log_size:int -> string -> t
  (** The constructor for indexes.

      @param fresh whether an existing index should be overwritten.
      @param read_only whether read-only mode is enabled for this index.
      @param log_size the maximum number of bindings in the `log` IO. *)

  val clear : t -> unit
  (** [clear t] clears [t] so that there are no more bindings in it. *)

  val find : t -> key -> value
  (** [find t k] is the binding of [k] in [t]. *)

  val mem : t -> key -> bool
  (** [mem t k] is [true] iff [k] is bound in [t]. *)

  exception Invalid_key_size of key

  exception Invalid_value_size of value
  (** The exceptions raised when trying to add a key or a value of different
      size than encoded_size *)

  val replace : t -> key -> value -> unit
  (** [replace t k v] binds [k] to [v] in [t], replacing any existing binding of
      [k]. *)

  val filter : t -> (key * value -> bool) -> unit
  (** [filter t p] removes all the bindings (k, v) that do not satisfy [p]. This
      operation is costly and blocking. *)

  val iter : (key -> value -> unit) -> t -> unit
  (** Iterates over the index bindings. Limitations:

      - Order is not specified.
      - In case of recent replacements of existing values (since the last
        merge), this will hit both the new and old bindings.
      - May not observe recent concurrent updates to the index by other
        processes. *)

  val flush : ?with_fsync:bool -> t -> unit
  (** Flushes all internal buffers of the [IO] instances. If [with_fsync] is
      [true], this also flushes the OS caches for each [IO] instance. *)

  val close : t -> unit
  (** Closes all resources used by [t]. *)
end

module Make (K : Key) (V : Value) (IO : IO) (M : MUTEX) (T : THREAD) :
  S with type key = K.t and type value = V.t

(** These modules should not be used. They are exposed purely for testing
    purposes. *)
module Private : sig
  module Hook : sig
    type 'a t

    val v : ('a -> unit) -> 'a t
  end

  module Search : module type of Search

  module Io_array : module type of Io_array

  module Fan : module type of Fan

  module type S = sig
    include S

    type async
    (** The type of asynchronous computation. *)

    val force_merge : ?hook:[ `After | `Before ] Hook.t -> t -> async
    (** [force_merge t] forces a merge for [t]. Optionally, a hook can be passed
        that will be called twice:

        - [`Before]: immediately before merging (while holding the merge lock);
        - [`After]: immediately after merging (while holding the merge lock). *)

    val await : async -> unit
    (** Wait for an asynchronous computation to finish. *)
  end

  module Make (K : Key) (V : Value) (IO : IO) (M : MUTEX) (T : THREAD) :
    S with type key = K.t and type value = V.t
end
