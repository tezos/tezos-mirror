(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This library provides functors to build various kinds of stores using
    mirage's {{:https://github.com/mirage/index} index} and Octez
    {{:https://gitlab.com/nomadic-labs/data-encoding} data-encoding}
    libraries.

    It is tailored to build stores for the Layer 2 nodes of Tezos (Tx-rollups
    and Smart rollups).

    The stores built with this library support concurrent accesses thanks to the
    use of the light scheduler provided by {!Lwt_idle_waiter} for exclusive
    write access. *)

open Store_sigs

(** {2 Signatures} *)

(** An iterator for the GC functions. *)
type ('key, 'value) gc_iterator =
  | Retain of 'key list  (** A simple list of keys to retain. *)
  | Iterator of {
      first : 'key;  (** The key at which the iteration starts for the GC.  *)
      next : 'key -> 'value -> 'key option Lwt.t;
          (** A function to compute the next element to explore from the last
              expired key, value binding in the store. We explicit the value
              here because the next element can be computed from it without
              accessing the store (e.g. to iterate over L2 blocks, we start from
              the head and the next element is computed from the predecessor
              field of the block. *)
    }  (** An iterator. The GC stops when the [next] function returns [None]. *)

(** A store for single updatable values. Values are stored in a file on disk and
    are kept in memory in a cache. *)
module type SINGLETON_STORE = sig
  (** The type of the singleton store. *)
  type +'a t

  (** The type of values stored in this singleton store. *)
  type value

  (** Load (or initializes) a singleton store in the file [path]. *)
  val load : path:string -> 'a mode -> 'a t tzresult Lwt.t

  (** Reads the current value from the disk. Returns [None] if the
      file does not exist. *)
  val read : [> `Read] t -> value option tzresult Lwt.t

  (** Write the value to disk. *)
  val write : [> `Write] t -> value -> unit tzresult Lwt.t

  (** Deletes the value from the disk. *)
  val delete : [> `Write] t -> unit tzresult Lwt.t

  (** [readonly t] returns a read only version of the store [t]. *)
  val readonly : [> `Read] t -> [`Read] t
end

(** An index store mapping keys to values. It uses an index file internally. *)
module type INDEXABLE_STORE = sig
  (** The type of store built on indexes. *)
  type +'a t

  (** The type of keys for the store. *)
  type key

  (** The type of values stored in the index, *)
  type value

  (** Load (or initializes) a store in the file [path]. If [readonly] is [true],
      the store will only be accessed in read only mode. *)
  val load :
    path:string -> index_buffer_size:int -> 'a mode -> 'a t tzresult Lwt.t

  (** Returns [true] if the key has a value associated in
      the store. *)
  val mem : [> `Read] t -> key -> bool tzresult Lwt.t

  (** Returns the value associated to a key in the store,
      or [None] otherwise. *)
  val find : [> `Read] t -> key -> value option tzresult Lwt.t

  (** Add an association from a key to a value in the
      store. If [flush] (default to [true]) is set, the index is written on disk
      right away. *)
  val add : ?flush:bool -> [> `Write] t -> key -> value -> unit tzresult Lwt.t

  (** Closes the store. After this call the store cannot be accessed anymore
      (unless one calls {!load} again). *)
  val close : _ t -> unit tzresult Lwt.t

  (** [readonly t] returns a read only version of the store [t]. *)
  val readonly : [> `Read] t -> [`Read] t

  (** [gc ?async t iter] garbage collects data stored in the index [t] by
      keeping only the ones that are reachable by [iter]. This call
      runs the GC asynchronously unless [async] is [false]. If a GC is already
      ongoing this new request is ignored and this call is a no-op. *)
  val gc :
    ?async:bool -> rw t -> (key, value) gc_iterator -> unit tzresult Lwt.t

  (** [wait_gc_completion t] returns a blocking thread if a GC run is ongoing. *)
  val wait_gc_completion : 'a t -> unit Lwt.t

  (** [is_gc_finished t] returns [true] if there is no GC running. *)
  val is_gc_finished : 'a t -> bool
end

(** An index store mapping keys to values. Keys are associated to optional
    values in the index which allows them to be removed. *)
module type INDEXABLE_REMOVABLE_STORE = sig
  include INDEXABLE_STORE

  (** Removes an association from the store. Does nothing if the key was not
      registered. *)
  val remove : ?flush:bool -> [> `Write] t -> key -> unit tzresult Lwt.t
end

(** An indexed file (i.e. a file and an index) mapping keys to values. Contrary
    to {!INDEXABLE_STORE}, the values can vary in size. Internally, values are
    stored, concatenated, in a append only file. The index file associates keys
    to offsets in this file (and a header to retrieve information more
    efficiently).
*)
module type INDEXED_FILE = sig
  (** The type of indexed file store. *)
  type +'a t

  (** The type of keys for the store. *)
  type key

  (** The type of headers stored in the index. The header can contain fixed size
      information that can be accessed more efficiently than the full value. *)
  type header

  (** The type of values stored in the file. *)
  type value

  (** Returns [true] if the key has a value associated in
      the store. *)
  val mem : [> `Read] t -> key -> bool tzresult Lwt.t

  (** Returns the header for a key if it exists in the store. *)
  val header : [> `Read] t -> key -> header option tzresult Lwt.t

  (** Read a full value and header from the indexed file store. *)
  val read : [> `Read] t -> key -> (value * header) option tzresult Lwt.t

  (** Append a new binding to the indexed file store. *)
  val append :
    ?flush:bool ->
    [> `Write] t ->
    key:key ->
    header:header ->
    value:value ->
    unit tzresult Lwt.t

  (** Loads a new or existing indexed file store in the directory [path]. *)
  val load :
    path:string ->
    index_buffer_size:int ->
    cache_size:int ->
    'a mode ->
    'a t tzresult Lwt.t

  (** Close the index and the file. One must call {!load} again to read or write
      data in the store. *)
  val close : _ t -> unit tzresult Lwt.t

  (** [readonly t] returns a read only version of the store [t]. *)
  val readonly : [> `Read] t -> [`Read] t

  (** [gc ?async t iter] garbage collects data stored in the store [t] by
      keeping only the ones reachable by [iter]. This call runs the GC
      asynchronously unless [async] is [false]. If a GC is already
      ongoing this new request is ignored and this call is a no-op. *)
  val gc :
    ?async:bool ->
    rw t ->
    (key, value * header) gc_iterator ->
    unit tzresult Lwt.t

  (** [wait_gc_completion t] returns a blocking thread if a GC run is currently
      ongoing. *)
  val wait_gc_completion : 'a t -> unit Lwt.t

  (** [is_gc_finished t] returns [true] if there is no GC running. *)
  val is_gc_finished : 'a t -> bool
end

(** Same as {!INDEXED_FILE} but where headers are extracted from values. *)
module type SIMPLE_INDEXED_FILE = sig
  include INDEXED_FILE

  (** Append a new binding to the indexed file store. *)
  val append :
    ?flush:bool -> [> `Write] t -> key:key -> value:value -> unit tzresult Lwt.t
end

(** Names for stores.  *)
module type NAME = sig
  val name : string
end

(** Values that can be used as keys for indices. *)
module type INDEX_KEY = sig
  include Index.Key.S

  val pp : Format.formatter -> t -> unit
end

(** Values that can be encoded. *)
module type ENCODABLE_VALUE = sig
  type t

  val name : string

  val encoding : t Data_encoding.t
end

(** Values that can be encoded and whose encoding is a fixed size. *)
module type FIXED_ENCODABLE_VALUE = sig
  include ENCODABLE_VALUE

  val fixed_size : int
end

(** Values with a given fixed size header. *)
module type ENCODABLE_VALUE_HEADER = sig
  include ENCODABLE_VALUE

  module Header : FIXED_ENCODABLE_VALUE
end

(** {2 Functors}  *)

module Make_singleton (S : ENCODABLE_VALUE) :
  SINGLETON_STORE with type value := S.t

module Make_indexable (_ : NAME) (K : INDEX_KEY) (V : Index.Value.S) :
  INDEXABLE_STORE with type key := K.t and type value := V.t

module Make_indexable_removable (_ : NAME) (K : INDEX_KEY) (V : Index.Value.S) :
  INDEXABLE_REMOVABLE_STORE with type key := K.t and type value := V.t

module Make_indexed_file (_ : NAME) (K : INDEX_KEY) (V : ENCODABLE_VALUE_HEADER) :
  INDEXED_FILE
    with type key := K.t
     and type value := V.t
     and type header := V.Header.t

module Make_simple_indexed_file
    (_ : NAME)
    (K : INDEX_KEY) (V : sig
      include ENCODABLE_VALUE_HEADER

      val header : t -> Header.t
    end) :
  SIMPLE_INDEXED_FILE
    with type key := K.t
     and type value := V.t
     and type header := V.Header.t

(** {2 Helper functors} *)

module Make_fixed_encodable (V : ENCODABLE_VALUE) :
  FIXED_ENCODABLE_VALUE with type t = V.t

module Make_index_value (E : FIXED_ENCODABLE_VALUE) :
  Index.Value.S with type t = E.t

module Make_index_key (E : sig
  include FIXED_ENCODABLE_VALUE

  val equal : t -> t -> bool
end) : INDEX_KEY with type t = E.t
