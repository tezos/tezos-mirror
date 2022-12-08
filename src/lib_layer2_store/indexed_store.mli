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

(** Describes the different representations that can be stored persistently. *)

open Store_sigs

(** {2 Signatures} *)

(** A store composed of a single file on disk *)
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
end

(** An index store mapping keys to values. It is composed of an index only. *)
module type INDEXABLE_STORE = sig
  (** The type of store build in indexes *)
  type +'a t

  (** The type of keys for the *)
  type key

  (** The type of values stored in the index *)
  type value

  (** Load (or initializes) a store in the file [path]. If [readonly] is [true],
      the store will only be accessed in read only mode. *)
  val load : path:string -> 'a mode -> 'a t tzresult Lwt.t

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

  (** Closes the store. After this call the store cannot be accessed anymore. *)
  val close : _ t -> unit tzresult Lwt.t
end

(** An index store mapping keys to values. Keys are associated to optional
    values in the index which allows them to be removed. *)
module type INDEXABLE_REMOVABLE_STORE = sig
  include INDEXABLE_STORE

  (** Removes an association from the store. Does nothing if the key was not
      registered. *)
  val remove : ?flush:bool -> [> `Write] t -> key -> unit tzresult Lwt.t
end

(** An indexed file (i.e. a file and an index) mapping keys to values. The
    values can vary in size. *)
module type INDEXED_FILE = sig
  (** The type of indexed file store *)
  type +'a t

  (** The type of keys  *)
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

  (** Read a full value from the indexed file store. *)
  val read : [> `Read] t -> key -> value option tzresult Lwt.t

  (** Append a new binding to the indexed file store. *)
  val append :
    ?flush:bool ->
    [> `Write] t ->
    key:key ->
    header:header ->
    value:value ->
    unit tzresult Lwt.t

  (** Loads a new or existing indexed file store in the directory [path]. *)
  val load : path:string -> cache_size:int -> 'a mode -> 'a t tzresult Lwt.t

  (** Close the index and the file. *)
  val close : _ t -> unit tzresult Lwt.t
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

module Make_indexable (_ : NAME) (K : Index.Key.S) (V : Index.Value.S) :
  INDEXABLE_STORE with type key := K.t and type value := V.t

module Make_indexable_removable (_ : NAME) (K : Index.Key.S) (V : Index.Value.S) :
  INDEXABLE_REMOVABLE_STORE with type key := K.t and type value := V.t

module Make_indexed_file
    (_ : NAME)
    (K : Index.Key.S)
    (V : ENCODABLE_VALUE_HEADER) :
  INDEXED_FILE
    with type key := K.t
     and type value := V.t
     and type header := V.Header.t

module Make_simple_indexed_file
    (_ : NAME)
    (K : Index.Key.S) (V : sig
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
end) : Index.Key.S with type t = E.t
