(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** The type of a [path]. [path]s are [strings]. They are used to determine
    the location where the data should be persisted on disk. *)
type path = string list

(** Keys in Map-like storage functors are represented as strings. *)
type key_path_representation = string

type rw = [`Read | `Write]

type ro = [`Read]

type _ mode = Read_only : ro mode | Read_write : rw mode

(** [BACKEND] is the module type defining the backend for persisting data to
    disk. It is used by the functors in [Store_utils] to create Storage
    modules that persist data structures on disk. *)
module type BACKEND = sig
  (** The type for representing storage backends that can be accessed by the
      module. The type parameter indicates whether the storage can be only read
      or both read and written. *)
  type +'a t

  (** [name] is the name of the store. *)
  val name : string

  (** [make_key_path path raw_key] constructs a new path from [path]
      and the [raw_key] key_path_representation. *)
  val make_key_path : path -> key_path_representation -> path

  (** [load location] loads the backend storage from [location]. *)
  val load : 'a mode -> string -> 'a t tzresult Lwt.t

  (** [flush location] flushes to disk a sequence of changes to the data stored
      at [location]. *)
  val flush : [> `Write] t -> unit tzresult

  (** [close t] closes the storage backend [t]. *)
  val close : _ t -> unit tzresult Lwt.t

  (** [set_exn t path b] sets the contents for the store [t] at [path] to the
      sequence of bytes [b]. The write operation can fail, in which case an
      exception is thrown. *)
  val set : [> `Write] t -> path -> bytes -> unit tzresult Lwt.t

  (** [get t path] returns the contents for the store [t] at the location
      indicated by [path]. It can fail if [t] does not have any content
      stored at [path]. *)
  val get : [> `Read] t -> path -> bytes tzresult Lwt.t

  (** [mem t path] returns whether the storage backend [t] contains any
      data at the location indicated by [path]. *)
  val mem : [> `Read] t -> path -> bool tzresult Lwt.t

  (** [find t path] is the same as [get t path], except that an optional
      value is returned. This value is [None] if the backend storage [t]
      does not have any content stored at location [path]. *)
  val find : [> `Read] t -> path -> bytes option tzresult Lwt.t

  (** [path_to_string] converts a path to a string. *)
  val path_to_string : path -> string

  (** [readonly t] returns a read only version of the storage [t]. *)
  val readonly : [> `Read] t -> [`Read] t
end

(** Module type respresenting a [Mutable_value] that is persisted on store. *)
module type Mutable_value = sig
  (** The type of the [store] that is used for persisting data on disk. *)
  type +'a store

  (** The type of the values that will be persisted. *)
  type value

  (** Path in the irmin tree. *)
  val path_key : path

  (** [set store value] persists [value] for this [Mutable_value] on [store]. *)
  val set : [> `Write] store -> value -> unit tzresult Lwt.t

  (** [get store] retrieves the value persisted in [store] for this
      [Mutable_value]. If the underlying storage backend fails to retrieve the
      contents of the mutable value by throwing an exception, then the
      exception is propagated by [get store].  *)
  val get : [> `Read] store -> value tzresult Lwt.t

  (** [find store] returns an optional value containing the value persisted
        in [store] for this [Mutable_value]. If no value is persisted for the
        [Mutable_value], [None] is returned. *)
  val find : [> `Read] store -> value option tzresult Lwt.t
end

(** This module contains information about where to store and retrieve contents
    of storage persisted on disk. *)
module type STORAGE_INFO = sig
  (** [path] is a value of type path, it identifies the location of a piece of
      storage in the underlying store. *)
  val path : path
end

(** [KEY] is a module type used to construct [Map]-like storage modules.
    It defines how keys are used to determine a path on disk. *)
module type KEY = sig
  (** [key] is the type used for keys of maps persisted on disk. *)
  type key

  val to_path_representation : key -> key_path_representation
end

(** [VALUE] is a module type to define values that will be persisted on disk
    via storage modules. *)
module type VALUE = sig
  (** The type of the [value] being persisted on disk. *)
  type value

  (** [name] is a string for identifying values of this type. It is
      used by encoders of [Map]-like storage modules. *)
  val name : string

  (** [encoding] defines how values are serialized and deserialized when being
      written to or read from the underlying store. *)
  val encoding : value Data_encoding.t
end

(** Generic module type for maps to be persisted on disk. *)
module type Map = sig
  (** The type of the [store] that is used for persisting data on disk. *)
  type +'a store

  (** The type of keys persisted by the map. *)
  type key

  (** The type of values persisted by the map. *)
  type value

  (** Path in the irmin tree. *)
  val path : path

  (** [mem store key] checks whether there is a binding of the map for key [key]
      in [store]. *)
  val mem : [> `Read] store -> key -> bool tzresult Lwt.t

  (** [get store key] retrieves from [store] the value associated with [key] in
      the map. It raises an error if such a value does not exist. *)
  val get : [> `Read] store -> key -> value tzresult Lwt.t

  (** [find store key] retrieves from [store] the value associated with [key]
      in the map. If the value exists it is returned as an optional value.
      Otherwise, [None] is returned. *)
  val find : [> `Read] store -> key -> value option tzresult Lwt.t

  (** [find_with_default ~on_default store key] retrieves from [store] the
      value associated with [key] in the map.
      If the value exists it is returned as is.
      Otherwise, [on_default] is returned. *)
  val find_with_default :
    [> `Read] store -> key -> on_default:(unit -> value) -> value tzresult Lwt.t

  (** [add store key value] adds a binding from [key] to [value] to the map, and
      persists it to disk. *)
  val add : [> `Write] store -> key -> value -> unit tzresult Lwt.t
end

(** Generic module type for append-only maps to be persisted on disk. *)
module type Append_only_map = sig
  include Map

  (** [add store key value] adds a binding from [key] to [value] to the map, and
      persists it to disk. If [key] already exists in the store, it must be
      bound to [value]. *)
  val add : rw store -> key -> value -> unit tzresult Lwt.t
end

(** [Nested_map] is a map where values are indexed by both a primary and
    secondary key. It allows more flexibility over a map whose keys are
    tupls of the form `(primary_key, secondary_key)`. In particular, it
    provides functions to retrieve all the bindings in a map that share the
    same primary_key.
*)
module type Nested_map = sig
  (** The type of the [store] that is used for persisting data on disk. *)
  type +'a store

  (** [primary_key] is the type of primary keys for the [Nested_map]. *)
  type primary_key

  (** [secondary_key] is the type of secondary keys for the [Nested_map]. *)
  type secondary_key

  (** [value] is the type of values that are going to be persisted on disk,
      indexed by primary and secondary key. *)
  type value

  (** Path in the irmin tree. *)
  val path : path

  (** [mem store ~primary_key ~secondary_key] returns whether there is a
      value for the nested map persisted on [store] for the nested map,
      indexed by [primary_key] and then by [secondary_key]. *)
  val mem :
    [> `Read] store ->
    primary_key:primary_key ->
    secondary_key:secondary_key ->
    bool tzresult Lwt.t

  (** [get store ~primary_key ~secondary_key] retrieves from [store] the value
      of the nested map associated with [primary_key] and [secondary_key], if
      any. If such a value does not exist, it fails with error
      [Store_errors.Cannot_read_key_from_store k], where [k] is the string
      representation of the primary key. *)
  val get :
    [> `Read] store ->
    primary_key:primary_key ->
    secondary_key:secondary_key ->
    value tzresult Lwt.t

  (** [find store ~primary_key ~secondary_key] is the same as
      [get store ~primary_key ~secondary_key], except that no error is thrown
      and an optional value is returned instead. The returned value is
      [None] if there is not a value bound to [primary_key] and [seconary_key]
      in the [store] for the [Nested_map]. *)
  val find :
    [> `Read] store ->
    primary_key:primary_key ->
    secondary_key:secondary_key ->
    value option tzresult Lwt.t

  (** [list secondary_keys store ~primary_key] retrieves from [store] the list
      of bindings of the nested map that share the same [~primary_key]. For
      each of these bindings, both the secondary_key and value are returned. *)
  val list_secondary_keys_with_values :
    [> `Read] store ->
    primary_key:primary_key ->
    (secondary_key * value) list tzresult Lwt.t

  (** [list_secondary_keys store ~primary_key] retrieves from [store]
      the list of secondary_keys for which a value indexed by both
      [primary_key] and secondary key is persisted on disk. *)
  val list_secondary_keys :
    [> `Read] store ->
    primary_key:primary_key ->
    secondary_key list tzresult Lwt.t

  (** [list_values store ~primary_key] retrieves from [store] the list of
      values for which a binding with primary key [primary_key] and
      arbitrary secondary key exists. *)
  val list_values :
    [> `Read] store -> primary_key:primary_key -> value list tzresult Lwt.t

  (** [add store ~primary_key ~secondary_key value] persists [value] to
      disk. The value is bound to the [primary_key] and [secondary_key].
  *)
  val add :
    rw store ->
    primary_key:primary_key ->
    secondary_key:secondary_key ->
    value ->
    unit tzresult Lwt.t
end

(** [COMPARABLE_KEY] is a module type used to define secondary keys in nested
    maps. *)
module type COMPARABLE_KEY = sig
  (** The type of a comparable [key]. *)
  type key

  (** [name] is a string to identify the value of [key]s. *)
  val name : string

  (** [compare key1 key2] compares [key1] with [key2]. *)
  val compare : key -> key -> int

  (** [encoding] is the encoding for values of type [key]. *)
  val encoding : key Data_encoding.t
end

module type Store = sig
  type +'a t

  (** [Make_updatable_map(S)(K)(V)] constructs a [Map] which can be persisted on
      store. The module [S] defines storage-dependent information about how the
      map will be saved on and retrieved from the store (for example, it defines
      the map location in the store). The module [K] defines the information
      related to keys of the map, and the module [V] contains information about
      how values will be stored to and retrieved from the store. The resulting
      map allows to update the contents of an existing value for a key.
*)
  module Make_updatable_map (S : STORAGE_INFO) (K : KEY) (V : VALUE) :
    Map with type 'a store = 'a t and type key = K.key and type value = V.value

  (** [Make_append_only_map(S)(K)(V)] constructs an [Append_only_map] which can be
      persisted on store. The module [S] defines storage-dependent information
      about how the map will be saved on and retrieved from the store (for
      example, it defines the map location in the store). The module [K]
      contains information related to keys of the map, and the module [V]
      contains information about how values will be stored to and retrieved from
      the store. The resulting map forbids updating the contents of an existing
      value with a new value, different from the previous one.
*)
  module Make_append_only_map (S : STORAGE_INFO) (K : KEY) (V : VALUE) :
    Append_only_map
      with type 'a store = 'a t
       and type key = K.key
       and type value = V.value

  (** [Make_mutable_value(S)(V)] constructs a [Mutable_value] for persisting a
      mutable value in a store. The module parameter [S] defines the location of
      the mutable value in the store, and the module parameter [V] contains
      information about the type of values that the constructed module will
      persist in the underlying store. *)
  module Make_mutable_value (S : STORAGE_INFO) (V : VALUE) :
    Mutable_value with type 'a store = 'a t and type value = V.value

  (** Make_nested_map(S)(K1)(K2)(V) constructs a [Nested_map] module using
      module parameter [S] to define where the map is going to be persisted on
      store, [K1] and [K2] to define the primary and secondary key,
      respectively, and [V] to define the values of the resulting
      [Nested_map]. *)
  module Make_nested_map
      (S : STORAGE_INFO)
      (K1 : KEY)
      (K2 : COMPARABLE_KEY)
      (V : VALUE) :
    Nested_map
      with type 'a store = 'a t
       and type primary_key = K1.key
       and type secondary_key = K2.key
       and type value = V.value
end
