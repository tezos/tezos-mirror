(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3739
              Add .mli *)

module Maker = Irmin_pack_unix.Maker (Tezos_context_encoding.Context.Conf)

module IStore = struct
  include Maker.Make (Tezos_context_encoding.Context.Schema)
  module Schema = Tezos_context_encoding.Context.Schema
end

type t = IStore.t

type tree = IStore.tree

type path = string list

let load data_dir =
  let open Lwt_syntax in
  let* repo = IStore.Repo.v (Irmin_pack.config data_dir) in
  IStore.main repo

let flush store = IStore.flush (IStore.repo store)

let close store = IStore.Repo.close (IStore.repo store)

let info message =
  let date =
    Tezos_base.Time.(System.now () |> System.to_protocol |> Protocol.to_seconds)
  in
  Irmin.Info.Default.v ~author:"Tezos smart-contract rollup node" ~message date

(** Module type respresenting a [Mutable_value] that is persisted on store. *)
module type Mutable_value = sig
  (** the type of the values that will be persisted. *)
  type value

  (** [path_key] is the path that is used to locate the variable in the backend
      storage. *)
  val path_key : path

  (** [set store value] persists [value] on [store] at the path specified by
      [path_key] *)
  val set : t -> value -> unit Lwt.t

  (** [get store] retrievs the value persisted at location [path_key] in the
      store. If no value is persisted at the location indicated by [path_key],
      an error is thrown. *)
  val get : t -> value Lwt.t

  (** [find store] retrievs the value persisted at location [path_key] in the
      store, and returns it as an optional value.  If no value is persisted at
      the location indicated by [path_key], [None] is returned *)
  val find : t -> value option Lwt.t
end

(** The module [KeyValue] specifies information about how maps should be
    persisted on disk. *)
module type KeyValue = sig
  (** [path] is used to identify where a map is persisted on disk.  *)
  val path : path

  (** [keep_last_n_entries_in_memory] specifies how many entries for a
      map should be cached in memory. It is currently not used. *)
  val keep_last_n_entries_in_memory : int

  (** [key] is the type used for keys of maps persisted on disk. *)
  type key

  (** [string_of_key key] constructs the location where an individual [value]
      from the map is persisted on disk.  *)
  val string_of_key : key -> string

  (** [value] is the type of values that are going to be persisted on disk. *)
  type value

  (** [value_encoding] determines how a [value] should be encoded/decoded
      when being persisted to or retrieved from disk. *)
  val value_encoding : value Data_encoding.t
end

(** Generic module type for maps to be persisted on disk. *)
module type Map = sig
  (** The type of keys persisted by the map. *)
  type key

  (** The type of values persisted by the map. *)
  type value

  (** [mem store key] checks whether there is a binding of the map for key [key]
      in [store]. *)
  val mem : t -> key -> bool Lwt.t

  (** [get store key] retrieves from [store] the value associated with [key] in
      the map. It raises an error if such a value does not exist. *)
  val get : t -> key -> value Lwt.t

  (** [find store key] retrieves from [store] the value associated with [key]
      in the map. If the value exists it is returned as an optional value.
      Otherwise, [None] is returned. *)
  val find : t -> key -> value option Lwt.t

  (** [find_with_default ~on_default store key] retrieves from [store] the
      value associated with [key]
      in the map. If the value exists it is returned as is. Otherwise,
      [on_default] is returned.*)
  val find_with_default : t -> key -> on_default:(unit -> value) -> value Lwt.t

  val add : t -> key -> value -> unit Lwt.t
end

module Make_map (P : KeyValue) = struct
  type key = P.key

  type value = P.value

  (* Ignored for now. *)
  let _ = P.keep_last_n_entries_in_memory

  let path_key = P.path

  let make_key key = path_key @ [P.string_of_key key]

  let mem store key = IStore.mem store (make_key key)

  let decode_value encoded_value =
    Data_encoding.Binary.of_bytes_exn P.value_encoding encoded_value

  let get store key =
    let open Lwt_syntax in
    let+ e = IStore.get store (make_key key) in
    decode_value e

  let find store key =
    let open Lwt_option_syntax in
    let+ value = IStore.find store (make_key key) in
    decode_value value

  let find_with_default store key ~on_default =
    let open Lwt_syntax in
    let+ value = find store key in
    Option.value_f value ~default:on_default
end

(** [Make_updatable_map(P)] constructs a [Map] which can be persisted on store,
    using the information provided by [P] for stroing keys and values. The
    resulting map allows to update the contents of an existing value for a key.
*)
module Make_updatable_map (P : KeyValue) = struct
  include Make_map (P)

  let add store key value =
    let full_path = String.concat "/" (P.path @ [P.string_of_key key]) in
    let encode v = Data_encoding.Binary.to_bytes_exn P.value_encoding v in
    let encoded_value = encode value in
    let info () = info full_path in
    IStore.set_exn ~info store (make_key key) encoded_value
end

(** [Make_append_only_map(P)] constructs a [Map] which can be persisted on
    store, using the information provided by [P] for stroing keys and values.
    The resulting map prevents updating the value for a key to a differet one.
*)
module Make_append_only_map (P : KeyValue) = struct
  include Make_map (P)

  let add store key value =
    let open Lwt_syntax in
    let* existing_value = find store key in
    let full_path = String.concat "/" (P.path @ [P.string_of_key key]) in
    let encode v = Data_encoding.Binary.to_bytes_exn P.value_encoding v in
    let encoded_value = encode value in
    match existing_value with
    | None ->
        let info () = info full_path in
        IStore.set_exn ~info store (make_key key) encoded_value
    | Some existing_value ->
        (* To be robust to interruption in the middle of processes,
           we accept to redo some work when we restart the node.
           Hence, it is fine to insert twice the same value for a
           given value. *)
        if not (Bytes.equal (encode existing_value) encoded_value) then
          Stdlib.failwith
            (Printf.sprintf
               "Key %s already exists with a different value"
               full_path)
        else return_unit
end

(** [Make_mutable_value(P)] constructs a [Mutable_value] for persisting mutable
    values on disk. The type of values to be stored as well as how these are
    encoded into disk are specified by the functor argument [P]. *)
module Make_mutable_value (P : sig
  val path : path

  type value

  val value_encoding : value Data_encoding.t
end) =
struct
  type value = P.value

  let path_key = P.path

  let decode_value encoded_value =
    Data_encoding.Binary.of_bytes_exn P.value_encoding encoded_value

  let set store value =
    let encoded_value =
      Data_encoding.Binary.to_bytes_exn P.value_encoding value
    in
    let info () = info (String.concat "/" P.path) in
    IStore.set_exn ~info store path_key encoded_value

  let get store =
    let open Lwt_syntax in
    let+ value = IStore.get store path_key in
    decode_value value

  let find store =
    let open Lwt_option_syntax in
    let+ value = IStore.find store path_key in
    decode_value value
end

(** [Nested_map] is a map where values are indexed by both a primary and
    secondary key. It allows more flexibility over a map whose keys are
    tupls of the form `(primary_key, secondary_key)`. In particular, it
    provides functions to retrieve all the bindings in a map that share the
    same primary_key.
*)
module type Nested_map = sig
  (** [primary_key] is the type of primary keys for the [Nested_map]. *)
  type primary_key

  (** [secondary_key] is the type of secondary keys for the [Nested_map]. *)
  type secondary_key

  (** [value] is the type of values that are going to be persisted on disk,
      indexed by primary and secondary key. *)
  type value

  (** [mem store ~primary_key ~secondary_key] returns whether there is a
      value for the nested map persisted on [store] for the nested map,
      indexed by [primary_key] and then by [secondary_key]. *)
  val mem :
    t -> primary_key:primary_key -> secondary_key:secondary_key -> bool Lwt.t

  (** [get store ~primary_key ~secondary_key] retrieves from [store] the value
      of the nested map associated with [primary_key] and [secondary_key], if
      any. If such a value does not exist, it raises an error. *)
  val get :
    t -> primary_key:primary_key -> secondary_key:secondary_key -> value Lwt.t

  (** [list secondary_keys store ~primary_key] retrieves from [store] the list
      of bindings of the nested map that share the same [~primary_key]. For
      each of these bindings, both the secondary_key and value are returned. *)
  val list_secondary_keys_with_values :
    t -> primary_key:primary_key -> (secondary_key * value) list Lwt.t

  (** [list_secondary_keys store ~primary_key] retrieves from [store]
      the list of secondary_keys for which a value indexed by both
      [primary_key] and secondary key is persisted on disk. *)
  val list_secondary_keys :
    t -> primary_key:primary_key -> secondary_key list Lwt.t

  (** [list_values store ~primary_key] retrieves from [store] the list of
      values for which a binding with primary key [primary_key] and
      arbitrary secondary key exists. *)
  val list_values : t -> primary_key:primary_key -> value list Lwt.t

  val add :
    t ->
    primary_key:primary_key ->
    secondary_key:secondary_key ->
    value ->
    unit Lwt.t
end

(** [DoubleKeyValue] provides information about how a [Nested_map] is persisted
    on disk. *)
module type DoubleKeyValue = sig
  (** [key] is the type of the primary key. *)
  type key

  (** [string_of_key key] converts [key] into a string. *)
  val string_of_key : key -> string

  (** [path] specifies the location where a [Nested_map] is persisted. *)
  val path : string list

  (** [keep_last_ne_entries_in_memory] specifies how many entries in a
      [Nested_map] should be retained in memory. It is currently not used. *)
  val keep_last_n_entries_in_memory : int

  (** [value] is the type of values of a [Nested_map] to be persisted on
      disk. *)
  type value

  (** [value_name] is a string identifying the type of a [value]. *)
  val value_name : string

  (** The type of the [secodary_key]. *)
  type secondary_key

  (** [secondary_key_name] is a string identifying the type of
      [secondary_key]. *)
  val secondary_key_name : string

  (** [compare_secondary_key key1 key2] compares two secondary keys. *)
  val compare_secondary_keys : secondary_key -> secondary_key -> int

  (** [secondary_key_encoding] is an encoding specifying how secondary keys
      should be persisted on disk. *)
  val secondary_key_encoding : secondary_key Data_encoding.t

  (** [value_encoding] is an encoding specifying how values are persisted on
      disk. *)
  val value_encoding : value Data_encoding.t
end

(** [Make_nested_map(K)] build a [Nested_map] using the information provided
    in [K] to persist keys and values. The result is a [Nested_map]  which
    wraps an [Updatable map] whose keys have type [K.key], and whose values
    are [K.value] valued in-memory maps indexed by [K.secondary_key]. *)
module Make_nested_map (K : DoubleKeyValue) = struct
  type primary_key = K.key

  type secondary_key = K.secondary_key

  type value = K.value

  module Secondary_key_map = Map.Make (struct
    type t = K.secondary_key

    let compare = K.compare_secondary_keys
  end)

  module Map_as_value = struct
    type key = K.key

    type value = K.value Secondary_key_map.t

    let path = K.path

    let keep_last_n_entries_in_memory = K.keep_last_n_entries_in_memory

    let string_of_key = K.string_of_key

    let value_encoding =
      Data_encoding.conv
        (fun dal_slots_map -> Secondary_key_map.bindings dal_slots_map)
        (fun dal_slots_bindings ->
          Secondary_key_map.of_seq @@ List.to_seq dal_slots_bindings)
        Data_encoding.(
          list
          @@ obj2
               (req K.secondary_key_name K.secondary_key_encoding)
               (req K.value_name K.value_encoding))
  end

  module M = Make_updatable_map (Map_as_value)

  (* Return the bindings associated with a primary key. *)
  let list_secondary_keys_with_values store ~primary_key =
    let open Lwt_syntax in
    let+ slots_map = M.find store primary_key in
    Option.fold ~none:[] ~some:Secondary_key_map.bindings slots_map

  (* Check whether the updatable store contains an entry
     for the primary_key, which itself contains a
     binding whose key is ~secondary_key. *)
  let mem store ~primary_key ~secondary_key =
    let open Lwt_syntax in
    let* primary_key_binding_exists = M.mem store primary_key in
    if not primary_key_binding_exists then return false
    else
      let+ secondary_key_map = M.get store primary_key in
      Secondary_key_map.mem secondary_key secondary_key_map

  (* unsafe call. The existence of a value for
     primary and secondary key in the store must be
     checked before invoking this function. *)
  let get store ~primary_key ~secondary_key =
    let open Lwt_syntax in
    let+ secondary_key_map = M.get store primary_key in
    match Secondary_key_map.find secondary_key secondary_key_map with
    | None ->
        (* value for primary and secondary key does not exist *)
        assert false
    | Some value -> value

  (* Returns the set of keys from the bindings of
     ~primary_key in the store. *)
  let list_secondary_keys store ~primary_key =
    let open Lwt_syntax in
    let+ secondary_keys_with_values =
      list_secondary_keys_with_values store ~primary_key
    in
    List.map (fun (key, _value) -> key) secondary_keys_with_values

  (* Returns the set of values from the bindings of
     ~primary_key in the store. *)
  let list_values store ~primary_key =
    let open Lwt_syntax in
    let+ secondary_keys_with_values =
      list_secondary_keys_with_values store ~primary_key
    in
    List.map (fun (_key, value) -> value) secondary_keys_with_values

  (* Updates the entry of the updatable map with key ~primary_key
     by adding to it a binding with key ~secondary_key and
     value `value`.*)
  let add store ~primary_key ~secondary_key value =
    let open Lwt_syntax in
    let* value_map = M.find store primary_key in
    let value_map = Option.value ~default:Secondary_key_map.empty value_map in
    let value_can_be_added =
      match Secondary_key_map.find secondary_key value_map with
      | None -> true
      | Some old_value ->
          Data_encoding.Binary.(
            Bytes.equal
              (to_bytes_exn K.value_encoding old_value)
              (to_bytes_exn K.value_encoding value))
    in
    if value_can_be_added then
      let updated_map = Secondary_key_map.add secondary_key value value_map in
      M.add store primary_key updated_map
    else
      Stdlib.failwith
        (Printf.sprintf
           "A binding for binding %s under block_hash %s already exists with a \
            different %s"
           (Data_encoding.Binary.to_string_exn
              K.secondary_key_encoding
              secondary_key)
           (K.string_of_key primary_key)
           K.value_name)
end
