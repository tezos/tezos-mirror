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

module type Mutable_value = sig
  type value

  val path_key : path

  val set : t -> value -> unit Lwt.t

  val get : t -> value Lwt.t

  val find : t -> value option Lwt.t
end

module type KeyValue = sig
  val path : path

  val keep_last_n_entries_in_memory : int

  type key

  val string_of_key : key -> string

  type value

  val value_encoding : value Data_encoding.t
end

module Make_map (P : KeyValue) = struct
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

module Make_updatable_map (P : KeyValue) = struct
  include Make_map (P)

  let add store key value =
    let full_path = String.concat "/" (P.path @ [P.string_of_key key]) in
    let encode v = Data_encoding.Binary.to_bytes_exn P.value_encoding v in
    let encoded_value = encode value in
    let info () = info full_path in
    IStore.set_exn ~info store (make_key key) encoded_value
end

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

(* A nested map is a (IStore) updatable map whose values are themselves
   maps constructed via the `Map.Make` functor, encoded in the Irmin
   store as lists of bindings. The module returned by the
   `Make_nested_map` functor implement some utility functions on top of
   Make_nested_map, to provide functionalities for  iterating over the
   bindings of the store value.
   It also shadows the get, add and mem operations of updatable_maps,
   to allow updating and retrieveing only one binding from the value of the
   store. The keys of the IStore updatable map are referred to
   as primary keys. The keys of the values of the updatable
   map are referred to as secondary keys.
*)
module Make_nested_map (K : sig
  type key

  val string_of_key : key -> string

  val path : string list

  val keep_last_n_entries_in_memory : int

  type secondary_key

  val secondary_key_name : string

  val value_name : string

  val compare_secondary_keys : secondary_key -> secondary_key -> int

  type value

  val secondary_key_encoding : secondary_key Data_encoding.t

  val value_encoding : value Data_encoding.t
end) =
struct
  module Secondary_key_map = Map.Make (struct
    type t = K.secondary_key

    let compare = K.compare_secondary_keys
  end)

  include Make_updatable_map (struct
    include K

    type value = K.value Secondary_key_map.t

    (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3732
       Use data encodings for maps once they are available in the
       `Data_encoding` library.
    *)
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
  end)

  (* Return the bindings associated with a primary key. *)
  let list_secondary_keys_with_values store ~primary_key =
    let open Lwt_syntax in
    let+ slots_map = find store primary_key in
    Option.fold ~none:[] ~some:Secondary_key_map.bindings slots_map

  (* Check whether the updatable store contains an entry
     for the primary_key, which itself contains a
     binding whose key is ~secondary_key. *)
  let mem store ~primary_key ~secondary_key =
    let open Lwt_syntax in
    let* primary_key_binding_exists = mem store primary_key in
    if not primary_key_binding_exists then return false
    else
      let+ secondary_key_map = get store primary_key in
      Secondary_key_map.mem secondary_key secondary_key_map

  (* unsafe call. The existence of a value for
     primary and secondary key in the store must be
     checked before invoking this function. *)
  let get store ~primary_key ~secondary_key =
    let open Lwt_syntax in
    let+ secondary_key_map = get store primary_key in
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
    let* value_map = find store primary_key in
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
      add store primary_key updated_map
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
