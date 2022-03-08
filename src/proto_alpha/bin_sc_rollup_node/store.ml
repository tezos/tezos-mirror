(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol.Alpha_context
module Maker = Irmin_pack_unix.Maker (Tezos_context_encoding.Context.Conf)
module IStore = Maker.Make (Tezos_context_encoding.Context.Schema)

type t = IStore.t

type path = string list

let load configuration =
  let open Lwt_syntax in
  let open Configuration in
  let* repo = IStore.Repo.v (Irmin_pack.config configuration.data_dir) in
  IStore.main repo

let close store = IStore.Repo.close (IStore.repo store)

let info message =
  let date = Unix.gettimeofday () |> int_of_float |> Int64.of_int in
  Irmin.Info.Default.v ~author:"Tezos smart-contract rollup node" ~message date

module Make_append_only_map (P : sig
  val path : path

  val keep_last_n_entries_in_memory : int

  type key

  val string_of_key : key -> string

  type value

  val value_encoding : value Data_encoding.t
end) =
struct
  (* Ignored for now. *)
  let _ = P.keep_last_n_entries_in_memory

  let path_key = P.path

  let make_key key = path_key @ [P.string_of_key key]

  let mem store key = IStore.mem store (make_key key)

  let decode_value encoded_value =
    Lwt.return
    @@ Data_encoding.Binary.of_bytes_exn P.value_encoding encoded_value

  let get store key = IStore.get store (make_key key) >>= decode_value

  let add store key value =
    let open Lwt_syntax in
    let* already_exists = mem store key in
    assert (not already_exists) ;
    let encoded_value =
      Data_encoding.Binary.to_bytes_exn P.value_encoding value
    in
    let info () = info (String.concat "/" P.path ^ P.string_of_key key) in
    IStore.set_exn ~info store (make_key key) encoded_value
end

module Make_mutable_value (P : sig
  val path : path

  type value

  val value_encoding : value Data_encoding.t
end) =
struct
  let path_key = P.path

  let decode_value encoded_value =
    Lwt.return
    @@ Data_encoding.Binary.of_bytes_exn P.value_encoding encoded_value

  let set store value =
    let encoded_value =
      Data_encoding.Binary.to_bytes_exn P.value_encoding value
    in
    let info () = info (String.concat "/" P.path) in
    IStore.set_exn ~info store path_key encoded_value

  let get store = IStore.get store path_key >>= decode_value

  let find store =
    let open Lwt_syntax in
    let* value = IStore.find store path_key in
    Option.map_s decode_value value
end

module IStoreTree = struct
  include
    Tezos_context_helpers.Context.Make_tree
      (Tezos_context_encoding.Context.Conf)
      (struct
        include IStore
        module Schema = Tezos_context_encoding.Context.Schema
      end)

  type t = IStore.t

  type tree = IStore.tree

  type key = path

  type value = bytes
end

module Inbox = Sc_rollup.Inbox.MakeHashingScheme (IStoreTree)

module MessageTrees = struct
  let key block_hash = ["message_tree"; Block_hash.to_b58check block_hash]

  (** [get store block_hash] retrieves the message tree for [block_hash]. If it is not present, an empty
      tree is returned. *)
  let get store block_hash =
    let open Lwt_syntax in
    let+ tree = IStore.find_tree store (key block_hash) in
    Option.value ~default:(IStoreTree.empty ()) tree

  (** [set store block_hash message_tree] set the message tree for [block_hash]. *)
  let set store block_hash message_tree =
    IStore.set_tree_exn
      ~info:(fun () -> info "Update messages tree")
      store
      (key block_hash)
      message_tree
end

module Messages = Make_append_only_map (struct
  let path = ["messages"]

  let keep_last_n_entries_in_memory = 10

  type key = Block_hash.t

  let string_of_key = Block_hash.to_b58check

  type value = string list

  let value_encoding = Data_encoding.(list string)
end)

module Inboxes = Make_append_only_map (struct
  let path = ["inboxes"]

  let keep_last_n_entries_in_memory = 10

  type key = Block_hash.t

  let string_of_key = Block_hash.to_b58check

  type value = Sc_rollup.Inbox.t

  let value_encoding = Sc_rollup.Inbox.encoding
end)

module Histories = Make_append_only_map (struct
  let path = ["histories"]

  let keep_last_n_entries_in_memory = 10

  type key = Block_hash.t

  let string_of_key = Block_hash.to_b58check

  type value = Inbox.history

  let value_encoding = Inbox.history_encoding
end)
