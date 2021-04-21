(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
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

open Tezos_storage_encoding.Context
module AO = Irmin.Content_addressable (Irmin_mem.Append_only)
module RW = Irmin_mem.Atomic_write
module Store =
  Irmin.Make_ext (AO) (RW) (Metadata) (Contents) (Path) (Branch) (Hash) (Node)
    (Commit)

type t = Store.tree

type tree = t

type key = string list

type value = bytes

module Tree = Tezos_storage_helpers.Context.Make_tree (Store)
include Tree

let data_key key = "data" :: key

let mem t key = Tree.mem t (data_key key)

let mem_tree t key = Tree.mem_tree t (data_key key)

let list t ?offset ?length key = Tree.list t ?offset ?length (data_key key)

let find t key = Tree.find t (data_key key)

let add t key data = Tree.add t (data_key key) data

let remove t key = Tree.remove t (data_key key)

let find_tree t key = Tree.find_tree t (data_key key)

let add_tree t key tree = Tree.add_tree t (data_key key) tree

let fold ?depth t key ~init ~f = Tree.fold ?depth t (data_key key) ~init ~f

let current_protocol_key = ["protocol"]

let get_protocol t =
  Tree.find t current_protocol_key
  >>= function
  | None ->
      assert false
  | Some data ->
      Lwt.return (Protocol_hash.of_bytes_exn data)

let add_protocol t key =
  let key = Protocol_hash.to_bytes key in
  Tree.add t current_protocol_key key

let empty = Store.Tree.empty

let concrete_encoding : Store.Tree.concrete Data_encoding.t =
  let open Data_encoding in
  mu "memory_context" (fun encoding ->
      let map_encoding = list (tup2 string encoding) in
      union
        [ case
            ~title:"tree"
            (Tag 0)
            map_encoding
            (function `Tree map -> Some map | `Contents _ -> None)
            (fun map -> `Tree map);
          case
            ~title:"value"
            (Tag 1)
            bytes
            (function `Contents (v, _) -> Some v | `Tree _ -> None)
            (fun v -> `Contents (v, ())) ])

let encoding : t Data_encoding.t =
  Data_encoding.conv
    (fun t ->
      let tree = Store.Tree.to_concrete t in
      let tree =
        (* This is safe as store.Tree will never call any blocking
           functions. *)
        match Lwt.state tree with Return t -> t | _ -> assert false
      in
      tree)
    Store.Tree.of_concrete
    concrete_encoding
