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

(* Backend-agnostic operations on the context *)

module Environment_context = Tezos_protocol_environment.Context
module Memory_context = Tezos_protocol_environment.Memory_context

let err_implementation_mismatch =
  Tezos_protocol_environment.err_implementation_mismatch

(** Values of type [index] are used to [checkout] contexts specified by their hash. *)
type index =
  | Disk_index of Context.index
  | Memory_index of Tezos_context_memory.Context.index

let index (context : Environment_context.t) =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Disk_index (Context.index ctxt)
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Memory_index (Tezos_context_memory.Context.index ctxt)
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let mem (context : Environment_context.t) key =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} -> Context.mem ctxt key
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.mem ctxt key
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let mem_tree (context : Environment_context.t) key =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} -> Context.mem_tree ctxt key
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.mem_tree ctxt key
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let find (context : Environment_context.t) key =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} -> Context.find ctxt key
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.find ctxt key
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let add (context : Environment_context.t) key data =
  let open Lwt_syntax in
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add ctxt key data in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt = Tezos_context_memory.Context.add ctxt key data in
      Memory_context.wrap_memory_context ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let fold_value ?depth (context : Environment_context.t) key ~order ~init ~f =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.fold ?depth ctxt key ~order ~init ~f:(fun k tree acc ->
          let v () = Context.Tree.to_value tree in
          f k v acc)
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let open Tezos_context_memory in
      Context.fold ?depth ctxt key ~order ~init ~f:(fun k tree acc ->
          let v () = Context.Tree.to_value tree in
          f k v acc)
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let add_protocol (context : Environment_context.t) proto_hash =
  let open Lwt_syntax in
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add_protocol ctxt proto_hash in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt = Tezos_context_memory.Context.add_protocol ctxt proto_hash in
      Memory_context.wrap_memory_context ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let get_protocol (context : Environment_context.t) =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} -> Context.get_protocol ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.get_protocol ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let add_predecessor_block_metadata_hash (context : Environment_context.t) hash =
  let open Lwt_syntax in
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add_predecessor_block_metadata_hash ctxt hash in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt =
        Tezos_context_memory.Context.add_predecessor_block_metadata_hash
          ctxt
          hash
      in
      Memory_context.wrap_memory_context ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let add_predecessor_ops_metadata_hash (context : Environment_context.t) hash =
  let open Lwt_syntax in
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add_predecessor_ops_metadata_hash ctxt hash in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt =
        Tezos_context_memory.Context.add_predecessor_ops_metadata_hash ctxt hash
      in
      Memory_context.wrap_memory_context ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let hash ~time ?message (context : Environment_context.t) =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.hash ~time ?message ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.hash ~time ?message ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let get_test_chain (context : Environment_context.t) =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.get_test_chain ctxt
  | Context {kind = Memory_context.Context; _} ->
      Lwt.return Test_chain_status.Not_running
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let add_test_chain (context : Environment_context.t) status =
  let open Lwt_syntax in
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add_test_chain ctxt status in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt = Tezos_context_memory.Context.add_test_chain ctxt status in
      Memory_context.wrap_memory_context ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let fork_test_chain (context : Environment_context.t) ~protocol ~expiration =
  let open Lwt_syntax in
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.fork_test_chain ctxt ~protocol ~expiration in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt =
        Tezos_context_memory.Context.fork_test_chain ctxt ~protocol ~expiration
      in
      Memory_context.wrap_memory_context ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let commit ~time ?message (context : Environment_context.t) =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.commit ~time ?message ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.commit ~time ?message ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let gc context_index context_hash =
  match context_index with
  | Disk_index index -> Context.gc index context_hash
  | Memory_index index -> Tezos_context_memory.Context.gc index context_hash

let sync = function
  | Disk_index index -> Context.sync index
  | Memory_index index -> Tezos_context_memory.Context.sync index

let commit_test_chain_genesis (context : Environment_context.t) block_header =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.commit_test_chain_genesis ctxt block_header
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.commit_test_chain_genesis ctxt block_header
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let compute_testchain_genesis (context : Environment_context.t) block_hash =
  match context with
  | Context {kind = Shell_context.Context; _} ->
      Context.compute_testchain_genesis block_hash
  | Context {kind = Memory_context.Context; _} ->
      Tezos_context_memory.Context.compute_testchain_genesis block_hash
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let merkle_tree (context : Environment_context.t) leaf_kind path =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.merkle_tree ctxt leaf_kind path
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.merkle_tree ctxt leaf_kind path
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let merkle_tree_v2 (context : Environment_context.t) leaf_kind path =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.merkle_tree_v2 ctxt leaf_kind path
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.merkle_tree_v2 ctxt leaf_kind path
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let commit_genesis context_index ~chain_id ~time ~protocol =
  match context_index with
  | Disk_index index -> Context.commit_genesis index ~chain_id ~time ~protocol
  | Memory_index index ->
      Tezos_context_memory.Context.commit_genesis
        index
        ~chain_id
        ~time
        ~protocol

let checkout context_index context_hash =
  let open Lwt_syntax in
  match context_index with
  | Disk_index index ->
      let+ ctxt = Context.checkout index context_hash in
      Option.map Shell_context.wrap_disk_context ctxt
  | Memory_index index ->
      let+ ctxt = Tezos_context_memory.Context.checkout index context_hash in
      Option.map Memory_context.wrap_memory_context ctxt

let checkout_exn context_index context_hash =
  let open Lwt_syntax in
  match context_index with
  | Disk_index index ->
      let+ ctxt = Context.checkout_exn index context_hash in
      Shell_context.wrap_disk_context ctxt
  | Memory_index index ->
      let+ ctxt =
        Tezos_context_memory.Context.checkout_exn index context_hash
      in
      Memory_context.wrap_memory_context ctxt

let exists context_index context_hash =
  match context_index with
  | Disk_index index -> Context.exists index context_hash
  | Memory_index index -> Tezos_context_memory.Context.exists index context_hash

let retrieve_commit_info context_index header =
  match context_index with
  | Disk_index index -> Context.retrieve_commit_info index header
  | Memory_index index ->
      Tezos_context_memory.Context.retrieve_commit_info index header

let close context_index =
  match context_index with
  | Disk_index index -> Context.close index
  | Memory_index index -> Tezos_context_memory.Context.close index
