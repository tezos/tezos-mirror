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

open Protocol
open Alpha_context
module Maker = Irmin_pack_unix.Maker (Tezos_context_encoding.Context.Conf)

module IStore = struct
  include Maker.Make (Tezos_context_encoding.Context.Schema)
  module Schema = Tezos_context_encoding.Context.Schema
end

module IStoreTree =
  Tezos_context_helpers.Context.Make_tree
    (Tezos_context_encoding.Context.Conf)
    (IStore)

type tree = IStore.tree

type index = {path : string; repo : IStore.Repo.t}

type t = {index : index; tree : tree}

type commit = IStore.commit

type hash = IStore.hash

type path = string list

let hash_encoding =
  let open Data_encoding in
  conv
    (fun h -> IStore.Hash.to_raw_string h |> Bytes.unsafe_of_string)
    (fun b -> Bytes.unsafe_to_string b |> IStore.Hash.unsafe_of_raw_string)
    (Fixed.bytes IStore.Hash.hash_size)

let hash_to_raw_string = IStore.Hash.to_raw_string

let pp_hash fmt h =
  IStore.Hash.to_raw_string h
  |> Hex.of_string |> Hex.show |> Format.pp_print_string fmt

let load configuration =
  let open Lwt_syntax in
  let open Configuration in
  let path = default_context_dir configuration.data_dir in
  let+ repo = IStore.Repo.v (Irmin_pack.config path) in
  {path; repo}

let close ctxt = IStore.Repo.close ctxt.repo

let raw_commit ?(message = "") index tree =
  let info = IStore.Info.v ~author:"Tezos" 0L ~message in
  IStore.Commit.v index.repo ~info ~parents:[] tree

let commit ?message ctxt =
  let open Lwt_syntax in
  let+ commit = raw_commit ?message ctxt.index ctxt.tree in
  IStore.Commit.hash commit

let checkout index key =
  let open Lwt_syntax in
  let* o = IStore.Commit.of_hash index.repo key in
  match o with
  | None -> return_none
  | Some commit ->
      let tree = IStore.Commit.tree commit in
      return_some {index; tree}

let empty index = {index; tree = IStore.Tree.empty ()}

let is_empty ctxt = IStore.Tree.is_empty ctxt.tree

module Proof (Hash : sig
  type t

  val of_context_hash : Context_hash.t -> t
end) =
struct
  module IStoreProof =
    Tezos_context_helpers.Context.Make_proof
      (IStore)
      (Tezos_context_encoding.Context.Conf)

  module Tree = struct
    include IStoreTree

    type nonrec t = index

    type tree = IStore.tree

    type key = path

    type value = bytes
  end

  type tree = Tree.tree

  type proof = IStoreProof.Proof.tree IStoreProof.Proof.t

  let hash_tree tree = Hash.of_context_hash (Tree.hash tree)

  let proof_encoding =
    Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V1.Tree32
    .tree_proof_encoding

  let proof_before proof =
    let (`Value hash | `Node hash) = proof.IStoreProof.Proof.before in
    Hash.of_context_hash hash

  let proof_after proof =
    let (`Value hash | `Node hash) = proof.IStoreProof.Proof.after in
    Hash.of_context_hash hash

  let produce_proof index tree step =
    let open Lwt_syntax in
    (* Committing the context is required by Irmin to produce valid proofs. *)
    let* _commit_key = raw_commit index tree in
    match Tree.kinded_key tree with
    | Some k ->
        let* p = IStoreProof.produce_tree_proof index.repo k step in
        return (Some p)
    | None -> return None

  let verify_proof proof step =
    (* The rollup node is not supposed to verify proof. We keep
       this part in case this changes in the future. *)
    let open Lwt_syntax in
    let* result = IStoreProof.verify_tree_proof proof step in
    match result with
    | Ok v -> return (Some v)
    | Error _ ->
        (* We skip the error analysis here since proof verification is not a
           job for the rollup node. *)
        return None
end

(** Aggregated collection of messages from the L1 inbox *)
module MessageTrees = struct
  type value = tree

  let key = ["message_tree"]

  let find ctxt = IStore.Tree.find_tree ctxt.tree key

  let set ctxt tree =
    let open Lwt_syntax in
    let* tree = IStore.Tree.add_tree ctxt.tree key tree in
    let ctxt = {ctxt with tree} in
    return ctxt
end

module Inbox = struct
  include Sc_rollup.Inbox

  include Sc_rollup.Inbox.Make_hashing_scheme (struct
    include Proof (Hash)

    type t = index

    let commit_tree index _key tree =
      let open Lwt_syntax in
      let info () = IStore.Info.v ~author:"Tezos" 0L ~message:"" in
      let* (_ : IStore.commit) =
        IStore.Commit.v index.repo ~info:(info ()) ~parents:[] tree
      in
      return ()

    let from_inbox_hash inbox_hash =
      let ctxt_hash = Hash.to_context_hash inbox_hash in
      let store_hash =
        IStore.Hash.unsafe_of_raw_string (Context_hash.to_string ctxt_hash)
      in
      `Node store_hash

    let lookup_tree index hash =
      IStore.Tree.of_hash index.repo (from_inbox_hash hash)
  end)
end

(** State of the PVM that this rollup node deals with *)
module PVMState = struct
  type value = tree

  let key = ["pvm_state"]

  let find ctxt = IStore.Tree.find_tree ctxt.tree key

  let lookup tree path = IStore.Tree.find tree path

  let set ctxt state =
    let open Lwt_syntax in
    let+ tree = IStore.Tree.add_tree ctxt.tree key state in
    {ctxt with tree}
end
