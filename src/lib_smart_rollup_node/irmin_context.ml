(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

open Store_sigs
open Context_sigs
module Context_encoding = Tezos_context_encoding.Context_binary

(* We shadow [Tezos_context_encoding] to prevent accidentally using
   [Tezos_context_encoding.Context] instead of
   [Tezos_context_encoding.Context_binary] during a future
   refactoring.*)
module Tezos_context_encoding = struct end

module Maker = Irmin_pack_unix.Maker (Context_encoding.Conf)

module IStore = struct
  include Maker.Make (Context_encoding.Schema)
  module Schema = Context_encoding.Schema
end

module IStoreTree =
  Tezos_context_helpers.Context.Make_tree (Context_encoding.Conf) (IStore)

type repo = IStore.Repo.t

type tree = IStore.tree

type 'a raw_index = ('a, repo) Context_sigs.raw_index

type 'a index = ('a, repo) Context_sigs.index

type rw_index = [`Read | `Write] index

type ro_index = [`Read] index

type 'a t = ('a, repo, tree) Context_sigs.t

type rw = [`Read | `Write] t

type ro = [`Read] t

type commit = IStore.commit

type hash = Smart_rollup_context_hash.t

type path = string list

let () = assert (Smart_rollup_context_hash.size = IStore.Hash.hash_size)

let impl_name = "Irmin"

let equality_witness : (repo, tree) Context_sigs.equality_witness =
  (Context_sigs.Equality_witness.make (), Context_sigs.Equality_witness.make ())

let hash_to_istore_hash h =
  Smart_rollup_context_hash.to_string h |> IStore.Hash.unsafe_of_raw_string

let istore_hash_to_hash h =
  IStore.Hash.to_raw_string h |> Smart_rollup_context_hash.of_string_exn

let load : type a. cache_size:int -> a mode -> string -> a raw_index Lwt.t =
 fun ~cache_size mode path ->
  let open Lwt_syntax in
  let readonly = match mode with Read_only -> true | Read_write -> false in
  let+ repo =
    IStore.Repo.v
      (Irmin_pack.config
         ~readonly
           (* Note that the use of GC in the context requires that
            * the [always] indexing strategy not be used. *)
         ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal
         ~lru_size:cache_size
         path)
  in
  {path; repo}

let close ctxt =
  let _interrupted_gc = IStore.Gc.cancel ctxt.repo in
  IStore.Repo.close ctxt.repo

let readonly (index : [> `Read] index) = (index :> [`Read] index)

let raw_commit ?(message = "") index tree =
  let info = IStore.Info.v ~author:"Tezos" 0L ~message in
  IStore.Commit.v index.repo ~info ~parents:[] tree

let commit ?message ctxt =
  let open Lwt_syntax in
  let+ commit = raw_commit ?message ctxt.index ctxt.tree in
  IStore.Commit.hash commit |> istore_hash_to_hash

let checkout index key =
  let open Lwt_syntax in
  let* o = IStore.Commit.of_hash index.repo (hash_to_istore_hash key) in
  match o with
  | None -> return_none
  | Some commit ->
      let tree = IStore.Commit.tree commit in
      return_some {index; tree}

let empty index = {index; tree = IStore.Tree.empty ()}

let is_empty ctxt = IStore.Tree.is_empty ctxt.tree

let split ctxt = IStore.split ctxt.repo

(* adapted from lib_context/disk/context.ml *)
let gc index ?(callback : unit -> unit Lwt.t = fun () -> Lwt.return ())
    (hash : hash) =
  let open Lwt_syntax in
  let repo = index.repo in
  let istore_hash = hash_to_istore_hash hash in
  let* commit_opt = IStore.Commit.of_hash index.repo istore_hash in
  match commit_opt with
  | None ->
      Fmt.failwith "%a: unknown context hash" Smart_rollup_context_hash.pp hash
  | Some commit -> (
      let finished = function
        | Ok (stats : Irmin_pack_unix.Stats.Latest_gc.stats) ->
            let total_duration =
              Irmin_pack_unix.Stats.Latest_gc.total_duration stats
            in
            let finalise_duration =
              Irmin_pack_unix.Stats.Latest_gc.finalise_duration stats
            in
            let* () = callback () in
            Event.ending_context_gc
              ( Time.System.Span.of_seconds_exn total_duration,
                Time.System.Span.of_seconds_exn finalise_duration )
        | Error (`Msg err) -> Event.context_gc_failure err
      in
      let commit_key = IStore.Commit.key commit in
      let* launch_result = IStore.Gc.run ~finished repo commit_key in
      match launch_result with
      | Error (`Msg err) -> Event.context_gc_launch_failure err
      | Ok false -> Event.context_gc_already_launched ()
      | Ok true -> Event.starting_context_gc hash)

let wait_gc_completion index =
  let open Lwt_syntax in
  let* r = IStore.Gc.wait index.repo in
  match r with
  | Ok _stats_opt -> return_unit
  | Error (`Msg _msg) ->
      (* Logs will be printed by the [gc] caller. *)
      return_unit

let is_gc_finished index = IStore.Gc.is_finished index.repo

let index context = context.index

module Proof (Hash : sig
  type t

  val of_context_hash : Context_hash.t -> t
end) (Proof_encoding : sig
  val proof_encoding :
    Tezos_context_sigs.Context.Proof_types.tree
    Tezos_context_sigs.Context.Proof_types.t
    Data_encoding.t
end) =
struct
  module IStoreProof =
    Tezos_context_helpers.Context.Make_proof (IStore) (Context_encoding.Conf)

  module Tree = struct
    include IStoreTree

    type t = rw_index

    type tree = IStore.tree

    type key = path

    type value = bytes
  end

  type tree = Tree.tree

  type proof = IStoreProof.Proof.tree IStoreProof.Proof.t

  let hash_tree tree = Hash.of_context_hash (Tree.hash tree)

  let proof_encoding = Proof_encoding.proof_encoding

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
        return_some p
    | None -> return_none

  let verify_proof proof step =
    (* The rollup node is not supposed to verify proof. We keep
       this part in case this changes in the future. *)
    let open Lwt_syntax in
    let* result = IStoreProof.verify_tree_proof proof step in
    match result with
    | Ok v -> return_some v
    | Error _ ->
        (* We skip the error analysis here since proof verification is not a
           job for the rollup node. *)
        return_none
end

(** State of the PVM that this rollup node deals with. *)
module PVMState = struct
  type value = tree

  let key = ["pvm_state"]

  let empty () = IStore.Tree.empty ()

  let find ctxt = IStore.Tree.find_tree ctxt.tree key

  let lookup tree path = IStore.Tree.find tree path

  let set ctxt state =
    let open Lwt_syntax in
    let+ tree = IStore.Tree.add_tree ctxt.tree key state in
    {ctxt with tree}
end

let load ~cache_size mode path =
  let open Lwt_result_syntax in
  let*! index = load ~cache_size mode path in
  return index

module Internal_for_tests = struct
  let get_a_tree key =
    let tree = IStore.Tree.empty () in
    IStore.Tree.add tree [key] Bytes.empty
end
