(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** This module implements the signature of
    src/lib_protocol_environment/environment_context_intf.ml. Most functions are
    in Lwt because it needs to follow the module's signature, but we could
    remove most of it and just wrap it where it's purely necessary. *)

(* TODO: Most of the values are stored under `/data` in the tree. Not all of them,
   it's handled really badly, we need to check thoroughly where it needs to be a
   [data_key]. *)

module Tezedge = Octez_rust_tezos_context.Rust_tezedge_gen

[@@@warning "-duplicate-definitions"]

type patch_context = t -> t tzresult Lwt.t

(* TODO: get rid of this and use tezedge type directly *)
and extra_payload = {patch_context : patch_context option; base_path : string}

and index = {index : Tezedge.index; extra : extra_payload}

and t = {context : Tezedge.context; extra : extra_payload}

[@@@warning "+duplicate-definitions"]

type key = string list

let data_key key = "data" :: key

type value = bytes

type tree = Tezedge.tree

let equal_config _ _ = assert false

let mem_tree ctxt key =
  Lwt.return (Tezedge.mem_tree ctxt.context (data_key key))

let find_raw ctxt key = Tezedge.find ctxt.context key

let find ctxt key = Lwt.return (find_raw ctxt (data_key key))

let find_tree ctxt key =
  Lwt.return (Tezedge.find_tree ctxt.context (data_key key))

let list ctxt ?offset ?length key =
  Lwt.return @@ Array.to_list
  @@ Tezedge.list ctxt.context offset length (data_key key)

let length ctxt key = Lwt.return (Tezedge.length ctxt.context (data_key key))

let add_raw {context; extra} (key : key) (v : value) =
  let context = Tezedge.add context key v in
  Lwt.return {context; extra}

let add (ctxt : t) (key : key) (v : value) = add_raw ctxt (data_key key) v

[@@@warning "+duplicate-definitions"]

let add_tree {context; extra} key tree =
  let context = Tezedge.add_tree context (data_key key) tree in
  Lwt.return {context; extra}

let remove_raw {context; extra} key =
  let context = Tezedge.remove context key in
  Lwt.return {context; extra}

let remove ctxt key = remove_raw ctxt (data_key key)

let config _ = assert false

module Tree = struct
  let mem tree key = Lwt.return (Tezedge.tree_mem tree key)

  let mem_tree tree key = Lwt.return (Tezedge.tree_mem_tree tree key)

  let find tree key = Lwt.return (Tezedge.tree_find tree key)

  let find_tree tree key = Lwt.return (Tezedge.tree_find_tree tree key)

  let list tree ?offset ?length key =
    Lwt.return @@ Array.to_list @@ Tezedge.tree_list tree offset length key

  let length tree key = Lwt.return (Tezedge.tree_length tree key)

  let add tree key value = Lwt.return (Tezedge.tree_add tree key value)

  let add_tree tree key value =
    Lwt.return (Tezedge.tree_add_tree tree key value)

  let remove tree key = Lwt.return (Tezedge.tree_remove tree key)

  let fold ?depth t k ~order ~init ~f =
    let open Lwt_syntax in
    let depth : Tezedge.o_caml_depth option =
      Option.map
        (function
          | `Eq i -> Tezedge.Eq i
          | `Le i -> Le i
          | `Lt i -> Lt i
          | `Ge i -> Ge i
          | `Gt i -> Gt i)
        depth
    in
    let order : Tezedge.o_caml_order =
      match order with
      | `Sorted -> Tezedge.Sorted
      | `Undefined -> Tezedge.Undefined
    in
    let walker = Tezedge.make_tree_walker t depth k order in
    let rec aux acc =
      match Tezedge.tree_walker_next walker with
      | None -> Lwt.return acc
      | Some (key, tree) ->
          let* acc = f (Array.to_list key) tree acc in
          aux acc
    in
    aux init

  let config _ = assert false

  let empty {context; _} = Tezedge.tree_empty context

  let is_empty tree = Tezedge.tree_is_empty tree

  let kind tree =
    match Tezedge.tree_kind tree with Tezedge.Value -> `Value | Tree -> `Tree

  let to_value tree = Lwt.return (Tezedge.tree_to_value tree)

  let of_value ctxt value =
    Lwt.return (Tezedge.tree_of_value ctxt.context value)

  let hash tree = Context_hash.of_bytes_exn @@ Tezedge.tree_hash tree

  let equal tree1 tree2 = Tezedge.tree_equal tree1 tree2

  let clear ?depth:_ _ =
    (* It wasn't implemented in Tezedge branch, we'll see. *)
    assert false

  let pp _ _ = assert false
end

let protocol_key = ["protocol"]

let test_chain_key = ["test_chain"]

let predecessor_block_metadata_hash_key = ["predecessor_block_metadata_hash"]

let predecessor_ops_metadata_hash_key = ["predecessor_ops_metadata_hash"]

let get_protocol ctxt =
  match find_raw ctxt protocol_key with
  | None -> assert false
  | Some v -> Lwt.return (Protocol_hash.of_bytes_exn v)

let set_hash_version _ _ = assert false

let get_hash_version _ = assert false

module Proof = Tezos_context_sigs.Context.Proof_types

let verify_tree_proof _ _ = assert false

let verify_stream_proof _ _ = assert false

let fold ?depth t k ~order ~init ~f =
  let tree = Tezedge.get_tree t.context in
  Tree.fold ?depth tree (data_key k) ~order ~init ~f

let mem ctxt key = Lwt.return (Tezedge.mem ctxt.context (data_key key))

let index {context; extra} =
  let index = Tezedge.index context in
  {index; extra}

let init ?patch_context dir =
  let index = Tezedge.index_init dir in
  {index; extra = {patch_context; base_path = dir}}

let close index = Tezedge.index_close index.index

let raw_commit ~time ?(message = "") context =
  Tezedge.commit context.context (Time.Protocol.to_seconds time) "Tezos" message

let commit ~time ?message context =
  Lwt.return @@ Context_hash.of_bytes_exn @@ raw_commit ~time ?message context

let add_protocol ctxt protocol =
  let bytes = Protocol_hash.to_bytes protocol in
  add_raw ctxt protocol_key bytes

let add_test_chain ctxt v =
  let bytes = Data_encoding.Binary.to_bytes_exn Test_chain_status.encoding v in
  add_raw ctxt test_chain_key bytes

let fork_test_chain t ~protocol ~expiration =
  add_test_chain t (Test_chain_status.Forking {protocol; expiration})

let hash ~time ?(message = "") {context; _} =
  let hash =
    Tezedge.hash context (Time.Protocol.to_seconds time) "Tezos" message
  in
  Context_hash.of_bytes_exn hash

let commit_genesis {index; extra} ~chain_id:_ ~time ~protocol =
  let open Lwt_result_syntax in
  let context = Tezedge.context_init index in
  let context = {context; extra} in
  let* context =
    match extra.patch_context with
    | None -> return context
    | Some patch_context -> patch_context context
  in
  let*! context = add_protocol context protocol in
  let*! context = add_test_chain context Not_running in
  let*! commit = commit context ~time ~message:"Genesis" in
  return commit

let exists (index : index) context_hash =
  Tezedge.exists index.index context_hash

let checkout {index; extra} context_hash =
  Option.map
    (fun context -> {context; extra})
    (Tezedge.checkout index context_hash)

let checkout_exn index context_hash =
  match checkout index context_hash with
  | None ->
      raise
        (Invalid_argument
           (Format.asprintf "Cannot checkout %a" Context_hash.pp context_hash))
  | Some ch -> Lwt.return ch

let set_protocol = add_protocol

let get_test_chain t =
  match find_raw t test_chain_key with
  | None -> assert false
  | Some data -> (
      match Data_encoding.Binary.of_bytes Test_chain_status.encoding data with
      | Error re ->
          Format.kasprintf
            (fun s -> Lwt.fail (Failure s))
            "Error in Context.get_test_chain: %a"
            Data_encoding.Binary.pp_read_error
            re
      | Ok r -> Lwt.return r)

let add_predecessor_block_metadata_hash t hash =
  let bytes =
    Data_encoding.Binary.to_bytes_exn Block_metadata_hash.encoding hash
  in
  add_raw t predecessor_block_metadata_hash_key bytes

let add_predecessor_ops_metadata_hash t hash =
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation_metadata_list_list_hash.encoding
      hash
  in
  add_raw t predecessor_ops_metadata_hash_key bytes

let compute_testchain_genesis forked_block =
  Block_hash.hash_bytes [Block_hash.to_bytes forked_block]

let compute_testchain_chain_id genesis =
  Chain_id.of_block_hash (Block_hash.hash_bytes [Block_hash.to_bytes genesis])

let commit_test_chain_genesis _ _ = assert false

let merkle_tree_v2 _ctx _leaf_kind _key = assert false

let export_snapshot {index = _; extra = {base_path; _}} context_hash ~path =
  Tezedge.export_snapshot base_path context_hash path
