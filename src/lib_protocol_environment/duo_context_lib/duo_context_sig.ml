(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Irmin_disk = Tezos_context_disk.Context
module Irmin_mem = Tezos_context_memory.Context
module Brassaia_disk = Tezos_context_brassaia.Tezos_context.Context

module Brassaia_mem = Tezos_context_brassaia_memory.Tezos_context_memory.Context

module Tezedge = Tezos_context_tezedge.Context

module Internal = struct
  type wrapped_backend =
    | Irmin_disk
    | Irmin_mem
    | Brassaia_disk
    | Brassaia_mem
    | Tezedge

  type wrapped_index =
    | Irmin_disk_index of Irmin_disk.index
    | Brassaia_disk_index of Brassaia_disk.index
    | Irmin_mem_index of Irmin_mem.index
    | Brassaia_mem_index of Brassaia_mem.index
    | Tezedge_index of Tezedge.index

  type wrapped_context =
    | Irmin_disk_context of Irmin_disk.t
    | Irmin_mem_context of Irmin_mem.t
    | Brassaia_disk_context of Brassaia_disk.t
    | Brassaia_mem_context of Brassaia_mem.t
    | Tezedge_context of Tezedge.t

  type wrapped_tree =
    | Irmin_disk_tree of Irmin_disk.tree
    | Irmin_mem_tree of Irmin_mem.tree
    | Brassaia_disk_tree of Brassaia_disk.tree
    | Brassaia_mem_tree of Brassaia_mem.tree
    | Tezedge_tree of Tezedge.tree

  type index = {index_1 : wrapped_index; index_2 : wrapped_index}

  type t = {context_1 : wrapped_context; context_2 : wrapped_context}

  type key = string list

  type value = bytes

  type tree = {tree_1 : wrapped_tree; tree_2 : wrapped_tree}

  type wrapped_node_key =
    | Irmin_disk_nk of Irmin_disk.node_key
    | Irmin_mem_nk of Context_hash.t
    | Brassaia_disk_nk of Brassaia_disk.node_key
    | Brassaia_mem_nk of Context_hash.t

  type node_key = {node_key_1 : wrapped_node_key; node_key_2 : wrapped_node_key}

  type wrapped_value_key =
    | Irmin_disk_vk of Irmin_disk.value_key
    | Irmin_mem_vk of Context_hash.t
    | Brassaia_disk_vk of Brassaia_disk.value_key
    | Brassaia_mem_vk of Context_hash.t

  type value_key = {
    value_key_1 : wrapped_value_key;
    value_key_2 : wrapped_value_key;
  }

  type wrapped_kinded_key =
    | Irmin_disk_kk of Irmin_disk.kinded_key
    | Irmin_mem_kk of Irmin_mem.kinded_key
    | Brassaia_disk_kk of Brassaia_disk.kinded_key
    | Brassaia_mem_kk of Brassaia_mem.kinded_key

  type kinded_key = [`Node of node_key | `Value of value_key]
end

(** Because we want to avoid a mismatch between a duo_context module instance
    and a duo_context value, we do not want the user to be able to craft values
    of these types.

    For instance, {!module-Duo_context_lib.Duo_context} and
    {!module-Duo_context_lib.Duo_context_memory} share the same internal type
    representation, but we want to prevent the user to be able to use a
    {!type-Duo_context_lib.Duo_context.tree} with functions from
    {!module-Duo_context_lib.Duo_context_memory}.

    With this type abstraction, {!type-Duo_context_lib.Duo_context.tree} and
    {!type-Duo_context_lib.Duo_context_memory.tree} types will be made
    incompatible.
*)
module Abstract_types = struct
  type index

  type t

  type key = string list

  type value = bytes

  type tree

  type node_key

  type value_key

  type kinded_key = [`Node of node_key | `Value of value_key]
end

module type CONTEXT = sig
  include module type of Abstract_types

  val name : string

  module Tree : sig
    val mem : tree -> key -> bool Lwt.t

    val mem_tree : tree -> key -> bool Lwt.t

    val find : tree -> key -> value option Lwt.t

    val find_tree : tree -> key -> tree option Lwt.t

    val list :
      tree -> ?offset:int -> ?length:int -> key -> (string * tree) trace Lwt.t

    val length : tree -> key -> int Lwt.t

    val add : tree -> key -> value -> tree Lwt.t

    val add_tree : tree -> key -> tree -> tree Lwt.t

    val remove : tree -> key -> tree Lwt.t

    val fold :
      ?depth:Tezos_context_sigs.Context.depth ->
      tree ->
      key ->
      order:[`Sorted | `Undefined] ->
      init:'a ->
      f:(key -> tree -> 'a -> 'a Lwt.t) ->
      'a Lwt.t

    val config : tree -> Tezos_context_sigs.Config.t

    val empty : t -> tree

    val is_empty : tree -> bool

    val kind : tree -> Tezos_context_sigs.Context.Kind.t

    val to_value : tree -> value option Lwt.t

    val of_value : t -> value -> tree Lwt.t

    val hash : tree -> Context_hash.t

    val equal : tree -> tree -> bool

    val clear : ?depth:int -> tree -> unit

    val pp : Format.formatter -> tree -> unit

    type raw =
      [`Tree of raw Tezos_base.TzPervasives.String.Map.t | `Value of value]

    val raw_encoding : Tezos_context_helpers.Context.raw Data_encoding.t

    val to_raw : tree -> raw Lwt.t

    val of_raw : raw -> tree

    val unshallow : tree -> tree Lwt.t

    val is_shallow : tree -> bool

    val kinded_key : tree -> kinded_key option
  end

  module Proof = Tezos_context_sigs.Context.Proof_types

  val add_protocol : t -> Protocol_hash.t -> t Lwt.t

  val equal_config :
    Tezos_context_sigs.Config.t -> Tezos_context_sigs.Config.t -> bool

  val mem : t -> key -> bool Lwt.t

  val mem_tree : t -> key -> bool Lwt.t

  val find : t -> key -> value option Lwt.t

  val find_tree : t -> key -> tree option Lwt.t

  val list :
    t -> ?offset:int -> ?length:int -> key -> (string * tree) trace Lwt.t

  val length : t -> key -> int Lwt.t

  val add : t -> key -> value -> t Lwt.t

  val add_tree : t -> key -> tree -> t Lwt.t

  val remove : t -> key -> t Lwt.t

  val fold :
    ?depth:Tezos_context_sigs.Context.depth ->
    t ->
    key ->
    order:[`Sorted | `Undefined] ->
    init:'a ->
    f:(key -> tree -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  val config : t -> Tezos_context_sigs.Config.t

  val get_protocol : t -> Protocol_hash.t Lwt.t

  val fork_test_chain :
    t -> protocol:Protocol_hash.t -> expiration:Time.Protocol.t -> t Lwt.t

  val set_hash_version : t -> Context_hash.version -> t tzresult Lwt.t

  val get_hash_version : t -> Context_hash.version

  val verify_tree_proof :
    Proof.tree Proof.t ->
    (tree -> (tree * 'a) Lwt.t) ->
    ( tree * 'a,
      [ `Proof_mismatch of string
      | `Stream_too_long of string
      | `Stream_too_short of string ] )
    result
    Lwt.t

  val verify_stream_proof :
    Proof.stream Proof.t ->
    (tree -> (tree * 'a) Lwt.t) ->
    ( tree * 'a,
      [ `Proof_mismatch of string
      | `Stream_too_long of string
      | `Stream_too_short of string ] )
    result
    Lwt.t

  val index : t -> index

  val gc : index -> Context_hash.t -> unit Lwt.t

  val wait_gc_completion : index -> unit Lwt.t

  val is_gc_allowed : index -> bool

  val split : index -> unit Lwt.t

  val sync : index -> unit Lwt.t

  val exists : index -> Context_hash.t -> bool Lwt.t

  val close : index -> unit Lwt.t

  val compute_testchain_chain_id : Block_hash.t -> Chain_id.t

  val add_predecessor_block_metadata_hash :
    t -> Block_metadata_hash.t -> t Lwt.t

  val add_predecessor_ops_metadata_hash :
    t -> Operation_metadata_list_list_hash.t -> t Lwt.t

  val hash : time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t

  val commit_test_chain_genesis : t -> Block_header.t -> Block_header.t Lwt.t

  val get_test_chain : t -> Test_chain_status.t Lwt.t

  val add_test_chain : t -> Test_chain_status.t -> t Lwt.t

  val commit :
    time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t Lwt.t

  val commit_genesis :
    index ->
    chain_id:Chain_id.t ->
    time:Time.Protocol.t ->
    protocol:Protocol_hash.t ->
    Context_hash.t tzresult Lwt.t

  val compute_testchain_genesis : Block_hash.t -> Block_hash.t

  val export_snapshot : index -> Context_hash.t -> path:string -> unit Lwt.t

  val merkle_tree :
    t ->
    Proof.merkle_leaf_kind ->
    key ->
    Tezos_context_sigs.Context.Proof_types.merkle_tree Lwt.t

  val merkle_tree_v2 :
    t -> Proof.merkle_leaf_kind -> key -> Proof.tree Proof.t Lwt.t

  val checkout : index -> Context_hash.t -> t option Lwt.t

  val checkout_exn : index -> Context_hash.t -> t Lwt.t

  val set_protocol : t -> Protocol_hash.t -> t Lwt.t
end

module type CONTEXT_PARAM = sig
  val backend_1 : Internal.wrapped_backend

  val backend_2 : Internal.wrapped_backend

  type index_1

  type index_2

  val make_index : index_1 -> index_2 -> Internal.index
end

module type EXPORTED = sig
  include CONTEXT

  type _ Tezos_protocol_environment.Context.kind +=
    | Context : t Tezos_protocol_environment.Context.kind

  val equality_witness :
    (t, tree) Tezos_protocol_environment.Context.equality_witness

  val ops : (t, tree) Tezos_protocol_environment.Context.ops

  val impl_name : string

  val checkout :
    index -> Context_hash.t -> Tezos_protocol_environment.Context.t option Lwt.t

  val checkout_exn :
    index -> Context_hash.t -> Tezos_protocol_environment.Context.t Lwt.t

  val wrap_context : t -> Tezos_protocol_environment.Context.t

  val unwrap_context : Tezos_protocol_environment.Context.t -> t
end
