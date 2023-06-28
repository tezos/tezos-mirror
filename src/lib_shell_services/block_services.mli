(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

module Proof = Tezos_context_sigs.Context.Proof_types

type version = Version_0 | Version_1 | Version_2

type chain = [`Main | `Test | `Hash of Chain_id.t]

type chain_prefix = unit * chain

val chain_path : (unit, chain_prefix) Tezos_rpc.Path.t

val parse_chain : string -> (chain, string) result

val chain_to_string : chain -> string

val chain_arg : chain Tezos_rpc.Arg.t

(** A representation of a block's position relatively to a known
    block of a chain. *)
type block =
  [ `Genesis  (** The genesis block *)
  | `Head of int
    (** The [n]th predecessor of the [current_head] block if [n > 0].
      If [n = 0], represents the [current_head]. [n] should
      not be negative since the [current_head] does not have
      successors. *)
  | `Alias of [`Caboose | `Checkpoint | `Savepoint] * int
    (** The [n]th predecessor of the [caboose], the [checkpoint]
      or the [savepoint] if [n > 0]. If [n = 0], represents the block itself.
      If [n < 0], represents the [n]th successor.  *)
  | `Hash of Block_hash.t * int
    (** The [n]th predecessor of the block of given [hash] if [n > 0].
      If [n = 0], represents the block itself.
      Otherwise, if [n < 0], represents the [n]th successor.*)
  | `Level of Int32.t  (** The block at a given [level] *) ]

val parse_block : string -> (block, string) result

type range = [`Level of Int32.t] * [`Level of Int32.t]

(** A block range in the form [level..level]. Currently, this function supports
    only [level..level]. *)
val parse_block_range : string -> (range, string) result

type block_or_range = Block of block | Range of range

val parse_block_or_range : string -> (block_or_range, string) result

val to_string : block -> string

type prefix = (unit * chain) * block

val dir_path : (chain_prefix, chain_prefix) Tezos_rpc.Path.t

val path : (chain_prefix, chain_prefix * block) Tezos_rpc.Path.t

val mempool_path : ('a, 'b) Tezos_rpc.Path.t -> ('a, 'b) Tezos_rpc.Path.t

val live_blocks_path : ('a, 'b) Tezos_rpc.Path.t -> ('a, 'b) Tezos_rpc.Path.t

type operation_list_quota = {max_size : int; max_op : int option}

val raw_context_encoding : Proof.raw_context Data_encoding.t

(** [raw_context_insert (k,v) c] inserts a key-value pair [(k,v)] in a raw_context [c].
    If [k] collides to a existing sub-tree in [c], the sub-tree is replaced by a new key-value pair. *)
val raw_context_insert :
  string list * Proof.raw_context -> Proof.raw_context -> Proof.raw_context

module type PROTO = sig
  val hash : Protocol_hash.t

  type block_header_data

  val block_header_data_encoding : block_header_data Data_encoding.t

  type block_header_metadata

  val block_header_metadata_encoding : block_header_metadata Data_encoding.t

  type operation_data

  type operation_receipt

  type operation = {
    shell : Operation.shell_header;
    protocol_data : operation_data;
  }

  val operation_data_encoding : operation_data Data_encoding.t

  val operation_data_encoding_with_legacy_attestation_name :
    operation_data Data_encoding.t

  val operation_receipt_encoding : operation_receipt Data_encoding.t

  val operation_receipt_encoding_with_legacy_attestation_name :
    operation_receipt Data_encoding.t

  val operation_data_and_receipt_encoding :
    (operation_data * operation_receipt) Data_encoding.t

  val operation_data_and_receipt_encoding_with_legacy_attestation_name :
    (operation_data * operation_receipt) Data_encoding.t
end

type protocols = {
  current_protocol : Protocol_hash.t;
  next_protocol : Protocol_hash.t;
}

val protocols :
  #Tezos_rpc.Context.simple ->
  ?chain:chain ->
  ?block:block ->
  unit ->
  protocols tzresult Lwt.t

module Make (Proto : PROTO) (Next_proto : PROTO) : sig
  val path : (unit, chain_prefix * block) Tezos_rpc.Path.t

  type raw_block_header = {
    shell : Block_header.shell_header;
    protocol_data : Proto.block_header_data;
  }

  type block_header = {
    chain_id : Chain_id.t;
    hash : Block_hash.t;
    shell : Block_header.shell_header;
    protocol_data : Proto.block_header_data;
  }

  type block_metadata = {
    protocol_data : Proto.block_header_metadata;
    test_chain_status : Test_chain_status.t;
    max_operations_ttl : int;
    max_operation_data_length : int;
    max_block_header_length : int;
    operation_list_quota : operation_list_quota list;
  }

  type operation_receipt =
    | Empty
    | Too_large
    | Receipt of Proto.operation_receipt

  type operation = {
    chain_id : Chain_id.t;
    hash : Operation_hash.t;
    shell : Operation.shell_header;
    protocol_data : Proto.operation_data;
    receipt : operation_receipt;
  }

  type block_info = {
    chain_id : Chain_id.t;
    hash : Block_hash.t;
    header : raw_block_header;
    metadata : block_metadata option;
    operations : operation list list;
  }

  val block_info_encoding : block_info Data_encoding.t

  open Tezos_rpc.Context

  val info :
    #simple ->
    ?force_metadata:bool ->
    ?metadata:[`Always | `Never] ->
    ?chain:chain ->
    ?block:block ->
    unit ->
    block_info tzresult Lwt.t

  val hash :
    #simple ->
    ?chain:chain ->
    ?block:block ->
    unit ->
    Block_hash.t tzresult Lwt.t

  val raw_header :
    #simple -> ?chain:chain -> ?block:block -> unit -> Bytes.t tzresult Lwt.t

  val header :
    #simple ->
    ?chain:chain ->
    ?block:block ->
    unit ->
    block_header tzresult Lwt.t

  val metadata :
    #simple ->
    ?chain:chain ->
    ?block:block ->
    unit ->
    block_metadata tzresult Lwt.t

  val metadata_hash :
    #simple ->
    ?chain:chain ->
    ?block:block ->
    unit ->
    Block_metadata_hash.t tzresult Lwt.t

  val resulting_context_hash :
    #simple ->
    ?chain:chain ->
    ?block:block ->
    unit ->
    Context_hash.t tzresult Lwt.t

  module Header : sig
    val shell_header :
      #simple ->
      ?chain:chain ->
      ?block:block ->
      unit ->
      Block_header.shell_header tzresult Lwt.t

    val protocol_data :
      #simple ->
      ?chain:chain ->
      ?block:block ->
      unit ->
      Proto.block_header_data tzresult Lwt.t

    val raw_protocol_data :
      #simple -> ?chain:chain -> ?block:block -> unit -> Bytes.t tzresult Lwt.t
  end

  module Operations : sig
    val operations :
      #simple ->
      ?force_metadata:bool ->
      ?metadata:[`Always | `Never] ->
      ?chain:chain ->
      ?block:block ->
      unit ->
      operation list list tzresult Lwt.t

    val operations_in_pass :
      #simple ->
      ?force_metadata:bool ->
      ?metadata:[`Always | `Never] ->
      ?chain:chain ->
      ?block:block ->
      int ->
      operation list tzresult Lwt.t

    val operation :
      #simple ->
      ?force_metadata:bool ->
      ?metadata:[`Always | `Never] ->
      ?chain:chain ->
      ?block:block ->
      int ->
      int ->
      operation tzresult Lwt.t
  end

  module Operation_hashes : sig
    val operation_hashes :
      #simple ->
      ?chain:chain ->
      ?block:block ->
      unit ->
      Operation_hash.t list list tzresult Lwt.t

    val operation_hashes_in_pass :
      #simple ->
      ?chain:chain ->
      ?block:block ->
      int ->
      Operation_hash.t list tzresult Lwt.t

    val operation_hash :
      #simple ->
      ?chain:chain ->
      ?block:block ->
      int ->
      int ->
      Operation_hash.t tzresult Lwt.t
  end

  module Operation_metadata_hashes : sig
    val root :
      #simple ->
      ?chain:chain ->
      ?block:block ->
      unit ->
      Operation_metadata_list_list_hash.t tzresult Lwt.t

    val operation_metadata_hashes :
      #simple ->
      ?chain:chain ->
      ?block:block ->
      unit ->
      Operation_metadata_hash.t list list tzresult Lwt.t

    val operation_metadata_hashes_in_pass :
      #simple ->
      ?chain:chain ->
      ?block:block ->
      int ->
      Operation_metadata_hash.t list tzresult Lwt.t

    val operation_metadata_hash :
      #simple ->
      ?chain:chain ->
      ?block:block ->
      int ->
      int ->
      Operation_metadata_hash.t tzresult Lwt.t
  end

  module Context : sig
    val read :
      #simple ->
      ?chain:chain ->
      ?block:block ->
      ?depth:int ->
      string list ->
      Proof.raw_context tzresult Lwt.t

    val merkle_tree :
      #simple ->
      ?chain:chain ->
      ?block:block ->
      ?holey:bool ->
      string list ->
      Proof.tree Proof.t option tzresult Lwt.t
  end

  module Helpers : sig
    module Forge : sig
      val block_header :
        #Tezos_rpc.Context.simple ->
        ?chain:chain ->
        ?block:block ->
        Block_header.t ->
        Bytes.t tzresult Lwt.t
    end

    module Preapply : sig
      val block :
        #simple ->
        ?chain:chain ->
        ?block:block ->
        ?sort:bool ->
        ?timestamp:Time.Protocol.t ->
        protocol_data:Next_proto.block_header_data ->
        Next_proto.operation list list ->
        (Block_header.shell_header * error Preapply_result.t list) tzresult
        Lwt.t

      val operations :
        #simple ->
        ?chain:chain ->
        ?block:block ->
        ?version:version ->
        Next_proto.operation list ->
        (Next_proto.operation_data * Next_proto.operation_receipt) list tzresult
        Lwt.t
    end

    val complete :
      #simple ->
      ?chain:chain ->
      ?block:block ->
      string ->
      string list tzresult Lwt.t
  end

  module Mempool : sig
    type t = {
      validated : (Operation_hash.t * Next_proto.operation) list;
      refused : (Next_proto.operation * error list) Operation_hash.Map.t;
      outdated : (Next_proto.operation * error list) Operation_hash.Map.t;
      branch_refused : (Next_proto.operation * error list) Operation_hash.Map.t;
      branch_delayed : (Next_proto.operation * error list) Operation_hash.Map.t;
      unprocessed : Next_proto.operation Operation_hash.Map.t;
    }

    (** Call RPC GET /chains/[chain]/mempool/pending_operations

    - Default [version] is [0].
    - Default [validated] is [true].
    - Default [branch_delayed] is [true].
    - Default [branch_refused] is [true].
    - Default [refused] is [true].
    - Default [outdated] is [true].
    - Default [validation_passes] is [[]] *)
    val pending_operations :
      #simple ->
      ?chain:chain ->
      ?version:version ->
      ?validated:bool ->
      ?branch_delayed:bool ->
      ?branch_refused:bool ->
      ?refused:bool ->
      ?outdated:bool ->
      ?validation_passes:int list ->
      unit ->
      t tzresult Lwt.t

    (** Call RPC POST /chains/[chain]/mempool/ban_operation *)
    val ban_operation :
      #simple ->
      ?chain:chain ->
      Operation_hash.t ->
      unit Tezos_error_monad.Error_monad.tzresult Lwt.t

    (** Call RPC POST /chains/[chain]/mempool/unban_operation *)
    val unban_operation :
      #simple ->
      ?chain:chain ->
      Operation_hash.t ->
      unit Tezos_error_monad.Error_monad.tzresult Lwt.t

    (** Call RPC POST /chains/[chain]/mempool/unban_all_operations *)
    val unban_all_operations :
      #simple ->
      ?chain:chain ->
      unit ->
      unit Tezos_error_monad.Error_monad.tzresult Lwt.t

    (** Call RPC GET /chains/[chain]/mempool/monitor_operations *)
    val monitor_operations :
      #streamed ->
      ?chain:chain ->
      ?version:version ->
      ?validated:bool ->
      ?branch_delayed:bool ->
      ?branch_refused:bool ->
      ?refused:bool ->
      ?outdated:bool ->
      ?validation_passes:int list ->
      unit ->
      (((Operation_hash.t * Next_proto.operation) * error trace option) list
       Lwt_stream.t
      * stopper)
      tzresult
      Lwt.t

    (** Call RPC POST /chains/[chain]/mempool/request_operations *)
    val request_operations :
      #simple ->
      ?chain:chain ->
      ?peer_id:P2p_peer.Id.t ->
      unit ->
      unit tzresult Lwt.t
  end

  val live_blocks :
    #simple ->
    ?chain:chain ->
    ?block:block ->
    unit ->
    Block_hash.Set.t tzresult Lwt.t

  module S : sig
    val hash :
      ([`GET], prefix, prefix, unit, unit, Block_hash.t) Tezos_rpc.Service.t

    val info :
      ( [`GET],
        prefix,
        prefix,
        < force_metadata : bool ; metadata : [`Always | `Never] option >,
        unit,
        block_info )
      Tezos_rpc.Service.t

    val header :
      ([`GET], prefix, prefix, unit, unit, block_header) Tezos_rpc.Service.t

    val raw_header :
      ([`GET], prefix, prefix, unit, unit, Bytes.t) Tezos_rpc.Service.t

    val metadata :
      ([`GET], prefix, prefix, unit, unit, block_metadata) Tezos_rpc.Service.t

    val metadata_hash :
      ( [`GET],
        prefix,
        prefix,
        unit,
        unit,
        Block_metadata_hash.t )
      Tezos_rpc.Service.t

    val protocols :
      ([`GET], prefix, prefix, unit, unit, protocols) Tezos_rpc.Service.t

    val resulting_context_hash :
      ([`GET], prefix, prefix, unit, unit, Context_hash.t) Tezos_rpc.Service.t

    module Header : sig
      val shell_header :
        ( [`GET],
          prefix,
          prefix,
          unit,
          unit,
          Block_header.shell_header )
        Tezos_rpc.Service.t

      val protocol_data :
        ( [`GET],
          prefix,
          prefix,
          unit,
          unit,
          Proto.block_header_data )
        Tezos_rpc.Service.t

      val raw_protocol_data :
        ([`GET], prefix, prefix, unit, unit, Bytes.t) Tezos_rpc.Service.t
    end

    module Operations : sig
      val operations :
        ( [`GET],
          prefix,
          prefix,
          < force_metadata : bool ; metadata : [`Always | `Never] option >,
          unit,
          operation list list )
        Tezos_rpc.Service.t

      val operations_in_pass :
        ( [`GET],
          prefix,
          prefix * int,
          < force_metadata : bool ; metadata : [`Always | `Never] option >,
          unit,
          operation list )
        Tezos_rpc.Service.t

      val operation :
        ( [`GET],
          prefix,
          (prefix * int) * int,
          < force_metadata : bool ; metadata : [`Always | `Never] option >,
          unit,
          operation )
        Tezos_rpc.Service.t
    end

    module Operation_hashes : sig
      val operation_hashes :
        ( [`GET],
          prefix,
          prefix,
          unit,
          unit,
          Operation_hash.t list list )
        Tezos_rpc.Service.t

      val operation_hashes_in_pass :
        ( [`GET],
          prefix,
          prefix * int,
          unit,
          unit,
          Operation_hash.t list )
        Tezos_rpc.Service.t

      val operation_hash :
        ( [`GET],
          prefix,
          (prefix * int) * int,
          unit,
          unit,
          Operation_hash.t )
        Tezos_rpc.Service.t
    end

    module Operation_metadata_hashes : sig
      val root :
        ( [`GET],
          prefix,
          prefix,
          unit,
          unit,
          Operation_metadata_list_list_hash.t )
        Tezos_rpc.Service.t

      val operation_metadata_hashes :
        ( [`GET],
          prefix,
          prefix,
          unit,
          unit,
          Operation_metadata_hash.t list list )
        Tezos_rpc.Service.t

      val operation_metadata_hashes_in_pass :
        ( [`GET],
          prefix,
          prefix * int,
          unit,
          unit,
          Operation_metadata_hash.t list )
        Tezos_rpc.Service.t

      val operation_metadata_hash :
        ( [`GET],
          prefix,
          (prefix * int) * int,
          unit,
          unit,
          Operation_metadata_hash.t )
        Tezos_rpc.Service.t
    end

    module Context : sig
      val read :
        ( [`GET],
          prefix,
          prefix * string list,
          < depth : int option >,
          unit,
          Proof.raw_context )
        Tezos_rpc.Service.t

      val merkle_tree :
        ( [`GET],
          prefix,
          prefix * string list,
          < holey : bool option >,
          unit,
          Proof.merkle_tree option )
        Tezos_rpc.Service.t

      val merkle_tree_v2 :
        ( [`GET],
          prefix,
          prefix * string list,
          < holey : bool option >,
          unit,
          Proof.tree Proof.t option )
        Tezos_rpc.Service.t
    end

    module Helpers : sig
      module Forge : sig
        val block_header :
          ( [`POST],
            prefix,
            prefix,
            unit,
            Block_header.t,
            Bytes.t )
          Tezos_rpc.Service.service
      end

      module Preapply : sig
        type block_param = {
          protocol_data : Next_proto.block_header_data;
          operations : Next_proto.operation list list;
        }

        val block :
          ( [`POST],
            prefix,
            prefix,
            < sort_operations : bool ; timestamp : Time.Protocol.t option >,
            block_param,
            Block_header.shell_header * error Preapply_result.t list )
          Tezos_rpc.Service.t

        val operations :
          ( [`POST],
            prefix,
            prefix,
            < version : version >,
            Next_proto.operation list,
            version
            * (Next_proto.operation_data * Next_proto.operation_receipt) list
          )
          Tezos_rpc.Service.t
      end

      val complete :
        ( [`GET],
          prefix,
          prefix * string,
          unit,
          unit,
          string list )
        Tezos_rpc.Service.t
    end

    module Mempool : sig
      val encoding : Mempool.t Data_encoding.t

      (** Define RPC GET /chains/[chain]/mempool/pending_operations *)
      val pending_operations :
        ('a, 'b) Tezos_rpc.Path.t ->
        ( [`GET],
          'a,
          'b,
          < version : version
          ; applied : bool option
          ; validated : bool
          ; branch_delayed : bool
          ; branch_refused : bool
          ; refused : bool
          ; outdated : bool
          ; validation_passes : int list >,
          unit,
          version * Mempool.t )
        Tezos_rpc.Service.t

      (** Define RPC POST /chains/[chain]/mempool/ban_operation *)
      val ban_operation :
        ('a, 'b) Tezos_rpc.Path.t ->
        ([`POST], 'a, 'b, unit, Operation_hash.t, unit) Tezos_rpc.Service.t

      (** Define RPC POST /chains/[chain]/mempool/unban_operation *)
      val unban_operation :
        ('a, 'b) Tezos_rpc.Path.t ->
        ([`POST], 'a, 'b, unit, Operation_hash.t, unit) Tezos_rpc.Service.t

      (** Define RPC POST /chains/[chain]/mempool/unban_all_operations *)
      val unban_all_operations :
        ('a, 'b) Tezos_rpc.Path.t ->
        ([`POST], 'a, 'b, unit, unit, unit) Tezos_rpc.Service.t

      (** Define RPC GET /chains/[chain]/mempool/monitor_operations *)
      val monitor_operations :
        ('a, 'b) Tezos_rpc.Path.t ->
        ( [`GET],
          'a,
          'b,
          < version : version
          ; applied : bool option
          ; validated : bool
          ; branch_delayed : bool
          ; branch_refused : bool
          ; refused : bool
          ; outdated : bool
          ; validation_passes : int list >,
          unit,
          version
          * ((Operation_hash.t * Next_proto.operation) * error trace option)
            list )
        Tezos_rpc.Service.t

      (** Define RPC GET /chains/[chain]/mempool/filter *)
      val get_filter :
        ('a, 'b) Tezos_rpc.Path.t ->
        ( [`GET],
          'a,
          'b,
          < include_default : bool >,
          unit,
          Data_encoding.json )
        Tezos_rpc.Service.t

      (** Define RPC POST /chains/[chain]/mempool/filter *)
      val set_filter :
        ('a, 'b) Tezos_rpc.Path.t ->
        ( [`POST],
          'a,
          'b,
          unit,
          Data_encoding.json,
          Data_encoding.json )
        Tezos_rpc.Service.t

      (** Define RPC POST /chains/[chain]/mempool/request_operations *)
      val request_operations :
        ('a, 'b) Tezos_rpc.Path.t ->
        ( [`POST],
          'a,
          'b,
          < peer_id : P2p_peer_id.t option >,
          unit,
          unit )
        Tezos_rpc.Service.t
    end

    val live_blocks :
      ([`GET], prefix, prefix, unit, unit, Block_hash.Set.t) Tezos_rpc.Service.t
  end
end

module Fake_protocol : PROTO

module Empty : module type of Make (Fake_protocol) (Fake_protocol)
