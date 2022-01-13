(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module Operations_source : sig
  type t =
    | Local of {filename : string}
    | Remote of {uri : Uri.t; http_headers : (string * string) list option}

  val encoding : t Data_encoding.t
end

(** [generate_seed_nonce ()] is a random nonce that is typically used
    in block headers. When baking, bakers generate random nonces whose
    hash is committed in the block they bake. They will typically
    reveal the aforementioned nonce during the next cycle. *)
val generate_seed_nonce : unit -> Nonce.t

(** [inject_block cctxt blk ?force ~priority ~timestamp ~fitness
    ~seed_nonce ~src_sk ops liquidity_baking_escape_vote]
    tries to inject a block in the node. If
    [?force] is set, the fitness check will be bypassed. [priority]
    will be used to compute the baking slot (level is
    precomputed). [src_sk] is used to sign the block header. *)
val inject_block :
  #Protocol_client_context.full ->
  ?force:bool ->
  ?seed_nonce_hash:Nonce_hash.t ->
  chain:Chain_services.chain ->
  shell_header:Block_header.shell_header ->
  priority:int ->
  delegate_pkh:Signature.Public_key_hash.t ->
  delegate_sk:Client_keys.sk_uri ->
  level:Raw_level.t ->
  Operation.raw list list ->
  liquidity_baking_escape_vote:bool ->
  Block_hash.t tzresult Lwt.t

type error += Failed_to_preapply of Tezos_base.Operation.t * error list

(** [forge_block cctxt ?fee_threshold ?force ?extra_operations ?best_effort
   ?sort ?timestamp ?max_priority ?priority ?ignore_node_mempool ~seed_nonce
   ~src_sk pk_hash parent_blk] injects a block in the node. In addition of
   inject_block, it will:

    * Baking priority: If [`Auto] is used, it will be computed from the public
   key hash of the specified contract, optionally capped to a maximum value, and
   optionally restricting for free baking slot.

    * Timestamp: If [?timestamp] is set, and is compatible with the computed
   baking priority, it will be used. Otherwise, it will be set at the best
   baking priority.

    * Fee Threshold: If [?fee_threshold] is given, operations with fees lower
   than it are not added to the block.

    * if [?extra_operations] is provided, bake a block including the operations
   recorded in this (out-of-node) operations resource.

    * if [?ignore_node_mempool] is set to true, do not ask the nodes for
   operations to be included in the block to be baked. Use in conjunction with
   [~extra_operations] to restrict the operations to the ones contained in
   [extra_operations].  Defaults to [false].  *)
val forge_block :
  #Protocol_client_context.full ->
  ?force:bool ->
  ?best_effort:bool ->
  ?sort:bool ->
  ?minimal_fees:Tez.t ->
  ?minimal_nanotez_per_gas_unit:Q.t ->
  ?minimal_nanotez_per_byte:Q.t ->
  ?timestamp:Time.Protocol.t ->
  ?ignore_node_mempool:bool ->
  ?extra_operations:Operations_source.t ->
  ?context_path:string ->
  ?seed_nonce_hash:Nonce_hash.t ->
  liquidity_baking_escape_vote:bool ->
  chain:Chain_services.chain ->
  priority:[`Set of int | `Auto of public_key_hash * int option] ->
  delegate_pkh:Signature.Public_key_hash.t ->
  delegate_sk:Client_keys.sk_uri ->
  Block_services.block ->
  Block_hash.t tzresult Lwt.t

val create :
  #Protocol_client_context.full ->
  user_activated_upgrades:User_activated.upgrades ->
  ?minimal_fees:Tez.t ->
  ?minimal_nanotez_per_gas_unit:Q.t ->
  ?minimal_nanotez_per_byte:Q.t ->
  ?max_priority:int ->
  ?per_block_vote_file:string ->
  ?extra_operations:Operations_source.t ->
  chain:Chain_services.chain ->
  context_path:string ->
  public_key_hash list ->
  Client_baking_blocks.block_info tzresult Lwt_stream.t ->
  unit tzresult Lwt.t

(**/**)

module Internal_for_tests : sig
  module PrioritizedOperation : sig
    type t = private High of packed_operation | Low of packed_operation

    (** prioritize operations coming from an external source (file, uri, ...)*)
    val extern : packed_operation -> t

    (** prioritize operations coming from a node *)
    val node : packed_operation -> t

    val compare_priority : t -> t -> int
  end

  val get_manager_content :
    PrioritizedOperation.t -> (public_key_hash * counter) option

  val sort_manager_operations :
    max_size:int ->
    hard_gas_limit_per_block:Saturation_repr.may_saturate Saturation_repr.t ->
    minimal_fees:Tez.t ->
    minimal_nanotez_per_gas_unit:Q.t ->
    minimal_nanotez_per_byte:Q.t ->
    PrioritizedOperation.t trace ->
    PrioritizedOperation.t trace
end
