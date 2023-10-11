(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

type head_info = {
  proto : int; (* the [proto_level] from the head's shell header *)
  level : int32;
}

(** A [ready_ctx] value contains globally needed informations for a running dal
    node. It is available when both cryptobox is initialized and the plugin
    for dal has been loaded.

   A [ready_ctx] also has a field [shards_proofs_precomputation] that contains
   the (costly) precomputation needed to get shard proofs.
*)
type ready_ctxt = {
  cryptobox : Cryptobox.t;
  proto_parameters : Dal_plugin.proto_parameters;
  plugin : (module Dal_plugin.T);
  shards_proofs_precomputation : Cryptobox.shards_proofs_precomputation;
  plugin_proto : int;  (** Protocol level of the plugin. *)
  last_seen_head : head_info option;
      (** The level of the last seen head of the L1 node. *)
}

(** The status of the dal node *)
type status = Ready of ready_ctxt | Starting

(** A [t] value contains both the status and the dal node configuration. It's
    field are available through accessors *)
type t

(** [init config store gs_worker transport_layer cctx] creates a [t] with a
    status set to [Starting] using the given dal node configuration [config],
    node store [store], gossipsub worker instance [gs_worker], transport layer
    instance [transport_layer], and tezos node client context [cctx]. *)
val init :
  Configuration_file.t ->
  Store.node_store ->
  Gossipsub.Worker.t ->
  Gossipsub.Transport_layer.t ->
  Tezos_rpc.Context.generic ->
  Metrics.t ->
  t

(** Raised by [set_ready] when the status is already [Ready _] *)
exception Status_already_ready

(** [set_ready ctxt dal_plugin cryptobox proto_parameters plugin_proto] updates
    in place the status value to [Ready], and initializes the inner [ready_ctxt]
    value with the given parameters.

    @raise Status_already_ready when the status is already [Ready _] *)
val set_ready :
  t ->
  (module Tezos_dal_node_lib.Dal_plugin.T) ->
  Cryptobox.t ->
  Dal_plugin.proto_parameters ->
  int ->
  unit tzresult

(** Updates the plugin and the protocol level. *)
val update_plugin_in_ready :
  t -> (module Tezos_dal_node_lib.Dal_plugin.T) -> int -> unit

type error += Node_not_ready

(** Updates the [last_seen_head] field of the "ready context" with the given
    info.  Assumes the node's status is ready. Otherwise it returns
    [Node_not_ready]. *)
val update_last_seen_head : t -> head_info -> unit tzresult

(** [get_ready ctxt] extracts the [ready_ctxt] value from a context [t]. It
    propagates [Node_not_ready] if status is not ready yet. If called multiple
    times, it replaces current values for [ready_ctxt] with new ones *)
val get_ready : t -> ready_ctxt tzresult

(** [get_profile_ctxt ctxt] returns the profile context.  *)
val get_profile_ctxt : t -> Profile_manager.t

(** [set_profile_ctxt ctxt pctxt] sets the profile context.  *)
val set_profile_ctxt : t -> Profile_manager.t -> unit

(** [get_config ctxt] returns the dal node configuration *)
val get_config : t -> Configuration_file.t

(** [get_status ctxt] returns the dal node status *)
val get_status : t -> status

(** [get_store ctxt] returns the dal node store. *)
val get_store : t -> Store.node_store

(** [get_gs_worker ctxt] returns the Gossipsub worker state. *)
val get_gs_worker : t -> Gossipsub.Worker.t

(** [get_tezos_node_cctxt ctxt] returns the Tezos node's client context *)
val get_tezos_node_cctxt : t -> Tezos_rpc.Context.generic

(** [get_neighbors_cctxts ctxt] returns the dal node neighbors client contexts *)
val get_neighbors_cctxts : t -> Dal_node_client.cctxt list

(** [fetch_assigned_shard_indices ctxt ~level ~pkh] fetches from L1 the shard
    indices assigned to [pkh] at [level].  It internally caches the DAL
    committee with [level] as the key with FIFO strategy. *)
val fetch_assigned_shard_indices :
  t ->
  level:int32 ->
  pkh:Tezos_crypto.Signature.Public_key_hash.t ->
  int list tzresult Lwt.t

(** [fetch_committee ctxt ~level] fetches from L1 the shard indices assigned
    to all attesters at [level].  It internally caches the DAL committee with
    [level] as the key with FIFO strategy. *)
val fetch_committee :
  t -> level:int32 -> Committee_cache.committee tzresult Lwt.t

(** Module for P2P-related accessors.  *)
module P2P : sig
  (** [get_topics t] returns the list of topics the node is subscribed to. *)
  val get_topics : t -> Types.Topic.t list
end
