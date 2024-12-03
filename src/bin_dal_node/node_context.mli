(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023-2024 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** A [t] value contains the needed informations for a running a DAL node. Its
    fields are available through accessors. *)
type t

(** [init] creates a [t] value based on the given arguments. *)
val init :
  Configuration_file.t ->
  Profile_manager.t ->
  Cryptobox.t ->
  Cryptobox.shards_proofs_precomputation option ->
  Dal_plugin.proto_parameters ->
  Proto_plugins.t ->
  Store.t ->
  Gossipsub.Worker.t ->
  Gossipsub.Transport_layer.t ->
  Tezos_rpc.Context.generic ->
  t

(** Returns all the registered plugins *)
val get_all_plugins : t -> (module Dal_plugin.T) list

(** Returns the plugin to be used for the given (block) level.
    Recall that, for a migration level L:
    * to retrieve the metadata of the block L, one should use the plugin for the
      old protocol;
    * to retrieve context-related information, one should use the plugin for the
      new protocol.
    This function returns the plugin of [metadata.protocols.next_protocol], so it is
    tailored for the second use case. To get the plugin for the first use-case, just
    get the plugin for the predecessor of the target level. *)
val get_plugin_for_level : t -> level:int32 -> (module Dal_plugin.T) tzresult

(** Tries to add a new plugin for the protocol with level [proto_level] to be used
    starting with the given [block_level].

    It returns an error if the node is not ready, if the
    [Chain_services.Blocks.protocols] RPC fails, or if the plugin is not
    registered. *)
val may_add_plugin :
  t ->
  Rpc_context.t ->
  block_level:int32 ->
  proto_level:int ->
  unit tzresult Lwt.t

(** Set the protocol plugins to the given value. *)
val set_proto_plugins : t -> Proto_plugins.t -> unit

(** Reconstruct the given slot id by calling the [reconstruct]
    function unless a reconstruction for the given slot id is alredy
    ongoing in which case the ongoing promise is returned instead. *)
val may_reconstruct :
  reconstruct:(Types.slot_id -> (bytes, Errors.other) result Lwt.t) ->
  Types.slot_id ->
  t ->
  (bytes, Errors.other) result Lwt.t

(** [get_profile_ctxt ctxt] returns the profile context.  *)
val get_profile_ctxt : t -> Profile_manager.t

(** [load_profile_ctxt ctxt] tries to load the profile context from disk. *)
val load_profile_ctxt : t -> Profile_manager.t option Lwt.t

(** [set_profile_ctxt ctxt ?save pctxt] sets the profile context. If [save] is
    set, which is [true] by default, the profile context is saved on
    disk. *)
val set_profile_ctxt : t -> ?save:bool -> Profile_manager.t -> unit Lwt.t

(** [get_config ctxt] returns the dal node configuration *)
val get_config : t -> Configuration_file.t

(** [get_cryptobox ctxt] returns the DAL node's cryptobox *)
val get_cryptobox : t -> Cryptobox.t

(** [get_proto_parameters ctxt] returns the DAL node's current protocol parameters. *)
val get_proto_parameters : t -> Dal_plugin.proto_parameters

(** [get_shards_proofs_precomputation ctxt] returns the shards proof's precomputation. *)
val get_shards_proofs_precomputation :
  t -> Cryptobox.shards_proofs_precomputation option

(** [get_store ctxt] returns the dal node store. *)
val get_store : t -> Store.t

(** [get_gs_worker ctxt] returns the Gossipsub worker state. *)
val get_gs_worker : t -> Gossipsub.Worker.t

(** [get_tezos_node_cctxt ctxt] returns the Tezos node's client context. *)
val get_tezos_node_cctxt : t -> Tezos_rpc.Context.generic

(** [get_neighbors_cctxts ctxt] returns the client contexts of the DAL node's
    neighbors. *)
val get_neighbors_cctxts : t -> Dal_node_client.cctxt list

(** [get_ongoing_amplification ctxt] returns the slot ids for which there are
    ongoing amplifications. *)
val get_ongoing_amplifications : t -> Types.Slot_id.Set.t

(** [set_ongoing_amplification ctxt ongoing_amplifications] set the slot ids for
    which there are ongoing amplifications. *)
val set_ongoing_amplifications : t -> Types.Slot_id.Set.t -> unit

(** [storage_period ctxt proto_parameters] returns for how many levels should
    the node store data about attested slots. This depends on the node's profile
    and its history mode.  *)
val storage_period :
  t -> Dal_plugin.proto_parameters -> [`Always | `Finite of int]

(** [level_to_gc ctxt proto_parameters ~current_level] returns the oldest level
    that should have attested data (like shards and slots, skip list cells)
    stored; during [current_level], such data for commitments published at the
    returned level will be removed. The returned level is non-negative. In case
    no removal is needed (either because the node is thus configured, or the
    current_level is not big enough), the function returns [None]. *)
val level_to_gc :
  t -> Dal_plugin.proto_parameters -> current_level:int32 -> int32 option

(** [fetch_assigned_shard_indices ctxt ~level ~pkh] fetches from L1 the shard
    indices assigned to [pkh] at [level].  It internally caches the DAL
    committee with [level] as the key with FIFO strategy. *)
val fetch_assigned_shard_indices :
  t -> level:int32 -> pkh:Signature.Public_key_hash.t -> int list tzresult Lwt.t

(** [get_fetched_assigned_shard_indices] is a pure variant of
    {!fetch_assigned_shard_indices} that doesn't fetch the committee from L1 if
    not found locally. The function returns [None] if no committee is found
    locally for the given level, or [Some shard_indexes] otherwise, where the
    list of shards is empty if the given [pkh] is not in the committee for the
    givel [level]. *)
val get_fetched_assigned_shard_indices :
  t ->
  level:int32 ->
  pkh:Signature.public_key_hash ->
  Committee_cache.shard_indexes option

(** [fetch_committee ctxt ~level] fetches from L1 the shard indices assigned
    to all attesters at [level].  It internally caches the DAL committee with
    [level] as the key with FIFO strategy. *)
val fetch_committee :
  t -> level:int32 -> Committee_cache.committee tzresult Lwt.t

(** [version ctxt] returns the current version of the node *)
val version : t -> Types.Version.t

(** Module for P2P-related accessors.  *)
module P2P : sig
  (** [connect t ?timeout point] initiates a connection to the point
      [point]. The promise returned by this function is resolved once
      the P2P handhshake successfully completes. If the [timeout] is
      set, an error is returned if the P2P handshake takes more than
      [timeout] to complete. *)
  val connect :
    t -> ?timeout:Ptime.Span.t -> P2p_point.Id.t -> unit tzresult Lwt.t

  (** [disconnect_point t ?wait point] initiaties a disconnection to
      the point [point]. The promise returned by this function is
      fullfiled when the socket is closed on our side. If [wait] is
      [true], we do not close the socket before having canceled all
      the current messages in the write buffer. Should not matter in
      practice.

      Due to the following issue https://gitlab.com/tezos/tezos/-/issues/5319

      it may occur that a discconnection takes several minutes. *)
  val disconnect_point : t -> ?wait:bool -> P2p_point.Id.t -> unit Lwt.t

  (** [disconnect_peer t ?wait point] initiaties a disconnection to
      the point [peer]. The promise returned by this function is
      fullfiled when the socket is closed on our side. If [wait] is
      [true], we do not close the socket before having canceled all
      the current messages in the write buffer. Should not matter in
      practice.

      Due to the following issue https://gitlab.com/tezos/tezos/-/issues/5319

      it may occur that a discconnection takes several minutes. *)
  val disconnect_peer : t -> ?wait:bool -> P2p_peer.Id.t -> unit Lwt.t

  (** [get_points ?connected t] returns a list of points. If
      [connected] is [true] (default), it returns only points we are
      currently connected. Otherwise, it returns a list of known
      points (points for which we were already successfully connected
      in the past.) *)
  val get_points : ?connected:bool -> t -> P2p_point.Id.t list tzresult Lwt.t

  (** [get_points_info ?connected t] returns a list of info for
      points. If [connected] is [true] (default), it returns only info
      for points we are currently connected. Otherwise, it returns a
      list of infos for known points (points for which we were already
      successfully connected in the past.) *)
  val get_points_info :
    ?connected:bool ->
    t ->
    (P2p_point.Id.t * P2p_point.Info.t) list tzresult Lwt.t

  (** [get_point_info t point] returns the info of the corresponding
    point if found. *)
  val get_point_info :
    t -> P2p_point.Id.t -> P2p_point.Info.t option tzresult Lwt.t

  (** [get_peers ?connected t] returns a list of peers. If [connected]
      is [true] (default), it returns only the peers we are connected
      to. Otherwise, it returns a list of known peers (peers for which
      we were already successfully connected in the past.) *)
  val get_peers : ?connected:bool -> t -> P2p_peer.Id.t list tzresult Lwt.t

  (** [get_peers_info ?connected t] returns a list of info for
      peers. If [connected] is [true] (default), it returns only info
      for peers we are currently connected. Otherwise, it returns a
      list of infos for known peers (peers for which we were already
      successfully connected in the past.) *)
  val get_peers_info :
    ?connected:bool ->
    t ->
    (P2p_peer.Id.t * Types.P2P.Peer.Info.t) list tzresult Lwt.t

  (** [get_peer_info t peer] returns the info of the corresponding peer if found. *)
  val get_peer_info :
    t -> P2p_peer.Id.t -> Types.P2P.Peer.Info.t option tzresult Lwt.t

  (** [patch_peer t peer acl] patches the acl of the given peer and
      returns the info of the corresponding peer if found. *)
  val patch_peer :
    t ->
    P2p_peer.Id.t ->
    [`Ban | `Open | `Trust] option ->
    Types.P2P.Peer.Info.t option tzresult Lwt.t

  module Gossipsub : sig
    (** [get_topics t] returns the list of topics the node is subscribed to. *)
    val get_topics : t -> Types.Topic.t list

    (** [get_topics_peers ~subscribed t] returns an association list between
        the topics of connected peers and the connected peers subscribed to that
        topic, when [subscribed = false]. When [subscribed = true], then the
        returned value is restricted to the topics this node is subscribed to. *)
    val get_topics_peers :
      subscribed:bool -> t -> (Types.Topic.t * Types.Peer.t list) list

    (** [get_slot_indexes_peers ~subscribed t] returns an association list
        between the slot_indexes of topics of connected peers and the connected
        peers subscribed to those topics, when [subscribed = false]. When
        [subscribed = true], then the returned value is restricted to the topics
        this node is subscribed to. *)
    val get_slot_indexes_peers :
      subscribed:bool -> t -> (Types.slot_index * Types.Peer.t list) list

    (** [get_pkhs_peers ~subscribed t] returns an association list between the
        pkhs of topics of connected peers and the connected peers subscribed to
        those topics, when [subscribed = false]. When [subscribed = true], then
        the returned value is restricted to the topics this node is subscribed
        to. *)
    val get_pkhs_peers :
      subscribed:bool ->
      t ->
      (Signature.public_key_hash * Types.Peer.t list) list

    (** [get_connections t] returns the list of connections. If
        [ignore_bootstrap_topics] (false by default) is set to [true],
        bootstrap topics will not be included in the result *)
    val get_connections :
      ?ignore_bootstrap_topics:bool ->
      t ->
      (Types.Peer.t * Types.Gossipsub.connection) list

    (** [get_scores t] returns the score of peers with a known score. *)
    val get_scores : t -> (Types.Peer.t * Types.Score.t) list

    (** [get_backoffs t] returns the backoffs of peers with a backoff. *)
    val get_backoffs :
      t -> (Types.Topic.t * (Types.Peer.t * Types.Time.t) list) list

    (** [get_message_cache t] returns the number of message ids in the message
        cache, grouped by time slot and topic. *)
    val get_message_cache : t -> (int64 * (Types.Topic.t * int) list) list
  end
end
