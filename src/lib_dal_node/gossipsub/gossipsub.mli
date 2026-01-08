(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
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

(** This module exposes the instantiations of the Gossipsub and Octez-p2p
    libraries to be used by the DAL node to connect to and exchange data with
    peers. *)

(** The worker module exposes instantiation of the Gossipsub worker functor,
    alongside the config used to instantiate the functor and the default values
    of the GS parameters. *)
module Worker : sig
  module Config :
    module type of Gs_interface.Worker_config
      with type GS.Topic.t = Types.Topic.t
       and type GS.Message_id.t = Types.Message_id.t
       and type GS.Message.t = Types.Message.t
       and type GS.Peer.t = Types.Peer.t
       and type GS.Span.t = Types.Span.t
       and type GS.Time.t = Types.Time.t
       and type 'a Monad.t = 'a Lwt.t
       and type Point.t = Types.Point.t

  module Default_parameters : module type of Gs_default_parameters

  include
    Gossipsub_intf.WORKER
      with type GS.Topic.t = Types.Topic.t
       and type GS.Message_id.t = Types.Message_id.t
       and type GS.Message.t = Types.Message.t
       and type GS.Peer.t = Types.Peer.t
       and type GS.Span.t = Types.Span.t
       and type GS.Time.t = Types.Time.t
       and type Point.t = Types.Point.t

  module Logging : sig
    val event : verbose:bool -> event -> unit Monad.t
  end

  (** A hook to set or update messages and messages IDs validation
      function. Should be called once at startup and every time the DAL
      parameters change. *)
  module Validate_message_hook : sig
    val set :
      (?message:GS.Message.t ->
      message_id:GS.Message_id.t ->
      unit ->
      [`Valid | `Unknown | `Outdated | `Invalid]) ->
      unit

    val set_batch :
      ((Types.Peer.t
       * Types.Topic.t
       * Types.Message_id.t
       * Types.Message.t
       * Types.Peer.Set.t)
       list ->
      [`Invalid | `Outdated | `Unknown | `Valid] list) ->
      unit
  end
end

(** The transport layer module exposes the needed primitives, interface and
    default parameters for the instantiation of the Octez-p2p library. *)
module Transport_layer : sig
  module Interface : module type of Transport_layer_interface

  module Default_parameters : module type of Transport_layer_default_parameters

  type t

  (** [create ~network_name ~is_bootstrap_peer ~public_addr config limits]
      creates a new instance of type {!t}. It is a wrapper on top of
      {!P2p.create}. *)
  val create :
    network_name:Distributed_db_version.Name.t ->
    public_addr:P2p_point.Id.t ->
    is_bootstrap_peer:bool ->
    P2p.config ->
    P2p_limits.t ->
    t tzresult Lwt.t

  (** [shutdown t] shutdowns the transport layer and ensures that
      active connections are closed. *)
  val shutdown : t -> unit Lwt.t

  (** [activate ?additional_points t] activates the given transport layer [t]. It
      is a wrapper on top of {!P2p.activate}. If some [additional_points] are
      given, they are added to [t]'s known points. *)
  val activate : ?additional_points:P2p_point.Id.t list -> t -> unit Lwt.t

  (** [connect t ?timeout point] initiates a connection to the point
      [point]. The promise returned by this function is resolved once
      the P2P handshake successfully completes. If the [timeout] is
      set, an error is returned if the P2P handshake takes more than
      [timeout] to complete. *)
  val connect :
    t -> ?timeout:Ptime.Span.t -> P2p_point.Id.t -> unit tzresult Lwt.t

  (** [disconnect_point t ?wait point] initiates a disconnection to
      the point [point]. The promise returned by this function is
      fulfilled when the socket is closed on our side. If [wait] is
      [true], we do not close the socket before having cancelled all
      the current messages in the write buffer. Should not matter in
      practice.

      Due to the following issue https://gitlab.com/tezos/tezos/-/issues/5319

      it may occur that a disconnection takes several minutes. *)
  val disconnect_point : t -> ?wait:bool -> P2p_point.Id.t -> unit Lwt.t

  (** [disconnect_peer t ?wait point] initiates a disconnection to
      the point [peer]. The promise returned by this function is
      fulfilled when the socket is closed on our side. If [wait] is
      [true], we do not close the socket before having cancelled all
      the current messages in the write buffer. Should not matter in
      practice.

      Due to the following issue https://gitlab.com/tezos/tezos/-/issues/5319

      it may occur that a disconnection takes several minutes. *)
  val disconnect_peer :
    t -> ?wait:bool -> Crypto_box.Public_key_hash.t -> unit Lwt.t

  (** [get_points ?connected t] returns a list of points. If
      [connected] is [true] (default), it returns only points we are
      currently connected. Otherwise, it returns a list of known
      points (points for which we were already successfully connected
      in the past.) *)
  val get_points : ?connected:bool -> t -> P2p_point.Id.t list tzresult

  (** [get_points_info ?connected t] returns a list of info for
      points. If [connected] is [true] (default), it returns only info
      for points we are currently connected. Otherwise, it returns a
      list of infos for known points (points for which we were already
      successfully connected in the past.) *)
  val get_points_info :
    ?connected:bool -> t -> (P2p_point.Id.t * P2p_point.Info.t) list tzresult

  (** [get_point_info t point] returns the info of the corresponding
      point if found. *)
  val get_point_info : t -> P2p_point.Id.t -> P2p_point.Info.t option tzresult

  (** [get_peers ?connected t] returns a list of peers. If [connected]
      is [true] (default), it returns only the peers we are connected
      to. Otherwise, it returns a list of known peers (peers for which we
      were already successfully connected in the past.) *)
  val get_peers : ?connected:bool -> t -> P2p_peer.Id.t list tzresult

  (** [get_peers_info ?connected t] returns a list of info for
      peers. If [connected] is [true] (default), it returns only info
      for peers we are currently connected. Otherwise, it returns a
      list of infos for known peers (peers for which we were already
      successfully connected in the past.) *)
  val get_peers_info :
    ?connected:bool ->
    t ->
    (P2p_peer.Id.t * Types.P2P.Peer.Info.t) list tzresult

  (** [get_peer_info t peer] returns the info of the corresponding peer if found. *)
  val get_peer_info :
    t -> P2p_peer.Id.t -> Types.P2P.Peer.Info.t option tzresult

  (** [patch_peer t peer acl] patches the acl of the corresponding
      peer if found and returns the info. When [acl] is [None] this is
      equivalent to [get_peer_info]. *)
  val patch_peer :
    t ->
    P2p_peer.Id.t ->
    [`Ban | `Open | `Trust] option ->
    Types.P2P.Peer.Info.t option tzresult Lwt.t
end

(** This module implements the list of hooks that allow interconnecting the
    Gossipsub worker with the transport layer. They are exposed via the
    {!Transport_layer_hooks.activate} function below. *)
module Transport_layer_hooks : sig
  (** See {!Gs_transport_connection.activate}. *)
  val activate :
    Worker.t ->
    Transport_layer.t ->
    app_out_callback:
      (Types.Message.t -> Types.Message_id.t -> unit tzresult Lwt.t) ->
    app_in_callback:(Types.Message_id.t -> Types.Peer.t -> unit tzresult Lwt.t) ->
    verbose:bool ->
    unit Lwt.t
end

(** [version ~network_name] returns the current version of the P2P. *)
val version : network_name:Distributed_db_version.Name.t -> Network_version.t

module Profiler : sig
  open Tezos_profiler.Profiler

  val gossipsub_profiler : profiler

  val init : (name:string -> instance option) -> unit

  val create_reset_block_section : profiler -> Block_hash.t * metadata -> unit
end
