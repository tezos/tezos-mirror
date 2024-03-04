(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Tezos_rpc.Context

val self : #simple -> P2p_peer.Id.t tzresult Lwt.t

val stat : #simple -> P2p_stat.t tzresult Lwt.t

val events :
  #streamed ->
  (P2p_connection.P2p_event.t Lwt_stream.t * stopper) tzresult Lwt.t

val connect :
  #simple -> timeout:Ptime.Span.t -> P2p_point.Id.t -> unit tzresult Lwt.t

module S : sig
  val self : ([`GET], unit, unit, unit, unit, P2p_peer.Id.t) Tezos_rpc.Service.t

  val stat : ([`GET], unit, unit, unit, unit, P2p_stat.t) Tezos_rpc.Service.t

  val events :
    ( [`GET],
      unit,
      unit,
      unit,
      unit,
      P2p_connection.P2p_event.t )
    Tezos_rpc.Service.t

  val connect :
    ( [`PUT],
      unit,
      unit * P2p_point.Id.t,
      < timeout : Ptime.Span.t >,
      unit,
      unit )
    Tezos_rpc.Service.t
end

module Connections : sig
  open Tezos_rpc.Context

  type connection_info = Connection_metadata.t P2p_connection.Info.t

  val list : #simple -> connection_info list tzresult Lwt.t

  val info : #simple -> P2p_peer.Id.t -> connection_info tzresult Lwt.t

  val kick : #simple -> ?wait:bool -> P2p_peer.Id.t -> unit tzresult Lwt.t

  module S : sig
    val list :
      ([`GET], unit, unit, unit, unit, connection_info list) Tezos_rpc.Service.t

    val info :
      ( [`GET],
        unit,
        unit * P2p_peer.Id.t,
        unit,
        unit,
        connection_info )
      Tezos_rpc.Service.t

    val kick :
      ( [`DELETE],
        unit,
        unit * P2p_peer.Id.t,
        < wait : bool >,
        unit,
        unit )
      Tezos_rpc.Service.t
  end
end

module Points : sig
  val list :
    ?filter:P2p_point.Filter.t list ->
    #simple ->
    (P2p_point.Id.t * P2p_point.Info.t) list tzresult Lwt.t

  val info : #simple -> P2p_point.Id.t -> P2p_point.Info.t tzresult Lwt.t

  val events :
    #streamed ->
    P2p_point.Id.t ->
    (P2p_point.Pool_event.t list Lwt_stream.t * stopper) tzresult Lwt.t

  val patch :
    #simple ->
    P2p_point.Id.t ->
    [`Ban | `Open | `Trust] option * P2p_peer.Id.t option ->
    P2p_point.Info.t tzresult Lwt.t

  val banned : #simple -> P2p_point.Id.t -> bool tzresult Lwt.t

  module S : sig
    val list :
      ( [`GET],
        unit,
        unit,
        < filters : P2p_point.Filter.t list >,
        unit,
        (P2p_point.Id.t * P2p_point.Info.t) list )
      Tezos_rpc.Service.t

    val info :
      ( [`GET],
        unit,
        unit * P2p_point.Id.t,
        unit,
        unit,
        P2p_point.Info.t )
      Tezos_rpc.Service.t

    val patch_input_encoding :
      ([`Ban | `Open | `Trust] option * P2p_peer.Id.t option) Data_encoding.t

    val patch :
      ( [`PATCH],
        unit,
        unit * P2p_point.Id.t,
        unit,
        [`Ban | `Open | `Trust] option * P2p_peer.Id.t option,
        P2p_point.Info.t )
      Tezos_rpc.Service.service

    val events :
      ( [`GET],
        unit,
        unit * P2p_point.Id.t,
        < monitor : bool >,
        unit,
        P2p_point.Pool_event.t list )
      Tezos_rpc.Service.t

    (* DEPRECATED *)
    val ban :
      ( [`GET],
        unit,
        unit * P2p_point.Id.t,
        unit,
        unit,
        unit )
      Tezos_rpc.Service.t

    (* DEPRECATED *)
    val unban :
      ( [`GET],
        unit,
        unit * P2p_point.Id.t,
        unit,
        unit,
        unit )
      Tezos_rpc.Service.t

    (* DEPRECATED *)
    val trust :
      ( [`GET],
        unit,
        unit * P2p_point.Id.t,
        unit,
        unit,
        unit )
      Tezos_rpc.Service.t

    (* DEPRECATED *)
    val untrust :
      ( [`GET],
        unit,
        unit * P2p_point.Id.t,
        unit,
        unit,
        unit )
      Tezos_rpc.Service.t

    val banned :
      ( [`GET],
        unit,
        unit * P2p_point.Id.t,
        unit,
        unit,
        bool )
      Tezos_rpc.Service.t
  end
end

module Peers : sig
  val list :
    ?filter:P2p_peer.Filter.t list ->
    #simple ->
    (P2p_peer.Id.t * (Peer_metadata.t, Connection_metadata.t) P2p_peer.Info.t)
    list
    tzresult
    Lwt.t

  val info :
    #simple ->
    P2p_peer.Id.t ->
    (Peer_metadata.t, Connection_metadata.t) P2p_peer.Info.t tzresult Lwt.t

  val events :
    #streamed ->
    P2p_peer.Id.t ->
    (P2p_peer.Pool_event.t list Lwt_stream.t * stopper) tzresult Lwt.t

  val patch :
    #simple ->
    P2p_peer.Id.t ->
    [`Ban | `Open | `Trust] option ->
    (Peer_metadata.t, Connection_metadata.t) P2p_peer.Info.t tzresult Lwt.t

  val banned : #simple -> P2p_peer.Id.t -> bool tzresult Lwt.t

  module S : sig
    val list :
      ( [`GET],
        unit,
        unit,
        < filters : P2p_peer.Filter.t list >,
        unit,
        (P2p_peer.Id.t
        * (Peer_metadata.t, Connection_metadata.t) P2p_peer.Info.t)
        list )
      Tezos_rpc.Service.t

    val info :
      ( [`GET],
        unit,
        unit * P2p_peer.Id.t,
        unit,
        unit,
        (Peer_metadata.t, Connection_metadata.t) P2p_peer.Info.t )
      Tezos_rpc.Service.t

    val events :
      ( [`GET],
        unit,
        unit * P2p_peer.Id.t,
        < monitor : bool >,
        unit,
        P2p_peer.Pool_event.t list )
      Tezos_rpc.Service.t

    val patch_input_encoding : [`Ban | `Open | `Trust] option Data_encoding.t

    val patch :
      ( [`PATCH],
        unit,
        unit * Tezos_crypto.Crypto_box.Public_key_hash.t,
        unit,
        [`Ban | `Open | `Trust] option,
        (Peer_metadata.t, Connection_metadata.t) P2p_peer.Info.t )
      Tezos_rpc.Service.service

    val ban :
      ([`GET], unit, unit * P2p_peer.Id.t, unit, unit, unit) Tezos_rpc.Service.t

    val unban :
      ([`GET], unit, unit * P2p_peer.Id.t, unit, unit, unit) Tezos_rpc.Service.t

    val trust :
      ([`GET], unit, unit * P2p_peer.Id.t, unit, unit, unit) Tezos_rpc.Service.t

    val untrust :
      ([`GET], unit, unit * P2p_peer.Id.t, unit, unit, unit) Tezos_rpc.Service.t

    val banned :
      ([`GET], unit, unit * P2p_peer.Id.t, unit, unit, bool) Tezos_rpc.Service.t
  end
end

module ACL : sig
  (** Structure that provides a friendly and bounded list of currently
      greylisted IPs. *)
  type ip_list = {
    ips : Ipaddr.V6.t list;
        (** If [not_reliable_since] is [None] contains the list of currently
             greylisted IPs. *)
    not_reliable_since : Ptime.t option;
        (** Contains the date of the first time that the list of IPs has been
         overflowed and became not reliable. *)
  }

  val clear : #simple -> unit -> unit tzresult Lwt.t

  val get_greylisted_peers : #simple -> P2p_peer.Id.t list tzresult Lwt.t

  val get_greylisted_ips : #simple -> ip_list tzresult Lwt.t

  module S : sig
    val clear : ([`GET], unit, unit, unit, unit, unit) Tezos_rpc.Service.t

    val clear_delete :
      ([`DELETE], unit, unit, unit, unit, unit) Tezos_rpc.Service.t

    val get_greylisted_peers :
      ([`GET], unit, unit, unit, unit, P2p_peer.Id.t list) Tezos_rpc.Service.t

    val get_greylisted_ips :
      ([`GET], unit, unit, unit, unit, ip_list) Tezos_rpc.Service.t
  end
end
