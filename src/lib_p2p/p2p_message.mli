(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module defines the messages of the P2p layers.

    P2p messages are parameterized by a generic upper-layer message type
    ['msg]. The P2p layer serializes these ['msg] using a ['msg encoding']
    provided by the client of this lib.

    The P2p protocol is simple and can be deduced mostly from the message type.
    To make the network topology more dynamic, it implements a simple peer
    swapping mechanism which works as follows.

    Peer A sends a message : [Swap_request (point, peer)] to B.
    If B is already connected to the peer, the message is ignored.
    Otherwise B picks a peer [peer'] at point [point'] and connect to [peer]. If
    successful, it sends a response [Swap_ack (point', peer')] to A. Upon
    reception of [Swap_ack]. B tries to connected to [peer']. If successful,
    it disconnect from [peer]. *)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4610
     It would be interesting to measure the effect of the swap request
     mechanism on an actual network. Is it the added complexity worth it,
     wouldn't it  be enough to rely on [Advertise]? *)

type 'msg t =
  | Bootstrap  (** Welcome message sent by a peer upon connection *)
  | Advertise of P2p_point.Id.t list
      (** Response to a [Bootstrap] message, contains list of known points *)
  | Swap_request of P2p_point.Id.t * P2p_peer.Id.t
      (** Propose new peer/point and ask a peer/point to swap with *)
  | Swap_ack of P2p_point.Id.t * P2p_peer.Id.t
      (** Response to a swap request and propose peer/point to swap with. *)
  | Message of 'msg  (** Generic upper-layer message *)
  | Disconnect  (** Ending of connection, unused for now *)

val encoding : 'a P2p_params.app_message_encoding list -> 'a t Data_encoding.t
