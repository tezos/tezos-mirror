(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** An [Answerer.t] is a set of callback functions, parameterized by
    [conn_info] record. The [conn_info] records contains values useful
    for the callback functions to perform their task, and known after
    the connection is set up.

    The callback functions are called when the node receives `P2p_messages.t`
    messages. The parameters are the values carried by the message, and
    a [request_info] record that contains values pertaining to the connection
    that may change during the life of the connection. *)

type 'msg conn_info = {
  peer_id : P2p_peer.Id.t;
  is_private : bool;
  write_advertise : P2p_point.Id.t list -> bool tzresult;
      (** [write_advertise points] must send the message [Advertise(points)]
          to the internal [peer_id]. It must return [Ok true] if the message
          has been successfully sent, [Ok false] if the message has been
          dropped, or fails with a corresponding error otherwise. *)
  write_swap_ack : P2p_point.Id.t -> P2p_peer.Id.t -> bool tzresult;
      (** [write_swap_ack (p1, p2)] must send the message [Swap_ack(p1, p2)]
          to the internal [peer_id]. It must return [Ok true] if the message
          has been successfully sent, [Ok false] if the message has been
          dropped, or fails with a corresponding error otherwise. *)
  messages : (int * 'msg) Lwt_pipe.Maybe_bounded.t;
}

type request_info = {
  last_sent_swap_request : (Time.System.t * P2p_peer.Id.t) option;
}

type 'msg callback = {
  bootstrap : request_info -> unit tzresult Lwt.t;
  advertise : request_info -> P2p_point.Id.t list -> unit Lwt.t;
  message : request_info -> int -> 'msg -> unit Lwt.t;
  swap_request : request_info -> P2p_point.Id.t -> P2p_peer.Id.t -> unit Lwt.t;
  swap_ack : request_info -> P2p_point.Id.t -> P2p_peer.Id.t -> unit Lwt.t;
}

type 'msg t = 'msg conn_info -> 'msg callback
