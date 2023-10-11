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

(** This module defines the relevant data structures to instantiate the
    gossipsub worker. *)

(** From the Gossipsub point of view, a peer is given by a cryptographic node
    identity {!P2p_peer.Id.t}. It's up to the caller to associate the
    {!P2p_peer.Id.t} to a {!P2p_point.Id.t} if needed (to e.g. implement peers
    exchange, which needs addresses and ports instead of cryptographic
    identities). *)
type peer = P2p_peer.Id.t

module Span : Gossipsub_intf.SPAN

(** Encodings for various types above. *)

val span_encoding : Span.t Data_encoding.t

module Monad : sig
  type 'a t = 'a Lwt.t

  val return : 'a -> 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val sleep : Span.t -> unit t
end

module Worker_config :
  Gossipsub_intf.WORKER_CONFIGURATION
    with type GS.Topic.t = Types.Topic.t
     and type GS.Message_id.t = Types.Message_id.t
     and type GS.Message.t = Types.Message.t
     and type GS.Peer.t = peer
     and module GS.Span = Span
     and module Monad = Monad

module Worker_instance :
  Gossipsub_intf.WORKER
    with type GS.Topic.t = Types.Topic.t
     and type GS.Message_id.t = Types.Message_id.t
     and type GS.Message.t = Types.Message.t
     and type GS.Peer.t = peer
     and module GS.Span = Span
     and module Monad = Monad

module Validate_message_hook : sig
  val set :
    (Types.Message.t -> Types.Message_id.t -> [`Invalid | `Unknown | `Valid]) ->
    unit
end
