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

module Worker_config :
  Gossipsub_intf.WORKER_CONFIGURATION
    with type GS.Topic.t = Types.Topic.t
     and type GS.Message_id.t = Types.Message_id.t
     and type GS.Message.t = Types.Message.t
     and type GS.Peer.t = Types.Peer.t
     and type GS.Span.t = Types.Span.t
     and type GS.Time.t = Types.Time.t
     and type 'a Monad.t = 'a Lwt.t

module Worker_instance :
  Gossipsub_intf.WORKER
    with type GS.Topic.t = Types.Topic.t
     and type GS.Message_id.t = Types.Message_id.t
     and type GS.Message.t = Types.Message.t
     and type GS.Peer.t = Types.Peer.t
     and type GS.Span.t = Types.Span.t
     and type GS.Time.t = Types.Time.t
     and type 'a Monad.t = 'a Lwt.t

module Validate_message_hook : sig
  val set :
    (?message:Worker_instance.GS.Message.t ->
    message_id:Worker_instance.GS.Message_id.t ->
    unit ->
    [`Valid | `Unknown | `Outdated | `Invalid]) ->
    unit
end
