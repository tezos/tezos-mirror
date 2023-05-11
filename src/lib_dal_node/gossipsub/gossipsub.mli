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

type topic = Gs_interface.topic

type message_id = Gs_interface.message_id

type message = Gs_interface.message

type peer = Gs_interface.peer

val topic_encoding : topic Data_encoding.t

val message_id_encoding : message_id Data_encoding.t

val message_encoding : message Data_encoding.t

module Worker : sig
  module Config :
    module type of Gs_interface.Worker
      with type GS.Topic.t = topic
       and type GS.Message_id.t = message_id
       and type GS.Message.t = message
       and type GS.Peer.t = peer

  module Default_parameters : module type of Gs_default_parameters

  include
    Gossipsub_intf.WORKER
      with type GS.Topic.t = topic
       and type GS.Message_id.t = message_id
       and type GS.Message.t = message
       and type GS.Peer.t = peer
       and module GS.Span = Config.GS.Span
end
