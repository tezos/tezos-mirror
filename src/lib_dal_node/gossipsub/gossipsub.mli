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

(** Below, we expose the main types needed for the integration with the existing
    DAL node alongside their encodings. *)

type topic = Gs_interface.topic

type message_id = Gs_interface.message_id

type message = Gs_interface.message

type peer = Gs_interface.peer

val topic_encoding : topic Data_encoding.t

val message_id_encoding : message_id Data_encoding.t

val message_encoding : message Data_encoding.t

(** The worker module exposes instantiation of the Gossipsub worker functor,
    alongside the config used to instantiate the functor and the default values
    of the GS parameters. *)
module Worker : sig
  module Config :
    module type of Gs_interface.Worker_config
      with type GS.Topic.t = topic
       and type GS.Message_id.t = message_id
       and type GS.Message.t = message
       and type GS.Peer.t = peer
       and module GS.Span = Gs_interface.Span

  module Default_parameters : module type of Gs_default_parameters

  include
    Gossipsub_intf.WORKER
      with type GS.Topic.t = topic
       and type GS.Message_id.t = message_id
       and type GS.Message.t = message
       and type GS.Peer.t = peer
       and module GS.Span = Config.GS.Span
end

(** The transport layer module exposes the needed primitives, interface and
    default parameters for the instantiation of the Octez-p2p library. *)
module Transport_layer : sig
  module Interface : module type of Transport_layer_interface

  module Default_parameters : module type of Transport_layer_default_parameters

  type t

  val create :
    network_name:string -> P2p.config -> P2p_limits.t -> t tzresult Lwt.t

  val activate : t -> unit
end
