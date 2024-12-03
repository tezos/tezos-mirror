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

(** [activate gs_worker transport_layer ~app_messages_handler] connects the
    given [gs_worker] and [transport_layer]. (Dis)connections and messages of
    the transport layer are forwarded to the GS worker. P2P output messages and
    (dis)connection requests are forwarded from the GS worker to the transport
    layer.

    The [app_messages_handler] is invoked when some application messages are put
    by the Gossipsub worker in the application output stream.
*)
val activate :
  Gs_interface.Worker_instance.t ->
  ( Gs_interface.Worker_instance.p2p_message,
    Types.P2P.Metadata.Peer.t,
    Types.P2P.Metadata.Connection.t )
  P2p.t ->
  app_messages_callback:
    (Types.Message.t -> Types.Message_id.t -> unit tzresult Lwt.t) ->
  unit Lwt.t
