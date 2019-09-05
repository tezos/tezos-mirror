(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t

module Message = Distributed_db_message

type p2p = (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.net

type connection =
  (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.connection

type callback = {
  notify_branch : P2p_peer.Id.t -> Block_locator.t -> unit;
  notify_head : P2p_peer.Id.t -> Block_header.t -> Mempool.t -> unit;
  disconnection : P2p_peer.Id.t -> unit;
}

type chain_db = {
  chain_state : State.Chain.t;
  operation_db : Distributed_db_requester.Raw_operation.t;
  block_header_db : Distributed_db_requester.Raw_block_header.t;
  operation_hashes_db : Distributed_db_requester.Raw_operation_hashes.t;
  operations_db : Distributed_db_requester.Raw_operations.t;
  mutable callback : callback;
  active_peers : P2p_peer.Set.t ref;
  active_connections : connection P2p_peer.Table.t;
}

val run :
  register:(t -> unit) ->
  unregister:(unit -> unit) ->
  p2p ->
  State.t ->
  Distributed_db_requester.Raw_protocol.t ->
  chain_db Chain_id.Table.t ->
  P2p_peer.Id.t ->
  connection ->
  unit

val shutdown : t -> unit Lwt.t
