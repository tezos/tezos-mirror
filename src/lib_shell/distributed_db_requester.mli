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

(** This module creates FULL_REQUESTER modules for several resources.
    Protocols, operation, block_header, operation_hashes, operations.

    To do so, it instanciates the `Requester.Make` functor using the P2p layer
    for sending request, and a `State.t` or `State.Chain.t` for storage. *)

module Message = Distributed_db_message

type 'a request_param = {
  p2p : (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.t;
  data : 'a;
  active : unit -> P2p_peer.Set.t;
  send : P2p_peer.Id.t -> Message.t -> unit;
}

module type EXTENDED_REQUESTER = sig
  include Requester.FULL_REQUESTER

  val state_of_t :
    t -> Chain_validator_worker_state.Distributed_db_state.table_scheduler
end

module type EXTENDED_REQUESTER_2 = sig
  include EXTENDED_REQUESTER

  val clear_all : t -> Block_hash.t -> int -> unit
end

module Raw_protocol :
  EXTENDED_REQUESTER
    with type key = Protocol_hash.t
     and type param = unit
     and type request_param = unit request_param
     and type store = Store.t
     and type value = Protocol.t
     and type notified_value = Protocol.t

module Raw_operation :
  EXTENDED_REQUESTER
    with type key = Operation_hash.t
     and type param = unit
     and type request_param = unit request_param
     and type store = Store.chain_store
     and type value = Operation.t
     and type notified_value = Operation.t

module Raw_block_header :
  EXTENDED_REQUESTER
    with type key = Block_hash.t
     and type param = unit
     and type request_param = unit request_param
     and type store = Store.chain_store
     and type value = Block_header.t
     and type notified_value = Block_header.t

module Raw_operations :
  EXTENDED_REQUESTER_2
    with type key = Block_hash.t * int
     and type request_param = unit request_param
    (* root of merkle tree for this block, used to check the notified
        value. *)
     and type param = Operation_list_list_hash.t
     and type store = Store.chain_store
     and type value = Operation.t list
    (* notified value contain the queried value, plus the merkle tree hashes
          needed to recompute the merkle tree root. *)
     and type notified_value = Operation.t list * Operation_list_list_hash.path
