(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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
open Tezos_rpc_http_server
open Protocol

type block_id =
  [ `Head
  | `L2_block of L2block.hash
  | `Tezos_block of Block_hash.t
  | `Level of L2block.level ]

val destruct_block_id : string -> (block_id, string) result

type context_id = [block_id | `Context of Tx_rollup_l2_context_hash.t]

(** Starts the RPC server of the tx_rollup_node. *)
val start : Configuration.t -> State.t -> RPC_server.server tzresult Lwt.t

(** Returns the balance for an l2-address and a ticket. *)
val balance :
  #RPC_context.simple ->
  block_id ->
  Alpha_context.Ticket_hash.t ->
  Tx_rollup_l2_address.t ->
  Tx_rollup_l2_qty.t Error_monad.tzresult Lwt.t

(** Returns the current counter for the given address. *)
val counter :
  #RPC_context.simple ->
  block_id ->
  Tx_rollup_l2_address.t ->
  int64 Error_monad.tzresult Lwt.t

(** Returns the tx-rollup-node inbox for a given block. *)
val inbox :
  #RPC_context.simple -> block_id -> Inbox.t option Error_monad.tzresult Lwt.t

(** Returns the L2 block in the tx-rollup-node. *)
val block :
  #RPC_context.simple -> block_id -> L2block.t option Error_monad.tzresult Lwt.t

(** Returns the whole queue of L2 transactions. *)
val get_queue :
  #RPC_context.simple -> L2_transaction.t list Error_monad.tzresult Lwt.t

(** Returns an L2 transaction in the queue given a transaction hash. *)
val get_transaction :
  #RPC_context.simple ->
  L2_transaction.hash ->
  L2_transaction.t option Error_monad.tzresult Lwt.t

(** Inject an L2 transaction in the queue of the rollup node and returns
    the transaction hash. *)
val inject_transaction :
  #RPC_context.simple ->
  ?eager_batch:bool ->
  L2_transaction.t ->
  L2_transaction.hash Error_monad.tzresult Lwt.t
