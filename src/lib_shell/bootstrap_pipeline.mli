(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

(** The bootstrap pipeline works as follows:
    1. From a locator, it computes a list of subchains (identified by
       a [Block_locator.step]) to fetch.
    2. A worker starts to fetch all the headers (top to bottom) from a subchain,
       starting with the top subchain.
    3. A worker starts to download the list of operations by batch of blocks
       once a batch of headers is available.
    4. A worker validates blocks one by one (bottom to top).

    These three workers work concurrently.
*)

type t

(** [create ?notify_new_block ~block_header_timeout
    ~block_operations_timeout validator peer ddb locator] initializes a
    [bootstrap_pipeline] for a [locator] sent by [peer]. It uses the
    [Distributed_db] to fetch the data needed and [validator] to
    validate blocks one by one with the function
    [Block_validator.validate].

    Moreover:
    - [notify_new_block] is a called everytime a valid block
    is received.
    - [block_header_timeout] is the timeout to fetch a [block_header].
    - [block_operations_timeout] is the timeout to fetch a block
    operation.

    If a timeout is triggered, the whole [bootstrap_pipeline] is
    canceled.*)
val create :
  ?notify_new_block:(Store.Block.t -> unit) ->
  block_header_timeout:Time.System.Span.t ->
  block_operations_timeout:Time.System.Span.t ->
  Block_validator.t ->
  P2p_peer.Id.t ->
  Distributed_db.chain_db ->
  Block_locator.t ->
  t

val wait : t -> unit tzresult Lwt.t

val cancel : t -> unit Lwt.t

(** [length pipeline] returns the number of the headers and blocks fetched *)
val length : t -> Peer_validator_worker_state.pipeline_length

val length_zero : Peer_validator_worker_state.pipeline_length
