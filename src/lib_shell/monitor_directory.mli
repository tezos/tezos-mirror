(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

val monitor_head :
  head_watcher:
    (Block_hash.t * Block_header.t) Lwt_stream.t * Lwt_watcher.stopper ->
  Store.t ref ->
  Chain_services.chain ->
  < next_protocols : Protocol_hash.t trace
  ; protocols : Protocol_hash.t trace
  ; .. > ->
  (Block_hash.t * Block_header.t) Tezos_rpc.Answer.t Lwt.t

val applied_blocks :
  applied_blocks_watcher:
    (Store.chain_store * Store.Block.t) Lwt_stream.t * Lwt_watcher.stopper ->
  < chains : Chain_services.chain trace
  ; next_protocols : Protocol_hash.t trace
  ; protocols : Protocol_hash.t trace
  ; .. > ->
  (Chain_id.t * Block_hash.t * Block_header.t * Operation.t trace trace)
  Tezos_rpc.Answer.t
  Lwt.t

val build_rpc_directory :
  Validator.t -> Chain_validator.t -> unit Tezos_rpc.Directory.t
