(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_dal_node_services

let handle_slot_pages ctxt (_, commitment) () () =
  let open Lwt_result_syntax in
  let*? {cryptobox; _} = Node_context.get_ready ctxt in
  Slot_manager.get_slot_pages
    cryptobox
    (Node_context.get_store ctxt).shard_store
    commitment

let handle_shard ctxt ((_, commitment), shard) () () =
  Slot_manager.get_shard
    (Node_context.get_store ctxt).shard_store
    commitment
    shard

let handle_shards ctxt (_, commitment) () shards =
  Slot_manager.get_shards
    (Node_context.get_store ctxt).shard_store
    commitment
    shards

let register_show_slot_pages ctxt dir =
  Tezos_rpc.Directory.register dir Services.slot_pages (handle_slot_pages ctxt)

let shards_service :
    ( [`POST],
      unit,
      unit * Cryptobox.commitment,
      unit,
      int list,
      Cryptobox.shard list )
    Tezos_rpc.Service.service =
  Services.shards

let register_shards ctxt dir =
  Tezos_rpc.Directory.register dir shards_service (handle_shards ctxt)

let register_shard ctxt dir =
  Tezos_rpc.Directory.register dir Services.shard (handle_shard ctxt)

let shards_rpc ctxt commitment shards =
  Tezos_rpc.Context.make_call shards_service ctxt ((), commitment) () shards

let shard_rpc ctxt commitment shard =
  Tezos_rpc.Context.make_call
    Services.shard
    ctxt
    (((), commitment), shard)
    ()
    ()
