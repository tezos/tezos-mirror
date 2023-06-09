(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

open Protocol
open Alpha_context

module type Batcher_sig = sig
  (** [init config ~signer node_ctxt] initializes and starts the batcher for
    [signer]. If [config.simulation] is [true] (the default), messages added to
    the batcher are simulated in an incremental simulation context. *)
  val init :
    Configuration.batcher ->
    signer:public_key_hash ->
    _ Node_context.t ->
    unit tzresult Lwt.t

  (** Create L2 batches of operations from the queue and pack them in an L1 batch
    operation. The batch operation is queued in the injector for injection on
    the Tezos node. *)
  val batch : unit -> unit tzresult Lwt.t

  (** Notify a new L2 head to the batcher worker. *)
  val new_head : Layer1.head -> unit tzresult Lwt.t

  (** Shutdown the batcher, waiting for the ongoing request to be processed. *)
  val shutdown : unit -> unit Lwt.t
end

module type RPC_server_sig = sig
  (** [start node_ctxt config] starts an RPC server listening for requests on
      the port [config.rpc_port] and address [config.rpc_addr]. *)
  val start :
    Node_context.rw ->
    Configuration.t ->
    Tezos_rpc_http_server.RPC_server.server tzresult Lwt.t

  (** Shutdown a running RPC server. When this function is called, the rollup
          node will stop listening to incoming requests. *)
  val shutdown : Tezos_rpc_http_server.RPC_server.server -> unit Lwt.t
end

module type S = sig
  module Batcher : Batcher_sig

  module RPC_server : RPC_server_sig
end
