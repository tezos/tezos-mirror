(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** [fetch_dal_config cctxt] fetches the DAL configuration from the connected L1
    node. *)
val fetch_dal_config :
  Rpc_context.t -> Tezos_crypto_dal_octez_dal_config.Dal_config.t tzresult Lwt.t

(** [wait_for_l1_bootstrapped cctxt] waits until the L1 node signals it is
    bootstrapped. *)
val wait_for_l1_bootstrapped : Rpc_context.t -> unit tzresult Lwt.t

(** [infer_dal_network_name cctxt] infers the DAL network name from the
    connected L1 node. Uses legacy "dal-sandbox" if version <= V22, or
    "DAL_<CHAIN_NAME>" otherwise. *)
val infer_dal_network_name :
  Rpc_context.t -> Distributed_db_version.Name.t tzresult Lwt.t

(** [wait_for_block_with_plugin cctxt] waits until a block is available with a
    known DAL plugin, and returns its header along with the plugin module. *)
val wait_for_block_with_plugin :
  Rpc_context.t -> (Block_header.t * Proto_plugins.t) tzresult Lwt.t

(** [fetch_l1_chain_id cctxt] fetches the chain id from the connected L1
    node. *)
val fetch_l1_chain_id : Rpc_context.t -> Chain_id.t tzresult Lwt.t
