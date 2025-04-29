(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* The set of supported RPCs depends on the mode and on the chain
   family.

   For the sequencer, only very few JSON RPCs need to be supported.
   For the observer, proxy, and RPC modes, we assume that a single
   chain is followed even when the multichain feature is activated and
   the set of supported RPCs depends on the chain family.

   The ['f rpc_server_family] GADT keeps track of the dependency to
   the chain family.
*)

type 'f rpc_server_family =
  | Multichain_sequencer_rpc_server : _ rpc_server_family
  | Single_chain_node_rpc_server :
      'f L2_types.chain_family
      -> 'f rpc_server_family

let check_rpc_server_config (type f) (rpc_server_family : f rpc_server_family)
    (config : Configuration.t) =
  match (rpc_server_family, config.experimental_features.rpc_server) with
  | Single_chain_node_rpc_server Michelson, Dream ->
      Result_syntax.tzfail Node_error.Dream_rpc_tezlink
  | _ -> Result_syntax.return_unit
