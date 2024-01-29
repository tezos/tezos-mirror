(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc

let evm_services_root = Path.(root / "evm")

let get_blueprint_service =
  Service.get_service
    ~description:"Fetch the contents of a blueprint"
    ~query:Query.empty
    ~output:Blueprint_types.payload_encoding
    Path.(evm_services_root / "blueprint" /: Arg.uint63)

let register_get_blueprint_service ctxt dir =
  Directory.opt_register1 dir get_blueprint_service (fun level () () ->
      let open Lwt_syntax in
      let number = Ethereum_types.Qty (Z.of_int64 level) in
      let* blueprint = Evm_context.find_blueprint ctxt number in
      return_ok blueprint)

let register ctxt dir = register_get_blueprint_service ctxt dir

let get_blueprint ~evm_node_endpoint Ethereum_types.(Qty level) =
  Tezos_rpc_http_client_unix.RPC_client_unix.call_service
    [Media_type.json]
    ~base:evm_node_endpoint
    get_blueprint_service
    ((), Z.to_int64 level)
    ()
    ()
