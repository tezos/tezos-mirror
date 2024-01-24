(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc

let sequencer_root = Path.(root / "sequencer")

let get_blueprint_service =
  Service.get_service
    ~description:"Fetch the contents of a blueprint"
    ~query:Query.empty
    ~output:Blueprint_types.encoding
    Path.(sequencer_root / "blueprint" /: Arg.uint63)

let register ctxt dir =
  Directory.opt_register1 dir get_blueprint_service (fun level () () ->
      let open Lwt_syntax in
      let number = Ethereum_types.Qty (Z.of_int64 level) in
      let* blueprint = Blueprint_store.find ctxt number in
      return_ok blueprint)
