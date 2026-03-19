(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Environment
open Environment.Error_monad
open Protocol.Alpha_context
open Services_registration_plugin

let custom_root =
  (RPC_path.(open_root / "context" / "destination")
    : RPC_context.t RPC_path.context)

module S = struct
  open Data_encoding

  let index =
    RPC_service.get_service
      ~description:
        "Returns the index assigned to the address if it was indexed by the \
         opcode INDEX_ADDRESS, otherwise returns null"
      ~query:RPC_query.empty
      ~output:(option n)
      RPC_path.(custom_root /: Destination.rpc_arg / "index")
end

let index ctxt block destination =
  RPC_context.make_call1 S.index ctxt block destination () ()

let register () =
  let open Lwt_result_syntax in
  register1 ~chunked:false S.index (fun ctxt destination () () ->
      let* _ctxt, index_opt = Address_registry.find ctxt destination in
      return index_opt)
