(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Local_services

let build_directory (base_dir : string) (mem_only : bool)
    (mockup_env : Registration.mockup_environment) (chain_id : Chain_id.t)
    (rpc_context : Tezos_protocol_environment.rpc_context) :
    unit RPC_directory.t =
  let write_context rpc_context =
    let (module Mockup_environment) = mockup_env in
    Persistence.overwrite_mockup
      ~chain_id
      ~protocol_hash:Mockup_environment.protocol_hash
      ~rpc_context
      ~base_dir
  in
  let (module Mockup_environment) = mockup_env in
  let proto_directory =
    (* register protocol-specific RPCs *)
    Directory.prefix
      Tezos_shell_services.Chain_services.path
      (Directory.prefix
         Tezos_shell_services.Block_services.path
         (Directory.map
            (fun (_chain, _block) -> Lwt.return rpc_context)
            Mockup_environment.directory))
  in
  let shell_directory =
    let (module Mockup_environment) = mockup_env in
    Local_services.build_shell_directory
      base_dir
      mockup_env
      chain_id
      rpc_context
      mem_only
      write_context
  in
  let base = Directory.merge shell_directory proto_directory in
  RPC_directory.register_describe_directory_service
    base
    RPC_service.description_service
