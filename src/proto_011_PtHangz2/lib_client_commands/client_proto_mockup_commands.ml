(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_clic

let protocol_constants_arg =
  Clic.arg
    ~doc:"a JSON file that contains protocol constants to set."
    ~long:"protocol-constants"
    ~placeholder:"path"
    (Clic.parameter (fun _ x -> return x))

let bootstrap_accounts_arg =
  Clic.arg
    ~doc:
      "a JSON file that contains definitions of bootstrap accounts to create."
    ~long:"bootstrap-accounts"
    ~placeholder:"path"
    (Clic.parameter (fun _ x -> return x))

let asynchronous_flag =
  Clic.switch
    ~long:"asynchronous"
    ~doc:"put operations in mempool and require baking to include in the chain"
    ()

let load_json_file (cctxt : Protocol_client_context.full) json_file =
  match json_file with
  | None -> return None
  | Some filename ->
      cctxt#read_file filename >>=? fun json_string ->
      return (Some (Ezjsonm.from_string json_string :> Data_encoding.json))

let create_mockup_command_handler
    (constants_overrides_file, bootstrap_accounts_file, asynchronous)
    (cctxt : Protocol_client_context.full) =
  load_json_file cctxt constants_overrides_file
  >>=? fun constants_overrides_json ->
  load_json_file cctxt bootstrap_accounts_file
  >>=? fun bootstrap_accounts_json ->
  Tezos_mockup.Persistence.create_mockup
    ~cctxt:(cctxt :> Tezos_client_base.Client_context.full)
    ~protocol_hash:Protocol.hash
    ~constants_overrides_json
    ~bootstrap_accounts_json
    ~asynchronous
  >>=? fun () ->
  Tezos_mockup_commands.Mockup_wallet.populate cctxt bootstrap_accounts_file

let create_mockup_command : Protocol_client_context.full Clic.command =
  let open Clic in
  command
    ~group:Tezos_mockup_commands.Mockup_commands.group
    ~desc:"Create a mockup environment."
    (args3 protocol_constants_arg bootstrap_accounts_arg asynchronous_flag)
    (prefixes ["create"; "mockup"] @@ stop)
    create_mockup_command_handler

let commands () = [create_mockup_command]
