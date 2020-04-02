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

let create_mockup_command_handler _ (cctxt : Protocol_client_context.full) =
  let base_dir = cctxt#get_base_dir in
  ( match Tezos_mockup.Persistence.classify_base_dir base_dir with
  | Tezos_mockup.Persistence.Base_dir_is_nonempty ->
      cctxt#error "directory %s is nonempty" base_dir
  | Tezos_mockup.Persistence.Base_dir_is_mockup ->
      cctxt#error
        "%s seems to have already been initialized as a mockup directory"
        base_dir
  | Tezos_mockup.Persistence.Base_dir_does_not_exist
  | Tezos_mockup.Persistence.Base_dir_is_empty ->
      Tezos_mockup.Persistence.create_mockup
        ~protocol_hash:Protocol.hash
        ~base_dir )
  >>=? fun () -> Tezos_mockup_commands.Mockup_wallet.populate cctxt

let create_mockup_command : Protocol_client_context.full Clic.command =
  let open Clic in
  command
    ~group:Tezos_mockup_commands.Mockup_commands.group
    ~desc:"Create a mockup environment."
    no_options
    (prefixes ["create"; "mockup"] @@ stop)
    create_mockup_command_handler

let commands () = [create_mockup_command]
