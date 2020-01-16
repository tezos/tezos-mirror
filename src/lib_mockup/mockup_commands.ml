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

(* Workaround for the fact that the client directory is not available from
   the client context. *)
let base_dir = ref None

let set_base_dir dir = base_dir := Some dir

let group =
  {Clic.name = "mockup"; title = "Commands for creating mockup environments"}

let list_mockup_command_handler _ _ =
  let available = Registration.get_registered_contexts () in
  List.iter
    (fun (mockup : (module Registration.Mockup_sig)) ->
      let module Mockup = (val mockup) in
      Format.printf "%a@." Protocol_hash.pp Mockup.protocol_hash)
    available ;
  return ()

let create_mockup_command_handler _ protocol_hash
    (cctxt : Tezos_client_base.Client_context.full) =
  let res = Protocol_hash.of_b58check_opt protocol_hash in
  match res with
  | Some protocol_hash ->
      ( match !base_dir with
      | None ->
          cctxt#error "--base-dir not set"
      | Some base_dir -> (
        match Tezos_mockup.Persistence.classify_base_dir base_dir with
        | Tezos_mockup.Persistence.Base_dir_is_nonempty ->
            cctxt#error "directory %s is nonempty" base_dir
        | Tezos_mockup.Persistence.Base_dir_is_mockup ->
            cctxt#error
              "%s seems to have already been initialized as a mockup directory"
              base_dir
        | Tezos_mockup.Persistence.Base_dir_does_not_exist
        | Tezos_mockup.Persistence.Base_dir_is_empty ->
            Tezos_mockup.Persistence.create_mockup ~protocol_hash ~base_dir )
      )
      >>=? fun () -> Mockup_wallet.populate cctxt
  | None ->
      cctxt#error "invalid protocol hash"

let list_mockup_command : Tezos_client_base.Client_context.full Clic.command =
  let open Clic in
  command
    ~group
    ~desc:"List available protocols available for mockup construction."
    no_options
    (prefixes ["list"; "mockup"; "protocols"] @@ stop)
    list_mockup_command_handler

let create_mockup_command : Tezos_client_base.Client_context.full Clic.command
    =
  let open Clic in
  command
    ~group
    ~desc:"Create a mockup environment."
    no_options
    ( prefixes ["create"; "mockup"; "for"; "protocol"]
    @@ string
         ~name:"protocol-hash"
         ~desc:
           "hash of the protocol for which a mockup environment will be created"
    @@ stop )
    create_mockup_command_handler

let commands () = [create_mockup_command; list_mockup_command]
