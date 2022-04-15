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

let group =
  {Clic.name = "mockup"; title = "Commands for creating mockup environments"}

let list_mockup_command_handler _ _ =
  let available = Registration.get_registered_environments () in
  List.iter
    (fun (mockup : (module Registration.MOCKUP)) ->
      let module Mockup = (val mockup) in
      Format.printf "%a@." Protocol_hash.pp Mockup.protocol_hash)
    available ;
  Lwt_result_syntax.return_unit

let list_mockup_command : Tezos_client_base.Client_context.full Clic.command =
  let open Clic in
  command
    ~group
    ~desc:"List available protocols available for mockup construction."
    no_options
    (prefixes ["list"; "mockup"; "protocols"] @@ stop)
    list_mockup_command_handler

let migrate_mockup_command_handler () next_protococol_hash
    (cctxt : Tezos_client_base.Client_context.full) =
  match Protocol_hash.of_b58check next_protococol_hash with
  | Error _ as result -> Lwt.return result
  | Ok next_protocol_hash ->
      Migration.migrate_mockup ~cctxt ~protocol_hash:None ~next_protocol_hash

let migrate_mockup_command : Tezos_client_base.Client_context.full Clic.command
    =
  let open Clic in
  command
    ~group
    ~desc:"Migrates an on-disk mockup context from a protocol to another."
    no_options
    (prefixes ["migrate"; "mockup"; "to"]
    @@ string ~name:"hash" ~desc:"Protocol hash of the next protocol"
    @@ stop)
    migrate_mockup_command_handler

let commands () = [list_mockup_command; migrate_mockup_command]
