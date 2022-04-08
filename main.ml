(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 - 2021 Nomadic Labs, <contact@nomadic-labs.com>        *)
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
open Lwt_result_syntax

let select_commands ctxt Client_config.{protocol; chain; block; _} =
  match protocol with
  | Some protocol ->
      return
        (Tezos_client_commands.Client_commands.commands_for_version
           protocol
           None)
  | None -> (
      let*! protocol = Shell_services.Blocks.protocols ctxt ~chain ~block () in
      match protocol with
      | Ok {next_protocol; _} ->
          return
            (Tezos_client_commands.Client_commands.commands_for_version
               next_protocol
               None)
      | Error err ->
          let () = Format.eprintf "%a@." Error_monad.pp_print_trace err in
          return [])

let () =
  let () = PtGRANAD_machine.register_commands () in
  let () = PtHangz2_machine.register_commands () in
  Client_main_run.run (module Client_config) ~select_commands
