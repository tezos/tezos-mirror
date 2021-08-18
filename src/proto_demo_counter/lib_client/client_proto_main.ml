(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol
module Commands = Client_proto_commands

let commands : Protocol_client_context.full Clic.command list =
  let open Clic in
  let open Client_proto_args in
  let group =
    {name = "Demo_counter"; title = "Commands for protocol Demo_counter"}
  in
  [
    command
      ~group
      ~desc:"Bake a block"
      no_options
      (prefixes ["bake"]
      @@ msg_param ~name:"message" ~desc:"message in block header"
      @@ stop)
      (fun () msg cctxt -> Commands.bake cctxt msg);
    command
      ~group
      ~desc:"Increment A"
      no_options
      (fixed ["increment"; "a"])
      (fun () cctxt -> Commands.inject_op cctxt Proto_operation.IncrA);
    command
      ~group
      ~desc:"Increment B"
      no_options
      (fixed ["increment"; "b"])
      (fun () cctxt -> Commands.inject_op cctxt Proto_operation.IncrB);
    command
      ~group
      ~desc:"Transfer from A to B"
      no_options
      (prefixes ["transfer"]
      @@ amount_param
           ~name:"amount"
           ~desc:"amount taken from A and given to B (possibly negative)"
      @@ stop)
      (fun () amount cctxt ->
        Commands.inject_op cctxt (Proto_operation.Transfer amount));
    command
      ~group
      ~desc:"Get A counter"
      no_options
      (fixed ["get"; "a"])
      (fun () cctxt -> Commands.get_counter cctxt `A);
    command
      ~group
      ~desc:"Get B counter"
      no_options
      (fixed ["get"; "b"])
      (fun () cctxt -> Commands.get_counter cctxt `B);
  ]

let () =
  let f = Clic.map_command (new Protocol_client_context.wrap_full) in
  let command_list = List.map f commands in
  Client_commands.register Protocol.hash (fun _network -> command_list)
