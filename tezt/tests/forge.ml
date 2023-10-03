(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component: Forging operations
   Subject: An example test demonstrating how to forge and inject an operation.
   Invocation: dune exec tezt/tests/main.exe -- -f forge.ml

               Note that it can be run with [dune exec tezt/tests/main.exe -- -f forge.ml --commands]
               to see the commands that are run.
*)

let forge =
  Protocol.register_test ~__FILE__ ~title:"forge" ~tags:["forge"; "transfer"]
  @@ fun protocol ->
  let* _node, client = Client.init_with_protocol `Client ~protocol () in
  let* (`OpHash _str) =
    Operation.Manager.(inject [make @@ transfer ()] client)
  in
  let* () = Client.bake_for_and_wait client in
  let* _ = Client.RPC.call client @@ RPC.get_chain_block_operations () in
  unit

let register ~protocols = forge protocols
