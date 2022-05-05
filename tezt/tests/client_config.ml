(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Client configuration
   Invocation:   dune exec tezt/tests/main.exe -- --file client_config.ml
   Subject:      .
*)

let additional_bootstrap_accounts =
  Protocol.register_test
    ~__FILE__
    ~title:"additional bootstrap accounts"
    ~tags:["client"; "bootstrap"; "accounts"]
  @@ fun protocol ->
  let* (_node, client) =
    Client.init_with_protocol
      ~additional_bootstrap_account_count:2
      `Client
      ~protocol
      ()
  in
  let* bootstrap6 = Client.show_address ~alias:"bootstrap6" client in
  let* bootstrap7 = Client.show_address ~alias:"bootstrap7" client in
  Client.transfer
    ~amount:(Tez.of_int 2)
    ~giver:bootstrap6.public_key_hash
    ~receiver:bootstrap7.public_key_hash
    client

let register ~protocols = additional_bootstrap_accounts protocols
