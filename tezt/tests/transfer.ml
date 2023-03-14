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
   Component:    Client / Transfer
   Invocation:   dune exec tezt/tests/main.exe -- --file transfer.ml
   Subject:      Test transfers
*)

let test_zero_transfer_to_implicit_contract =
  Protocol.register_test
    ~__FILE__
    ~title:"Test Zero Transfer to Implicit Contract"
    ~tags:["client"; "transfer"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let pubkey = Account.Bootstrap.keys.(2).public_key_hash in
  let err =
    rexf
      "Transactions of 0ꜩ towards a contract without code are forbidden \
       \\(%s\\)."
      pubkey
  in
  Client.spawn_transfer
    ~amount:Tez.zero
    ~giver:(Account.Bootstrap.alias 2)
    ~receiver:(Account.Bootstrap.alias 3)
    client
  |> Process.check_error ~msg:err

let test_zero_transfer_to_nonexistent_contract =
  Protocol.register_test
    ~__FILE__
    ~title:"Test Zero Transfer to Nonexistent Contract"
    ~tags:["client"; "transfer"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let nonexistent = "KT1Fcq4inD44aMhmUiTEHR1QMQwJT7p2u641" in
  let err = rexf "Contract %s does not exist" nonexistent in
  Client.spawn_transfer
    ~amount:Tez.zero
    ~giver:(Account.Bootstrap.alias 2)
    ~receiver:nonexistent
    client
  |> Process.check_error ~msg:err

let register ~protocols =
  test_zero_transfer_to_implicit_contract protocols ;
  test_zero_transfer_to_nonexistent_contract protocols
