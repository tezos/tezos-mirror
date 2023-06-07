(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_big_map_to_self.ml
   Subject:      Tests sending big_map to self
*)

let test_big_map_to_self =
  Protocol.register_test
    ~__FILE__
    ~title:"Test sending big_map to self"
    ~tags:["client"; "michelson"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, contract =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap5"
      ~init:"{}"
      ~burn_cap:Tez.one
      client
      ["opcodes"; "big_map_to_self"]
      protocol
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:"bootstrap2"
      ~receiver:contract
      client
  in
  unit

let register ~protocols = test_big_map_to_self protocols
