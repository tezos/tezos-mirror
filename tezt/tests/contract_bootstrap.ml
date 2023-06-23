(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_bootstrap.ml
   Subject:      Tests that bootstrap contracts existence.
*)

let test_bootstrap_contract_with_given_hash =
  Protocol.register_test
    ~__FILE__
    ~title:"Bootstrap contract with given hash"
    ~tags:["bootstrap"; "contract"; "michelson"]
    ~supports:(Protocol.From_protocol 019)
    (fun protocol ->
      let address = "KT1KqcpWDCy8A3MSAPcxDFkg3LSSgFokTb12" in
      let script =
        Ezjsonm.value_from_string
          {|
{ "code" : [ { "prim": "parameter", "args": [ { "prim": "unit" } ] },
  { "prim": "storage", "args": [ { "prim": "unit" } ] },
  { "prim": "code",
    "args":
      [ [ { "prim": "CAR" },
          { "prim": "NIL", "args": [ { "prim": "operation" } ] },
          { "prim": "PAIR" } ] ] } ],
  "storage" : { "prim" : "Unit", "args" : [] } }|}
      in
      let bootstrap_contract : Protocol.bootstrap_contract =
        {delegate = None; amount = Tez.of_int 42; script; hash = Some address}
      in
      let base = Either.right (protocol, None) in
      let* parameter_file =
        Protocol.write_parameter_file
          ~bootstrap_contracts:[bootstrap_contract]
          ~base
          []
      in
      let nodes_args = Node.[Synchronisation_threshold 0; No_bootstrap_peers] in
      let* _node, client =
        Client.init_with_protocol
          ~parameter_file
          `Client
          ~protocol
          ~nodes_args
          ()
      in
      let* storage = Client.contract_storage address client in
      Check.((String.trim storage = "Unit") string)
        ~error_msg:"Contract should exist with a unit storage" ;
      unit)

let register ~protocols = test_bootstrap_contract_with_given_hash protocols
