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
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_hash_with_origination.ml
   Subject:      Tests that `get contract script hash for <contract>` return the
                 same value as directly hashing the script.
*)

let test_contract_hash_with_origination ~protocol () =
  let* client = Client.init_mockup ~protocol () in
  let prg = Michelson_script.(find ["opcodes"; "noop"] protocol |> path) in
  let script = read_file prg in
  let* contract =
    Client.originate_contract
      ~alias:"noop"
      ~amount:(Tez.of_int 1000)
      ~src:Constant.bootstrap1.alias
      ~prg
      ~init:"Unit"
      ~burn_cap:Tez.one
      client
  in
  let* received_script_hash = Client.hash_script client ~script in
  let* received_contract_hash = Client.get_contract_hash client ~contract in
  Check.(
    (received_script_hash = received_contract_hash)
      string
      ~__LOC__
      ~error_msg:"Expected contract hash %R, got %L") ;
  unit

let register ~protocols =
  List.iter
    (fun (title, test_function) ->
      Protocol.register_test
        ~__FILE__
        ~title
        ~tags:["client"; "michelson"]
        (fun protocol -> test_function ~protocol ())
        protocols)
    [
      ( "Test `get contract hash with origination for`",
        test_contract_hash_with_origination );
    ]
