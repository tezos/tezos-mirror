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
   Component:  Retro-compatibility tests
   Invocation: dune exec tezt/tests/main.exe -- -f retro.ml
   Subject:    Tests that binary encodings for usual operations
               are retro-compatible from one protocol to the next.
*)

type client_proto = {client : Client.t; proto : Protocol.t}

let init_client proto =
  let* client = Client.init_mockup ~protocol:proto () in
  return {client; proto}

let encode protocol what json =
  Codec.encode
    ~name:(String.concat "." [Protocol.encoding_prefix protocol; what])
    json

let decode protocol what hex =
  Codec.decode
    ~name:(String.concat "." [Protocol.encoding_prefix protocol; what])
    hex

let encode_and_sign_operation protocol client op =
  let* hex_unsigned =
    encode protocol "operation.unsigned" (Operation_core.json op)
  in
  let* signature = Operation_core.sign op client in
  let (`Hex signature) = Tezos_crypto.Signature.to_hex signature in
  return (hex_unsigned ^ signature)

let test_encoding_retrocompatible name operation =
  Protocol.register_test
    ~__FILE__
    ~title:(sf "Test retro compatibility for format of %s" name)
    ~tags:["retro"; name]
  @@ fun protocol ->
  let* client = init_client protocol
  and* client_prev =
    match Protocol.previous_protocol protocol with
    | None -> return None
    | Some proto ->
        let* c = init_client proto in
        return (Some c)
  in
  let* operation = Operation_core.Manager.operation operation client.client in
  let do_pair previous current =
    Log.info
      "Checking that unsigned %s encoded in protocol %s can be decoded by \
       protocol %s"
      name
      (Protocol.name current.proto)
      (Protocol.name previous.proto) ;
    let* hex_unsigned_current =
      encode current.proto "operation.unsigned" (Operation_core.json operation)
    in
    let* _ = decode previous.proto "operation.unsigned" hex_unsigned_current in
    Log.info
      "Checking that signed %s encoded in protocol %s can be decoded by \
       protocol %s"
      name
      (Protocol.name previous.proto)
      (Protocol.name current.proto) ;
    let* hex_signed_previous =
      encode_and_sign_operation previous.proto previous.client operation
    in
    let* _ = decode current.proto "operation" hex_signed_previous in
    unit
  in
  match client_prev with None -> unit | Some prev -> do_pair prev client

let simple_transfer =
  test_encoding_retrocompatible "transfer"
  @@ Operation_core.Manager.[make ~source:Constant.bootstrap1 @@ transfer ()]

let smart_contract_call =
  test_encoding_retrocompatible "call"
  @@ Operation_core.Manager.
       [
         make ~source:Constant.bootstrap1
         @@ call
              ~entrypoint:"super_entrypoint"
              ~arg:
                (Ezjsonm.from_string
                   {|
                   [{ "prim": "Pair",
                      "args": [{"int": "9999"},
                               {"string":"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"}]},
                    { "prim": "Pair",
                      "args": [{"string": "false"},
                              {"bytes":"deadbeef"}]}
                   ]
                   |})
              ();
       ]

let register ~protocols =
  simple_transfer protocols ;
  smart_contract_call protocols
