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
   Components: Michelson
   Invocation: dune exec tezt/tests/main.exe -- --file comparable_datatype.ml
   Subject: Tests for comparable data type
*)

let test_comparable_unit client () =
  let* () = Client.typecheck_data ~data:"{}" ~typ:"(set unit)" client in
  let* () = Client.typecheck_data ~data:"{Unit}" ~typ:"(set unit)" client in
  unit

let assert_typecheck_data_failure client ~data ~typ =
  let expected_msg = rex "ill-typed data" in
  let process = Client.spawn_typecheck_data ~data ~typ client in
  Process.check_error ~msg:expected_msg process

let test_comparable_options client () =
  let* () = Client.typecheck_data ~data:"{}" ~typ:"(set (option nat))" client in
  let* () =
    Client.typecheck_data
      ~data:"{None; Some 1; Some 2}"
      ~typ:"(set (option int))"
      client
  in
  let* () =
    assert_typecheck_data_failure
      client
      ~data:"{Some \"foo\"; Some \"bar\"}"
      ~typ:"(set (option string))"
  in
  let* () =
    assert_typecheck_data_failure
      ~data:"{Some Unit; None}"
      ~typ:"(set (option unit))"
      client
  in
  unit

let test_comparable_or client () =
  let* () =
    Client.typecheck_data ~data:"{}" ~typ:"(set (or unit bool))" client
  in
  let* () =
    Client.typecheck_data
      ~data:"{Left 3; Left 4; Right \"bar\"; Right \"foo\"}"
      ~typ:"(set (or nat string))"
      client
  in
  let* () =
    assert_typecheck_data_failure
      client
      ~data:"{Left 2; Left 1}"
      ~typ:"(set (or mutez unit))"
  in
  let* () =
    assert_typecheck_data_failure
      client
      ~data:"{Right True; Right False}"
      ~typ:"(set (or unit bool))"
  in
  let* () =
    assert_typecheck_data_failure
      client
      ~data:"{Right 0; Left 1}"
      ~typ:"(set (or nat nat))"
  in
  unit

(* Tests that badly-ordered set literals are rejected *)
let test_comparable_pair client () =
  let* () =
    Client.typecheck_data ~data:"{}" ~typ:"(set (pair nat string))" client
  in
  let* () =
    Client.typecheck_data
      ~data:"{Pair 0 \"foo\"}"
      ~typ:"(set (pair nat string))"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:"{Pair 0 \"foo\"; Pair 1 \"bar\"}"
      ~typ:"(set (pair nat string))"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:"{Pair 0 \"bar\"; Pair 0 \"foo\"; Pair 1 \"bar\"; Pair 1 \"foo\"}"
      ~typ:"(set (pair nat string))"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:"{}"
      ~typ:"(set (pair nat (pair string bytes)))"
      client
  in
  let* () =
    Client.typecheck_data ~data:"{}" ~typ:"(map (pair nat string) unit)" client
  in
  let* () =
    Client.typecheck_data
      ~data:"{Elt (Pair 0 \"foo\") Unit}"
      ~typ:"(map (pair nat string) unit)"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:"{Elt (Pair 0 \"foo\") Unit; Elt (Pair 1 \"bar\") Unit}"
      ~typ:"(map (pair nat string) unit)"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:
        "{Elt (Pair 0 \"bar\") Unit; Elt (Pair 0 \"foo\") Unit; Elt (Pair 1 \
         \"bar\") Unit; Elt (Pair 1 \"foo\") Unit}"
      ~typ:"(map (pair nat string) unit)"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:"{}"
      ~typ:"(map (pair nat (pair string bytes)) unit)"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:"{}"
      ~typ:"(big_map (pair nat string) unit)"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:"{Elt (Pair 0 \"foo\") Unit}"
      ~typ:"(big_map (pair nat string) unit)"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:"{Elt (Pair 0 \"foo\") Unit; Elt (Pair 1 \"bar\") Unit}"
      ~typ:"(big_map (pair nat string) unit)"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:
        "{Elt (Pair 0 \"bar\") Unit; Elt (Pair 0 \"foo\") Unit; Elt (Pair 1 \
         \"bar\") Unit; Elt (Pair 1 \"foo\") Unit}"
      ~typ:"(big_map (pair nat string) unit)"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:"{}"
      ~typ:"(big_map (pair nat (pair string bytes)) unit)"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:"{}"
      ~typ:"(set (pair (pair nat nat) nat))"
      client
  in
  let* () =
    Client.typecheck_data
      ~data:"{}"
      ~typ:"(set (pair (pair int nat) (pair bool bytes)))"
      client
  in
  unit

let test_order_of_pairs client () =
  let* () =
    assert_typecheck_data_failure
      ~data:"{Pair 0 \"foo\"; Pair 0 \"bar\"}"
      ~typ:"(set (pair nat string))"
      client
  in
  let* () =
    assert_typecheck_data_failure
      ~data:"{Pair 1 \"bar\"; Pair 0 \"foo\"}"
      ~typ:"(set (pair nat string))"
      client
  in
  unit

let test_comparable_chain_id client () =
  let* () = Client.typecheck_data ~data:"{}" ~typ:"(set chain_id)" client in
  let* chain1 = Client.RPC.call client @@ RPC.get_chain_chain_id () in
  let chain2 = "NetXZVhNXbDTx5M" in
  let data = sf {|{"%s" ; "%s"}|} chain1 chain2 in
  let* () = assert_typecheck_data_failure ~data ~typ:"(set chain_id)" client in
  let data = sf {|{"%s" ; "%s"}|} chain2 chain1 in
  let* () = Client.typecheck_data client ~data ~typ:"(set chain_id)" in
  unit

let test_comparable_signature client () =
  let* () = Client.typecheck_data ~data:"{}" ~typ:"(set signature)" client in
  let* hash_data_output = Client.hash_data ~data:"Unit" ~typ:"unit" client in
  let packed_data = hash_data_output.packed in
  let* sign1 =
    Client.sign_bytes ~signer:"bootstrap1" ~data:packed_data client
  in
  let* sign2 =
    Client.sign_bytes ~signer:"bootstrap2" ~data:packed_data client
  in
  let data = sf {|{"%s" ; "%s"}|} sign1 sign2 in
  let* () = assert_typecheck_data_failure ~data ~typ:"(set signature)" client in
  let data = sf {|{"%s" ; "%s"}|} sign2 sign1 in
  let* () = Client.typecheck_data ~data ~typ:"(set signature)" client in
  unit

let test_comparable_key client () =
  let* () = Client.typecheck_data ~data:"{}" ~typ:"(set key)" client in
  let pubkey1 = Constant.bootstrap1.public_key in
  let pubkey2 = Constant.bootstrap2.public_key in
  let data = sf {|{"%s" ; "%s"}|} pubkey1 pubkey2 in
  let* () = assert_typecheck_data_failure ~data ~typ:"(set key)" client in
  let data = sf {|{"%s" ; "%s"}|} pubkey2 pubkey1 in
  let* () = Client.typecheck_data ~data ~typ:"(set key)" client in
  unit

let test_comparable_key_different_schemes client () =
  let* sk1 = Client.gen_and_show_keys ~alias:"sk1" ~sig_alg:"ed25519" client in
  let* sk2 =
    Client.gen_and_show_keys ~alias:"sk2" ~sig_alg:"secp256k1" client
  in
  let* sk3 = Client.gen_and_show_keys ~alias:"sk3" ~sig_alg:"p256" client in
  let data =
    sf
      {|{"%s" ; "%s"; "%s"}|}
      sk1.Account.public_key
      sk2.Account.public_key
      sk3.Account.public_key
  in
  let* () = Client.typecheck_data ~data ~typ:"(set key)" client in
  let data =
    sf
      {|{"%s" ; "%s"; "%s"}|}
      sk1.Account.public_key
      sk3.Account.public_key
      sk2.Account.public_key
  in
  let* () = assert_typecheck_data_failure ~data ~typ:"(set key)" client in
  let data =
    sf
      {|{"%s" ; "%s"; "%s"}|}
      sk2.Account.public_key
      sk1.Account.public_key
      sk3.Account.public_key
  in
  let* () = assert_typecheck_data_failure ~data ~typ:"(set key)" client in
  let data =
    sf
      {|{"%s" ; "%s"; "%s"}|}
      sk2.Account.public_key
      sk3.Account.public_key
      sk1.Account.public_key
  in
  let* () = assert_typecheck_data_failure ~data ~typ:"(set key)" client in
  let data =
    sf
      {|{"%s" ; "%s"; "%s"}|}
      sk3.Account.public_key
      sk1.Account.public_key
      sk2.Account.public_key
  in
  let* () = assert_typecheck_data_failure ~data ~typ:"(set key)" client in
  let data =
    sf
      {|{"%s" ; "%s"; "%s"}|}
      sk3.Account.public_key
      sk2.Account.public_key
      sk1.Account.public_key
  in
  let* () = assert_typecheck_data_failure ~data ~typ:"(set key)" client in
  unit

let register ~protocols =
  List.iter
    (fun (title, test_function) ->
      Protocol.register_test
        ~__FILE__
        ~title
        ~tags:["client"; "michelson"; "typechecking"; "comparable"]
        ~uses_node:false
        (fun protocol ->
          let* client = Client.init_mockup ~protocol () in
          test_function client ())
        protocols)
    [
      ("Run `comparable_unit`", test_comparable_unit);
      ("Run `comparable_options`", test_comparable_options);
      ("Run `comparable_or`", test_comparable_or);
      ("Run `comparable_pair`", test_comparable_pair);
      ("Run `order_of_pairs`", test_order_of_pairs);
      ("Run `comparable_chain_id`", test_comparable_chain_id);
      ("Run `comparable_signature`", test_comparable_signature);
      ("Run `comparable_key`", test_comparable_key);
      ( "Run `comparable_key_different_schemes`",
        test_comparable_key_different_schemes );
    ]
