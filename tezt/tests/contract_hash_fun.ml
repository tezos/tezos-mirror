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
   Component:    Michelson: hash functions
   Invocation:   dune exec tezt/tests/main.exe -- --file test_contract_hash_fun.ml
   Subject:      Test Michelson hashing functions
*)

let random_iterations = 50

let bytes_length = 32

let bytes_to_hex_string bytes =
  Format.asprintf "0x%a" Hex.pp (Hex.of_bytes bytes)

let test_contract_hash_fun hash_fun_name (hash_fun : bytes -> bytes) =
  Protocol.register_test
    ~__FILE__
    ~title:("Test contract hash function: " ^ hash_fun_name)
    ~supports:(Protocol.From_protocol 10)
      (* Instructions SHA3 and KECCAK are both available since protocol 10 *)
    ~tags:["michelson"; "crypto"; "contract"; "hash"; hash_fun_name]
    ~uses_node:false
    (fun protocol ->
      let state = Random.State.make [||] in
      let bytes_random n =
        Bytes.init n (fun _ -> Random.State.int state 256 |> Char.chr)
      in
      let bytes = List.init bytes_length (fun _ -> bytes_random bytes_length) in
      let bytes_to_hash = List.map Bytes.of_string [""; "a"] @ bytes in
      let* client = Client.init_mockup ~protocol () in
      let* () =
        Lwt_list.iter_s
          (fun bytes_to_hash ->
            let hashed = hash_fun bytes_to_hash in
            let input = bytes_to_hex_string bytes_to_hash in
            let* {storage; _} =
              Client.run_script_at
                ~storage:"None"
                ~input
                client
                ["opcodes"; hash_fun_name]
                protocol
            in
            let expected_storage = bytes_to_hex_string hashed in
            Check.(
              (storage = sf "(Some %s)" expected_storage)
                ~__LOC__
                string
                ~error_msg:"Expected results %R, got %L") ;
            unit)
          bytes_to_hash
      in
      unit)

let register ~protocols =
  test_contract_hash_fun
    "keccak"
    Tezos_hacl.Hacl.Hash.Keccak_256.digest
    protocols ;
  test_contract_hash_fun "sha3" Tezos_hacl.Hacl.Hash.SHA3_256.digest protocols
