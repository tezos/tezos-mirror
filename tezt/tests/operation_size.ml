(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Operation size
   Invocation:   dune exec tezt/tests/main.exe -- --file operation_size.ml
   Subject:      Tests handling oversized contract call arguments
*)

let contract_path protocol kind contract =
  sf
    "tests_python/contracts_%s/%s/%s"
    (match protocol with
    | Protocol.Alpha -> "alpha"
    | _ -> sf "%03d" @@ Protocol.number protocol)
    kind
    contract

(* Test that a large operation under 32KB can be injected in the node
   (variant using a big nat). *)
let test_operation_size_with_nat_ok =
  Protocol.register_test
    ~__FILE__
    ~title:"operation size with nat ok"
    ~tags:["operation"; "script"; "size"]
  @@ fun protocol ->
  (* The encoding for nat uses a byte to encode 7 bits of the number so
     the size of 2 ** (7 * n) is about n bytes *)
  let arg =
    let exp = 7 * 30 * 1024 in
    Z.(pow (of_int 2) exp |> to_string)
  in
  let* _node, client = Client.init_with_protocol ~protocol `Client () in
  let prg = contract_path protocol "opcodes" "munch.tz" in
  let munch = "munch" in
  let* _contract_address =
    Client.originate_contract
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~alias:munch
      ~prg
      client
  in
  let* () = Client.bake_for_and_wait client in
  Client.transfer
    ~log_output:false
    ~amount:(Tez.of_int 10)
    ~giver:"bootstrap1"
    ~receiver:munch
    ~entrypoint:"nat"
    ~arg
    client

(* Test that a large operation over 32KB cannot be injected in the
   node, and the error is not a stack overflow (variant using a big
   nat). *)
let test_operation_size_with_nat_fail =
  Protocol.register_test
    ~__FILE__
    ~title:"operation size with nat fail"
    ~tags:["operation"; "script"; "size"]
  @@ fun protocol ->
  (* The encoding for nat uses a byte to encode 7 bits of the number so
     the size of 2 ** (7 * n) is about n bytes *)
  let arg =
    let exp = 7 * 33 * 1024 in
    Z.(pow (of_int 2) exp |> to_string)
  in
  let* _node, client = Client.init_with_protocol ~protocol `Client () in
  let prg = contract_path protocol "opcodes" "munch.tz" in
  let munch = "munch" in
  let* _contract_address =
    Client.originate_contract
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~alias:munch
      ~prg
      client
  in
  let* () = Client.bake_for_and_wait client in
  Client.spawn_transfer
    ~amount:(Tez.of_int 10)
    ~giver:"bootstrap1"
    ~receiver:munch
    ~entrypoint:"nat"
    ~arg
    client
  |> Process.check_error ~msg:(rex "Oversized operation")

let register ~protocols =
  test_operation_size_with_nat_ok protocols ;
  test_operation_size_with_nat_fail protocols
