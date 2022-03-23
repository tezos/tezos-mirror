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
   Component:    Client
   Invocation:   dune exec tezt/tests/main.exe -- --file client_run_view.ml
   Subject:      Check that run view command to tezos-client behaves correctly
 *)

let viewable_script =
  {|
{ parameter nat;
  storage nat;
  code { CAR; NIL operation ; PAIR };
  view "add_v" nat nat { UNPAIR; ADD };
  view "mul_v" nat nat { UNPAIR; MUL };
  view "value" unit nat { CDR };
}
|}

(* Initializes the client and a viewable contract with a storage of 10 *)
let init_with_contract ~protocol =
  let* client = Client.init_mockup ~protocol () in
  let* contract =
    Client.originate_contract
      ~alias:"viewable_script"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg:viewable_script
      ~init:"10"
      ~burn_cap:(Tez.of_int 1)
      client
  in
  Lwt.return (client, contract)

let test_run_view_generic ?unlimited_gas ~protocol ~view ~input ~expected () =
  let* (client, contract) = init_with_contract ~protocol in
  let* view = Client.run_view ?unlimited_gas ~view ~contract ?input client in
  if String.equal (String.trim view) expected then unit
  else Test.fail ~__LOC__ "Unexpected view result: %s" view

(* Runs view `add_v` with 10, should yield `20` *)
let test_run_view_add_v_10 ~protocol () =
  test_run_view_generic
    ~protocol
    ~view:"add_v"
    ~input:(Some "10")
    ~expected:"20"
    ()

(* Runs view `mul_v` with 10, should yield `100` *)
let test_run_view_mul_v_10 ~protocol () =
  test_run_view_generic
    ~protocol
    ~view:"mul_v"
    ~input:(Some "10")
    ~expected:"100"
    ()

(* Runs view `value` without input, should yield `10` *)
let test_run_view_value ~protocol () =
  test_run_view_generic ~protocol ~view:"value" ~input:None ~expected:"10" ()

let test_run_view_fail_generic ~protocol ~view ~contract ~input ~msg () =
  let* client = Client.init_mockup ~protocol () in
  let failed_command = Client.spawn_run_view ~view ~contract ?input client in
  Process.check_error ~exit_code:1 ~msg failed_command

(* Runs view `add_v` on an implict account and fails *)
let test_run_view_implicit_account ~protocol () =
  let msg = rex "A view was called on a contract with no script." in
  test_run_view_fail_generic
    ~protocol
    ~view:"add_v"
    ~contract:Constant.bootstrap1.public_key_hash
    ~input:(Some "10")
    ~msg
    ()

(* Runs view `add_v` on an unknown contract and fails *)
let test_run_view_unknown_contract ~protocol () =
  let msg = rex "A view was called on a contract with no script." in
  test_run_view_fail_generic
    ~protocol
    ~view:"add_v"
    ~contract:"KT1Lc9a9E7vqt6XYtkUbrErDGLQ55HztXV5N"
    ~input:(Some "10")
    ~msg
    ()

(* Runs view `unknown` on the viewable_contract and fails *)
let test_run_view_unknown_view ~protocol () =
  let* (client, contract) = init_with_contract ~protocol in
  let failed_command =
    Client.spawn_run_view ~view:"unknown" ~contract ~input:"10" client
  in
  let msg = rex "The contract ([^ ]+) does not have a view named `unknown`." in
  Process.check_error ~exit_code:1 ~msg failed_command

let make_for ~protocol () =
  List.iter
    (fun (title, f) ->
      Test.register ~__FILE__ ~title ~tags:["client"; "michelson"; "view"] f)
    [
      ("Run view `add_v` with 10", test_run_view_add_v_10 ~protocol);
      ("Run view `mul_v` with 10", test_run_view_mul_v_10 ~protocol);
      ("Run view `value` without input", test_run_view_value ~protocol);
      ("Run view on implicit account", test_run_view_implicit_account ~protocol);
      ( "Run view on non existing contract",
        test_run_view_unknown_contract ~protocol );
      ( "Run on non existing view `unknown`",
        test_run_view_unknown_view ~protocol );
    ]

let register ~protocols =
  List.iter
    (function
      | Protocol.Alpha as protocol -> make_for ~protocol ()
      | Protocol.Jakarta | Protocol.Ithaca -> ())
    protocols
