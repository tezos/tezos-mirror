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
  view
    "loop" int unit
    {CAR; DUP; EQ;
     IF{DROP; UNIT}
       {SELF_ADDRESS; SWAP; PUSH int -1; ADD; VIEW "loop" unit; ASSERT_SOME}};
  view "my_external_view"
       int
       int
       { LAMBDA int int { DUP ; MUL } ;
         SWAP ;
         UNPAIR ;
         DUP 3 ;
         SWAP ;
         EXEC ;
         SWAP ;
         DIG 2 ;
         SWAP ;
         INT ;
         EXEC ;
         ADD ;
         PUSH int 1000000 ;
         NEG ;
         ADD } ;
  view "v_external"
       address
       int
       { UNPAIR ;
         PUSH int 33 ;
         VIEW "my_external_view" int ;
         IF_NONE
           { DROP ; PUSH string "Call to 'my_external_view' returned None" ; FAILWITH }
           { ADD } } ;
  view "v_entrypoint"
       int
       int
       { LAMBDA int int { DUP ; MUL } ;
         SWAP ;
         UNPAIR ;
         DUP 3 ;
         SWAP ;
         EXEC ;
         SWAP ;
         DIG 2 ;
         SWAP ;
         INT ;
         EXEC ;
         ADD } ;
}
|}

(* Initializes the client and a viewable contract with a storage of 10 *)
let init_with_contract ?(alias = "viewable_script") ?(prg = viewable_script)
    ~protocol () =
  let* client = Client.init_mockup ~protocol () in
  let* contract =
    Client.originate_contract
      ~alias
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg
      ~init:"10"
      ~burn_cap:(Tez.of_int 1)
      client
  in
  Lwt.return (client, contract)

let test_run_view_generic ?unlimited_gas ~protocol ~view ~input ~expected () =
  let* (client, contract) = init_with_contract ~protocol () in
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

let test_run_view_v_entrypoint ~protocol () =
  test_run_view_generic
    ~protocol
    ~view:"v_entrypoint"
    ~input:(Some "10")
    ~expected:"200"
    ()

let test_run_view_my_external_view ~protocol () =
  test_run_view_generic
    ~protocol
    ~view:"my_external_view"
    ~input:(Some "10")
    ~expected:"-999800"
    ()

let check_storage_is contract client expected =
  let* s = Client.contract_storage contract client in
  let s = String.trim s in
  Log.info "Contract %s storage: got %s, expected %s@." contract s expected ;
  if String.equal s expected then unit
  else Test.fail ~__LOC__ "Unexpected storage result for %s" contract unit

(** Test running views of a smart contract that can call other external views.

     The external call is done by providing the address of an external smart
     contract that implements the desired interface. It could be 'SELF' or
     another deployed contract, as tested below. *)
let test_run_external_nested_view ~protocol () =
  let* (client, contract) =
    init_with_contract ~prg:viewable_script ~alias:"contract1" ~protocol ()
  in
  let* contract' =
    Client.originate_contract
      ~alias:"contract2"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg:viewable_script
      ~init:"10"
      ~burn_cap:(Tez.of_int 1)
      client
  in
  let view = "v_external" in
  let expected = "-998801" in
  let* () =
    Lwt_list.iter_s
      (fun external_contract ->
        let input = Format.sprintf "%S" external_contract in
        let* view_res =
          Client.run_view ?unlimited_gas:None ~view ~contract ~input client
        in
        Log.info "Call view %s: got %s, expected %s@." view view_res expected ;
        if String.equal (String.trim view_res) expected then unit
        else Test.fail ~__LOC__ "Unexpected view result: %s" view unit)
      [contract; contract']
  in
  let* () = check_storage_is contract client "10" in
  check_storage_is contract' client "10"

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
  let* (client, contract) = init_with_contract ~protocol () in
  let failed_command =
    Client.spawn_run_view ~view:"unknown" ~contract ~input:"10" client
  in
  let msg = rex "The contract ([^ ]+) does not have a view named `unknown`." in
  Process.check_error ~exit_code:1 ~msg failed_command

(* Runs high consumption view `loop` with 961 as input and default gas limit,
   and fails because of gas exhaustion. *)
let test_run_view_loop_default_limit ~protocol () =
  let* (client, contract) = init_with_contract ~protocol () in
  let failed_command =
    Client.spawn_run_view ~view:"loop" ~contract ~input:"961" client
  in
  let msg = rex "Gas limit exceeded during typechecking or execution." in
  Process.check_error ~exit_code:1 ~msg failed_command

(* Runs high consumption view `loop` with 961 as input with unlimited gas
   consumption. *)
let test_run_view_loop_unlimited_gas ~protocol =
  test_run_view_generic
    ~protocol
    ~view:"loop"
    ~input:(Some "961")
    ~expected:"Unit"
    ~unlimited_gas:true

let make_for ~protocol () =
  List.iter
    (fun (title, f) ->
      Test.register ~__FILE__ ~title ~tags:["client"; "michelson"; "view"] f)
    [
      ("Run view `add_v` with 10", test_run_view_add_v_10 ~protocol);
      ("Run view `mul_v` with 10", test_run_view_mul_v_10 ~protocol);
      ("Run view `value` without input", test_run_view_value ~protocol);
      ("Run view `v_entrypoint` with 10", test_run_view_v_entrypoint ~protocol);
      ( "Run view `my_external_view` with 10",
        test_run_view_my_external_view ~protocol );
      ( "Run view calling another contract",
        test_run_external_nested_view ~protocol );
      ("Run view on implicit account", test_run_view_implicit_account ~protocol);
      ( "Run view on non existing contract",
        test_run_view_unknown_contract ~protocol );
      ( "Run on non existing view `unknown`",
        test_run_view_unknown_view ~protocol );
      ( "Run view `loop` with default gas limit",
        test_run_view_loop_default_limit ~protocol );
      ( "Run view `loop` with unlimited gas",
        test_run_view_loop_unlimited_gas ~protocol );
    ]

let register ~protocols =
  List.iter
    (function
      | Protocol.Alpha as protocol -> make_for ~protocol ()
      | Protocol.Jakarta | Protocol.Ithaca -> ())
    protocols
