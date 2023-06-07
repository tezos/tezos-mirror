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
   Component:    Manager contracts
   Invocation:   dune exec tezt/tests/main.exe -- --file script_manager_contracts.ml
   Subject:      Tests origination and calls to manager contracts.
*)

let test_manager_contracts =
  Protocol.register_test ~__FILE__ ~title:"Manager" ~tags:["manager"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let check_delegate ~__LOC__ src expected_delegate =
    let* delegate = Client.get_delegate ~src client in
    Check.(
      (delegate = expected_delegate)
        (option string)
        ~__LOC__
        ~error_msg:"Expected delegate %R, got %L") ;
    unit
  in
  let check_balance ~__LOC__ account expected_balance =
    let* balance = Client.get_balance_for ~account client in
    Check.(
      (balance = expected_balance)
        Tez.typ
        ~__LOC__
        ~error_msg:"Expected %R, got %L") ;
    unit
  in
  let bootstrap1 = Constant.bootstrap1.alias in
  Log.info "Manager origination" ;
  let path =
    Michelson_script.(find ["entrypoints"; "manager"] protocol |> path)
  in
  let pubkey = Constant.bootstrap2.public_key_hash in
  let* _contract =
    Client.originate_contract
      ~burn_cap:Tez.one
      ~alias:"manager"
      ~src:bootstrap1
      ~prg:path
      ~init:("\"" ^ pubkey ^ "\"")
      ~amount:(Tez.of_int 1000)
      client
  in
  let* _contract =
    Client.originate_contract
      ~burn_cap:Tez.one
      ~alias:"manager2"
      ~src:bootstrap1
      ~prg:path
      ~init:("\"" ^ pubkey ^ "\"")
      ~amount:(Tez.of_int 1000)
      client
  in

  Log.info "Delegatable origination" ;
  let path =
    Michelson_script.(
      find ["entrypoints"; "delegatable_target"] protocol |> path)
  in
  let pubkey = Constant.bootstrap2.public_key_hash in
  let* _contract =
    Client.originate_contract
      ~burn_cap:Tez.one
      ~alias:"delegatable_target"
      ~prg:path
      ~src:bootstrap1
      ~init:(sf {|Pair %S (Pair "hello" 45)|} pubkey)
      ~amount:(Tez.of_int 1000)
      client
  in
  Log.info "Target with entrypoints origination" ;
  let path =
    Michelson_script.(
      find ["entrypoints"; "big_map_entrypoints"] protocol |> path)
  in
  let* _contract =
    Client.originate_contract
      ~burn_cap:Tez.one
      ~alias:"target"
      ~prg:path
      ~src:bootstrap1
      ~init:"Pair {} {}"
      ~amount:(Tez.of_int 1000)
      client
  in
  Log.info "Target without entrypoints origination" ;
  let path =
    Michelson_script.(
      find ["entrypoints"; "no_entrypoint_target"] protocol |> path)
  in
  let* _contract =
    Client.originate_contract
      ~burn_cap:Tez.one
      ~alias:"target_no_entrypoints"
      ~src:bootstrap1
      ~prg:path
      ~init:{|Pair "hello" 42|}
      ~amount:(Tez.of_int 1000)
      client
  in
  Log.info "Target without default origination" ;
  let path =
    Michelson_script.(
      find ["entrypoints"; "no_default_target"] protocol |> path)
  in
  let* _contract =
    Client.originate_contract
      ~burn_cap:Tez.one
      ~alias:"target_no_default"
      ~src:bootstrap1
      ~prg:path
      ~init:{|Pair "hello" 42|}
      ~amount:(Tez.of_int 1000)
      client
  in
  Log.info "Target with root origination" ;
  let path =
    Michelson_script.(find ["entrypoints"; "rooted_target"] protocol |> path)
  in
  let* _contract =
    Client.originate_contract
      ~burn_cap:Tez.one
      ~alias:"rooted_target"
      ~src:bootstrap1
      ~prg:path
      ~init:{|Pair "hello" 42|}
      ~amount:(Tez.of_int 1000)
      client
  in
  Log.info "Manager set delegate" ;
  let*! () = Client.set_delegate ~src:"manager" ~delegate:"bootstrap2" client in
  let bootstrap2_pkh = Constant.bootstrap2.public_key_hash in
  let*! () =
    Client.set_delegate
      ~src:"delegatable_target"
      ~delegate:bootstrap2_pkh
      client
  in
  let delegate = Constant.bootstrap2.public_key_hash in
  let* () = check_delegate ~__LOC__ "manager" (Some delegate) in
  let* () = check_delegate ~__LOC__ "delegatable_target" (Some delegate) in
  let*! () = Client.set_delegate ~src:"manager" ~delegate:"bootstrap3" client in
  let*! () =
    Client.set_delegate ~src:"delegatable_target" ~delegate:"bootstrap3" client
  in
  let delegate = Constant.bootstrap3.public_key_hash in
  let* () = check_delegate ~__LOC__ "manager" (Some delegate) in
  let* () = check_delegate ~__LOC__ "delegatable_target" (Some delegate) in
  Log.info "Manager withdraw delegate" ;
  let* () = Client.withdraw_delegate ~src:"manager" client in
  let* () = Client.withdraw_delegate ~src:"delegatable_target" client in
  let* () = check_delegate ~__LOC__ "manager" None in
  let* () = check_delegate ~__LOC__ "delegatable_target" None in
  Log.info "Transfer to manager" ;
  let* balance = Client.get_balance_for ~account:"manager" client in
  let* balance_bootstrap =
    Client.get_balance_for ~account:"bootstrap2" client
  in
  let amount = Tez.of_mutez_int 10_001_000 in
  let fee = Tez.of_mutez_int 0_000_475 in
  let* () =
    Client.transfer
      ~amount
      ~giver:"bootstrap2"
      ~receiver:"manager"
      ~gas_limit:((128 * 15450) + 108)
      ~fee
      client
  in
  let* () = check_balance ~__LOC__ "manager" Tez.(balance + amount) in
  let* () =
    check_balance ~__LOC__ "bootstrap2" Tez.(balance_bootstrap - fee - amount)
  in
  Log.info "Simple transfer from manager to implicit" ;
  let* balance = Client.get_balance_for ~account:"manager" client in
  let* balance_bootstrap =
    Client.get_balance_for ~account:"bootstrap2" client
  in
  let amount = Tez.of_mutez_int 10_100_000 in
  let fee = Tez.of_mutez_int 0_000_636 in
  let* () =
    Client.transfer
      ~amount
      ~giver:"manager"
      ~receiver:"bootstrap2"
      ~gas_limit:((128 * 26350) + 12)
      ~fee
      client
  in
  let* () = check_balance ~__LOC__ "manager" Tez.(balance - amount) in
  let* () =
    check_balance ~__LOC__ "bootstrap2" Tez.(balance_bootstrap + amount - fee)
  in
  Log.info "Transfer from manager to manager" ;
  let* balance = Client.get_balance_for ~account:"manager" client in
  let* balance_dest = Client.get_balance_for ~account:"manager2" client in
  let* balance_bootstrap =
    Client.get_balance_for ~account:"bootstrap2" client
  in
  let amount = Tez.of_int 10 in
  let fee = Tez.of_mutez_int 0_000_824 in
  let* () =
    Client.transfer
      ~amount
      ~giver:"manager"
      ~receiver:"manager2"
      ~gas_limit:((128 * 44950) + 112)
      ~fee
      client
  in
  let* () = check_balance ~__LOC__ "manager" Tez.(balance - amount) in
  let* () = check_balance ~__LOC__ "manager2" Tez.(balance_dest + amount) in
  let* () = check_balance ~__LOC__ "bootstrap2" Tez.(balance_bootstrap - fee) in
  Log.info "Transfer from manager to default" ;
  let amount = Tez.of_int 10 in
  let* () =
    Client.transfer
      ~amount
      ~giver:"manager"
      ~receiver:"bootstrap2"
      ~entrypoint:"default"
      client
  in
  let* () =
    Client.transfer
      ~amount
      ~giver:"manager"
      ~receiver:"manager"
      ~entrypoint:"default"
      client
  in
  Log.info "Transfer from manager to target" ;
  let* () =
    Client.transfer
      ~amount
      ~giver:"manager"
      ~receiver:"target"
      ~burn_cap:(Tez.of_mutez_int 0_356_000)
      client
  in
  Log.info "Transfer from manager to entrypoint with args" ;
  let arg = {|Pair "hello" 42|} in
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~giver:"manager"
      ~receiver:"target"
      ~entrypoint:"add_left"
      ~arg
      ~burn_cap:(Tez.of_mutez_int 0_067_000)
      client
  in
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~giver:"manager"
      ~receiver:"target"
      ~entrypoint:"mem_left"
      ~arg:{|"hello"|}
      client
  in
  let* () =
    Client.call
      ~source:"manager"
      ~destination:"target"
      ~entrypoint:"add_left"
      ~arg
      ~burn_cap:(Tez.of_int 0_067_000)
      client
  in
  let* () =
    Client.call
      ~source:"manager"
      ~destination:"target"
      ~entrypoint:"mem_left"
      ~arg:{|"hello"|}
      ~burn_cap:(Tez.of_int 0_067_000)
      client
  in
  Log.info "Transfer from manager no entrypoint with args" ;
  let arg = "Left Unit" in
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~giver:"manager"
      ~receiver:"target_no_entrypoints"
      ~arg
      client
  in
  let* () =
    Client.call
      ~source:"manager"
      ~destination:"target_no_entrypoints"
      ~arg
      client
  in
  Log.info "Transfer from manager to no default with args" ;
  let arg = "Left Unit" in
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~giver:"manager"
      ~receiver:"target_no_default"
      ~arg
      client
  in
  let* () =
    Client.call ~source:"manager" ~destination:"target_no_default" ~arg client
  in
  Log.info "Transfer from manager to rooted target with args" ;
  let arg = "Left Unit" in
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~giver:"manager"
      ~receiver:"rooted_target"
      ~arg
      ~entrypoint:"root"
      client
  in
  let* () =
    Client.call
      ~source:"manager"
      ~destination:"rooted_target"
      ~arg
      ~entrypoint:"root"
      client
  in
  unit

let register ~protocols = test_manager_contracts protocols
