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
   Component:    FA1.2 client commands
   Invocation:   dune exec tezt/tests/main.exe -- --file client_fa12.ml
   Subject:      Tests the client's FA1.2 commands
*)

open Fa12

let register_fa12_test ~title ?(tags = []) test_body protocols =
  fa12_scripts
  |> List.iter @@ fun fa12_script ->
     let script_tag =
       fa12_script.name |> List.rev |> List.hd
       |> String.map (function '.' -> '_' | c -> c)
     in
     Protocol.register_test
       ~__FILE__
       ~title:
         (sf "test fa1.2, %s [%s]" title (String.concat "/" fa12_script.name))
       ~tags:(["client"; "fa12"; script_tag] @ tags)
       (fun protocol ->
         let* client = Client.init_mockup ~protocol () in
         let admin = Account.Bootstrap.keys.(2) in
         let* fa12_alias, fa12_address =
           originate_fa12
             ~src:Account.Bootstrap.keys.(0).public_key_hash
             ~admin
             ~fa12_script
             client
             protocol
         in
         let initial_mint = Tez.of_mutez_int 20000 in
         let* () =
           mint
             ~admin
             ~mint:initial_mint
             ~dest:Account.Bootstrap.keys.(1)
             ~fa12_address
             ~fa12_script
             client
         in
         (* originate viewer *)
         let* _viewer_alias, viewer_address =
           Client.originate_contract_at
             ~amount:Tez.zero
             ~src:Account.Bootstrap.keys.(0).public_key_hash
             ~init:"0"
             ~burn_cap:(Tez.of_int 1)
             client
             ["mini_scenarios"; "nat_id"]
             protocol
         in
         test_body protocol client ~fa12_address ~fa12_alias ~viewer_address)
       protocols

let test_check_contract =
  register_fa12_test ~title:"check contract implements FA1.2 interface"
  @@ fun _protocol client ~fa12_address ~fa12_alias:_ ~viewer_address:_ ->
  Client.check_contract_implements_fa1_2 ~contract:fa12_address client

let test_check_contract_fail =
  Protocol.register_test
    ~__FILE__
    ~title:(sf "test fa1.2, check contract fail")
    ~tags:["client"; "fa12"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let identity = Account.Bootstrap.keys.(2).public_key_hash in
  let init = sf {|"%s"|} identity in
  let* _alias, not_fa12_address =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init
      ~burn_cap:(Tez.of_int 2)
      client
      ["entrypoints"; "manager"]
      protocol
  in
  Client.spawn_check_contract_implements_fa1_2 ~contract:not_fa12_address client
  |> Process.check_error ~msg:(rex "Not a supported FA1\\.2 contract")

let test_get_balance_offchain =
  register_fa12_test ~title:"get balance offchain"
  @@ fun _protocol client ~fa12_address ~fa12_alias:_ ~viewer_address:_ ->
  let* balance =
    Client.from_fa1_2_contract_get_balance
      ~contract:fa12_address
      ~from:Constant.bootstrap2.alias
      client
  in
  Check.(
    (balance = 20000)
      int
      ~error_msg:"Expected balance of bootstrap2 to be %R, got %L") ;
  unit

let test_get_allowance_offchain =
  register_fa12_test ~title:"get allowance offchain"
  @@ fun _protocol client ~fa12_address ~fa12_alias:_ ~viewer_address:_ ->
  let* allowance =
    Client.from_fa1_2_contract_get_allowance
      ~contract:fa12_address
      ~owner:Constant.bootstrap2.alias
      ~operator:Constant.bootstrap3.alias
      client
  in
  Check.(
    (allowance = 0)
      int
      ~error_msg:"Expected allowance of bootstrap2 to be %L, got %R") ;
  unit

let test_get_total_supply_offchain =
  register_fa12_test ~title:"get total_supply offchain"
  @@ fun _protocol client ~fa12_address ~fa12_alias:_ ~viewer_address:_ ->
  let* total_supply =
    Client.from_fa1_2_contract_get_total_supply ~contract:fa12_address client
  in
  Check.(
    (total_supply = 20000)
      int
      ~error_msg:"Expected total supply of bootstrap2 to be %L, got %R") ;
  unit

let read_viewer client viewer_address =
  let* storage = Client.contract_storage viewer_address client in
  return (int_of_string (String.trim storage))

let check_viewer ~__LOC__ ~error_msg client viewer_address expected =
  let* value = read_viewer client viewer_address in
  Check.((expected = value) int ~__LOC__ ~error_msg) ;
  unit

let test_get_balance_callback =
  register_fa12_test ~title:"get balance callback"
  @@ fun _protocol client ~fa12_address ~fa12_alias:_ ~viewer_address ->
  let* () =
    Client.from_fa1_2_contract_get_balance_callback
      ~contract:fa12_address
      ~from:Constant.bootstrap2.alias
      ~callback:viewer_address
      ~burn_cap:Tez.one
      client
  in
  check_viewer
    ~__LOC__
    client
    viewer_address
    20000
    ~error_msg:"Expected balance of bootstrap2 to be %R, got %L"

let test_get_allowance_callback =
  register_fa12_test ~title:"get allowance callback"
  @@ fun _protocol client ~fa12_address ~fa12_alias:_ ~viewer_address ->
  let* () =
    Client.from_fa1_2_contract_get_allowance_callback
      ~contract:fa12_address
      ~from:Constant.bootstrap2.alias
      ~to_:Constant.bootstrap3.alias
      ~callback:viewer_address
      ~burn_cap:Tez.one
      client
  in
  check_viewer
    ~__LOC__
    client
    viewer_address
    0
    ~error_msg:"Expected allownace of bootstrap2 to be %R, got %L"

let test_get_total_supply_callback =
  register_fa12_test ~title:"get total_supply callback"
  @@ fun _protocol client ~fa12_address ~fa12_alias:_ ~viewer_address ->
  let* () =
    Client.from_fa1_2_contract_get_total_supply_callback
      ~contract:fa12_address
      ~from:Constant.bootstrap2.alias
      ~callback:viewer_address
      ~burn_cap:Tez.one
      client
  in
  check_viewer
    ~__LOC__
    client
    viewer_address
    20000
    ~error_msg:"Expected total supply of bootstrap2 to be %L, got %R"

let test_incremental =
  register_fa12_test ~title:"test incremental"
  @@ fun _protocol client ~fa12_address ~fa12_alias ~viewer_address:_ ->
  let burn_cap = Tez.one in
  let contract = fa12_address in
  let check_expected_balance ~__LOC__ from expected =
    let* balance =
      Client.from_fa1_2_contract_get_balance ~contract ~from client
    in
    Check.(
      (balance = expected)
        int
        ~__LOC__
        ~error_msg:("Expected balance of " ^ from ^ " to be %R, got %L")) ;
    unit
  in
  let check_expected_allowance ~__LOC__ ~owner ~operator expected =
    let* allowance =
      Client.from_fa1_2_contract_get_allowance ~contract ~owner ~operator client
    in
    Check.(
      (allowance = expected)
        int
        ~__LOC__
        ~error_msg:
          ("Expected allowance of " ^ owner ^ " to " ^ operator
         ^ " to be %R, got %L")) ;
    unit
  in
  Log.info "Test transfer" ;
  let* () =
    Client.from_fa1_2_contract_transfer
      ~burn_cap
      ~contract
      ~amount:100
      ~from:Constant.bootstrap2.alias
      ~to_:Constant.bootstrap3.alias
      client
  in
  let* () = check_expected_balance ~__LOC__ Constant.bootstrap2.alias 19900 in
  Log.info "Test transfer not enough balance" ;
  let* () =
    let msg =
      match fa12_alias with
      | "fa12_reference" -> Some (rex "Not enough balance")
      | _ -> None
    in
    Client.spawn_from_fa1_2_contract_transfer
      ~burn_cap
      ~contract
      ~amount:200
      ~from:Constant.bootstrap3.alias
      ~to_:Constant.bootstrap2.alias
      client
    |> Process.check_error ?msg
  in
  Log.info "Test approve" ;
  let* () =
    Client.from_fa1_2_contract_approve
      ~burn_cap
      ~contract
      ~as_:Constant.bootstrap2.alias
      ~from:Constant.bootstrap3.alias
      ~amount:20
      client
  in
  let* () =
    check_expected_allowance
      ~__LOC__
      ~owner:Constant.bootstrap2.alias
      ~operator:Constant.bootstrap3.alias
      20
  in
  Log.info "Test unsafe allowance change" ;
  let* () =
    let msg =
      match fa12_alias with
      | "fa12_reference" -> Some (rex "Unsafe allowance change")
      | _ -> None
    in
    Client.spawn_from_fa1_2_contract_approve
      ~burn_cap
      ~contract
      ~as_:Constant.bootstrap2.alias
      ~from:Constant.bootstrap3.alias
      ~amount:30
      client
    |> Process.check_error ?msg
  in
  Log.info "Test transfer as" ;
  let* () =
    Client.from_fa1_2_contract_transfer
      ~burn_cap
      ~contract
      ~amount:10
      ~from:Constant.bootstrap2.alias
      ~to_:Constant.bootstrap3.alias
      ~as_:Constant.bootstrap3.alias
      client
  in
  let* () = check_expected_balance ~__LOC__ Constant.bootstrap2.alias 19890 in
  let* () = check_expected_balance ~__LOC__ Constant.bootstrap3.alias 110 in
  let* () =
    check_expected_allowance
      ~__LOC__
      ~owner:Constant.bootstrap2.alias
      ~operator:Constant.bootstrap3.alias
      10
  in
  Log.info "Test transfer as not enough allowance" ;
  let* () =
    let msg =
      match fa12_alias with
      | "fa12_reference" -> Some (rex "Not enough allowance")
      | _ -> None
    in
    Client.spawn_from_fa1_2_contract_transfer
      ~burn_cap
      ~contract
      ~amount:20
      ~from:Constant.bootstrap2.alias
      ~to_:Constant.bootstrap3.alias
      ~as_:Constant.bootstrap3.alias
      client
    |> Process.check_error ?msg
  in
  Log.info "Test multiple transfers" ;
  let mk_batch_transfer ~dst ~contract amount : Ezjsonm.value =
    `O
      [
        ("destination", `String dst);
        ("amount", `String (string_of_int amount));
        ("token_contract", `String contract);
      ]
  in
  let dump_batch ops =
    let json = `A ops in
    JSON.encode_u json
  in
  let transfers_json =
    dump_batch
      [
        mk_batch_transfer ~dst:Constant.bootstrap4.alias ~contract 100;
        mk_batch_transfer ~dst:Constant.bootstrap5.alias ~contract 10;
      ]
  in
  let* () =
    Client.multiple_fa1_2_transfers
      ~burn_cap
      ~src:Constant.bootstrap2.alias
      ~transfers_json
      client
  in
  let* () = check_expected_balance ~__LOC__ Constant.bootstrap2.alias 19780 in
  let* () = check_expected_balance ~__LOC__ Constant.bootstrap4.alias 100 in
  let* () = check_expected_balance ~__LOC__ Constant.bootstrap5.alias 10 in
  let* () =
    check_expected_allowance
      ~__LOC__
      ~owner:Constant.bootstrap2.alias
      ~operator:Constant.bootstrap3.alias
      10
  in
  Log.info "Test multiple transfers fail" ;
  let transfers_json =
    dump_batch
      [
        mk_batch_transfer ~dst:Constant.bootstrap4.alias ~contract 100;
        mk_batch_transfer ~dst:Constant.bootstrap5.alias ~contract 100000;
      ]
  in
  let* bal_sender_before =
    Client.from_fa1_2_contract_get_balance
      ~contract
      ~from:Constant.bootstrap2.alias
      client
  in
  let* bal_receiver1_before =
    Client.from_fa1_2_contract_get_balance
      ~contract
      ~from:Constant.bootstrap4.alias
      client
  in
  let* bal_receiver2_before =
    Client.from_fa1_2_contract_get_balance
      ~contract
      ~from:Constant.bootstrap5.alias
      client
  in
  let* () =
    Client.spawn_multiple_fa1_2_transfers
      ~burn_cap
      ~src:Constant.bootstrap2.alias
      ~transfers_json
      client
    |> Process.check_error ~msg:(rex "multiple transfers simulation failed")
  in
  let* () =
    check_expected_balance ~__LOC__ Constant.bootstrap2.alias bal_sender_before
  in
  let* () =
    check_expected_balance
      ~__LOC__
      Constant.bootstrap4.alias
      bal_receiver1_before
  in
  let* () =
    check_expected_balance
      ~__LOC__
      Constant.bootstrap5.alias
      bal_receiver2_before
  in
  Log.info "Test multiple transfers as" ;
  (* Reset allowance *)
  let* () =
    Client.from_fa1_2_contract_approve
      ~burn_cap
      ~contract
      ~as_:Constant.bootstrap2.alias
      ~from:Constant.bootstrap3.alias
      ~amount:0
      client
  in
  let* () =
    Client.from_fa1_2_contract_approve
      ~burn_cap
      ~contract
      ~as_:Constant.bootstrap2.alias
      ~from:Constant.bootstrap3.alias
      ~amount:30
      client
  in

  let* () =
    check_expected_allowance
      ~__LOC__
      ~owner:Constant.bootstrap2.alias
      ~operator:Constant.bootstrap3.alias
      30
  in
  let transfers_json =
    dump_batch
      [
        mk_batch_transfer ~dst:Constant.bootstrap4.alias ~contract 10;
        mk_batch_transfer ~dst:Constant.bootstrap5.alias ~contract 20;
      ]
  in
  let* () =
    Client.multiple_fa1_2_transfers
      ~burn_cap
      ~src:Constant.bootstrap2.alias
      ~transfers_json
      ~as_:Constant.bootstrap3.alias
      client
  in
  let* () = check_expected_balance ~__LOC__ Constant.bootstrap2.alias 19750 in
  let* () = check_expected_balance ~__LOC__ Constant.bootstrap4.alias 110 in
  let* () = check_expected_balance ~__LOC__ Constant.bootstrap5.alias 30 in
  let* () =
    check_expected_allowance
      ~__LOC__
      ~owner:Constant.bootstrap2.alias
      ~operator:Constant.bootstrap3.alias
      0
  in
  unit

let register ~protocols =
  test_check_contract protocols ;
  test_check_contract_fail protocols ;
  test_get_balance_offchain protocols ;
  test_get_allowance_offchain protocols ;
  test_get_total_supply_offchain protocols ;
  test_get_balance_callback protocols ;
  test_get_allowance_callback protocols ;
  test_get_total_supply_callback protocols ;
  test_incremental protocols
