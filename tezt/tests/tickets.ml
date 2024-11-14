(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022-2024 Marigold <contact@marigold.dev>                   *)
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
   Invocation:   dune exec tezt/tests/main.exe -- --file tickets.ml
   Subject:      Regression tests for tickets
*)

let hooks = Tezos_regression.hooks

let setup_node protocol ~direct_ticket_spending_enable =
  let base = Either.right (protocol, None) in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base
      [(["direct_ticket_spending_enable"], `Bool direct_ticket_spending_enable)]
  in
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* node, client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ~nodes_args ()
  in
  return (node, client)

type ticket_constructor = Pair | Ticket

let encode_ticket ~constructor ~ticketer ~content ~amount =
  match constructor with
  | Pair -> sf {|Pair %S (Pair %S %d)|} ticketer content amount
  | Ticket -> sf {|Ticket %S string %S %d|} ticketer content amount

let test_create_and_remove_tickets =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Create and remove tickets"
    ~tags:["client"; "michelson"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, contract_id =
    Client.originate_contract_at
      ~amount:(Tez.of_int 200)
      ~src:"bootstrap1"
      ~init:"{}"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "add_clear_tickets"]
      protocol
  in
  let* () =
    (* Add ticket with payload (Pair 1 "A") *)
    Client.transfer
      ~burn_cap:(Tez.of_int 2)
      ~amount:Tez.zero
      ~giver:"bootstrap2"
      ~receiver:contract_id
      ~entrypoint:"add"
      ~arg:{|Pair 1 "A"|}
      ~hooks
      client
  in
  let* () =
    (* Remove tickets by calling clear entrypoint *)
    Client.transfer
      ~burn_cap:(Tez.of_int 2)
      ~amount:Tez.zero
      ~giver:"bootstrap2"
      ~receiver:contract_id
      ~entrypoint:"clear"
      ~arg:"Unit"
      ~hooks
      client
  in
  let* () =
    (* Add ticket with payload (Pair 1 "B") *)
    Client.transfer
      ~burn_cap:(Tez.of_int 2)
      ~amount:Tez.zero
      ~giver:"bootstrap2"
      ~receiver:contract_id
      ~entrypoint:"add"
      ~arg:{|Pair 1 "B"|}
      ~hooks
      client
  in
  let* () =
    (* Add ticket with payload (Pair 1 "C") *)
    Client.transfer
      ~burn_cap:(Tez.of_int 2)
      ~amount:Tez.zero
      ~giver:"bootstrap2"
      ~receiver:contract_id
      ~entrypoint:"add"
      ~arg:{|Pair 1 "C"|}
      ~hooks
      client
  in
  unit

(* This test originates two contracts. One for receiving a big-map with string
   tickets. Another for creating and sending a big-map with string tickets.
   Scanning the big-map for tickets uses the function [Big_map.list_key_values]
   so the regression tests include gas costs associated with calling this
   function. *)
let test_send_tickets_in_big_map =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Send tickets in bigmap"
    ~tags:["client"; "michelson"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _receive_contract_alias, receive_contract_hash =
    Client.originate_contract_at
      ~amount:(Tez.of_int 200)
      ~src:"bootstrap1"
      ~init:"{}"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "receive_tickets_in_big_map"]
      protocol
  in
  let* _alias_contract_hash, send_contract_hash =
    Client.originate_contract_at
      ~amount:(Tez.of_int 200)
      ~src:"bootstrap1"
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "send_tickets_in_big_map"]
      protocol
  in
  let* () =
    Client.transfer
      ~burn_cap:(Tez.of_int 30)
      ~storage_limit:1000000
      ~amount:Tez.zero
      ~giver:"bootstrap2"
      ~receiver:send_contract_hash
      ~arg:(sf "%S" receive_contract_hash)
      ~hooks
      client
  in
  unit

let assert_ticket_balance ?hooks ~contract ~ticketer ~ty ~contents ~expected
    client =
  let* actual =
    Client.ticket_balance
      ?hooks
      ~contract
      ~ticketer
      ~content_type:ty
      ~content:contents
      client
  in
  let expected = Int.to_string expected in
  return
  @@ Check.((String.trim actual = expected) ~__LOC__ string)
       ~error_msg:"expected %R, got %L"

(* This test originates one contract which mints and sends tickets to the address
   passed in the parameter. In this test, the receiver of the ticket is an
   implicit account. *)
let test_send_tickets_to_implicit_account =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Send tickets from contracts to implicit accounts"
    ~tags:["client"; "michelson"]
    ~uses_node:false
    ~supports:(Protocol.From_protocol 16)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_send"]
      protocol
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "Pair %S 1" Constant.bootstrap1.public_key_hash)
      ~hooks
      client
  in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:Constant.bootstrap1.alias
      ~ticketer
      ~ty:"string"
      ~contents:"\"Ticket\""
      ~expected:1
      client
  in
  unit

(* Tests that an implicit account can send a single ticket to originated
   using the [Transfer] manager operation. *)
let test_direct_transfer_tickets_from_implicit_account_to_originated
    ~ticket_constructor protocol =
  let* _node, client =
    setup_node protocol ~direct_ticket_spending_enable:true
  in
  (* Deposit tickets to the implicit account. *)
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "tickets_send"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "Pair %S 1" Constant.bootstrap1.public_key_hash)
      client
  in
  let* () = Client.bake_for_and_wait client in
  (* Assert that the implicit account holds the ticket. *)
  let* () =
    assert_ticket_balance
      ~contract:Constant.bootstrap1.alias
      ~ticketer
      ~ty:"string"
      ~contents:"\"Ticket\""
      ~expected:1
      client
  in
  (* Originate contract that stores tickets sent to it's "save" entrypoint.  *)
  let* _alias, bag =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"{}"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "tickets_bag"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  (* Transfer ticket from implicit to originated using the [Transaction] manager operation. *)
  match ticket_constructor with
  | Pair ->
      let* () =
        Process.check_error
          ~msg:(rex "invalid primitive Pair, only Ticket can be used here")
          (Client.spawn_transfer
             ~burn_cap:Tez.one
             ~amount:Tez.zero
             ~giver:Constant.bootstrap1.alias
             ~receiver:bag
             ~entrypoint:"save"
             ~arg:
               (encode_ticket
                  ~constructor:Pair
                  ~ticketer
                  ~content:"Ticket"
                  ~amount:1)
             ~hooks
             client)
      in
      unit
  | Ticket ->
      let* () =
        Client.transfer
          ~burn_cap:Tez.one
          ~amount:Tez.zero
          ~giver:Constant.bootstrap1.alias
          ~receiver:bag
          ~entrypoint:"save"
          ~arg:
            (encode_ticket
               ~constructor:Ticket
               ~ticketer
               ~content:"Ticket"
               ~amount:1)
          ~hooks
          client
      in
      let* () = Client.bake_for_and_wait client in
      let* () =
        assert_ticket_balance
          ~contract:Constant.bootstrap1.alias
          ~ticketer
          ~ty:"string"
          ~contents:"\"Ticket\""
          ~expected:0
          client
      in
      let* () =
        assert_ticket_balance
          ~contract:bag
          ~ticketer
          ~ty:"string"
          ~contents:"\"Ticket\""
          ~expected:1
          client
      in
      unit

let test_direct_transfer_tickets_from_implicit_account_to_originated_with_pair_constructor
    =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Send Pair tickets from implicit account to originated directly"
    ~tags:["client"; "michelson"; "implicit"; "ticket"; "originated"]
    ~supports:(Protocol.From_protocol 22)
  @@ fun protocol ->
  test_direct_transfer_tickets_from_implicit_account_to_originated
    protocol
    ~ticket_constructor:Pair

let test_direct_transfer_tickets_from_implicit_account_to_originated_with_ticket_constructor
    =
  Protocol.register_regression_test
    ~__FILE__
    ~title:
      "Send tickets with Ticket constructor from implicit account to \
       originated directly"
    ~tags:["client"; "michelson"; "implicit"; "ticket"; "originated"]
    ~supports:(Protocol.From_protocol 19)
  @@ fun protocol ->
  test_direct_transfer_tickets_from_implicit_account_to_originated
    protocol
    ~ticket_constructor:Ticket

(* Tests that an implicit account can send a tickets to originated
   using the [Transfer] manager operation. The parameter of the
   transfer is made complex to check that the [Transfer] properly
   scans the parameter and transfers all included tickets. *)
let test_direct_transfer_tickets_from_implicit_account_to_originated_complex
    ~ticket_constructor protocol =
  let* _node, client =
    setup_node protocol ~direct_ticket_spending_enable:true
  in
  (* Deposit tickets to the implicit account. *)
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"{}"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "tickets_mint_and_store_complex_param"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  let first_ticket_content = "Ticket1" in
  let second_ticket_content = "Ticket2" in
  let amount = 3 in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~entrypoint:"mint_and_send"
      ~arg:
        (sf
           "Pair %S (Pair %d %S)"
           first_ticket_content
           amount
           Constant.bootstrap1.public_key_hash)
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~entrypoint:"mint_and_send"
      ~arg:
        (sf
           "Pair %S (Pair %d %S)"
           second_ticket_content
           amount
           Constant.bootstrap1.public_key_hash)
      client
  in
  let* () = Client.bake_for_and_wait client in
  (* Assert that the implicit account holds the ticket. *)
  let* () =
    assert_ticket_balance
      ~contract:Constant.bootstrap1.alias
      ~ticketer
      ~ty:"string"
      ~contents:(sf "%S" first_ticket_content)
      ~expected:amount
      client
  in
  let* () =
    assert_ticket_balance
      ~contract:Constant.bootstrap1.alias
      ~ticketer
      ~ty:"string"
      ~contents:(sf "%S" second_ticket_content)
      ~expected:amount
      client
  in
  let* () = Client.bake_for_and_wait client in
  (* Send tickets from implicit to originated. *)
  let first_ticket_sent = 1 in
  let second_ticket_sent = 2 in
  let complex_arg_with_tickets =
    sf
      {|Pair 99 {Pair "garbage" (%s) ; Pair "garbage" (%s)}|}
      (encode_ticket
         ~constructor:ticket_constructor
         ~ticketer
         ~content:first_ticket_content
         ~amount:first_ticket_sent)
      (encode_ticket
         ~constructor:ticket_constructor
         ~ticketer
         ~content:second_ticket_content
         ~amount:second_ticket_sent)
  in
  match ticket_constructor with
  | Pair ->
      let* () =
        Process.check_error
          ~msg:(rex "invalid primitive Pair, only Ticket can be used here")
          (Client.spawn_transfer
             ~hooks
             ~burn_cap:Tez.one
             ~amount:Tez.zero
             ~giver:Constant.bootstrap1.alias
             ~entrypoint:"store"
             ~receiver:ticketer
             ~arg:complex_arg_with_tickets
             client)
      in
      unit
  | Ticket ->
      let* () =
        Client.transfer
          ~hooks
          ~burn_cap:Tez.one
          ~amount:Tez.zero
          ~giver:Constant.bootstrap1.alias
          ~entrypoint:"store"
          ~receiver:ticketer
          ~arg:complex_arg_with_tickets
          client
      in
      let* () = Client.bake_for_and_wait client in
      (* Check that ticket balance is removed for implicit account. *)
      let* () =
        assert_ticket_balance
          ~contract:Constant.bootstrap1.alias
          ~ticketer
          ~ty:"string"
          ~contents:(sf "%S" first_ticket_content)
          ~expected:(amount - first_ticket_sent)
          client
      in
      let* () =
        assert_ticket_balance
          ~contract:Constant.bootstrap1.alias
          ~ticketer
          ~ty:"string"
          ~contents:(sf "%S" second_ticket_content)
          ~expected:(amount - second_ticket_sent)
          client
      in
      (* Check that ticket balance is added for originated contract. *)
      let* () =
        assert_ticket_balance
          ~contract:ticketer
          ~ticketer
          ~ty:"string"
          ~contents:(sf "%S" first_ticket_content)
          ~expected:first_ticket_sent
          client
      in
      let* () =
        assert_ticket_balance
          ~contract:ticketer
          ~ticketer
          ~ty:"string"
          ~contents:(sf "%S" second_ticket_content)
          ~expected:second_ticket_sent
          client
      in
      unit

let test_direct_transfer_tickets_from_implicit_account_to_originated_complex_with_ticket_constructor
    =
  Protocol.register_regression_test
    ~__FILE__
    ~title:
      "Send tickets with Ticket constructor (with complex parameters) from \
       implicit account to originated directly"
    ~tags:["client"; "michelson"; "implicit"; "ticket"; "originated"]
    ~supports:(Protocol.From_protocol 19)
  @@ fun protocol ->
  test_direct_transfer_tickets_from_implicit_account_to_originated_complex
    protocol
    ~ticket_constructor:Ticket

let test_direct_transfer_tickets_from_implicit_account_to_originated_complex_with_pair_constructor
    =
  Protocol.register_regression_test
    ~__FILE__
    ~title:
      "Send Pair tickets (with complex parameters) from implicit account to \
       originated directly"
    ~tags:["client"; "michelson"; "implicit"; "ticket"; "originated"]
    ~supports:(Protocol.From_protocol 22)
  @@ fun protocol ->
  test_direct_transfer_tickets_from_implicit_account_to_originated_complex
    protocol
    ~ticket_constructor:Pair

(* This test originates one contract which mints and sends tickets to the address
   passed in the parameter. In this test, the receiver of the ticket is an
   implicit account *)
let test_send_tickets_to_implicit_account_non_zero_amount =
  Protocol.register_regression_test
    ~__FILE__
    ~title:
      "Send tickets from contracts to implicit accounts with some Tez along"
    ~tags:["client"; "michelson"]
    ~uses_node:false
    ~supports:(Protocol.From_protocol 16)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.one
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_send_with_tez"]
      protocol
  in
  let* tez_before = Client.get_balance_for ~account:ticketer client in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "%S" Constant.bootstrap1.public_key_hash)
      ~hooks
      client
  in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:Constant.bootstrap1.alias
      ~ticketer
      ~ty:"string"
      ~contents:"\"Ticket\""
      ~expected:1
      client
  in
  let* tez_after = Client.get_balance_for ~account:ticketer client in
  let balance_drop = Tez.(to_mutez @@ (tez_before - tez_after)) in
  Check.((balance_drop = 1) int ~__LOC__) ~error_msg:"expected %R, got %L" ;
  unit

(* A contract handle accepting a list of ticket should reject implicit account destination *)
let test_send_tickets_to_implicit_with_wrong_type =
  Protocol.register_regression_test
    ~__FILE__
    ~title:
      "Send tickets from contracts to implicit accounts with the wrong type \
       must fail"
    ~tags:["client"; "michelson"]
    ~uses_node:false
    ~supports:(Protocol.From_protocol 16)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.one
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "send_ticket_list"]
      protocol
  in
  let* _alias, blackhole =
    Client.originate_contract_at
      ~amount:Tez.one
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_list_blackhole"]
      protocol
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "%S" blackhole)
      ~hooks
      client
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "%S" Constant.bootstrap1.public_key_hash)
      ~hooks
      ~expect_failure:true
      client
  in
  unit

let test_ticket_transfer_commutative =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Send tickets between originated contracts and implicit accounts"
    ~tags:["client"; "michelson"]
    ~uses_node:false
    ~supports:(Protocol.From_protocol 16)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_send"]
      protocol
  in
  let* _alias, bag =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"{}"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_bag"]
      protocol
  in
  let* _alias, blackhole =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_blackhole"]
      protocol
  in
  (* 1. ticket minter contract -> bootstrap1 (originated -> implicit) *)
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "Pair %S 3" Constant.bootstrap1.public_key_hash)
      ~hooks
      client
  in
  let ty = "string" in
  let contents = {|"Ticket"|} in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:Constant.bootstrap1.alias
      ~ticketer
      ~ty
      ~contents
      ~expected:3
      client
  in
  (* 2. bootstrap1 -> bootstrap2 (implicit -> implicit) *)
  let*! () =
    Client.transfer_tickets
      ~burn_cap:Tez.one
      ~qty:2L
      ~src:Constant.bootstrap1.alias
      ~destination:Constant.bootstrap2.alias
      ~entrypoint:"default"
      ~contents
      ~ty
      ~ticketer
      ~hooks
      client
  in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:Constant.bootstrap1.alias
      ~ticketer
      ~ty
      ~contents
      ~expected:1
      client
  in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:Constant.bootstrap2.alias
      ~ticketer
      ~ty
      ~contents
      ~expected:2
      client
  in
  (* 3. bootstrap2 -> bag (implicit -> originated) *)
  let*! () =
    Client.transfer_tickets
      ~burn_cap:Tez.one
      ~qty:1L
      ~src:Constant.bootstrap2.alias
      ~destination:bag
      ~entrypoint:"save"
      ~contents
      ~ty
      ~ticketer
      ~hooks
      client
  in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:Constant.bootstrap2.alias
      ~ticketer
      ~ty
      ~contents
      ~expected:1
      client
  in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:bag
      ~ticketer
      ~ty
      ~contents
      ~expected:1
      client
  in
  (* 4. bag -> blackhole (originated -> originated) *)
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:bag
      ~entrypoint:"send"
      ~arg:(sf "%S" blackhole)
      ~hooks
      client
  in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:bag
      ~ticketer
      ~ty
      ~contents
      ~expected:0
      client
  in
  assert_ticket_balance
    ~hooks
    ~contract:blackhole
    ~ticketer
    ~ty
    ~contents
    ~expected:0
    client

let test_ticket_transfer_from_storage_to_implicit =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Sending ticket from contract storage to implicit accounts"
    ~tags:["client"; "michelson"]
    ~uses_node:false
    ~supports:(Protocol.From_protocol 16)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_send"]
      protocol
  in
  let* _alias, bag =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"{}"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_bag_implicit"]
      protocol
  in
  (* ticketer -> bootstrap1 *)
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "Pair %S 1" Constant.bootstrap1.public_key_hash)
      ~hooks
      client
  in
  let ty = "string" in
  let contents = {|"Ticket"|} in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:Constant.bootstrap1.alias
      ~ticketer
      ~ty
      ~contents
      ~expected:1
      client
  in
  (* bootstrap1 -> bag *)
  let*! () =
    Client.transfer_tickets
      ~burn_cap:Tez.one
      ~qty:1L
      ~src:Constant.bootstrap1.alias
      ~destination:bag
      ~entrypoint:"save"
      ~contents
      ~ty
      ~ticketer
      ~hooks
      client
  in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:Constant.bootstrap1.alias
      ~ticketer
      ~ty
      ~contents
      ~expected:0
      client
  in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:bag
      ~ticketer
      ~ty
      ~contents
      ~expected:1
      client
  in
  (* bag -> bootstrap2 *)
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:bag
      ~entrypoint:"send"
      ~arg:(sf "%S" Constant.bootstrap2.public_key_hash)
      ~hooks
      client
  in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:Constant.bootstrap2.alias
      ~ticketer
      ~ty
      ~contents
      ~expected:1
      client
  in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:bag
      ~ticketer
      ~ty
      ~contents
      ~expected:0
      client
  in
  unit

let test_zero_ticket_rejection =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Sending zero ticket from implicit accounts must be rejected"
    ~tags:["client"; "michelson"]
    ~uses_node:false
    ~supports:(Protocol.From_protocol 16)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_send"]
      protocol
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "Pair %S 1" Constant.bootstrap1.public_key_hash)
      ~hooks
      client
  in
  let ty = "string" and contents = {|"Ticket"|} in
  let* () =
    assert_ticket_balance
      ~hooks
      ~contract:Constant.bootstrap1.alias
      ~ticketer
      ~ty
      ~contents
      ~expected:1
      client
  in
  let*! () =
    Client.transfer_tickets
      ~burn_cap:Tez.one
      ~qty:0L
      ~src:Constant.bootstrap1.alias
      ~destination:Constant.bootstrap2.alias
      ~entrypoint:"default"
      ~contents
      ~ty
      ~ticketer
      ~expect_failure:true
      client
  in
  let* _alias, bag =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"{}"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_bag"]
      protocol
  in
  let*! () =
    Client.transfer_tickets
      ~burn_cap:Tez.one
      ~qty:0L
      ~src:Constant.bootstrap1.alias
      ~destination:bag
      ~entrypoint:"save"
      ~contents
      ~ty
      ~ticketer
      ~expect_failure:true
      ~hooks
      client
  in
  unit

let test_ticket_overdraft_rejection =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Overdrafting ticket from implicit accounts must be rejected"
    ~tags:["client"; "michelson"]
    ~uses_node:false
    ~supports:(Protocol.From_protocol 16)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_send"]
      protocol
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "Pair %S 1" Constant.bootstrap1.public_key_hash)
      ~hooks
      client
  in
  let ty = "string" and contents = {|"Ticket"|} in
  let*! () =
    Client.transfer_tickets
      ~burn_cap:Tez.one
      ~qty:2L
      ~src:Constant.bootstrap1.alias
      ~destination:Constant.bootstrap2.alias
      ~entrypoint:"default"
      ~contents
      ~ty
      ~ticketer
      ~expect_failure:true
      ~hooks
      client
  in
  let* _alias, bag =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"{}"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_bag"]
      protocol
  in
  let*! () =
    Client.transfer_tickets
      ~burn_cap:Tez.one
      ~qty:2L
      ~src:Constant.bootstrap1.alias
      ~destination:bag
      ~entrypoint:"save"
      ~contents
      ~ty
      ~ticketer
      ~expect_failure:true
      ~hooks
      client
  in
  unit

let test_ticket_of_wrong_type_rejection =
  Protocol.register_regression_test
    ~__FILE__
    ~title:
      "Sending ticket of wrong type from implicit accounts must be rejected"
    ~tags:["client"; "michelson"]
    ~uses_node:false
    ~supports:(Protocol.From_protocol 16)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_send"]
      protocol
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "Pair %S 1" Constant.bootstrap1.public_key_hash)
      ~hooks
      client
  in
  let*! () =
    Client.transfer_tickets
      ~burn_cap:Tez.one
      ~qty:2L
      ~src:Constant.bootstrap1.alias
      ~destination:Constant.bootstrap2.alias
      ~entrypoint:"default"
      ~contents:"0"
      ~ty:"nat"
      ~ticketer
      ~expect_failure:true
      ~hooks
      client
  in
  unit

let test_originated_implicit_can_be_equipotent =
  Protocol.register_regression_test
    ~__FILE__
    ~title:
      "Sending tickets to either implicit accounts or originated contracts \
       accepting tickets with default entrypoint should equally work"
    ~tags:["client"; "michelson"]
    ~uses_node:false
    ~supports:(Protocol.From_protocol 16)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_send"]
      protocol
  in
  let* _alias, blackhole =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "tickets_blackhole"]
      protocol
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "Pair %S 1" Constant.bootstrap1.public_key_hash)
      ~hooks
      client
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "Pair %S 1" blackhole)
      ~hooks
      client
  in
  unit

let setup_sc_enabled_node protocol ~parameters_ty =
  let base = Either.right (protocol, None) in
  let* parameter_file = Protocol.write_parameter_file ~base [] in
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* tezos_node, tezos_client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ~nodes_args ()
  in
  let* sc_rollup =
    Client.Sc_rollup.(
      originate
        ~burn_cap:(Tez.of_int 2)
        ~alias:"rollup"
        ~src:Constant.bootstrap1.alias
        ~kind:"wasm_2_0_0"
        ~parameters_ty
        ~boot_sector:Constant.wasm_echo_kernel_boot_sector
        tezos_client)
  in
  let* () = Client.bake_for_and_wait tezos_client in
  return (tezos_node, tezos_client, sc_rollup)

let assert_sc_rollup_ticket_balance client ~sc_rollup ~ticketer ~content_type
    ~content ~expected =
  let data : RPC_core.data =
    Data
      (Ezjsonm.value_from_string
      @@ sf
           {|{"ticketer": "%s", "content_type": {"prim": "%s"}, "content": {"string": "%s"}}|}
           ticketer
           content_type
           content)
  in
  let* actual =
    Client.RPC.call client
    @@ RPC.post_chain_block_context_smart_rollups_smart_rollup_ticket_balance
         ~sc_rollup
         ~data
         ()
  in
  return
  @@ Check.(
       (actual = expected)
         int
         ~__LOC__
         ~error_msg:"expected ticket amount %R, got %L")

let test_send_tickets_to_sc_rollup =
  Protocol.register_regression_test
    ~__FILE__
    ~title:
      "Minting then sending tickets to smart-contract rollup should succeed \
       with appropriate ticket updates field in receipt."
    ~tags:["client"; "michelson"; "sc_rollup"]
    ~supports:(Protocol.From_protocol 16)
  @@ fun protocol ->
  let* _node, client, sc_rollup =
    setup_sc_enabled_node protocol ~parameters_ty:"list (ticket string)"
  in
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "send_ticket_list_multiple"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "%S" sc_rollup)
      ~hooks
      client
  in
  let* () = Client.bake_for_and_wait client in
  (* The rollup should have 1 "Ticket" ticket and 1 "Ticket2" ticket. *)
  let* () =
    assert_sc_rollup_ticket_balance
      client
      ~sc_rollup
      ~ticketer
      ~content_type:"string"
      ~content:"Ticket"
      ~expected:1
  in
  let* () =
    assert_sc_rollup_ticket_balance
      client
      ~sc_rollup
      ~ticketer
      ~content_type:"string"
      ~content:"Ticket2"
      ~expected:1
  in
  (* The rollup only received tickets with:
     - ticketer: [ticketer]
     - content_type: string
     - content: "Ticket" or "Ticket2"
     Hence it should return 0 for any other kind of tickets. *)
  let* () =
    assert_sc_rollup_ticket_balance
      client
      ~sc_rollup
      ~ticketer
      ~content_type:"string"
      ~content:"foobar"
      ~expected:0
  in
  let* () =
    assert_sc_rollup_ticket_balance
      client
      ~sc_rollup
      ~ticketer
      ~content_type:"int"
      ~content:"Ticket"
      ~expected:0
  in
  let* () =
    assert_sc_rollup_ticket_balance
      client
      ~sc_rollup
      ~ticketer:Constant.bootstrap2.public_key_hash
      ~content_type:"string"
      ~content:"Ticket"
      ~expected:0
  in
  unit

let test_send_tickets_from_storage_to_sc_rollup =
  Protocol.register_regression_test
    ~__FILE__
    ~title:
      "Sending tickets from storage to smart-contract rollup should succeed \
       with appropriate ticket updates field in receipt."
    ~tags:["client"; "michelson"; "sc_rollup"]
    ~supports:(Protocol.From_protocol 16)
  @@ fun protocol ->
  let* _node, client, sc_rollup =
    setup_sc_enabled_node protocol ~parameters_ty:"list (ticket string)"
  in
  let* _alias, ticketer =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"{}"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "send_tickets_from_storage"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  let* () =
    (* Call "mint" entrypoint to mint tickets and store them in storage.  *)
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~entrypoint:"mint"
      ~hooks
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* () =
    (* Call "send" entrypoint to send tickets to sc-rollup from storage.  *)
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:ticketer
      ~arg:(sf "%S" sc_rollup)
      ~entrypoint:"send"
      ~hooks
      client
  in
  let* () = Client.bake_for_and_wait client in
  unit

let register ~protocols =
  test_create_and_remove_tickets protocols ;
  test_send_tickets_in_big_map protocols ;
  test_send_tickets_to_implicit_account protocols ;
  test_send_tickets_to_implicit_account_non_zero_amount protocols ;
  test_send_tickets_to_implicit_with_wrong_type protocols ;
  test_ticket_transfer_commutative protocols ;
  test_ticket_transfer_from_storage_to_implicit protocols ;
  test_zero_ticket_rejection protocols ;
  test_ticket_overdraft_rejection protocols ;
  test_ticket_of_wrong_type_rejection protocols ;
  test_originated_implicit_can_be_equipotent protocols ;
  test_send_tickets_to_sc_rollup protocols ;
  test_send_tickets_from_storage_to_sc_rollup protocols ;
  test_direct_transfer_tickets_from_implicit_account_to_originated_with_pair_constructor
    protocols ;
  test_direct_transfer_tickets_from_implicit_account_to_originated_with_ticket_constructor
    protocols ;
  test_direct_transfer_tickets_from_implicit_account_to_originated_complex_with_pair_constructor
    protocols ;
  test_direct_transfer_tickets_from_implicit_account_to_originated_complex_with_ticket_constructor
    protocols
