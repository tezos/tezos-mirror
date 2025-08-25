(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023-2024 Marigold <contact@marigold.dev>                   *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Tickets, direct spending from implicit accounts
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                   -- --file test_ticket_direct_spending.ml
    Subject:      Test direct spending of tickets from implicit accounts
*)

open Protocol
open Alpha_context

type constructor = Pair | Ticket

let originate_contract ~code ~storage originator block =
  let open Lwt_result_syntax in
  let storage = Script.lazy_expr (Expr.from_string storage) in
  let code = Script.lazy_expr (Expr.toplevel_from_string code) in
  let script = Script.{code; storage} in
  let* operation, contract_address =
    Op.contract_origination ~script (B block) originator
  in
  let* block = Block.bake ~operation block in
  return (contract_address, block)

let call_contract ~source ~contract ?entrypoint ~arg block =
  let open Lwt_result_syntax in
  let arg = Script.lazy_expr (Expr.from_string arg) in
  let* operation =
    Op.transaction
      ~parameters:arg
      ?entrypoint
      (B block)
      source
      contract
      Tez.zero
  in
  Block.bake ~operation block

let assert_ticket_balance ~loc ~ticketer ~expected_balance owner block =
  let open Lwt_result_wrap_syntax in
  let* incr = Incremental.begin_construction block in
  let ctxt = Incremental.alpha_ctxt incr in
  let token =
    Ticket_token.Ex_token
      {ticketer; contents_type = Script_typed_ir.unit_t; contents = ()}
  in
  let*@ key_hash, ctxt = Ticket_balance_key.of_ex_token ctxt ~owner token in
  let*@ balance_opt, _ctxt = Ticket_balance.get_balance ctxt key_hash in
  let balance = Option.value ~default:Z.zero balance_opt in
  Assert.equal_z ~loc balance (Z.of_int expected_balance)

let ticket_boomerang_script =
  {|
# This contract sends back a unit ticket of amount 1 to its sender
parameter unit;
storage unit;
code
  {
    DROP;
    NIL operation;
    SENDER; CONTRACT (ticket unit); ASSERT_SOME;
    PUSH mutez 0;
    PUSH nat 1; UNIT; TICKET; ASSERT_SOME;
    TRANSFER_TOKENS;
    CONS;
    UNIT; SWAP; PAIR
  }
    |}

let ticket_consumer_script =
  {|
# This contract consumes a unit ticket
parameter (ticket unit);
storage unit;
code
  {
    CDR;
    NIL operation;
    PAIR
  }
|}

(** This test validates the transfer of tickets from implicit accounts to originated accounts
    using the [Transaction] manager operation. It performs the following steps:

    1. Deposit a ticket into an implicit account using [ticket_boomerang_script].
    2. The implicit account then transfers the ticket to [ticket_consumer_script]
       using [Transaction].
    3. The outcome depends [direct_ticket_spending_enable] feature flag:
       - If enabled: The ticket transfer should be successful,
                     resulting in an updated balance in the ticket table.
       - If disabled: The transfer attempt should fail,
                      triggering a [Bad_contract_parameter] error. *)
let test_spending ~direct_ticket_spending_enable ~constructor () =
  let open Lwt_result_syntax in
  let constants =
    let default_constants =
      Tezos_protocol_alpha_parameters.Default_parameters.constants_test
    in
    {
      default_constants with
      consensus_threshold_size = 0;
      direct_ticket_spending_enable;
    }
  in
  let* block, (delegate, implicit) = Context.init_with_constants2 constants in
  (* Originate a contract sending tickets to whoever ask for them. *)
  let* boomerang, block =
    originate_contract
      ~code:ticket_boomerang_script
      ~storage:"Unit"
      delegate
      block
  in
  let boomerang_str = Format.asprintf "%a" Contract.pp boomerang in
  let* consumer, block =
    originate_contract
      ~code:ticket_consumer_script
      ~storage:"Unit"
      delegate
      block
  in
  let* () =
    assert_ticket_balance
      ~loc:__LOC__
      ~ticketer:boomerang
      ~expected_balance:0
      (Destination.Contract implicit)
      block
  in
  let* block =
    call_contract ~source:implicit ~contract:boomerang ~arg:"Unit" block
  in
  let* () =
    assert_ticket_balance
      ~loc:__LOC__
      ~ticketer:boomerang
      ~expected_balance:1
      (Destination.Contract implicit)
      block
  in
  let arg =
    match constructor with
    | Pair -> sf "Pair %S Unit 1" boomerang_str
    | Ticket -> sf "Ticket %S unit Unit 1" boomerang_str
  in
  if direct_ticket_spending_enable then
    let* block = call_contract ~source:implicit ~contract:consumer ~arg block in
    assert_ticket_balance
      ~loc:__LOC__
      ~ticketer:boomerang
      ~expected_balance:0
      (Destination.Contract implicit)
      block
  else
    let*! res = call_contract ~source:implicit ~contract:consumer ~arg block in
    Assert.proto_error ~loc:__LOC__ res (function
      | Script_interpreter.Bad_contract_parameter _ -> true
      | _ -> false)

let tests =
  [
    Tztest.tztest
      "Test ticket spending from implicit accounts (feature enabled) with Pair \
       ticket constructor"
      `Quick
      (test_spending ~direct_ticket_spending_enable:true ~constructor:Pair);
    Tztest.tztest
      "Test ticket spending from implicit accounts (feature disabled) with \
       Pair ticket constructor"
      `Quick
      (test_spending ~direct_ticket_spending_enable:false ~constructor:Pair);
    Tztest.tztest
      "Test ticket spending from implicit accounts (feature enabled) with \
       Ticket constructor"
      `Quick
      (test_spending ~direct_ticket_spending_enable:true ~constructor:Ticket);
    Tztest.tztest
      "Test ticket spending from implicit accounts (feature disabled) with \
       Ticket constructor"
      `Quick
      (test_spending ~direct_ticket_spending_enable:false ~constructor:Ticket);
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("ticket direct spending", tests)]
  |> Lwt_main.run
