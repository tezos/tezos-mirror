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

(** Testing
    -------
    Component:  Sc rollup L1/L2 communication
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_sc_rollup_transfer.ml
    Subject:    Test transfers from Michelson to smart contract rollups
*)

open Protocol
open Alpha_context

(* Helpers *)

exception Unexpected_error

let check_proto_error ~loc ~exp f trace =
  let open Lwt_result_syntax in
  let*? proto_trace =
    List.map_e
      (function
        | Environment.Ecoproto_error e -> Ok e
        | e ->
            error_with
              "At %s, expected protocol error %s, got non-protocol error %a in \
               trace %a"
              loc
              exp
              Error_monad.pp
              e
              Error_monad.pp_print_trace
              trace)
      trace
  in
  try f proto_trace
  with Unexpected_error ->
    failwith
      "At %s, expected error %s, got %a"
      loc
      exp
      Error_monad.pp_print_trace
      trace

let sc_originate = Test_sc_rollup.sc_originate

(* A contract with four entrypoints:
    - [transfer_non_zero] takes a [contract int] and attempts to transfer with a
      non-zero amount to it. Expected to fail.

    - [transfer_int] takes a [contract int] and transfers an int to it. Expected
      to succeed.

    - [transfer_zero_ticket] takes a [contract (ticket string)] and transfers a
      zero-amount ticket to it. Expected to fail.

    - [transfer_ticket] takes a [contract (ticket string)] and transfers a
      ticket to it. Expected to succeed.
*)
let contract_originate block account =
  let script =
    {|
        parameter (or (contract %transfer_non_zero int)
                      (or (contract %transfer_int int)
                          (or (contract %transfer_zero_ticket (ticket string))
                              (or (contract %transfer_ticket (ticket string))
                                  never))));
        storage unit;
        code {
          UNPAIR;
          IF_LEFT {
            # transfer_non_zero
            PUSH mutez 1;
            PUSH int 42;
            TRANSFER_TOKENS;
          } {
            IF_LEFT {
              # transfer_int
              PUSH mutez 0;
              PUSH int 42;
              TRANSFER_TOKENS;
            } {
              IF_LEFT {
                # transfer_zero_ticket
                PUSH mutez 0;
                PUSH nat 0;
                PUSH string "ticket payload";
                TICKET;
                ASSERT_SOME;
                TRANSFER_TOKENS;
              } {
                IF_LEFT {
                  # transfer ticket
                  PUSH mutez 0;
                  PUSH nat 137;
                  PUSH string "G";
                  TICKET;
                  ASSERT_SOME;
                  TRANSFER_TOKENS;
                } {
                  NEVER
                }
              }
            }
          };
          NIL operation;
          SWAP;
          CONS;
          PAIR }
|}
  in
  Contract_helpers.originate_contract_from_string_hash
    ~baker:(Context.Contract.pkh account)
    ~source_contract:account
    ~script
    ~storage:"Unit"
    block

let context_init parameters_ty =
  let open Lwt_result_syntax in
  let* b, c = Test_sc_rollup.context_init T1 in
  let* contract, _script, b = contract_originate b c in
  let* b, rollup = sc_originate b c ~parameters_ty in
  return (b, c, contract, rollup)

let transfer ?expect_apply_failure b ~from ~to_ ~param ~entrypoint =
  let open Lwt_result_syntax in
  let parameters = Script.lazy_expr (Expr.from_string param) in
  let* op =
    Op.transaction
      (B b)
      from
      (Contract.Originated to_)
      Tez.zero
      ~parameters
      ~entrypoint:(Entrypoint.of_string_strict_exn entrypoint)
      ~gas_limit:High
  in
  let* inc = Incremental.begin_construction b in
  let* inc = Incremental.add_operation ?expect_apply_failure inc op in
  Incremental.finalize_block inc

(* Tests *)

(* Test parsing a [contract] with a badly formatted sr1 address. *)
let test_transfer_to_bad_sc_rollup_address () =
  let open Lwt_result_syntax in
  let* b, c, contract, _rollup = context_init "unit" in
  let not_an_sc_rollup_address = {|"sr1Fq8fPi2NjhWUXtcXBggbL6zFjZctDamso"|} in
  let* (_b : Block.t) =
    transfer
      b
      ~from:c
      ~to_:contract
      ~param:not_an_sc_rollup_address
      ~entrypoint:"transfer_non_zero"
      ~expect_apply_failure:
        ( check_proto_error ~loc:__LOC__ ~exp:"Invalid_destination_b58check"
        @@ function
          | [
              Script_interpreter.Bad_contract_parameter _;
              Script_tc_errors.Invalid_constant (_loc, _expr, ty);
              Destination_repr.Invalid_destination_b58check _;
            ] ->
              Assert.equal_string
                ~loc:__LOC__
                "(contract int)"
                (Expr.to_string ty)
          | _ -> raise Unexpected_error )
  in
  return_unit

(* Now, the address is well-formatted but the rollup does not exist. *)
let test_transfer_to_unknown_sc_rollup_address () =
  let open Lwt_result_syntax in
  let* b, c, contract, _rollup = context_init "unit" in
  let unknown_sc_rollup_address = {|"sr1Fq8fPi2NjhWUXtcXBggbL6zFjZctGkmso"|} in
  let* (_b : Block.t) =
    transfer
      b
      ~from:c
      ~to_:contract
      ~param:unknown_sc_rollup_address
      ~entrypoint:"transfer_non_zero"
      ~expect_apply_failure:
        ( check_proto_error ~loc:__LOC__ ~exp:"Sc_rollup_does_not_exist"
        @@ function
          | [
              Script_interpreter.Bad_contract_parameter _;
              Script_tc_errors.Invalid_constant _;
              Sc_rollup_errors.Sc_rollup_does_not_exist _;
            ] ->
              return_unit
          | _ -> raise Unexpected_error )
  in
  return_unit

(* Now, let's originate an sc rollup, use its address but with a wrong type. *)
let test_transfer_to_wrongly_typed_sc_rollup () =
  let open Lwt_result_syntax in
  let* b, c, contract, rollup = context_init "unit" in
  let param = Format.sprintf "%S" (Sc_rollup.Address.to_b58check rollup) in
  let* (_b : Block.t) =
    transfer
      b
      ~from:c
      ~to_:contract
      ~param
      ~entrypoint:"transfer_non_zero"
      ~expect_apply_failure:
        ( check_proto_error ~loc:__LOC__ ~exp:"Inconsistent_types" @@ function
          | [
              Script_interpreter.Bad_contract_parameter _;
              Script_tc_errors.Invalid_constant _;
              Script_tc_errors.Inconsistent_types _;
              Script_tc_errors.Inconsistent_types _;
            ] ->
              return_unit
          | _ -> raise Unexpected_error )
  in
  return_unit

(* Use the correct type but with a non-zero amount. *)
let test_transfer_non_zero_amount () =
  let open Lwt_result_syntax in
  let* b, c, contract, rollup = context_init "int" in
  let param = Format.sprintf "%S" (Sc_rollup.Address.to_b58check rollup) in
  let* (_b : Block.t) =
    transfer
      b
      ~from:c
      ~to_:contract
      ~param
      ~entrypoint:"transfer_non_zero"
      ~expect_apply_failure:
        ( check_proto_error ~loc:__LOC__ ~exp:"Rollup_invalid_transaction_amount"
        @@ function
          | [
              Script_interpreter.Runtime_contract_error _;
              Script_interpreter_defs.Rollup_invalid_transaction_amount;
            ] ->
              return_unit
          | _ -> raise Unexpected_error )
  in
  return_unit

(* Use the correct type through an entrypoint but with a non-zero amount. *)
let test_transfer_non_zero_amount_via_entrypoint () =
  let open Lwt_result_syntax in
  let* b, c, contract, rollup = context_init "int" in
  let param = Format.sprintf "%S" (Sc_rollup.Address.to_b58check rollup) in
  let* (_b : Block.t) =
    transfer
      b
      ~from:c
      ~to_:contract
      ~param
      ~entrypoint:"transfer_non_zero"
      ~expect_apply_failure:
        ( check_proto_error ~loc:__LOC__ ~exp:"Rollup_invalid_transaction_amount"
        @@ function
          | [
              Script_interpreter.Runtime_contract_error _;
              Script_interpreter_defs.Rollup_invalid_transaction_amount;
            ] ->
              return_unit
          | _ -> raise Unexpected_error )
  in
  return_unit

(* Now, transfer with a zero-amount and check that the inbox has been updated correctly. *)
let test_transfer_works () =
  let open Lwt_result_wrap_syntax in
  let* b, c, contract, rollup = context_init "int" in
  let* inbox_before = Context.Sc_rollup.inbox (B b) in
  let* expected_inbox_after =
    let* inc = Incremental.begin_construction b in
    let ctxt = Incremental.alpha_ctxt inc in
    let payload = Expr.from_string "42" in
    let*@ ctxt =
      Sc_rollup.Inbox.add_deposit
        ctxt
        ~destination:rollup
        ~payload
        ~sender:contract
        ~source:(Context.Contract.pkh c)
    in
    let incr = Incremental.set_alpha_ctxt inc ctxt in
    let* block = Incremental.finalize_block incr in
    let* expected_inbox_after = Context.Sc_rollup.inbox (B block) in
    return expected_inbox_after
  in
  let param = Format.sprintf "%S" (Sc_rollup.Address.to_b58check rollup) in
  let* b = transfer b ~from:c ~to_:contract ~param ~entrypoint:"transfer_int" in
  let* inbox_after = Context.Sc_rollup.inbox (B b) in
  let* () =
    Assert.not_equal_with_encoding
      ~loc:__LOC__
      Sc_rollup.Inbox.encoding
      inbox_before
      inbox_after
  in
  Assert.equal_with_encoding
    ~loc:__LOC__
    Sc_rollup.Inbox.encoding
    inbox_after
    expected_inbox_after

(* Transfer of zero-amount ticket fails. *)
let test_transfer_zero_amount_ticket () =
  let open Lwt_result_syntax in
  let* b, c, contract, rollup = context_init "ticket string" in
  let param = Format.sprintf "%S" (Sc_rollup.Address.to_b58check rollup) in
  let* (_b : Block.t) =
    transfer
      b
      ~from:c
      ~to_:contract
      ~param
      ~entrypoint:"transfer_zero_ticket"
      ~expect_apply_failure:
        ( check_proto_error ~loc:__LOC__ ~exp:"Script_rejected" @@ function
          | [
              Script_interpreter.Runtime_contract_error _;
              Script_interpreter.Reject _;
            ] ->
              return_unit
          | _ -> raise Unexpected_error )
  in
  return_unit

(* Transfer of a non-zero-amount ticket works and the balance table is correctly updated. *)
let test_transfer_non_zero_amount_ticket () =
  let open Lwt_result_wrap_syntax in
  let* b, c, contract, rollup = context_init "ticket string" in
  let param = Format.sprintf "%S" (Sc_rollup.Address.to_b58check rollup) in
  let* b =
    transfer b ~from:c ~to_:contract ~param ~entrypoint:"transfer_ticket"
  in
  let* ticket_key_for_contract, ticket_key_for_rollup, ctxt =
    let* ticket_token =
      Ticket_helpers.string_ticket_token
        (Contract_hash.to_b58check contract)
        "G"
    in
    let* inc = Incremental.begin_construction b in
    let ctxt = Incremental.alpha_ctxt inc in
    let*@ ticket_key_for_contract, ctxt =
      Ticket_balance_key.of_ex_token
        ctxt
        ~owner:(Destination.Contract (Originated contract))
        ticket_token
    in
    let*@ ticket_key_for_rollup, _ctxt =
      Ticket_balance_key.of_ex_token
        ctxt
        ~owner:(Destination.Sc_rollup rollup)
        ticket_token
    in
    return (ticket_key_for_contract, ticket_key_for_rollup, ctxt)
  in
  (* The rollup is the owner of the tickets *)
  let* () =
    Ticket_helpers.assert_balance
      ctxt
      ~loc:__LOC__
      ticket_key_for_rollup
      (Some 137)
  in
  (* The contract didn't retain any ticket in the operation *)
  let* () =
    Ticket_helpers.assert_balance ctxt ~loc:__LOC__ ticket_key_for_contract None
  in
  return_unit

let tests =
  [
    Tztest.tztest
      "Transfer to a bad sc rollup address"
      `Quick
      test_transfer_to_bad_sc_rollup_address;
    Tztest.tztest
      "Transfer to an unknown rollup address"
      `Quick
      test_transfer_to_unknown_sc_rollup_address;
    Tztest.tztest
      "Transfer with a wrong type"
      `Quick
      test_transfer_to_wrongly_typed_sc_rollup;
    Tztest.tztest
      "Transfer with a non-zero amount"
      `Quick
      test_transfer_non_zero_amount_via_entrypoint;
    Tztest.tztest "Transfer works" `Quick test_transfer_works;
    Tztest.tztest
      "Transfer of zero-amount ticket"
      `Quick
      test_transfer_zero_amount_ticket;
    Tztest.tztest
      "Transfer of non-zero-amount ticket"
      `Quick
      test_transfer_non_zero_amount_ticket;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("sc rollup transfer", tests)]
  |> Lwt_main.run
