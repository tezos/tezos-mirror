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
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                -- test "^sc rollup transfer$"
    Subject:    Test transfers from Michelson to smart contract rollups
*)

open Protocol
open Alpha_context
open Lwt_result_syntax

(* Helpers *)

exception Unexpected_error

let check_proto_error ~loc ~exp f trace =
  let*? proto_trace =
    List.map_e
      (function
        | Environment.Ecoproto_error e -> ok e
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

(* A contract with one entrypoint:
    - [transfer_non_zero] takes a [contract int] and attempts to transfer with a
      non-zero amount to it. Expected to fail.
*)
let contract_originate block account =
  let script =
    {|
        parameter (or (contract %transfer_non_zero int) never);
        storage unit;
        code {
          UNPAIR;
          IF_LEFT {
            # transfer_non_zero
            PUSH mutez 1;
            PUSH int 42;
          } {
            NEVER
          };
          TRANSFER_TOKENS;
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

let context_init ty =
  let* b, c = Test_sc_rollup.context_init T1 in
  let* contract, _script, b = contract_originate b c in
  let* b, rollup = sc_originate b c ty in
  return (b, c, contract, rollup)

let transfer ?expect_apply_failure b ~from ~to_ ~param ~entrypoint =
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

(* Test parsing a [contract] with a badly formatted scr1 address. *)
let test_transfer_to_bad_sc_rollup_address () =
  let* b, c, contract, _rollup = context_init "unit" in
  let not_an_sc_rollup_address = {|"scr1HLXM32GacPNDrhHDLAssZG88eWqCUbyL"|} in
  let* _b =
    transfer
      b
      ~from:c
      ~to_:contract
      ~param:not_an_sc_rollup_address
      ~entrypoint:"transfer_non_zero"
      ~expect_apply_failure:
        (check_proto_error ~loc:__LOC__ ~exp:"Invalid_destination_b58check"
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
         | _ -> raise Unexpected_error)
  in
  return_unit

(* Now, the address is well-formatted but the rollup does not exist. *)
let test_transfer_to_unknown_sc_rollup_address () =
  let* b, c, contract, _rollup = context_init "unit" in
  let unknown_sc_rollup_address = {|"scr1HLXM32GacPNDrhHDLAssZG88eWqCUbyLF"|} in
  let* _b =
    transfer
      b
      ~from:c
      ~to_:contract
      ~param:unknown_sc_rollup_address
      ~entrypoint:"transfer_non_zero"
      ~expect_apply_failure:
        (check_proto_error ~loc:__LOC__ ~exp:"Sc_rollup_does_not_exist"
         @@ function
         | [
             Script_interpreter.Bad_contract_parameter _;
             Script_tc_errors.Invalid_constant _;
             Sc_rollup_errors.Sc_rollup_does_not_exist _;
           ] ->
             return_unit
         | _ -> raise Unexpected_error)
  in
  return_unit

(* Now, let's originate an sc rollup, use its address but with a wrong type. *)
let test_transfer_to_wrongly_typed_sc_rollup () =
  let* b, c, contract, rollup = context_init "unit" in
  let param = Format.sprintf "%S" (Sc_rollup.Address.to_b58check rollup) in
  let* _b =
    transfer
      b
      ~from:c
      ~to_:contract
      ~param
      ~entrypoint:"transfer_non_zero"
      ~expect_apply_failure:
        (check_proto_error ~loc:__LOC__ ~exp:"Inconsistent_types" @@ function
         | [
             Script_interpreter.Bad_contract_parameter _;
             Script_tc_errors.Invalid_constant _;
             Script_tc_errors.Inconsistent_types _;
             Script_tc_errors.Inconsistent_types _;
           ] ->
             return_unit
         | _ -> raise Unexpected_error)
  in
  return_unit

(* Use the correct type but with a non-zero amount. *)
let test_transfer_non_zero_amount () =
  let* b, c, contract, rollup = context_init "int" in
  let param = Format.sprintf "%S" (Sc_rollup.Address.to_b58check rollup) in
  let* _b =
    transfer
      b
      ~from:c
      ~to_:contract
      ~param
      ~entrypoint:"transfer_non_zero"
      ~expect_apply_failure:
        (check_proto_error ~loc:__LOC__ ~exp:"Rollup_invalid_transaction_amount"
         @@ function
         | [
             Script_interpreter.Runtime_contract_error _;
             Script_interpreter_defs.Rollup_invalid_transaction_amount;
           ] ->
             return_unit
         | _ -> raise Unexpected_error)
  in
  return_unit

(* Use the correct type through an entrypoint but with a non-zero amount. *)
let test_transfer_non_zero_amount_via_entrypoint () =
  let* b, c, contract, rollup =
    context_init "or (int %use_this_one) (unit %not_that_one)"
  in
  let param =
    Format.sprintf "%S" (Sc_rollup.Address.to_b58check rollup ^ "%use_this_one")
  in
  let* _b =
    transfer
      b
      ~from:c
      ~to_:contract
      ~param
      ~entrypoint:"transfer_non_zero"
      ~expect_apply_failure:
        (check_proto_error ~loc:__LOC__ ~exp:"Rollup_invalid_transaction_amount"
         @@ function
         | [
             Script_interpreter.Runtime_contract_error _;
             Script_interpreter_defs.Rollup_invalid_transaction_amount;
           ] ->
             return_unit
         | _ -> raise Unexpected_error)
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
  ]
