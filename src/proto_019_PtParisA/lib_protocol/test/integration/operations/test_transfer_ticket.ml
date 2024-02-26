(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
    Component:    Transfer_ticket logic
    Invocation:   dune exec src/proto_019_PtParisA/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_transfer_ticket.ml
    Subject:      Test ticket transfers
*)

open Protocol
open Alpha_context
open Tezos_micheline

(* In this test, a ticketer contract mints and transfers a ticket to an implicit account,
   who further transfers it to another implicit account.
   The ticket balance is inspected for correctness.
*)
let test_mint_deposit_withdraw_implicit_transfer () =
  let open Lwt_result_wrap_syntax in
  let* block, (account, another_account) =
    Context.init2 ~consensus_threshold:0 ()
  in
  let baker = Context.Contract.pkh account in
  let* ticketer, _, block =
    Contract_helpers.originate_contract_from_string
      ~script:
        {|
          parameter (pair nat nat address) ;
          storage unit ;
          code { CAR ;
                UNPAIR 3 ;
                DIG 2 ;
                CONTRACT (ticket nat) ;
                ASSERT_SOME ;
                # contract : nat %ct : nat %qty
                PUSH mutez 0 ;
                # tez : contract : nat %ct : nat %qty
                DIG 3 ;
                # nat %qty : tez : contract : nat %ct
                DIG 3 ;
                # nat %ct : nat %qty : tez : contract
                TICKET ;
                ASSERT_SOME ;
                TRANSFER_TOKENS ;
                NIL operation ;
                SWAP ;
                CONS ;
                UNIT ;
                SWAP ;
                PAIR }
      |}
      ~storage:"Unit"
      ~source_contract:account
      ~baker
      block
  in
  let contents = 42 in
  let* block =
    let* operation =
      Op.transaction
        (B block)
        ~entrypoint:Entrypoint.default
        ~parameters:
          (Expr_common.(
             pair_n [int (Z.of_int contents); int (Z.of_int 1); address account])
          |> Micheline.strip_locations |> Script.lazy_expr)
        ~fee:Tez.one
        account
        ticketer
        (Tez.of_mutez_exn 0L)
    in
    Block.bake ~operation block
  in
  let ty = Expr.from_string "nat" in
  let* block =
    let* operation =
      Op.transfer_ticket
        (B block)
        ~entrypoint:Entrypoint.default
        ~source:account
        ~ty:(Script.lazy_expr ty)
        ~contents:
          (Script.lazy_expr @@ Expr.from_string @@ string_of_int contents)
        ~amount:
          (WithExceptions.Option.get ~loc:__LOC__
          @@ Ticket_amount.of_zint @@ Z.of_int 1)
        ~destination:another_account
        ~ticketer
    in
    Block.bake ~operation block
  in
  let make_ex_token ctxt ~ticketer ~ty ~content =
    let*?@ Script_ir_translator.Ex_comparable_ty cty, ctxt =
      Script_ir_translator.parse_comparable_ty ctxt @@ Micheline.root ty
    in
    let*@ contents, ctxt =
      Script_ir_translator.parse_comparable_data ctxt cty
      @@ Micheline.root content
    in
    return
      (Ticket_token.Ex_token {contents_type = cty; ticketer; contents}, ctxt)
  in
  let* ctxt =
    let+ result = Incremental.begin_construction block in
    Incremental.alpha_ctxt result
  in
  let* token, ctxt =
    make_ex_token
      ctxt
      ~ticketer
      ~ty
      ~content:(Expr.from_string @@ string_of_int contents)
  in
  let*@ key, ctxt =
    Ticket_balance_key.of_ex_token ctxt ~owner:(Contract another_account) token
  in
  let*@ amount, _ = Ticket_balance.get_balance ctxt key in
  match amount with
  | Some amount -> Assert.equal_int ~loc:__LOC__ (Z.to_int amount) 1
  | _ -> return_unit

(* In this test, a ticketer contract is called to mint and send a ticket
   to an implicit account and a contract. Both destinations are given
   in a `contract (ticket nat)` value.
   Transfer should be possible since the target contract has the right
   parameter type under the given entrypoint.
*)
let test_contract_as_ticket_transfer_destination () =
  let open Lwt_result_wrap_syntax in
  let* block, (account, another_account) =
    Context.init2 ~consensus_threshold:0 ()
  in
  let baker = Context.Contract.pkh account in
  let* ticketer, _, block =
    Contract_helpers.originate_contract_from_string
      ~script:
        {|
          parameter (pair (contract (ticket nat)) nat nat) ;
          storage unit ;
          code { CAR ;
                UNPAIR 3 ;
                # contract (ticket nat) : nat %ct : nat %qty
                PUSH mutez 0 ;
                # tez : contract (ticket nat) : nat %ct : nat %qty
                DIG 3 ;
                # nat %qty : tez : contract (ticket nat) : nat %ct
                DIG 3 ;
                # nat %ct : nat %qty : tez : contract (ticket nat)
                TICKET ;
                ASSERT_SOME ;
                TRANSFER_TOKENS ;
                NIL operation ;
                SWAP ;
                CONS ;
                UNIT ;
                SWAP ;
                PAIR }
      |}
      ~storage:"Unit"
      ~source_contract:account
      ~baker
      block
  in
  let* bag, _, block =
    Contract_helpers.originate_contract_from_string
      ~script:
        {|
          parameter (or (ticket %save nat) (address %send));
          storage (list (ticket nat));
          code { UNPAIR ;
                IF_LEFT
                  { CONS ; NIL operation ; PAIR }
                  { SWAP ;
                    IF_CONS
                      { DIG 2 ;
                        CONTRACT %ticket (ticket nat) ;
                        ASSERT_SOME ;
                        PUSH mutez 0 ;
                        DIG 2 ;
                        TRANSFER_TOKENS ;
                        NIL operation ;
                        SWAP ;
                        CONS ;
                        PAIR }
                      { PUSH string "no ticket to send" ; FAILWITH }}}
      |}
      ~storage:"{}"
      ~source_contract:account
      ~baker
      block
  in
  let contents = 42 in
  let* block =
    let* operation =
      Op.transaction
        (B block)
        ~entrypoint:Entrypoint.default
        ~parameters:
          (Expr_common.(
             pair_n
               [
                 string
                   (Destination.(to_b58check (Contract account))
                   ^ Entrypoint.(to_address_suffix default));
                 int (Z.of_int contents);
                 int (Z.of_int 1);
               ])
          |> Micheline.strip_locations |> Script.lazy_expr)
        ~fee:Tez.one
        account
        ticketer
        (Tez.of_mutez_exn 0L)
    in
    Block.bake ~operation block
  in
  let ty = Expr.from_string "nat" in
  let* block =
    let* operation =
      Op.transfer_ticket
        (B block)
        ~entrypoint:Entrypoint.default
        ~source:account
        ~ty:(Script.lazy_expr ty)
        ~contents:
          (Script.lazy_expr @@ Expr.from_string @@ string_of_int contents)
        ~amount:
          (WithExceptions.Option.get ~loc:__LOC__
          @@ Ticket_amount.of_zint @@ Z.of_int 1)
        ~destination:another_account
        ~ticketer
    in
    Block.bake ~operation block
  in
  let make_ex_token ctxt ~ticketer ~ty ~content =
    let*?@ Script_ir_translator.Ex_comparable_ty cty, ctxt =
      Script_ir_translator.parse_comparable_ty ctxt @@ Micheline.root ty
    in
    let*@ contents, ctxt =
      Script_ir_translator.parse_comparable_data ctxt cty
      @@ Micheline.root content
    in
    return
      (Ticket_token.Ex_token {contents_type = cty; ticketer; contents}, ctxt)
  in
  let* ctxt =
    let+ result = Incremental.begin_construction block in
    Incremental.alpha_ctxt result
  in
  let* token, ctxt =
    make_ex_token
      ctxt
      ~ticketer
      ~ty
      ~content:(Expr.from_string @@ string_of_int contents)
  in
  let*@ key, ctxt =
    Ticket_balance_key.of_ex_token ctxt ~owner:(Contract another_account) token
  in
  let*@ amount, _ = Ticket_balance.get_balance ctxt key in
  let* () =
    match amount with
    | Some amount -> Assert.equal_int ~loc:__LOC__ (Z.to_int amount) 1
    | _ -> return_unit
  in
  let* block =
    let* operation =
      Op.transaction
        (B block)
        ~entrypoint:Entrypoint.default
        ~parameters:
          (Expr_common.(
             pair_n
               [
                 string
                   (Destination.(to_b58check (Contract bag))
                   ^ Entrypoint.(
                       to_address_suffix @@ of_string_strict_exn "save"));
                 int (Z.of_int contents);
                 int (Z.of_int 1);
               ])
          |> Micheline.strip_locations |> Script.lazy_expr)
        ~fee:Tez.one
        account
        ticketer
        (Tez.of_mutez_exn 0L)
    in
    Block.bake ~operation block
  in
  let* ctxt =
    let+ result = Incremental.begin_construction block in
    Incremental.alpha_ctxt result
  in
  let* token, ctxt =
    make_ex_token
      ctxt
      ~ticketer
      ~ty
      ~content:(Expr.from_string @@ string_of_int contents)
  in
  let*@ key, ctxt =
    Ticket_balance_key.of_ex_token ctxt ~owner:(Contract bag) token
  in
  let*@ amount, _ = Ticket_balance.get_balance ctxt key in
  match amount with
  | Some amount -> Assert.equal_int ~loc:__LOC__ (Z.to_int amount) 1
  | _ -> return_unit

let tests =
  [
    Tztest.tztest
      "ticket transfer operations"
      `Quick
      test_mint_deposit_withdraw_implicit_transfer;
    Tztest.tztest
      "'contract (ticket cty)' as transfer destination"
      `Quick
      test_contract_as_ticket_transfer_destination;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("transfer ticket", tests)]
  |> Lwt_main.run
