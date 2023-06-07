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
    Component:    Rollup layer 1 logic
    Invocation:   dune exec src/proto_017_PtNairob/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_zk_rollup.ml
    Subject:      Test zk rollup
*)

open Protocol
open Alpha_context
open Error_monad_operators

exception Zk_rollup_test_error of string

(* Number of operations in each private batch *)
let batch_size = 10

module Operator = Dummy_zk_rollup.Operator (struct
  let batch_size = batch_size
end)

(* Operation with payload = 1 *)
let true_op l1_dst rollup_id =
  Zk_rollup.Operation.
    {
      op_code = 0;
      price = Operator.Internal_for_tests.true_op.price;
      l1_dst;
      rollup_id;
      payload = [|Bls12_381.Fr.one|];
    }

let of_plonk_smap s = Zk_rollup.Account.SMap.of_seq @@ Plonk.SMap.to_seq s

(* Operation with payload = 0 *)
let false_op l1_dst rollup_id =
  {(true_op l1_dst rollup_id) with payload = [|Bls12_381.Fr.zero|]}

(** [check_proto_error_f f t] checks that the first error of [t]
    satisfies the boolean function [f]. *)
let check_proto_error_f f t =
  match t with
  | Environment.Ecoproto_error e :: _ when f e ->
      Assert.test_error_encodings e ;
      return_unit
  | _ -> failwith "Unexpected error: %a" Error_monad.pp_print_trace t

(** [check_proto_error e t] checks that the first error of [t]
    equals [e]. *)
let check_proto_error e t = check_proto_error_f (( = ) e) t

(* Check that originating a ZKRU fails when the feature flag is disabled. *)
let test_disable_feature_flag () =
  let open Lwt_result_syntax in
  let _prover_pp, public_parameters = Lazy.force Operator.lazy_pp in
  let* b, contract =
    Context.init_with_constants1
      {
        Context.default_test_constants with
        zk_rollup =
          {Context.default_test_constants.zk_rollup with enable = false};
      }
  in
  let* i = Incremental.begin_construction b in
  let* op, _zk_rollup =
    Op.zk_rollup_origination
      (I i)
      contract
      ~public_parameters
      ~circuits_info:(of_plonk_smap Operator.circuits)
      ~init_state:Operator.init_state
      ~nb_ops:1
  in
  let* (_i : Incremental.t) =
    Incremental.add_operation
      ~expect_failure:
        (check_proto_error Validate_errors.Manager.Zk_rollup_feature_disabled)
      i
      op
  in
  return_unit

(** [context_init n] initializes a context for testing in which the
  [zk_rollup_enable] constant is set to true. It returns the created
  context and [n] contracts. *)
let context_init =
  Context.init_with_constants_n
    {
      Context.default_test_constants with
      zk_rollup = {Context.default_test_constants.zk_rollup with enable = true};
      consensus_threshold = 0;
    }

(* Check that the expected origination fees are paid. *)
let test_origination_fees () =
  let open Lwt_result_syntax in
  let* ctxt, contracts = context_init 1 in
  let* constants = Context.get_constants (B ctxt) in
  let contract = Stdlib.List.hd contracts in
  let _prover_pp, public_parameters = Lazy.force Operator.lazy_pp in
  let expected_size =
    (* TODO: create ZK constant *)
    let origination_size = constants.parametric.tx_rollup.origination_size in
    let init_account =
      Zk_rollup.Account.
        {
          static =
            {
              public_parameters;
              state_length = 1;
              circuits_info = of_plonk_smap Operator.circuits;
              nb_ops = 1;
            };
          dynamic =
            {
              state = Operator.init_state;
              paid_l2_operations_storage_space = Z.of_int origination_size;
              used_l2_operations_storage_space = Z.zero;
            };
        }
    in
    let init_pl = Zk_rollup.(Empty {next_index = 0L}) in
    origination_size + Zk_rollup.Address.size
    + Data_encoding.Binary.length Zk_rollup.Account.encoding init_account
    + Data_encoding.Binary.length Zk_rollup.pending_list_encoding init_pl
  in
  let expected_fees =
    Tez.mul_exn constants.parametric.cost_per_byte expected_size
  in
  let* operation, _rollup =
    Op.zk_rollup_origination
      (B ctxt)
      contract
      ~public_parameters
      ~circuits_info:(of_plonk_smap Operator.circuits)
      ~init_state:Operator.init_state
      ~nb_ops:1
  in
  let* balance_before = Context.Contract.balance (B ctxt) contract in
  let* i = Incremental.begin_construction ctxt in
  let* i = Incremental.add_operation i operation in
  Assert.balance_was_debited
    ~loc:__LOC__
    (I i)
    contract
    balance_before
    expected_fees

let test_origination_negative_nb_ops () =
  let open Lwt_result_syntax in
  let* ctxt, contracts = context_init 1 in
  let contract = Stdlib.List.hd contracts in
  let _prover_pp, public_parameters = Lazy.force Operator.lazy_pp in
  let* operation, _rollup =
    Op.zk_rollup_origination
      (B ctxt)
      contract
      ~public_parameters
      ~circuits_info:(of_plonk_smap Operator.circuits)
      ~init_state:Operator.init_state
      ~nb_ops:(-1)
  in
  let* i = Incremental.begin_construction ctxt in
  let* (_i : Incremental.t) =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Zk_rollup_apply.Zk_rollup_negative_nb_ops)
      i
      operation
  in
  return_unit

(** Initializes the context and originates a ZKRU. *)
let init_and_originate n =
  let open Lwt_result_syntax in
  let* ctxt, contracts = context_init n in
  let contract = Stdlib.List.hd contracts in
  let _prover_pp, public_parameters = Lazy.force Operator.lazy_pp in
  let* operation, rollup =
    Op.zk_rollup_origination
      (B ctxt)
      contract
      ~public_parameters
      ~circuits_info:(of_plonk_smap Operator.circuits)
      ~init_state:Operator.init_state
      ~nb_ops:1
  in
  let* b = Block.bake ~operation ctxt in
  return (b, contracts, rollup)

let no_ticket op = (op, None)

(* Checks that originating two ZK rollups leads to different
   rollup addresses. *)
let test_originate_two_rollups () =
  let open Lwt_result_syntax in
  let* ctxt, contracts, zk_rollup1 = init_and_originate 1 in
  let contract = Stdlib.List.hd contracts in
  let _prover_pp, public_parameters = Lazy.force Operator.lazy_pp in
  let* operation, zk_rollup2 =
    Op.zk_rollup_origination
      (B ctxt)
      contract
      ~public_parameters
      ~circuits_info:(of_plonk_smap Operator.circuits)
      ~init_state:Operator.init_state
      ~nb_ops:1
  in
  let* (_b : Block.t) = Block.bake ~operation ctxt in
  assert (zk_rollup1 <> zk_rollup2) ;
  return_unit

(* Initializes the context and originates a ZKRU with [n_pending]
   operations. *)
let init_with_pending ?(n_pending = 1) n =
  let open Lwt_result_syntax in
  let* ctxt, contracts, zk_rollup = init_and_originate n in
  let contract = Stdlib.List.hd contracts in
  let pkh = match contract with Implicit pkh -> pkh | _ -> assert false in
  let* operation =
    Op.zk_rollup_publish
      (B ctxt)
      contract
      ~zk_rollup
      ~ops:
        (Stdlib.List.init n_pending (fun i ->
             no_ticket
             @@
             if i mod 2 = 0 then false_op pkh zk_rollup
             else true_op pkh zk_rollup))
  in
  let* b = Block.bake ~operation ctxt in
  return (b, contracts, zk_rollup, pkh)

(* Test for an invalid append:
   The operation being appended has an invalid op code.
*)
let test_append_out_of_range_op_code () =
  let open Lwt_result_syntax in
  let* ctxt, contracts, zk_rollup = init_and_originate 1 in
  let contract = Stdlib.List.hd contracts in
  let pkh = match contract with Implicit pkh -> pkh | _ -> assert false in
  let l2_op = false_op pkh zk_rollup in
  let* i = Incremental.begin_construction ctxt in
  let* operation =
    Op.zk_rollup_publish
      (I i)
      contract
      ~zk_rollup
      ~ops:[no_ticket {l2_op with op_code = 1}]
  in
  let* (_i : Incremental.t) =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error (Zk_rollup_storage.Zk_rollup_invalid_op_code 1))
      i
      operation
  in
  return_unit

(* Test for an invalid append:
   The operation being appended through an external op has positive price.
*)
let test_append_external_deposit () =
  let open Lwt_result_syntax in
  let* ctxt, contracts, zk_rollup = init_and_originate 1 in
  let contract = Stdlib.List.hd contracts in
  let pkh = match contract with Implicit pkh -> pkh | _ -> assert false in
  let l2_op = false_op pkh zk_rollup in
  let* i = Incremental.begin_construction ctxt in
  let* operation =
    Op.zk_rollup_publish
      (I i)
      contract
      ~zk_rollup
      ~ops:
        [no_ticket {l2_op with price = {l2_op.price with amount = Z.of_int 10}}]
  in
  let* (_i : Incremental.t) =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Zk_rollup.Errors.Deposit_as_external)
      i
      operation
  in
  return_unit

(* ------------------------- TESTS WITH TICKETS ------------------------- *)

(** [make_ticket_key ty contents ticketer zk_rollup] computes the ticket hash
    of the ticket containing [contents] of type [ty], crafted by [ticketer] and
    owned by [zk_rollup]. *)
let make_ticket_key ctxt ~ty ~contents ~ticketer zk_rollup =
  (match ctxt with
  | Context.B block -> Incremental.begin_construction block
  | Context.I incr -> return incr)
  >>=? fun incr ->
  let ctxt = Incremental.alpha_ctxt incr in
  Script_ir_translator.parse_comparable_ty ctxt ty
  >>??= fun (Ex_comparable_ty contents_type, ctxt) ->
  Script_ir_translator.parse_comparable_data ctxt contents_type contents
  >>=?? fun (contents, ctxt) ->
  Ticket_balance_key.of_ex_token
    ctxt
    ~owner:(Zk_rollup zk_rollup)
    (Ticket_token.Ex_token {ticketer; contents_type; contents})
  >|=?? fst

module Make_ticket (T : sig
  val ty_str : string

  type contents

  type contents_type

  val contents_type : contents_type Script_typed_ir.comparable_ty

  val contents_to_micheline : contents -> contents_type

  val contents_to_string : contents -> string
end) (C : sig
  val contents : T.contents
end) =
struct
  include T
  include C

  let ty = Expr.from_string ty_str

  let ex_token ~ticketer =
    Ticket_token.Ex_token
      {ticketer; contents_type; contents = contents_to_micheline contents}

  let contents_string = contents_to_string contents

  let contents_expr = Expr.from_string contents_string

  let ticket_hash ctxt ~ticketer ~zk_rollup =
    make_ticket_key
      ctxt
      ~ty:(Tezos_micheline.Micheline.root ty)
      ~contents:(Tezos_micheline.Micheline.root contents_expr)
      ~ticketer
      zk_rollup

  let zkru_ticket ~ticketer : Zk_rollup.Ticket.t =
    Zk_rollup.Ticket.{contents = contents_expr; ty; ticketer}

  let init_deposit_contract amount block account =
    let script =
      Format.asprintf
        {| parameter (pair address bytes);
         storage unit;
         code {
                # cast the address to contract type
                CAR;
                UNPAIR;
                CONTRACT %%deposit (pair (ticket %s) bytes);
                ASSERT_SOME;
                SWAP;
                PUSH mutez 0;
                SWAP;
                # create a ticket
                PUSH nat %a;
                PUSH %s %s;
                TICKET;
                ASSERT_SOME;
                PAIR ;
                TRANSFER_TOKENS;
                PUSH unit Unit;
                NIL operation;
                DIG 2 ;
                CONS;
                PAIR } |}
        ty_str
        Z.pp_print
        amount
        ty_str
        contents_string
    in
    Contract_helpers.originate_contract_from_string
      ~baker:(Context.Contract.pkh account)
      ~source_contract:account
      ~script
      ~storage:"Unit"
      block

  let deposit_op ~block ~zk_rollup ~(zk_op : Zk_rollup.Operation.t) ~account
      ~deposit_contract =
    let zk_op_literal =
      let bytes =
        Data_encoding.Binary.to_bytes_exn Zk_rollup.Operation.encoding zk_op
      in
      let (`Hex hex) = Hex.of_bytes bytes in
      "0x" ^ String.uppercase_ascii hex
    in
    Op.transaction
      (B block)
      ~entrypoint:Entrypoint.default
      ~parameters:
        (Script.lazy_expr @@ Expr.from_string
        @@ Printf.sprintf
             {| Pair %S %s |}
             (Zk_rollup.Address.to_b58check zk_rollup)
             zk_op_literal)
      ~fee:Tez.one
      account
      deposit_contract
      (Tez.of_mutez_exn 0L)

  (** Return an operation to originate a contract that will deposit [amount]
      tickets with l2 operation [op] on [zk_rollup] *)
  let init_deposit ~block ~amount ~zk_op ~zk_rollup ~account =
    init_deposit_contract amount block account
    >>=? fun (deposit_contract, _script, block) ->
    deposit_op ~block ~zk_rollup ~zk_op ~account ~deposit_contract
    >|=? fun op -> (block, op, deposit_contract)
end

module Nat_ticket = Make_ticket (struct
  let ty_str = "nat"

  type contents = int

  type contents_type = Script_int.n Script_int.num

  let contents_type = Script_typed_ir.nat_t

  let contents_to_string = string_of_int

  let contents_to_micheline c =
    WithExceptions.Option.get ~loc:__LOC__ @@ Script_int.(of_int c |> is_nat)
end)

module String_ticket = Make_ticket (struct
  let ty_str = "string"

  type contents = string

  type contents_type = Script_string.t

  let contents_type = Script_typed_ir.string_t

  let contents_to_string s = "\"" ^ s ^ "\""

  let contents_to_micheline c =
    WithExceptions.Result.get_ok ~loc:__LOC__ Script_string.(of_string c)
end)

let test_append_errors () =
  let open Lwt_result_syntax in
  let open Zk_rollup.Operation in
  (* Create two accounts and 1 zk rollup *)
  let* block, contracts, zk_rollup = init_and_originate 2 in
  let contract0 = Stdlib.List.nth contracts 0 in
  let contract1 = Stdlib.List.nth contracts 1 in
  (* Create and originate the deposit contract *)
  let module Nat_ticket = Nat_ticket (struct
    let contents = 1
  end) in
  let* deposit_contract, _script, block =
    Nat_ticket.init_deposit_contract (Z.of_int 10) block contract0
  in
  (* Preparing operation and ticket for tests *)
  let op =
    let pkh = match contract0 with Implicit pkh -> pkh | _ -> assert false in
    false_op pkh zk_rollup
  in
  let* ticket_hash =
    Nat_ticket.ticket_hash (B block) ~ticketer:deposit_contract ~zk_rollup
  in
  let ticket = Nat_ticket.zkru_ticket ~ticketer:contract0 in
  (* Start generating block *)
  let* i = Incremental.begin_construction block in
  (* Send ticket but price = 0 *)
  let* operation =
    let price = {id = ticket_hash; amount = Z.zero} in
    Op.zk_rollup_publish
      (I i)
      contract1
      ~zk_rollup
      ~ops:[({op with price}, Some ticket)]
  in
  let* (_i : Incremental.t) =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Zk_rollup.Errors.Invalid_deposit_amount)
      i
      operation
  in
  (* None ticket, price < 0 *)
  let* operation =
    let price = {id = ticket_hash; amount = Z.of_string "-10"} in
    Op.zk_rollup_publish
      (I i)
      contract1
      ~zk_rollup
      ~ops:[no_ticket {op with price}]
  in
  let* (_i : Incremental.t) =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Zk_rollup.Errors.Invalid_deposit_amount)
      i
      operation
  in
  (* Some ticket, price < 0, op.price ≠ hash(ticket, zkru) *)
  let* operation =
    let price =
      {
        id = Ticket_hash.of_bytes_exn (Bytes.create 32);
        amount = Z.of_string "-10";
      }
    in
    Op.zk_rollup_publish
      (I i)
      contract1
      ~zk_rollup
      ~ops:[({op with price}, Some ticket)]
  in
  let* (_i : Incremental.t) =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Zk_rollup.Errors.Invalid_deposit_ticket)
      i
      operation
  in
  return_unit

let assert_ticket_balance ~loc incr token owner expected =
  let ctxt = Incremental.alpha_ctxt incr in
  Ticket_balance_key.of_ex_token ctxt ~owner token >>=?? fun (key_hash, ctxt) ->
  Ticket_balance.get_balance ctxt key_hash >>=?? fun (balance, _) ->
  match (balance, expected) with
  | Some b, Some e -> Assert.equal_int ~loc (Z.to_int b) e
  | Some b, None ->
      failwith "%s: Expected no balance but got some %d" loc (Z.to_int b)
  | None, Some b -> failwith "%s: Expected balance %d but got none" loc b
  | None, None -> return ()

let test_invalid_deposit () =
  let open Lwt_result_syntax in
  (* Create 2 accounts and one zk rollups *)
  let* block, contracts, zk_rollup = init_and_originate 5 in
  let contract0 = Stdlib.List.nth contracts 0 in
  let contract1 = Stdlib.List.nth contracts 1 in
  let contract2 = Stdlib.List.nth contracts 2 in
  let contract3 = Stdlib.List.nth contracts 3 in
  let contract4 = Stdlib.List.nth contracts 4 in
  (* Create and originate the deposit contract *)
  let module Nat_ticket = Nat_ticket (struct
    let contents = 1
  end) in
  let* deposit_contract, _script, block =
    Nat_ticket.init_deposit_contract (Z.of_int 10) block contract0
  in
  let token = Nat_ticket.ex_token ~ticketer:deposit_contract in
  (* Generate ticket created by deposit contract and owned by rollup *)
  let* ticket_hash =
    Nat_ticket.ticket_hash (B block) ~ticketer:deposit_contract ~zk_rollup
  in
  let pkh = match contract0 with Implicit pkh -> pkh | _ -> assert false in
  (* ----- Start generating block *)
  let* i = Incremental.begin_construction block in
  (* check rollup exists with none of these particular tokens *)
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) None
  in
  (* ----- op.price = 0 *)
  let zk_op =
    {
      (false_op pkh zk_rollup) with
      price = {id = ticket_hash; amount = Z.of_int 0};
    }
  in
  let* operation =
    Nat_ticket.deposit_op
      ~block
      ~zk_rollup
      ~zk_op
      ~account:contract0
      ~deposit_contract
  in
  let* i =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Zk_rollup.Errors.Invalid_deposit_amount)
      i
      operation
  in
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) None
  in
  (* ----- hash(ticket, zkru) <> op.price *)
  let zk_op =
    {
      (false_op pkh zk_rollup) with
      price =
        {id = Ticket_hash.of_bytes_exn (Bytes.create 32); amount = Z.of_int 10};
    }
  in
  let* operation =
    Nat_ticket.deposit_op
      ~block
      ~zk_rollup
      ~zk_op
      ~account:contract1
      ~deposit_contract
  in
  let* i =
    Incremental.add_operation
      i
      ~expect_apply_failure:
        (check_proto_error Zk_rollup.Errors.Invalid_deposit_ticket)
      operation
  in
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) None
  in
  (* ----- op.price <> ticket amount *)
  let zk_op =
    {
      (false_op pkh zk_rollup) with
      price = {id = ticket_hash; amount = Z.of_int 12};
    }
  in
  let* operation =
    Nat_ticket.deposit_op
      ~block
      ~zk_rollup
      ~zk_op
      ~account:contract2
      ~deposit_contract
  in
  let* i =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Zk_rollup.Errors.Invalid_deposit_amount)
      i
      operation
  in
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) None
  in
  (* ----- ticket amount = 0 *)
  let* deposit_contract, _script, block =
    Nat_ticket.init_deposit_contract (Z.of_int 0) block contract0
  in
  (* Create append/deposit operation with ticket *)
  let zk_op =
    {(false_op pkh zk_rollup) with price = {id = ticket_hash; amount = Z.zero}}
  in
  let* operation =
    Nat_ticket.deposit_op
      ~block
      ~zk_rollup
      ~zk_op
      ~account:contract3
      ~deposit_contract
  in
  let* i = Incremental.begin_construction block in
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) None
  in
  let* i =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error_f (function
            | Script_interpreter.Runtime_contract_error _ -> true
            | _ -> false))
      i
      operation
  in

  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) None
  in
  (* ----- ticket size > Constants.tx_rollup_max_ticket_payload_size *)
  (* Contents size is such that, together with the ticketer address,
     they exceed the maximum size of an operation *)
  let contents_size = 15_000 in
  let module String_ticket = String_ticket (struct
    let contents = String.make contents_size 'a'
  end) in
  let* deposit_contract, _script, block =
    String_ticket.init_deposit_contract (Z.of_int 10) block contract0
  in
  let* ticket_hash =
    String_ticket.ticket_hash (B block) ~ticketer:deposit_contract ~zk_rollup
  in
  let token = String_ticket.ex_token ~ticketer:deposit_contract in
  (* Create append/deposit operation with ticket *)
  let zk_op =
    {
      (false_op pkh zk_rollup) with
      price = {id = ticket_hash; amount = Z.of_int 10};
    }
  in
  let* operation =
    String_ticket.deposit_op
      ~block
      ~zk_rollup
      ~zk_op
      ~account:contract4
      ~deposit_contract
  in
  let* i = Incremental.begin_construction block in
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) None
  in
  let* limit =
    let* constants = Context.get_constants (I i) in
    constants.parametric.tx_rollup.max_ticket_payload_size |> return
  in
  let* (_i : Incremental.t) =
    let payload_size = Saturation_repr.safe_int (contents_size + 216) in
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error
           (Zk_rollup.Errors.Ticket_payload_size_limit_exceeded
              {payload_size; limit}))
      i
      operation
  in
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) None
  in
  return_unit

(* Test for a valid update:
   - 1 private batch of 10 "true" operations
   - 1 public "false" operation
   On a ZKRU with the initial state and a pending list with
   1 operation ("false").
*)
let test_update () =
  let open Lwt_result_syntax in
  let* b, contracts, zk_rollup, pkh = init_with_pending 1 in
  let contract = Stdlib.List.hd contracts in
  let* i = Incremental.begin_construction b in
  let n_batches = 2 in
  let _, update =
    Operator.(
      craft_update
        init_state
        ~zk_rollup
        ~private_ops:
          (Stdlib.List.init n_batches (fun batch ->
               Stdlib.List.init batch_size
               @@ Fun.const
               @@ (if batch mod 2 = 0 then true_op else false_op) pkh zk_rollup))
        [false_op pkh zk_rollup])
  in
  let* operation = Op.zk_rollup_update (I i) contract ~zk_rollup ~update in
  let* _i = Incremental.add_operation i operation in
  return_unit

(* Test for an invalid update:
   - 1 public "true" operation
   On a ZKRU with the initial state and a pending list with
   1 operation ("false").
   The public operation proved is different from the one in the pending list.
*)
let test_update_false_proof () =
  let open Lwt_result_syntax in
  let* b, contracts, zk_rollup, pkh = init_with_pending 1 in
  let contract = Stdlib.List.hd contracts in
  let* i = Incremental.begin_construction b in
  (* Testing with proof on incorrect statement *)
  let _, update =
    Operator.(craft_update init_state ~zk_rollup [true_op pkh zk_rollup])
  in
  let* operation = Op.zk_rollup_update (I i) contract ~zk_rollup ~update in
  let* _i =
    Incremental.add_operation
      i
      ~expect_apply_failure:
        (check_proto_error Zk_rollup.Errors.Invalid_verification)
      operation
  in
  (* Testing with proof with incorrect private inputs *)
  let update =
    let _, Zk_rollup.Update.{pending_pis; private_pis; fee_pi; proof} =
      Operator.(craft_update init_state ~zk_rollup [true_op pkh zk_rollup])
    in
    let private_pis = List.rev private_pis in
    Zk_rollup.Update.{pending_pis; private_pis; fee_pi; proof}
  in
  let* operation = Op.zk_rollup_update (I i) contract ~zk_rollup ~update in
  let* _i =
    Incremental.add_operation
      i
      ~expect_apply_failure:
        (check_proto_error Zk_rollup.Errors.Invalid_verification)
      operation
  in
  return_unit

(* Test for an invalid update:
   A set of inputs for a public circuit is included in the list of
   inputs for private batches.
*)
let test_update_public_in_private () =
  let open Lwt_result_syntax in
  let* b, contracts, zk_rollup, pkh = init_with_pending 1 in
  let contract = Stdlib.List.hd contracts in
  let* i = Incremental.begin_construction b in
  let _, update =
    Operator.(craft_update init_state ~zk_rollup [true_op pkh zk_rollup])
  in
  let update =
    (* Circuit ID and inputs for a public circuit, which will be added to the
       [private_pis] list *)
    let name, op_pi = Stdlib.List.hd update.pending_pis in
    {
      update with
      private_pis =
        (name, {new_state = op_pi.new_state; fees = op_pi.fee})
        :: update.private_pis;
    }
  in
  let* operation = Op.zk_rollup_update (I i) contract ~zk_rollup ~update in
  let* _i =
    Incremental.add_operation
      i
      ~expect_apply_failure:(check_proto_error Zk_rollup.Errors.Invalid_circuit)
      operation
  in
  return_unit

(* Test for an invalid update:
   Two ZKRUs are originated: [zk_rollup1] and [zk_rollup2].
   An L2 [op] for [zk_rollup2] is appended to [zk_rollup1]'s pending list.
   This operation must be discarded, but a malicious validator tries to process
   it by making a proof for an update in which [op]'s [rollup_id] is changed
   from [zk_rollup2] to [zk_rollup1]. The verification must fail, because
   the Protocol uses the actual [op] from the pending list as input.
*)
let test_update_for_another_rollup () =
  let open Lwt_result_syntax in
  let _prover_pp, public_parameters = Lazy.force Operator.lazy_pp in
  let* b, contracts, zk_rollup1, pkh = init_with_pending 3 in
  let contract0 = Stdlib.List.hd contracts in
  let contract1 = Stdlib.List.nth contracts 1 in
  let contract2 = Stdlib.List.nth contracts 2 in
  let* i = Incremental.begin_construction b in
  (* Originate [zk_rollup2] *)
  let* operation, zk_rollup2 =
    Op.zk_rollup_origination
      (I i)
      contract0
      ~public_parameters
      ~circuits_info:(of_plonk_smap Operator.circuits)
      ~init_state:Operator.init_state
      ~nb_ops:1
  in
  let* i = Incremental.add_operation i operation in
  (* Append to [zk_rollup1] an op for [zk_rollup2] *)
  let* operation =
    Op.zk_rollup_publish
      (I i)
      contract1
      ~zk_rollup:zk_rollup1
      ~ops:[no_ticket @@ true_op pkh zk_rollup2]
  in
  let* i = Incremental.add_operation i operation in
  (* Craft the update, changing the "true" op to have zk_rollup1 as
     [rollup_id] *)
  let _, update =
    Operator.(
      craft_update
        init_state
        ~zk_rollup:zk_rollup1
        [false_op pkh zk_rollup1; true_op pkh zk_rollup1])
  in
  let* operation =
    Op.zk_rollup_update (I i) contract2 ~zk_rollup:zk_rollup1 ~update
  in
  let* _i =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Zk_rollup.Errors.Invalid_verification)
      i
      operation
  in
  return_unit

(* Test for an invalid update:
   The update sent by the prover processes more public operations than
   those in the pending list.
*)
let test_update_more_public_than_pending () =
  let open Lwt_result_syntax in
  (* test with number of pending operations < min_pending_to_process. *)
  let* b, contracts, zk_rollup, pkh = init_with_pending 1 in
  let contract = Stdlib.List.hd contracts in
  let* i = Incremental.begin_construction b in
  let _, update =
    Operator.(
      craft_update
        init_state
        ~zk_rollup
        [false_op pkh zk_rollup; true_op pkh zk_rollup])
  in
  let* operation = Op.zk_rollup_update (I i) contract ~zk_rollup ~update in
  let* _i =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Zk_rollup_storage.Zk_rollup_pending_list_too_short)
      i
      operation
  in
  (* test with number of pending operations >= min_pending_to_process. *)
  let* constants = Context.get_constants (I i) in
  let min_pending_to_process =
    constants.parametric.zk_rollup.min_pending_to_process
  in
  let* b, contracts, zk_rollup, pkh =
    init_with_pending ~n_pending:min_pending_to_process 1
  in
  let contract = Stdlib.List.hd contracts in
  let* i = Incremental.begin_construction b in
  let _, update =
    Operator.(
      craft_update
        init_state
        ~zk_rollup
        (Stdlib.List.init (min_pending_to_process + 1) (fun i ->
             if i mod 2 = 0 then false_op pkh zk_rollup
             else true_op pkh zk_rollup)))
  in
  let* operation = Op.zk_rollup_update (I i) contract ~zk_rollup ~update in
  let* _i =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Zk_rollup_storage.Zk_rollup_pending_list_too_short)
      i
      operation
  in
  return_unit

(* Test for an invalid update:
   The update sent by the prover contains a set of circuit inputs in which
   the [new_state] is larger than the ZKRU's [state_length].
*)
let test_update_inconsistent_state () =
  let open Lwt_result_syntax in
  let* b, contracts, zk_rollup, pkh = init_with_pending 1 in
  let contract = Stdlib.List.hd contracts in
  let* i = Incremental.begin_construction b in
  let _, update =
    Operator.(craft_update init_state ~zk_rollup [false_op pkh zk_rollup])
  in
  let open Zk_rollup.Update in
  let update =
    {
      update with
      pending_pis =
        List.map
          (fun (s, (op_pi : op_pi)) ->
            ( s,
              {
                op_pi with
                new_state = Array.append op_pi.new_state op_pi.new_state;
              } ))
          update.pending_pis;
    }
  in
  let* operation = Op.zk_rollup_update (I i) contract ~zk_rollup ~update in
  let* _i =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Zk_rollup.Errors.Inconsistent_state_update)
      i
      operation
  in
  return_unit

(* Test for an invalid update:
   The update sent by the prover processes fewer pending operations (p.o.) than
   allowed (the exact number of p.o. or at least min_pending_to_process).
   The pending list has a length of 2, while only 1 is processed.
*)
let test_update_not_enough_pending () =
  let open Lwt_result_syntax in
  let* b, contracts, zk_rollup, pkh = init_with_pending ~n_pending:2 1 in
  let contract = Stdlib.List.hd contracts in
  let* i = Incremental.begin_construction b in
  let _, update =
    Operator.(craft_update init_state ~zk_rollup [false_op pkh zk_rollup])
  in
  let* operation = Op.zk_rollup_update (I i) contract ~zk_rollup ~update in
  let* _i =
    Incremental.add_operation
      ~expect_apply_failure:(check_proto_error Zk_rollup.Errors.Pending_bound)
      i
      operation
  in
  return_unit

(* Test for a valid update:
   The update sent by the prover processes a prefix of the pending list,
   of the minimum length allowed.
*)
let test_update_valid_prefix () =
  let open Lwt_result_syntax in
  (* Checking when pending list has more than min_pending_to_process *)
  let min_pending_to_process =
    Context.default_test_constants.zk_rollup.min_pending_to_process
  in
  let* b, contracts, zk_rollup, pkh =
    init_with_pending ~n_pending:(min_pending_to_process + 1) 1
  in
  let contract = Stdlib.List.hd contracts in
  let* i = Incremental.begin_construction b in
  let _, update =
    Operator.(
      craft_update
        init_state
        ~zk_rollup
        (Stdlib.List.init min_pending_to_process (fun i ->
             if i mod 2 = 0 then false_op pkh zk_rollup
             else true_op pkh zk_rollup)))
  in
  (* Checking when pending list has less than min_pending_to_process *)
  let* operation = Op.zk_rollup_update (I i) contract ~zk_rollup ~update in
  let* _i = Incremental.add_operation i operation in
  let* b, contracts, zk_rollup, pkh = init_with_pending ~n_pending:2 1 in
  let contract = Stdlib.List.hd contracts in
  let* i = Incremental.begin_construction b in
  let _, update =
    Operator.(
      craft_update
        init_state
        ~zk_rollup
        [false_op pkh zk_rollup; true_op pkh zk_rollup])
  in
  let* operation = Op.zk_rollup_update (I i) contract ~zk_rollup ~update in
  let* _i = Incremental.add_operation i operation in
  return_unit

let test_valid_deposit_and_withdrawal () =
  let open Lwt_result_syntax in
  (* Create 2 accounts and one zk rollups *)
  let* block, contracts, zk_rollup = init_and_originate 2 in
  let contract0 = Stdlib.List.nth contracts 0 in
  let contract1 = Stdlib.List.nth contracts 1 in
  (* Create and originate the deposit contract *)
  let module Nat_ticket = Nat_ticket (struct
    let contents = 1
  end) in
  let* deposit_contract, _script, block =
    Nat_ticket.init_deposit_contract (Z.of_int 10) block contract0
  in
  let token = Nat_ticket.ex_token ~ticketer:deposit_contract in
  (* Generate ticket created by deposit contract and owned by rollup *)
  let* ticket_hash =
    Nat_ticket.ticket_hash (B block) ~ticketer:deposit_contract ~zk_rollup
  in
  let pkh = match contract0 with Implicit pkh -> pkh | _ -> assert false in
  (* Create append/deposit operation with ticket *)
  let zk_op =
    {
      (false_op pkh zk_rollup) with
      price = {id = ticket_hash; amount = Z.of_int 10};
    }
  in
  let* operation =
    Nat_ticket.deposit_op
      ~block
      ~zk_rollup
      ~zk_op
      ~account:contract0
      ~deposit_contract
  in
  (* ----- Start generating block *)
  let* i = Incremental.begin_construction block in
  (* check rollup exists with none of these particular tokens *)
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) None
  in
  (* ----- Add deposit operation to block*)
  let* i = Incremental.add_operation i operation in
  (* check *rollup* has 10 of these particular tokens (deposit has been processed) *)
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) (Some 10)
  in
  (* check *contract* has no tokens (deposit has been processed) *)
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Contract contract0) None
  in
  (* Create update operation to process the zk operation (which is a
     "deposit-withdrawal" for dummy rollup) *)
  let _, update =
    Operator.(craft_update init_state ~zk_rollup ~private_ops:[] [zk_op])
  in
  let* operation = Op.zk_rollup_update (I i) contract1 ~zk_rollup ~update in
  (* ----- Add update operation to block) *)
  let* i = Incremental.add_operation i operation in
  (* check *rollup* has no tokens (deposit was withdrawn) *)
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) None
  in
  (* check *contract* has 10 of these particular tokens (deposit was withdrawn) *)
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Contract contract0) (Some 10)
  in
  return_unit

let test_valid_deposit_and_external_withdrawal () =
  let open Lwt_result_syntax in
  (* Create 2 accounts and one zk rollups *)
  let* block, contracts, zk_rollup = init_and_originate 4 in
  let contract0 = Stdlib.List.nth contracts 0 in
  let contract1 = Stdlib.List.nth contracts 1 in
  let contract2 = Stdlib.List.nth contracts 2 in
  let contract3 = Stdlib.List.nth contracts 3 in
  (* Create and originate the deposit contract *)
  let module Nat_ticket = Nat_ticket (struct
    let contents = 1
  end) in
  let* deposit_contract, _script, block =
    Nat_ticket.init_deposit_contract (Z.of_int 10) block contract0
  in
  let token = Nat_ticket.ex_token ~ticketer:deposit_contract in
  (* Generate ticket created by deposit contract and owned by rollup *)
  let* ticket_hash =
    Nat_ticket.ticket_hash (B block) ~ticketer:deposit_contract ~zk_rollup
  in
  let pkh = match contract0 with Implicit pkh -> pkh | _ -> assert false in
  (* Create append/deposit operation with ticket *)
  let zk_op =
    {
      (false_op pkh zk_rollup) with
      price = {id = ticket_hash; amount = Z.of_int 10};
    }
  in
  let* operation =
    Nat_ticket.deposit_op
      ~block
      ~zk_rollup
      ~zk_op
      ~account:contract0
      ~deposit_contract
  in
  (* ----- Start generating block *)
  let* i = Incremental.begin_construction block in
  (* check rollup exists with none of these particular tokens *)
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) None
  in
  (* ----- Add deposit operation to block*)
  let* i = Incremental.add_operation i operation in
  (* check *rollup* has 10 of these particular tokens (deposit has been processed) *)
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) (Some 10)
  in
  (* check *contract* has no tokens (deposit has been processed) *)
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Contract contract0) None
  in
  (* Create update operation to process the zk operation (which is a
     "deposit" for dummy rollup) *)
  let s, update =
    Operator.(
      craft_update
        init_state
        ~zk_rollup
        ~private_ops:[]
        ~exit_validities:[false]
        [zk_op])
  in
  let* operation = Op.zk_rollup_update (I i) contract1 ~zk_rollup ~update in
  (* ----- Add update operation to block) *)
  let* i = Incremental.add_operation i operation in
  (* check *rollup* has 10 of these particular tokens (deposit has been processed) *)
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) (Some 10)
  in
  (* Create withdrawal operation with ticket *)
  let zk_op =
    {
      (false_op pkh zk_rollup) with
      price = {id = ticket_hash; amount = Z.of_int (-10)};
    }
  in
  let ticket = Nat_ticket.zkru_ticket ~ticketer:deposit_contract in
  let* operation =
    Op.zk_rollup_publish (I i) contract2 ~zk_rollup ~ops:[(zk_op, Some ticket)]
  in
  let* i = Incremental.add_operation i operation in
  (* Create update to process the withdrawal *)
  let _, update =
    Operator.(
      craft_update s ~zk_rollup ~private_ops:[] ~exit_validities:[true] [zk_op])
  in
  let* operation = Op.zk_rollup_update (I i) contract3 ~zk_rollup ~update in
  let* i = Incremental.add_operation i operation in
  (* check *rollup* has no tokens *)
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Zk_rollup zk_rollup) None
  in
  (* check *contract* has 10 of these particular tokens *)
  let* () =
    assert_ticket_balance ~loc:__LOC__ i token (Contract contract0) (Some 10)
  in
  return_unit

let tests =
  [
    Tztest.tztest
      "check feature flag is disabled"
      `Quick
      test_disable_feature_flag;
    Tztest.tztest "origination fees" `Quick test_origination_fees;
    Tztest.tztest "originate two rollups" `Quick test_originate_two_rollups;
    Tztest.tztest
      "origination negative nb_ops"
      `Quick
      test_origination_negative_nb_ops;
    Tztest.tztest
      "append with invalid op code"
      `Quick
      test_append_out_of_range_op_code;
    Tztest.tztest "append external deposit" `Quick test_append_external_deposit;
    Tztest.tztest "append check errors" `Quick test_append_errors;
    Tztest.tztest "invalid deposit" `Quick test_invalid_deposit;
    Tztest.tztest "update" `Quick test_update;
    Tztest.tztest "update with false proof" `Quick test_update_false_proof;
    Tztest.tztest
      "update with invalid circuit"
      `Quick
      test_update_public_in_private;
    Tztest.tztest
      "update with op for another rollup"
      `Quick
      test_update_for_another_rollup;
    Tztest.tztest
      "update with more public operations than pending"
      `Quick
      test_update_more_public_than_pending;
    Tztest.tztest
      "update with inconsistent state"
      `Quick
      test_update_inconsistent_state;
    Tztest.tztest
      "update with not enough pending"
      `Quick
      test_update_not_enough_pending;
    Tztest.tztest "update with valid prefix" `Quick test_update_valid_prefix;
    Tztest.tztest "valid deposit" `Quick test_valid_deposit_and_withdrawal;
    Tztest.tztest
      "valid deposit and external withdrawal"
      `Quick
      test_valid_deposit_and_external_withdrawal;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("zk rollup", tests)]
  |> Lwt_main.run
