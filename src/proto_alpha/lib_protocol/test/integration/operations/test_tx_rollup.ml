(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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
    Component:    Tx rollup layer 1 logic
    Invocation:   cd src/proto_alpha/lib_protocol/test/integration/operations \
                  && dune exec ./main.exe -- test "^tx rollup$"
    Subject:      Test rollup
*)

open Protocol
open Alpha_context
open Test_tez

(** [check_tx_rollup_exists ctxt tx_rollup] returns [()] iff [tx_rollup]
    is a valid address for a transaction rollup. Otherwise, it fails. *)
let check_tx_rollup_exists ctxt tx_rollup =
  Context.Tx_rollup.state ctxt tx_rollup >|=? fun _ -> ()

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

let is_implicit_exn x =
  match Alpha_context.Contract.is_implicit x with
  | Some x -> x
  | None -> raise (Invalid_argument "is_implicit_exn")

(** [test_disable_feature_flag] try to originate a tx rollup with the feature
    flag is deactivated and check it fails *)
let test_disable_feature_flag () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_origination (I i) contract >>=? fun (op, _tx_rollup) ->
  Incremental.add_operation
    ~expect_apply_failure:(check_proto_error Apply.Tx_rollup_feature_disabled)
    i
    op
  >>=? fun _i -> return_unit

(** [parsing_tests] try originating contracts using the
    type [tx_rollup_l2_address], test that it only works
    when rollups are enabled.
 *)
let parsing_tests =
  let test_origination ~tx_rollup_enable script_path initial_storage =
    Context.init1 ~tx_rollup_enable () >>=? fun (b, contract) ->
    Contract_helpers.originate_contract
      script_path
      initial_storage
      contract
      b
      (is_implicit_exn contract)
    >>= fun res ->
    if not tx_rollup_enable then
      Assert.error ~loc:__LOC__ res (function
          | Environment.Ecoproto_error
              (Script_tc_errors.Tx_rollup_addresses_disabled _) ->
              true
          | _ -> false)
    else
      match res with
      | Ok _ -> return_unit
      | Error err ->
          Alcotest.fail
            (Format.asprintf
               "Unexpected failure when parsing %s: %a"
               script_path
               pp_print_trace
               err)
  in
  List.concat_map
    (fun (description, path) ->
      [
        Tztest.tztest
          (Format.asprintf
             "Originating `%s` succeeds w/ tx rollups enabled"
             description)
          `Quick
          (fun () -> test_origination ~tx_rollup_enable:true path "Unit");
        Tztest.tztest
          (Format.asprintf
             "Originating `%s` fails w/ tx rollups disabled"
             description)
          `Quick
          (fun () -> test_origination ~tx_rollup_enable:false path "Unit");
      ])
    [
      ("deposit", "contracts/tx_rollup_deposit.tz");
      ("type", "contracts/tx_rollup_parse_type.tz");
      ("comparable_type", "contracts/tx_rollup_parse_comparable_type.tz");
      ("data", "contracts/tx_rollup_parse_data.tz");
    ]

let message_hash_testable : Tx_rollup_message.hash Alcotest.testable =
  Alcotest.testable Tx_rollup_message.pp_hash ( = )

let sint_testable : _ Saturation_repr.t Alcotest.testable =
  Alcotest.testable Saturation_repr.pp ( = )

let tx_rollup_state_testable : Tx_rollup_state.t Alcotest.testable =
  Alcotest.testable Tx_rollup_state.pp ( = )

let zestable : Z.t Alcotest.testable = Alcotest.testable Z.pp_print Z.equal

(** [tx_rollup_state_testable_no_storage] compares two
   [Tx_rollup_state.t], but ignores differences in the fields
   [allocated_storage] and [occupied_storage_size] like *)
let tx_rollup_state_testable_no_storage : Tx_rollup_state.t Alcotest.testable =
  let copy_storage ~state_from state =
    let open Tx_rollup_state.Internal_for_tests in
    state
    |> set_allocated_storage (get_allocated_storage state_from)
    |> set_occupied_storage (get_occupied_storage state_from)
  in
  Alcotest.testable Tx_rollup_state.pp (fun a b ->
      a = copy_storage ~state_from:a b)

let wrap = Environment.wrap_tzresult

let wrap_lwt m = m >|= wrap

(** [occupied_storage_size ctxt tx_rollup] returns occupied storage size *)
let occupied_storage_size ctxt tx_rollup =
  Context.Tx_rollup.state ctxt tx_rollup >|=? fun state ->
  Alpha_context.Tx_rollup_state.Internal_for_tests.get_occupied_storage state

(** [inbox_burn state size] computes the burn (per byte of message)
    one has to pay to submit a message to the current inbox. *)
let inbox_burn state size =
  Environment.wrap_tzresult (Tx_rollup_state.burn_cost ~limit:None state size)

(** [burn_per_byte state] returns the cost to insert one byte inside
    the inbox. *)
let burn_per_byte state = inbox_burn state 1

(** [context_init n] initializes a context with no consensus rewards
    to not interfere with balances prediction. It returns the created
    context and [n] contracts. *)
let context_init ?(tx_rollup_max_inboxes_count = 2100)
    ?(tx_rollup_rejection_max_proof_size = 30_000)
    ?(tx_rollup_max_ticket_payload_size = 10_240)
    ?(tx_rollup_finality_period = 1) ?(tx_rollup_origination_size = 60_000)
    ?(cost_per_byte = Tez.zero) n =
  Context.init_with_constants
    {
      Context.default_test_constants with
      consensus_threshold = 0;
      tx_rollup_enable = true;
      tx_rollup_finality_period;
      tx_rollup_withdraw_period = 1;
      tx_rollup_max_commitments_count = 3;
      tx_rollup_origination_size;
      tx_rollup_rejection_max_proof_size;
      tx_rollup_max_inboxes_count;
      endorsing_reward_per_slot = Tez.zero;
      baking_reward_bonus_per_slot = Tez.zero;
      baking_reward_fixed_portion = Tez.zero;
      tx_rollup_max_ticket_payload_size;
      cost_per_byte;
    }
    n

(** [context_init1] initializes a context with no consensus rewards
    to not interfere with balances prediction. It returns the created
    context and 1 contract. *)
let context_init1 ?tx_rollup_max_inboxes_count
    ?tx_rollup_max_ticket_payload_size ?tx_rollup_rejection_max_proof_size
    ?tx_rollup_finality_period ?tx_rollup_origination_size ?cost_per_byte () =
  context_init
    ?tx_rollup_max_inboxes_count
    ?tx_rollup_max_ticket_payload_size
    ?tx_rollup_rejection_max_proof_size
    ?tx_rollup_finality_period
    ?tx_rollup_origination_size
    ?cost_per_byte
    1
  >|=? function
  | (b, contract_1 :: _) -> (b, contract_1)
  | (_, _) -> assert false

(** [context_init2] initializes a context with no consensus rewards
    to not interfere with balances prediction. It returns the created
    context and 2 contracts. *)
let context_init2 ?tx_rollup_max_inboxes_count
    ?tx_rollup_max_ticket_payload_size ?cost_per_byte () =
  context_init
    ?tx_rollup_max_inboxes_count
    ?tx_rollup_max_ticket_payload_size
    ?cost_per_byte
    2
  >|=? function
  | (b, contract_1 :: contract_2 :: _) -> (b, contract_1, contract_2)
  | (_, _) -> assert false

(** [originate b contract] originates a tx_rollup from [contract],
    and returns the new block and the tx_rollup address. *)
let originate b contract =
  Op.tx_rollup_origination (B b) contract >>=? fun (operation, tx_rollup) ->
  Block.bake ~operation b >>=? fun b -> return (b, tx_rollup)

(** Initializes the context, originates a tx_rollup and submits a batch.

    Returns the first contract and its balance, the originated tx_rollup,
    the state with the tx_rollup, and the baked block with the batch submitted.
*)
let init_originate_and_submit ?(batch = String.make 5 'c')
    ?tx_rollup_origination_size () =
  context_init1 ?tx_rollup_origination_size () >>=? fun (b, contract) ->
  originate b contract >>=? fun (b, tx_rollup) ->
  Context.Contract.balance (B b) contract >>=? fun balance ->
  Op.tx_rollup_submit_batch (B b) contract tx_rollup batch >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Context.Tx_rollup.state (B b) tx_rollup >>=? fun state ->
  return ((contract, balance), state, tx_rollup, b)

let l2_parameters : Context.t -> Tx_rollup_l2_apply.parameters tzresult Lwt.t =
 fun ctxt ->
  Context.get_constants ctxt >>=? fun constants ->
  let tx_rollup_max_withdrawals_per_batch =
    constants.parametric.tx_rollup_max_withdrawals_per_batch
  in
  return Tx_rollup_l2_apply.{tx_rollup_max_withdrawals_per_batch}

let commitment_testable =
  Alcotest.testable Tx_rollup_commitment.pp Tx_rollup_commitment.( = )

let commitment_hash_testable =
  Alcotest.testable Tx_rollup_commitment_hash.pp Tx_rollup_commitment_hash.( = )

let public_key_hash_testable =
  Alcotest.testable Signature.Public_key_hash.pp Signature.Public_key_hash.( = )

let raw_level_testable = Alcotest.testable Raw_level.pp Raw_level.( = )

let inbox_testable = Alcotest.testable Tx_rollup_inbox.pp Tx_rollup_inbox.( = )

let rng_state = Random.State.make_self_init ()

let gen_l2_account () =
  let seed =
    Bytes.init 32 (fun _ -> char_of_int @@ Random.State.int rng_state 255)
  in
  let secret_key = Bls12_381.Signature.generate_sk seed in
  let public_key = Bls12_381.Signature.MinSig.derive_pk secret_key in
  (secret_key, public_key, Tx_rollup_l2_address.of_bls_pk public_key)

(** [make_ticket_key ty contents ticketer tx_rollup] computes the ticket hash
    of the ticket containing [contents] of type [ty], crafted by [ticketer] and
    owned by [tx_rollup]. *)
let make_ticket_key ctxt ~ty ~contents ~ticketer tx_rollup =
  (match ctxt with
  | Context.B block ->
      Incremental.begin_construction block >>=? fun incr -> return incr
  | Context.I incr -> return incr)
  >>=? fun incr ->
  let ctxt = Incremental.alpha_ctxt incr in
  Environment.wrap_tzresult @@ Script_ir_translator.parse_comparable_ty ctxt ty
  >>?= fun (Ex_comparable_ty contents_type, ctxt) ->
  wrap_lwt
  @@ Script_ir_translator.parse_comparable_data ctxt contents_type contents
  >>=? fun (contents, ctxt) ->
  wrap_lwt
  @@ Ticket_balance_key.of_ex_token
       ctxt
       ~owner:(Tx_rollup tx_rollup)
       (Ticket_token.Ex_token {ticketer; contents_type; contents})
  >|=? fst

(** [make_unit_ticket_key ticketer tx_rollup] computes the ticket hash of
    the unit ticket crafted by [ticketer] and owned by [tx_rollup]. *)
let make_unit_ticket_key ctxt ~ticketer tx_rollup =
  let open Tezos_micheline.Micheline in
  let open Michelson_v1_primitives in
  let ty = Prim (0, T_unit, [], []) in
  let contents = Prim (0, D_Unit, [], []) in
  make_ticket_key ctxt ~ty ~contents ~ticketer tx_rollup

let rng_state = Random.State.make_self_init ()

let print_deposit_arg tx_rollup account =
  let open Alpha_context.Script in
  Format.sprintf
    "Pair \"%s\" %s"
    (match tx_rollup with
    | `Typed pk -> Tx_rollup.to_b58check pk
    | `Raw str -> str)
    (match account with
    | `Hash pk -> Format.sprintf "\"%s\"" (Tx_rollup_l2_address.to_b58check pk)
    | `Raw str -> str)
  |> fun x ->
  Format.printf "%s\n@?" x ;
  x |> Expr.from_string |> lazy_expr

let assert_ok res = match res with Ok r -> r | Error _ -> assert false

let assert_some res = match res with Some r -> r | None -> assert false

let raw_level level = assert_ok @@ Raw_level.of_int32 level

(** Create an incomplete (but valid) commitment for a given level. It
    is incomplete in the sense that the Merkle roots for each message
    are {!Tx_rollup_commitment.empty_l2_context_hash}.  In the meantime
    provides the list of withdraw in a association list of
   [batch_index -> withdraw_list].  Be careful not to provide a too-big
   withdraw_list as the construction is expensive *)
let make_incomplete_commitment_for_batch i level tx_rollup withdraw_list =
  Context.Tx_rollup.inbox (I i) tx_rollup level >>=? fun metadata ->
  List.init ~when_negative_length:[] metadata.inbox_length (fun _ ->
      Tx_rollup_commitment.empty_l2_context_hash)
  >>?= fun batches_result ->
  let messages =
    List.mapi
      (fun i v ->
        Tx_rollup_commitment.hash_message_result
          {
            context_hash = v;
            withdrawals_merkle_root =
              List.assq i withdraw_list |> Option.value ~default:[]
              |> Tx_rollup_withdraw.Merkle.merklize_list;
          })
      batches_result
  in
  (match Tx_rollup_level.pred level with
  | None -> return_none
  | Some predecessor_level ->
      Context.Tx_rollup.commitment (I i) tx_rollup predecessor_level
      >|=? fun commitment_opt ->
      Option.map
        (fun Tx_rollup_commitment.Submitted_commitment.{commitment; _} ->
          Tx_rollup_commitment.hash commitment)
        commitment_opt)
  >>=? fun predecessor ->
  let inbox_merkle_root = metadata.merkle_root in
  let commitment : Tx_rollup_commitment.t =
    {level; messages; predecessor; inbox_merkle_root}
  in
  return (commitment, batches_result)

let check_bond ctxt tx_rollup contract count =
  let pkh = is_implicit_exn contract in
  wrap_lwt (Tx_rollup_commitment.pending_bonded_commitments ctxt tx_rollup pkh)
  >>=? fun (_, pending) ->
  Alcotest.(check int "Pending bonded commitment count correct" count pending) ;
  return ()

let rec bake_until i top =
  let level = Incremental.level i in
  if level >= top then return i
  else
    Incremental.finalize_block i >>=? fun b ->
    Incremental.begin_construction b >>=? fun i -> bake_until i top

let assert_retired retired =
  match retired with
  | `Retired -> return_unit
  | _ -> failwith "Expected retired"

let assert_ticket_balance ~loc block token owner expected =
  Incremental.begin_construction block >>=? fun incr ->
  let ctxt = Incremental.alpha_ctxt incr in
  wrap_lwt @@ Ticket_balance_key.of_ex_token ctxt ~owner token
  >>=? fun (key_hash, ctxt) ->
  wrap_lwt (Ticket_balance.get_balance ctxt key_hash) >>=? fun (balance, _) ->
  match (balance, expected) with
  | (Some b, Some e) -> Assert.equal_int ~loc (Z.to_int b) e
  | (Some b, None) ->
      failwith "%s: Expected no balance but got some %d" loc (Z.to_int b)
  | (None, Some b) -> failwith "%s: Expected balance %d but got none" loc b
  | (None, None) -> return ()

module Nat_ticket = struct
  let ty_str = "nat"

  let ty = Expr.from_string ty_str

  let contents_nat = 1

  let ex_token ~ticketer =
    let contents =
      WithExceptions.Option.get ~loc:__LOC__
      @@ Script_int.(of_int contents_nat |> is_nat)
    in
    Ticket_token.Ex_token
      {ticketer; contents_type = Script_typed_ir.nat_key; contents}

  let contents = Expr.from_string (string_of_int contents_nat)

  let int64_amount = 10L

  let amount = Tx_rollup_l2_qty.of_int64_exn int64_amount

  let ticket_hash ctxt ~ticketer ~tx_rollup =
    make_ticket_key
      ctxt
      ~ty:(Tezos_micheline.Micheline.root ty)
      ~contents:(Tezos_micheline.Micheline.root contents)
      ~ticketer
      tx_rollup

  let withdrawal ctxt ~ticketer ?(claimer = ticketer) ?(amount = amount)
      tx_rollup : Tx_rollup_withdraw.t tzresult Lwt.t =
    ticket_hash ctxt ~ticketer ~tx_rollup >|=? fun ticket_hash ->
    Tx_rollup_withdraw.{claimer = is_implicit_exn claimer; ticket_hash; amount}

  (** Return an operation to originate a contract that will deposit [amount]
      tickets to l2 address [pkh] on [tx_rollup] *)
  let init_deposit amount ?(pkh = "tz4MSfZsn6kMDczShy8PMeB628TNukn9hi2K") block
      tx_rollup account =
    let script =
      Format.asprintf
        {| parameter (pair address tx_rollup_l2_address);
         storage unit;
         code {
                # cast the address to contract type
                CAR;
                UNPAIR;
                CONTRACT %%deposit (pair (ticket nat) tx_rollup_l2_address);
                ASSERT_SOME;
                SWAP;
                PUSH mutez 0;
                SWAP;
                # create a ticket
                PUSH nat %a;
                PUSH %s %d;
                TICKET;
                PAIR ;
                TRANSFER_TOKENS;
                PUSH unit Unit;
                NIL operation;
                DIG 2 ;
                CONS;
                PAIR } |}
        Z.pp_print
        amount
        ty_str
        contents_nat
    in
    Contract_helpers.originate_contract_from_string
      ~baker:(is_implicit_exn account)
      ~source_contract:account
      ~script
      ~storage:"Unit"
      block
    >>=? fun (deposit_contract, _script, block) ->
    Op.transaction
      (B block)
      ~entrypoint:Entrypoint.default
      ~parameters:
        (Script.lazy_expr @@ Expr.from_string
        @@ Printf.sprintf {| Pair %S %S |} (Tx_rollup.to_b58check tx_rollup) pkh
        )
      ~fee:Tez.one
      account
      deposit_contract
      (Tez.of_mutez_exn 0L)
    >|=? fun op -> (op, block, deposit_contract)
end

(** ---- TESTS -------------------------------------------------------------- *)

(** [test_origination] originates a transaction rollup and checks that
    it burns the expected quantity of xtz. *)
let test_origination () =
  Context.init ~tx_rollup_enable:true 1 >>=? fun (b, contracts) ->
  let contract =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  Context.get_constants (B b)
  >>=? fun {parametric = {tx_rollup_origination_size; cost_per_byte; _}; _} ->
  Context.Contract.balance (B b) contract >>=? fun balance ->
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_origination (I i) contract >>=? fun (op, tx_rollup) ->
  Incremental.add_operation i op >>=? fun i ->
  check_tx_rollup_exists (I i) tx_rollup >>=? fun () ->
  cost_per_byte *? Int64.of_int tx_rollup_origination_size
  >>?= fun tx_rollup_origination_burn ->
  Assert.balance_was_debited
    ~loc:__LOC__
    (I i)
    contract
    balance
    tx_rollup_origination_burn

(** [test_two_originations] originates two transaction rollups in the
    same operation and checks that they have a different address. *)
let test_two_originations () =
  Context.init ~tx_rollup_enable:true 1 >>=? fun (b, contracts) ->
  let contract =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_origination (I i) contract >>=? fun (op1, _false_tx_rollup1) ->
  (* tx_rollup1 and tx_rollup2 are equal and both are false. The addresses are
     derived from a value called `origination_nonce` that is dependent of the
     tezos operation hash. Also each origination increment this value.

     Here the origination_nonce is wrong because it's not based on the injected
     operation (the combined one. Also the used origination nonce is not
     incremented between _false_tx_rollup1 and _false_tx_rollup2 as the protocol
     do. *)
  Op.tx_rollup_origination (I i) contract >>=? fun (op2, _false_tx_rollup2) ->
  Op.combine_operations ~source:contract (B b) [op1; op2] >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  let nonce =
    Origination_nonce.Internal_for_tests.initial (Operation.hash_packed op)
  in
  let txo1 = Tx_rollup.Internal_for_tests.originated_tx_rollup nonce in
  let nonce = Origination_nonce.Internal_for_tests.incr nonce in
  let txo2 = Tx_rollup.Internal_for_tests.originated_tx_rollup nonce in
  Assert.not_equal
    ~loc:__LOC__
    Tx_rollup.equal
    "Two transaction rollups originated in one operation have different \
     addresses"
    Tx_rollup.pp
    txo1
    txo2
  >>=? fun () ->
  check_tx_rollup_exists (I i) txo1 >>=? fun () ->
  check_tx_rollup_exists (I i) txo2 >>=? fun () -> return_unit

(** [test_burn_per_byte_update] checks [update_burn_per_byte] behaves
    according to its docstring. *)
let test_burn_per_byte_update () =
  let test ~inbox_ema ~burn_per_byte ~elapsed ~final_size ~hard_limit ~result =
    let burn_per_byte = Tez.of_mutez_exn burn_per_byte in
    let result = Tez.of_mutez_exn result in
    let state =
      Alpha_context.Tx_rollup_state.Internal_for_tests.make
        ~burn_per_byte
        ~inbox_ema
        ~allocated_storage:Z.zero
        ()
    in
    let factor = 120 (* default factor *) in
    let state =
      Alpha_context.Tx_rollup_state.Internal_for_tests.update_burn_per_byte
        state
        ~elapsed
        ~factor
        ~final_size
        ~hard_limit
    in
    let new_burn =
      match Alpha_context.Tx_rollup_state.burn_cost ~limit:None state 1 with
      | Ok x -> x
      | Error _ ->
          Stdlib.failwith "could not compute the fees for a message of 1 byte"
    in
    Assert.equal_tez ~loc:__LOC__ result new_burn
  in
  test
    ~inbox_ema:1_000
    ~burn_per_byte:1_000L
    ~elapsed:0
    ~final_size:1_000
    ~hard_limit:1_100
    ~result:1_000L
  >>=? fun () ->
  test
    ~inbox_ema:1_000
    ~burn_per_byte:1_000L
    ~elapsed:10
    ~final_size:1_000
    ~hard_limit:1_100
    ~result:816L
  >>=? fun () ->
  test
    ~inbox_ema:1_000
    ~burn_per_byte:1_000L
    ~elapsed:25
    ~final_size:1_000
    ~hard_limit:1_100
    ~result:383L
  >>=? fun () ->
  test
    ~inbox_ema:1_000
    ~burn_per_byte:1_000L
    ~elapsed:50
    ~final_size:1_000
    ~hard_limit:1_100
    ~result:113L
  >>=? fun () ->
  test
    ~inbox_ema:1_000
    ~burn_per_byte:1_000L
    ~elapsed:113
    ~final_size:1_000
    ~hard_limit:1_100
    ~result:0L
  >>=? fun () ->
  (* Fees per byte should remain constant *)
  test
    ~inbox_ema:1_000
    ~burn_per_byte:1_000L
    ~elapsed:0
    ~final_size:1_000
    ~hard_limit:1_100
    ~result:1_000L
  >>=? fun () ->
  (* Fees per byte should increase *)
  test
    ~inbox_ema:1_000
    ~burn_per_byte:1_000L
    ~elapsed:0
    ~final_size:1_000
    ~hard_limit:1_000
    ~result:1_050L
  >>=? fun () ->
  (* Fees per byte should decrease *)
  test
    ~inbox_ema:1_000
    ~burn_per_byte:1_000L
    ~elapsed:0
    ~final_size:1_000
    ~hard_limit:1_500
    ~result:950L
  >>=? fun () ->
  (* Fees per byte should increase even with [0] as its initial value *)
  test
    ~inbox_ema:1_000
    ~burn_per_byte:0L
    ~elapsed:0
    ~final_size:1_000
    ~hard_limit:1_000
    ~result:1L
  >>=? fun () -> return_unit

(** [test_add_batch] originates a tx rollup and fills one of its inbox
    with an arbitrary batch of data. *)
let test_add_batch () =
  let contents_size = 5 in
  let contents = String.make contents_size 'c' in
  init_originate_and_submit ~batch:contents ()
  >>=? fun ((contract, balance), state, tx_rollup, b) ->
  Context.Tx_rollup.inbox (B b) tx_rollup Tx_rollup_level.root >>=? fun inbox ->
  let contents_hash =
    Tx_rollup_message.(make_batch contents |> fst |> hash_uncarbonated)
  in
  let merkle_root = Tx_rollup_inbox.Merkle.merklize_list [contents_hash] in
  let expected_inbox =
    Tx_rollup_inbox.
      {inbox_length = 1; cumulated_size = contents_size; merkle_root}
  in
  Alcotest.check
    inbox_testable
    "Expected inbox is not the computed one"
    expected_inbox
    inbox ;
  inbox_burn state contents_size >>?= fun cost ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) contract balance cost

(** [test_storage_burn_for_adding_batch] originates a tx rollup with small [tx_rollup_origination_size],
    fills one of its inbox with an arbitrary batch of data and cause additional storage burn. *)
let test_storage_burn_for_adding_batch () =
  let contents_size = 5 in
  let contents = String.make contents_size 'c' in
  let tx_rollup_origination_size = 1 in
  init_originate_and_submit ~tx_rollup_origination_size ~batch:contents ()
  >>=? fun ((contract, balance), state, _tx_rollup, b) ->
  Context.get_constants (B b) >>=? fun {parametric = {cost_per_byte; _}; _} ->
  let final_allocated_storage =
    Alpha_context.Tx_rollup_state.Internal_for_tests.get_occupied_storage state
  in
  let extra_storage_space =
    Z.(sub final_allocated_storage @@ of_int tx_rollup_origination_size)
  in
  assert (Z.zero < extra_storage_space) ;
  inbox_burn state contents_size >>?= fun cost ->
  cost_per_byte *? Z.to_int64 extra_storage_space >>?= fun storage_burn_cost ->
  cost +? storage_burn_cost >>?= fun cost ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) contract balance cost

let test_add_batch_with_limit () =
  (* From an empty context the burn will be [Tez.zero], we set the hard limit to
     [Tez.zero], so [cost] >= [limit] *)
  let burn_limit = Tez.zero in
  let contents = String.make 5 'd' in
  context_init1 () >>=? fun (b, contract) ->
  originate b contract >>=? fun (b, tx_rollup) ->
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_submit_batch (I i) contract tx_rollup contents ~burn_limit
  >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f (function
          | Tx_rollup_errors.Submit_batch_burn_exceeded _ -> true
          | _ -> false))
  >>=? fun _ -> return_unit

(** [test_add_two_batches] originates a tx rollup and adds two
    arbitrary batches to one of its inboxes. Ensure that their order
    is correct. *)
let test_add_two_batches () =
  (*
    TODO: https://gitlab.com/tezos/tezos/-/issues/2331
    This test can be generalized using a property-based approach.
   *)
  let contents_size1 = 5 in
  let contents1 = String.make contents_size1 'c' in
  init_originate_and_submit ~batch:contents1 ()
  >>=? fun ((contract, balance), state, tx_rollup, b) ->
  Op.tx_rollup_submit_batch (B b) contract tx_rollup contents1 >>=? fun op1 ->
  Context.Contract.counter (B b) contract >>=? fun counter ->
  let contents_size2 = 6 in
  let contents2 = String.make contents_size2 'd' in
  Op.tx_rollup_submit_batch
    ~counter:Z.(add counter (of_int 1))
    (B b)
    contract
    tx_rollup
    contents2
  >>=? fun op2 ->
  Block.bake ~operations:[op1; op2] b >>=? fun b ->
  (* There were a first inbox with one message, and we are looking for
     its successor. *)
  Context.Tx_rollup.inbox (B b) tx_rollup Tx_rollup_level.(succ root)
  >>=? fun inbox ->
  Incremental.begin_construction b >>=? fun _incr ->
  let contents1_hash =
    Tx_rollup_message.hash_uncarbonated
      (Tx_rollup_message.make_batch contents1 |> fst)
  in
  let contents2_hash =
    Tx_rollup_message.hash_uncarbonated
      (Tx_rollup_message.make_batch contents2 |> fst)
  in
  let merkle_root =
    Tx_rollup_inbox.Merkle.merklize_list [contents1_hash; contents2_hash]
  in
  let expected_inbox =
    Tx_rollup_inbox.{inbox_length = 2; cumulated_size = 5 + 6; merkle_root}
  in
  Alcotest.(
    check
      inbox_testable
      "The expected inbox is not the computed one"
      inbox
      expected_inbox) ;
  inbox_burn state expected_inbox.cumulated_size >>?= fun cost ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) contract balance cost

(** Try to add a batch too large in an inbox. *)
let test_batch_too_big () =
  context_init1 () >>=? fun (b, contract) ->
  originate b contract >>=? fun (b, tx_rollup) ->
  Context.get_constants (B b) >>=? fun constant ->
  let contents =
    String.make
      (constant.parametric.tx_rollup_hard_size_limit_per_message + 1)
      'd'
  in
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_submit_batch (I i) contract tx_rollup contents >>=? fun op ->
  Incremental.add_operation
    i
    ~expect_apply_failure:
      (check_proto_error Tx_rollup_errors.Message_size_exceeds_limit)
    op
  >>=? fun _ -> return_unit

(** [fill_inbox b tx_rollup contract contents k] fills the inbox of
    [tx_rollup] with batches containing [contents] sent by [contract].
    Before exceeding the limit size of the inbox, the continuation [k]
    is called with two parameters: the incremental state of the block
    with the almost full inboxes, and an operation that would cause an
    error if applied. *)
let fill_inbox b tx_rollup contract contents k =
  let message_size = String.length contents in
  Context.get_constants (B b) >>=? fun constant ->
  let tx_rollup_inbox_limit =
    constant.parametric.tx_rollup_hard_size_limit_per_inbox
  in
  Context.Contract.counter (B b) contract >>=? fun counter ->
  Incremental.begin_construction b >>=? fun i ->
  let rec fill_inbox i inbox_size counter =
    (* By default, the [gas_limit] is the maximum gas that can be
       consumed by an operation. We set a lower (arbitrary) limit to
       be able to reach the size limit of an operation. *)
    Op.tx_rollup_submit_batch
      ~gas_limit:(Gas.Arith.integral_of_int_exn 100_000)
      ~counter
      (I i)
      contract
      tx_rollup
      contents
    >>=? fun op ->
    let new_inbox_size = inbox_size + message_size in
    if new_inbox_size < tx_rollup_inbox_limit then
      Incremental.add_operation i op >>=? fun i ->
      fill_inbox i new_inbox_size (Z.succ counter)
    else k i inbox_size op
  in

  fill_inbox i 0 counter

(** Try to add enough large batches to reach the size limit of an inbox. *)
let test_inbox_size_too_big () =
  context_init1 () >>=? fun (b, contract) ->
  Context.get_constants (B b) >>=? fun constant ->
  let tx_rollup_batch_limit =
    constant.parametric.tx_rollup_hard_size_limit_per_message - 1
  in
  let contents = String.make tx_rollup_batch_limit 'd' in
  originate b contract >>=? fun (b, tx_rollup) ->
  fill_inbox b tx_rollup contract contents (fun i _ op ->
      Incremental.add_operation
        i
        op
        ~expect_failure:
          (check_proto_error_f (function
              | Tx_rollup_errors.Inbox_size_would_exceed_limit _ -> true
              | _ -> false))
      >>=? fun _i -> return_unit)

(** Try to add enough batches to reach the batch count limit of an inbox. *)
let test_inbox_count_too_big () =
  context_init1 () >>=? fun (b, contract) ->
  let (_, _, pkh) = gen_l2_account () in
  Context.get_constants (B b) >>=? fun constant ->
  let message_count = constant.parametric.tx_rollup_max_messages_per_inbox in
  let contents = "some contents" in
  originate b contract >>=? fun (b, tx_rollup) ->
  Contract_helpers.originate_contract
    "contracts/tx_rollup_deposit.tz"
    "Unit"
    contract
    b
    (is_implicit_exn contract)
  >>=? fun (deposit_contract, b) ->
  Incremental.begin_construction b >>=? fun i ->
  let rec fill_inbox i counter n =
    (* By default, the [gas_limit] is the maximum gas that can be
       consumed by an operation. We set a lower (arbitrary) limit to
       be able to reach the size limit of an operation. *)
    Op.tx_rollup_submit_batch
      ~gas_limit:(Gas.Arith.integral_of_int_exn 2_500)
      ~counter
      (I i)
      contract
      tx_rollup
      contents
    >>=? fun op ->
    if n > 0 then
      Incremental.add_operation i op >>=? fun i ->
      fill_inbox i (Z.succ counter) (n - 1)
    else return (i, counter)
  in
  Context.Contract.counter (B b) contract >>=? fun counter ->
  fill_inbox i counter message_count >>=? fun (i, counter) ->
  Op.tx_rollup_submit_batch
    ~gas_limit:(Gas.Arith.integral_of_int_exn 2_500)
    ~counter
    (I i)
    contract
    tx_rollup
    contents
  >>=? fun op ->
  (* Submitting a new batch to a full inbox fails *)
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f @@ function
       | Tx_rollup_errors.Inbox_count_would_exceed_limit rollup ->
           rollup = tx_rollup
       | _ -> false)
  >>=? fun _i ->
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash pkh) in
  let fee = Test_tez.of_int 10 in
  Op.transaction
    ~counter
    ~fee
    (I i)
    contract
    deposit_contract
    Tez.zero
    ~parameters
  >>=? fun op ->
  (* Submitting a new deposit to a full inbox fails *)
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f @@ function
       | Tx_rollup_errors.Inbox_count_would_exceed_limit rollup ->
           rollup = tx_rollup
       | _ -> false)
  >>=? fun i ->
  ignore i ;
  return ()

(** [test_valid_deposit] checks that a smart contract can deposit
    tickets to a transaction rollup. *)
let test_valid_deposit () =
  let (_, _, pkh) = gen_l2_account () in
  context_init1 () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  Contract_helpers.originate_contract
    "contracts/tx_rollup_deposit.tz"
    "Unit"
    account
    b
    (is_implicit_exn account)
  >>=? fun (contract, b) ->
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash pkh) in
  let fee = Test_tez.of_int 10 in
  Op.transaction
    ~counter:(Z.of_int 2)
    ~fee
    (B b)
    account
    contract
    Tez.zero
    ~parameters
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Context.Tx_rollup.inbox (B b) tx_rollup Tx_rollup_level.root >>=? fun inbox ->
  make_unit_ticket_key (B b) ~ticketer:contract tx_rollup
  >>=? fun ticket_hash ->
  let (message, cumulated_size) =
    Tx_rollup_message.make_deposit
      (is_implicit_exn account)
      (Tx_rollup_l2_address.Indexable.value pkh)
      ticket_hash
      (Tx_rollup_l2_qty.of_int64_exn 10L)
  in
  let merkle_root =
    Tx_rollup_inbox.Merkle.merklize_list
      [Tx_rollup_message.hash_uncarbonated message]
  in
  let expected_inbox =
    Tx_rollup_inbox.{inbox_length = 1; cumulated_size; merkle_root}
  in
  Alcotest.(
    check
      inbox_testable
      "Expected inbox different from the computed one"
      inbox
      expected_inbox) ;
  return_unit

(** [test_additional_space_allocation_for_valid_deposit] originates a tx rollup with small [tx_rollup_origination_size], make a valid deposit and check additional space allocation *)
let test_additional_space_allocation_for_valid_deposit () =
  let (_, _, pkh) = gen_l2_account () in
  let tx_rollup_origination_size = 1 in
  context_init1 ~tx_rollup_origination_size () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  Contract_helpers.originate_contract
    "contracts/tx_rollup_deposit.tz"
    "Unit"
    account
    b
    (is_implicit_exn account)
  >>=? fun (contract, b) ->
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash pkh) in
  let fee = Test_tez.of_int 10 in
  Op.transaction
    ~counter:(Z.of_int 2)
    ~fee
    (B b)
    account
    contract
    Tez.zero
    ~parameters
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  occupied_storage_size (B b) tx_rollup >>=? fun final_allocated_storage ->
  let extra_storage_space =
    Z.(sub final_allocated_storage @@ of_int tx_rollup_origination_size)
  in
  return @@ assert (Z.zero < extra_storage_space)

(** [test_valid_deposit_inexistant_rollup] checks that the Michelson
    interpreter checks the existence of a transaction rollup prior to
    sending a deposit order. *)
let test_valid_deposit_inexistant_rollup () =
  let (_, _, pkh) = gen_l2_account () in
  context_init1 () >>=? fun (b, account) ->
  Contract_helpers.originate_contract
    "contracts/tx_rollup_deposit.tz"
    "Unit"
    account
    b
    (is_implicit_exn account)
  >>=? fun (contract, b) ->
  Incremental.begin_construction b >>=? fun i ->
  let parameters =
    print_deposit_arg (`Raw "txr1UTQm2gtoVJNvJRGfwora8GmM7D5dnEcdb") (`Hash pkh)
  in
  let fee = Test_tez.of_int 10 in
  Op.transaction ~fee (I i) account contract Tez.zero ~parameters >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f (function
          | Script_interpreter.Runtime_contract_error _ -> true
          | _ -> false))
  >>=? fun _ -> return_unit

(** [test_invalid_deposit_not_contract] checks a smart contract cannot
    deposit something that is not a ticket. *)
let test_invalid_deposit_not_ticket () =
  let (_, _, pkh) = gen_l2_account () in

  context_init1 () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  Contract_helpers.originate_contract
    "contracts/tx_rollup_deposit_incorrect_param.tz"
    "Unit"
    account
    b
    (is_implicit_exn account)
  >>=? fun (contract, b) ->
  Incremental.begin_construction b >>=? fun i ->
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash pkh) in
  let fee = Test_tez.of_int 10 in
  Op.transaction ~fee (I i) account contract Tez.zero ~parameters >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f (function
          | Script_interpreter.Bad_contract_parameter _ -> true
          | _ -> false))
  >>=? fun _ -> return_unit

let string_ticket_of_size expected_size =
  if expected_size < 0 && expected_size mod 8 <> 0 then
    Alcotest.fail
      (Format.asprintf
         "string_ticket_of_size: argument [expected_size] must be positive and \
          a multiple of 8") ;
  let ticket_contents_ty =
    Tezos_micheline.Micheline.Prim (0, Michelson_v1_primitives.T_string, [], [])
  in
  let (_, ticket_contents_ty_size) =
    Script_typed_ir_size.node_size ticket_contents_ty
  in
  Alcotest.(
    check
      (option sint_testable)
      "Expected size of ticket_contents type"
      (Saturation_repr.of_int_opt 40)
      (Some ticket_contents_ty_size)) ;
  let (_, empty_string_size) =
    Script_typed_ir_size.node_size (Expr_common.string "")
  in
  let ticket_contents =
    Expr_common.string
      (String.make
         (expected_size
         - Saturation_repr.to_int ticket_contents_ty_size
         - Saturation_repr.to_int empty_string_size)
         'a')
  in
  let (_, ticket_contents_size) =
    Script_typed_ir_size.node_size ticket_contents
  in
  Alcotest.(
    check
      (option sint_testable)
      "Expected size of ticket_contents type + ticket_contents"
      (Saturation_repr.of_int_opt expected_size)
      (Some Saturation_repr.(add ticket_contents_ty_size ticket_contents_size))) ;
  ticket_contents

(** [test_invalid_deposit_too_big_ticket] tests that depositing a ticket that
    has a content whose size exceeds [tx_rollup_max_ticket_payload_size] fails.*)
let test_invalid_deposit_too_big_ticket () =
  let (_, _, pkh) = gen_l2_account () in
  context_init1 () >>=? fun (b, account) ->
  Context.get_constants (B b) >>=? fun constant ->
  let tx_rollup_max_ticket_payload_size =
    constant.parametric.tx_rollup_max_ticket_payload_size
  in
  originate b account >>=? fun (b, tx_rollup) ->
  Contract_helpers.originate_contract
    "contracts/tx_rollup_deposit_string.tz"
    "Unit"
    account
    b
    (is_implicit_exn account)
  >>=? fun (contract, b) ->
  Incremental.begin_construction b >>=? fun i ->
  let ticket_contents =
    string_ticket_of_size (tx_rollup_max_ticket_payload_size + 8)
  in
  let parameters =
    Expr_common.(
      pair_n
        [
          string (Tx_rollup.to_b58check tx_rollup);
          string (Tx_rollup_l2_address.to_b58check pkh);
          ticket_contents;
        ])
    |> Tezos_micheline.Micheline.strip_locations |> Script.lazy_expr
  in
  let fee = Test_tez.of_int 10 in
  Op.transaction
    ~counter:(Z.of_int 2)
    ~fee
    (B b)
    account
    contract
    Tez.zero
    ~parameters
  >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f (function
          | Tx_rollup_errors_repr.Ticket_payload_size_limit_exceeded _ -> true
          | _ -> false))
  >>=? fun _ -> return_unit

(** [test_invalid_deposit_too_big_ticket_type] tests that depositing a
    ticket that has a content and type whose summed size exceeds
    [tx_rollup_max_ticket_payload_size] fails.*)
let test_invalid_deposit_too_big_ticket_type () =
  let (_, _, pkh) = gen_l2_account () in
  context_init1 () >>=? fun (b, account) ->
  Context.get_constants (B b) >>=? fun constant ->
  let tx_rollup_max_ticket_payload_size =
    constant.parametric.tx_rollup_max_ticket_payload_size
  in
  originate b account >>=? fun (b, tx_rollup) ->
  Contract_helpers.originate_contract
    "contracts/tx_rollup_deposit_pair_string.tz"
    "Unit"
    account
    b
    (is_implicit_exn account)
  >>=? fun (contract, b) ->
  Incremental.begin_construction b >>=? fun i ->
  let ticket_contents =
    string_ticket_of_size tx_rollup_max_ticket_payload_size
  in
  let parameters =
    Expr_common.(
      pair_n
        [
          string (Tx_rollup.to_b58check tx_rollup);
          string (Tx_rollup_l2_address.to_b58check pkh);
          ticket_contents;
        ])
    |> Tezos_micheline.Micheline.strip_locations |> Script.lazy_expr
  in
  let fee = Test_tez.of_int 10 in
  Op.transaction
    ~counter:(Z.of_int 2)
    ~fee
    (B b)
    account
    contract
    Tez.zero
    ~parameters
  >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f (function
          | Tx_rollup_errors_repr.Ticket_payload_size_limit_exceeded _ -> true
          | _ -> false))
  >>=? fun _ -> return_unit

(** [test_valid_deposit_big_ticket] tests that depositing a ticket whose size is exactly
    [tx_rollup_max_ticket_payload_size] succeeds.*)
let test_valid_deposit_big_ticket () =
  let (_, _, pkh) = gen_l2_account () in
  (* [overhead] is the number of bytes introduced by the wrapping of a
     string in a ticket. This encompasses the ticketer, amount and ty
     fields.

     This value has been fetched from the failing test, and acts as a
     regression value. *)
  let overhead = 112 in
  context_init1 () >>=? fun (b, account) ->
  Context.get_constants (B b) >>=? fun constant ->
  let tx_rollup_max_ticket_payload_size =
    constant.parametric.tx_rollup_max_ticket_payload_size
  in
  originate b account >>=? fun (b, tx_rollup) ->
  Contract_helpers.originate_contract
    "contracts/tx_rollup_deposit_string.tz"
    "Unit"
    account
    b
    (is_implicit_exn account)
  >>=? fun (contract, b) ->
  Incremental.begin_construction b >>=? fun i ->
  let ticket_contents =
    string_ticket_of_size (tx_rollup_max_ticket_payload_size - overhead)
  in
  let parameters =
    Expr_common.(
      pair_n
        [
          string (Tx_rollup.to_b58check tx_rollup);
          string (Tx_rollup_l2_address.to_b58check pkh);
          ticket_contents;
        ])
    |> Tezos_micheline.Micheline.strip_locations |> Script.lazy_expr
  in
  let fee = Test_tez.of_int 10 in
  Op.transaction
    ~counter:(Z.of_int 2)
    ~fee
    (B b)
    account
    contract
    Tez.zero
    ~parameters
  >>=? fun op ->
  Incremental.add_operation i op >>=? fun _ -> return_unit

(** [test_invalid_entrypoint] checks that a transaction to an invalid entrypoint
    of a transaction rollup fails. *)
let test_invalid_entrypoint () =
  let (_, _, pkh) = gen_l2_account () in

  context_init1 () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  Contract_helpers.originate_contract
    "contracts/tx_rollup_deposit_incorrect_param.tz"
    "Unit"
    account
    b
    (is_implicit_exn account)
  >>=? fun (contract, b) ->
  Incremental.begin_construction b >>=? fun i ->
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash pkh) in
  let fee = Test_tez.of_int 10 in
  Op.transaction ~fee (I i) account contract Tez.zero ~parameters >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f (function
          | Script_interpreter.Bad_contract_parameter _ -> true
          | _ -> false))
  >>=? fun _ -> return_unit

(** [test_invalid_l2_address] checks that a smart contract cannot make
    a deposit order to something that is not a valid layer-2 address. *)
let test_invalid_l2_address () =
  context_init1 () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  Contract_helpers.originate_contract
    "contracts/tx_rollup_deposit.tz"
    "Unit"
    account
    b
    (is_implicit_exn account)
  >>=? fun (contract, b) ->
  Incremental.begin_construction b >>=? fun i ->
  let parameters =
    print_deposit_arg (`Typed tx_rollup) (`Raw "\"invalid L2 address\"")
  in
  let fee = Test_tez.of_int 10 in
  Op.transaction ~fee (I i) account contract Tez.zero ~parameters >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f (function
          | Script_interpreter.Bad_contract_parameter _ -> true
          | _ -> false))
  >>=? fun _ -> return_unit

(** [test_valid_deposit_invalid_amount] checks that a transaction to a
    transaction rollup fails if the [amount] parameter is not null. *)
let test_valid_deposit_invalid_amount () =
  let (_, _, pkh) = gen_l2_account () in
  context_init1 () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  Contract_helpers.originate_contract
    "contracts/tx_rollup_deposit_one_mutez.tz"
    "Unit"
    account
    b
    (is_implicit_exn account)
  >>=? fun (contract, b) ->
  Incremental.begin_construction b >>=? fun i ->
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash pkh) in
  let fee = Test_tez.of_int 10 in
  Op.transaction ~fee (I i) account contract Tez.zero ~parameters >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error Apply.Tx_rollup_invalid_transaction_amount)
  >>=? fun _ -> return_unit

(** [test_deposit_too_many_tickets] checks that a deposit of
     too many tickets is rejected *)
let test_deposit_too_many_tickets () =
  let too_many = Z.succ (Z.of_int64 Int64.max_int) in
  let (_, _, pkh) = gen_l2_account () in
  context_init 1 >>=? fun (block, accounts) ->
  let account1 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth accounts 0
  in
  originate block account1 >>=? fun (block, tx_rollup) ->
  Nat_ticket.init_deposit too_many block tx_rollup account1
  >>=? fun (operation, b, deposit_contract) ->
  Block.bake ~operation b >>=? fun b ->
  let fee = Test_tez.of_int 10 in
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash pkh) in
  Op.transaction ~fee (B b) account1 deposit_contract Tez.zero ~parameters
  >>=? fun operation ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    i
    operation
    ~expect_failure:
      (check_proto_error Apply.Tx_rollup_invalid_transaction_amount)
  >>=? fun i ->
  ignore i ;
  return_unit

(** [test_deposit_by_non_internal_operation] checks that a transaction
    to the deposit entrypoint of a transaction rollup fails if it is
    not internal. *)
let test_deposit_by_non_internal_operation () =
  context_init1 () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  Op.unsafe_transaction (B b) account (Tx_rollup tx_rollup) Tez.zero
  >>=? fun operation ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation i operation >>= function
  | Ok _ ->
      failwith
        "Tx_rollup_non_internal_transaction error expected, but the operation \
         succeeded"
  | Error err -> check_proto_error Apply.Tx_rollup_non_internal_transaction err

(** Test that block finalization changes gas rates *)
let test_finalization () =
  context_init ~tx_rollup_max_inboxes_count:5_000 2 >>=? fun (b, contracts) ->
  let filler = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0 in
  let contract =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  originate b contract >>=? fun (b, tx_rollup) ->
  Context.get_constants (B b)
  >>=? fun {parametric = {tx_rollup_hard_size_limit_per_inbox; _}; _} ->
  (* Get the initial burn_per_byte. *)
  Context.Tx_rollup.state (B b) tx_rollup >>=? fun state ->
  burn_per_byte state >>?= fun cost ->
  Assert.equal_tez ~loc:__LOC__ Tez.zero cost >>=? fun () ->
  (* Fill the inbox. *)
  Context.get_constants (B b) >>=? fun constant ->
  let tx_rollup_batch_limit =
    constant.parametric.tx_rollup_hard_size_limit_per_message - 1
  in
  let contents = String.make tx_rollup_batch_limit 'd' in
  (* Repeating fill inbox and finalize block to increase EMA
     until EMA is enough to provoke a change of fees. *)
  let rec increase_ema n b tx_rollup f =
    f b tx_rollup >>=? fun (inbox_size, i) ->
    Incremental.finalize_block i >>=? fun b ->
    Context.Tx_rollup.state (B b) tx_rollup >>=? fun state ->
    let inbox_ema =
      Alpha_context.Tx_rollup_state.Internal_for_tests.get_inbox_ema state
    in
    if tx_rollup_hard_size_limit_per_inbox * 91 / 100 < inbox_ema then
      return (b, n, inbox_size)
    else increase_ema (n + 1) b tx_rollup f
  in
  ( increase_ema 1 b tx_rollup @@ fun b tx_rollup ->
    fill_inbox b tx_rollup filler contents (fun i size _ -> return (size, i)) )
  >>=? fun (b, n, inbox_size) ->
  let rec update_burn_per_byte_n_time n state =
    if n > 0 then
      let factor = 120 (* default factor *) in
      let elapsed = 0 (* the inbox was filled at every block *) in
      let state =
        Alpha_context.Tx_rollup_state.Internal_for_tests.update_burn_per_byte
          state
          ~elapsed
          ~factor
          ~final_size:inbox_size
          ~hard_limit:tx_rollup_hard_size_limit_per_inbox
      in
      update_burn_per_byte_n_time (n - 1) state
    else state
  in
  (* Check the fees we are getting after finalization are (1) strictly
     positive, and (2) the one we can predict with
     [update_burn_per_byte].

     [n - 2] comes from the following facts:

     - The [update_burn_per_byte] is called only on a new inbox

     - The [update_burn_per_byte] needs the predecessor inbox, hence
       it is not called on the first inbox *)
  let expected_state = update_burn_per_byte_n_time (n - 2) state in
  burn_per_byte expected_state >>?= fun expected_burn_per_byte ->
  Context.Tx_rollup.state (B b) tx_rollup >>=? fun state ->
  burn_per_byte state >>?= fun burn_per_byte ->
  assert (Tez.(zero < burn_per_byte)) ;
  Assert.equal_tez ~loc:__LOC__ expected_burn_per_byte burn_per_byte
  >>=? fun () ->
  (* Insert a small batch in a new block *)
  let contents_size = 5 in
  let contents = String.make contents_size 'c' in
  Context.Contract.balance (B b) contract >>=? fun balance ->
  Context.Contract.counter (B b) contract >>=? fun counter ->
  Op.tx_rollup_submit_batch ~counter (B b) contract tx_rollup contents
  >>=? fun op ->
  Block.bake b ~operation:op >>=? fun b ->
  (* Predict the cost we had to pay. *)
  Context.Tx_rollup.state (B b) tx_rollup >>=? fun state ->
  inbox_burn state contents_size >>?= fun cost ->
  (* Add upfront cost for the commitment hash   *)
  Context.get_constants (B b) >>=? fun {parametric = {cost_per_byte; _}; _} ->
  cost_per_byte
  *? Int64.of_int Tx_rollup_commitment_repr.Message_result_hash.size
  >>?= fun upfront_cost ->
  upfront_cost +? cost >>?= fun cost ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) contract balance cost

(** [test_commitment_duplication] originates a rollup, and makes a
    commitment. It attempts to add a second commitment for the same
    level, and ensures that this fails.  It adds a commitment with
    the wrong batch count and ensures that that fails. *)
let test_commitment_duplication () =
  context_init2 () >>=? fun (b, contract1, contract2) ->
  let pkh1 = is_implicit_exn contract1 in
  originate b contract1 >>=? fun (b, tx_rollup) ->
  Context.Contract.balance (B b) contract1 >>=? fun balance ->
  Context.Contract.balance (B b) contract2 >>=? fun balance2 ->
  (* In order to have a permissible commitment, we need a transaction. *)
  let contents = "batch" in
  Op.tx_rollup_submit_batch (B b) contract1 tx_rollup contents
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Incremental.begin_construction b >>=? fun i ->
  make_incomplete_commitment_for_batch i Tx_rollup_level.root tx_rollup []
  >>=? fun (commitment, _) ->
  (* Successfully fail to submit a different commitment from contract2 *)
  let batches2 : Tx_rollup_message_result_hash.t list =
    [Bytes.make 20 '1'; Bytes.make 20 '2']
    |> List.map (fun hash ->
           let context_hash = Context_hash.hash_bytes [hash] in
           Tx_rollup_commitment.hash_message_result
             {
               context_hash;
               withdrawals_merkle_root = Tx_rollup_withdraw.Merkle.empty;
             })
  in
  let commitment_with_wrong_count : Tx_rollup_commitment.t =
    {commitment with messages = batches2}
  in
  Op.tx_rollup_commit (I i) contract2 tx_rollup commitment_with_wrong_count
  >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:(check_proto_error Tx_rollup_errors.Wrong_batch_count)
  >>=? fun i ->
  (* Submit the correct one *)
  Context.get_level (I i) >>?= fun level ->
  let submitted_level = Raw_level.succ level in
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  Context.get_constants (I i) >>=? fun constants ->
  let cost = constants.parametric.tx_rollup_commitment_bond in
  Assert.balance_was_debited ~loc:__LOC__ (I i) contract1 balance cost
  >>=? fun () ->
  (* Successfully fail to submit a duplicate commitment *)
  Op.tx_rollup_commit (I i) contract2 tx_rollup commitment >>=? fun op ->
  (Incremental.add_operation i op >>= function
   | Ok _ -> failwith "an error was expected"
   | Error e ->
       check_proto_error_f
         (function Tx_rollup_errors.No_uncommitted_inbox -> true | _ -> false)
         e)
  >>=? fun _ ->
  (* No charge. *)
  Assert.balance_was_debited ~loc:__LOC__ (I i) contract2 balance2 Tez.zero
  >>=? fun () ->
  Context.Tx_rollup.state (I i) tx_rollup >>=? fun state ->
  let ctxt = Incremental.alpha_ctxt i in
  wrap_lwt (Tx_rollup_commitment.find ctxt tx_rollup state Tx_rollup_level.root)
  >>=? fun (_, commitment_opt) ->
  (match commitment_opt with
  | None -> raise (Invalid_argument "No commitment")
  | Some
      {
        commitment = expected_commitment;
        commitment_hash = expected_hash;
        committer;
        submitted_at;
        finalized_at;
      } ->
      Alcotest.(
        check commitment_testable "Commitment" expected_commitment commitment) ;
      Alcotest.(
        check commitment_hash_testable "Commitment hash" expected_hash
        @@ Tx_rollup_commitment.hash commitment) ;
      Alcotest.(check public_key_hash_testable "Committer" pkh1 committer) ;
      Alcotest.(
        check raw_level_testable "Submitted" submitted_level submitted_at) ;
      Alcotest.(check (option raw_level_testable) "Finalized" None finalized_at)) ;
  check_bond ctxt tx_rollup contract1 1 >>=? fun () ->
  check_bond ctxt tx_rollup contract2 0 >>=? fun () ->
  ignore i ;
  return ()

let test_commit_current_inbox () =
  context_init2 () >>=? fun (b, contract1, contract2) ->
  originate b contract1 >>=? fun (b, tx_rollup) ->
  (* In order to have a permissible commitment, we need a transaction. *)
  Incremental.begin_construction b >>=? fun i ->
  let contents = "batch" in
  let (message, _) = Tx_rollup_message.make_batch contents in
  let message_hash = Tx_rollup_message.hash_uncarbonated message in
  let inbox_hash = Tx_rollup_inbox.Merkle.merklize_list [message_hash] in
  Op.tx_rollup_submit_batch (I i) contract1 tx_rollup contents
  >>=? fun operation ->
  Incremental.add_operation i operation >>=? fun i ->
  Op.tx_rollup_commit
    (I i)
    contract2
    tx_rollup
    {
      level = Tx_rollup_level.root;
      inbox_merkle_root = inbox_hash;
      predecessor = None;
      messages = [Tx_rollup_message_result_hash.zero];
    }
  >>=? fun operation ->
  Incremental.add_operation
    i
    operation
    ~expect_failure:(check_proto_error Tx_rollup_errors.No_uncommitted_inbox)
  >>=? fun i ->
  ignore i ;
  return_unit

let make_transactions_in tx_rollup contract blocks b =
  let contents = "batch " in
  let rec aux cur blocks b =
    match blocks with
    | [] -> return b
    | hd :: rest when hd = cur ->
        Op.tx_rollup_submit_batch (B b) contract tx_rollup contents
        >>=? fun operation ->
        Block.bake ~operation b >>=? fun b -> aux (cur + 1) rest b
    | blocks ->
        let operations = [] in
        Block.bake ~operations b >>=? fun b -> aux (cur + 1) blocks b
  in
  aux 2 blocks b

let assert_ok res =
  match res with
  | Ok r -> r
  | Error _ -> raise (Invalid_argument "Error: assert_ok")

let tx_level level = assert_ok @@ Tx_rollup_level.of_int32 level

(** [test_storage_burn_for_commitment] test storage space allocation
    for adding and removing commitment and bond. *)
let test_storage_burn_for_commitment () =
  let check_storage_delta ~__POS__ msg ~size_before ~size_after ~expected_delta
      =
    Alcotest.(
      check
        ~pos:__POS__
        zestable
        msg
        (Z.of_int expected_delta)
        Z.(sub size_after size_before))
  in
  let tx_rollup_origination_size = 1 in
  context_init1 ~tx_rollup_origination_size () >>=? fun (b, contract) ->
  originate b contract >>=? fun (b, tx_rollup) ->
  make_transactions_in tx_rollup contract [2; 3; 4] b >>=? fun b ->
  (* test allocated storage size and balance before/after submit commitment *)
  Incremental.begin_construction b >>=? fun i ->
  occupied_storage_size (B b) tx_rollup >>=? fun storage_size_before_commit ->
  make_incomplete_commitment_for_batch i Tx_rollup_level.root tx_rollup []
  >>=? fun (commitment, _) ->
  Op.tx_rollup_commit (I i) contract tx_rollup commitment >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  occupied_storage_size (I i) tx_rollup >>=? fun storage_size_after_commit ->
  (* extra space should be allocated for submitting commitment *)
  let commitment_add_delta = 99 in
  check_storage_delta
    ~__POS__
    "Size increase after adding commitment"
    ~size_before:storage_size_before_commit
    ~size_after:storage_size_after_commit
    ~expected_delta:commitment_add_delta ;
  Incremental.finalize_block i >>=? fun b ->
  (* test freed storage space after finalize *)
  Op.tx_rollup_finalize (B b) contract tx_rollup >>=? fun op ->
  Block.bake ~operation:op b >>=? fun b ->
  occupied_storage_size (B b) tx_rollup >>=? fun freed_space_after_finalize ->
  let inbox_delta = -36 in
  check_storage_delta
    ~__POS__
    "Storage space is freed after finalize"
    ~size_before:storage_size_after_commit
    ~size_after:freed_space_after_finalize
    ~expected_delta:inbox_delta ;

  (* test freed storage space after remove commitment *)
  Op.tx_rollup_remove_commitment (B b) contract tx_rollup >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  occupied_storage_size (B b) tx_rollup
  >>=? fun freed_space_after_remove_commitment ->
  let commitment_remove_delta = -135 in
  check_storage_delta
    ~__POS__
    "Storage space is freed after removing commitment"
    ~size_before:freed_space_after_finalize
    ~size_after:freed_space_after_remove_commitment
    ~expected_delta:commitment_remove_delta ;
  let msg_preallocation_delta =
    Tx_rollup_commitment_repr.Message_result_hash.size
  in
  let finalization_delta = 4 in
  Alcotest.(
    check
      ~pos:__POS__
      int
      "The delta of adding and removing a commitment is zero (modulo \
       preallocation)"
      (-(commitment_add_delta + msg_preallocation_delta + finalization_delta))
      commitment_remove_delta) ;
  (* test freed storage space after return bond *)
  Op.tx_rollup_return_bond (B b) contract tx_rollup >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  occupied_storage_size (B b) tx_rollup
  >>=? fun freed_space_after_return_bond ->
  let bond_remove_delta = 0 in
  check_storage_delta
    ~__POS__
    "Storage space is freed after removing bond"
    ~size_before:freed_space_after_remove_commitment
    ~size_after:freed_space_after_return_bond
    ~expected_delta:bond_remove_delta ;
  return_unit

(** [test_storage_burn_for_commitment] test storage space allocation for adding and removing commitment and bond. *)
let test_storage_burn_for_commitment_and_bond () =
  let check_storage_delta ~__POS__ msg ~size_before ~size_after ~expected_delta
      =
    Alcotest.(
      check
        ~pos:__POS__
        zestable
        msg
        (Z.of_int expected_delta)
        Z.(sub size_after size_before))
  in
  let tx_rollup_origination_size = 1 in
  context_init1 ~tx_rollup_origination_size () >>=? fun (b, contract) ->
  originate b contract >>=? fun (b, tx_rollup) ->
  make_transactions_in tx_rollup contract [2; 3; 4] b >>=? fun b ->
  (* test allocated storage size and balance before/after submit commitment *)
  Incremental.begin_construction b >>=? fun i ->
  occupied_storage_size (B b) tx_rollup >>=? fun storage_size_before_commit ->
  make_incomplete_commitment_for_batch i Tx_rollup_level.root tx_rollup []
  >>=? fun (commitment, _) ->
  Op.tx_rollup_commit (I i) contract tx_rollup commitment >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  occupied_storage_size (I i) tx_rollup >>=? fun storage_size_after_commit ->
  (* extra space should be allocated for submitting commitment *)
  let commitment_sans_preallocation_delta = 99 in
  let adjust_unfinalized_commitments_count_delta = 4 in
  let commitment_add_delta =
    commitment_sans_preallocation_delta
    + adjust_unfinalized_commitments_count_delta
  in
  check_storage_delta
    ~__POS__
    "Size increase after adding commitment"
    ~size_before:storage_size_before_commit
    ~size_after:storage_size_after_commit
    ~expected_delta:commitment_add_delta ;
  Incremental.finalize_block i >>=? fun b ->
  (* test freed storage space after finalize *)
  Op.tx_rollup_finalize (B b) contract tx_rollup >>=? fun op ->
  Block.bake ~operation:op b >>=? fun b ->
  occupied_storage_size (B b) tx_rollup >>=? fun freed_space_after_finalize ->
  let inbox_delta = -40 in
  let commitment_finalization_delta = 4 in
  check_storage_delta
    ~__POS__
    "Storage space is freed after finalize"
    ~size_before:storage_size_after_commit
    ~size_after:freed_space_after_finalize
    ~expected_delta:(inbox_delta + commitment_finalization_delta) ;

  (* test freed storage space after remove commitment *)
  Op.tx_rollup_remove_commitment (B b) contract tx_rollup >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  occupied_storage_size (B b) tx_rollup
  >>=? fun freed_space_after_remove_commitment ->
  let commitment_remove_delta = -135 in
  check_storage_delta
    ~__POS__
    "Storage space is freed after removing commitment"
    ~size_before:freed_space_after_finalize
    ~size_after:freed_space_after_remove_commitment
    ~expected_delta:commitment_remove_delta ;
  Alcotest.(
    check
      ~pos:__POS__
      int
      "The delta of adding and removing a commitment is zero (modulo \
       preallocation)"
      (-commitment_add_delta
     - Tx_rollup_commitment_repr.Message_result_hash.size)
      commitment_remove_delta) ;
  (* test freed storage space after return bond *)
  Op.tx_rollup_return_bond (B b) contract tx_rollup >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  occupied_storage_size (B b) tx_rollup
  >>=? fun freed_space_after_return_bond ->
  let bond_remove_delta = -4 in
  check_storage_delta
    ~__POS__
    "Storage space is freed after removing bond"
    ~size_before:freed_space_after_remove_commitment
    ~size_after:freed_space_after_return_bond
    ~expected_delta:bond_remove_delta ;
  return_unit

(** [test_commitment_predecessor] tests commitment predecessor edge cases  *)
let test_commitment_predecessor () =
  context_init1 () >>=? fun (b, contract1) ->
  originate b contract1 >>=? fun (b, tx_rollup) ->
  (* Transactions in blocks 2, 3, 6 *)
  make_transactions_in tx_rollup contract1 [2; 3; 6] b >>=? fun b ->
  Incremental.begin_construction b >>=? fun i ->
  (* Check error: Commitment for nonexistent block *)
  let bogus_hash =
    Tx_rollup_commitment_hash.of_bytes_exn
      (Bytes.of_string "tcu1deadbeefdeadbeefdeadbeefdead")
  in
  make_incomplete_commitment_for_batch i Tx_rollup_level.root tx_rollup []
  >>=? fun (commitment, _) ->
  let commitment_for_invalid_inbox = {commitment with level = tx_level 10l} in
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment_for_invalid_inbox
  >>=? fun op ->
  let error =
    Tx_rollup_errors.Commitment_too_early
      {provided = tx_level 10l; expected = tx_level 0l}
  in
  Incremental.add_operation i op ~expect_failure:(check_proto_error error)
  >>=? fun _ ->
  (* Now we submit a real commitment *)
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  (* Commitment without predecessor for block with predecessor*)
  make_incomplete_commitment_for_batch
    i
    Tx_rollup_level.(succ root)
    tx_rollup
    []
  >>=? fun (commitment, _) ->
  let commitment_with_missing_predecessor =
    {commitment with predecessor = None}
  in
  Op.tx_rollup_commit
    (I i)
    contract1
    tx_rollup
    commitment_with_missing_predecessor
  >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f @@ function
       | Tx_rollup_errors.Wrong_predecessor_hash {provided = None; expected} ->
           expected = commitment.predecessor
       | _ -> false)
  >>=? fun i ->
  (* Commitment refers to a predecessor which does not exist *)
  let commitment_with_wrong_pred =
    {commitment with predecessor = Some bogus_hash}
  in
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment_with_wrong_pred
  >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f @@ function
       | Tx_rollup_errors.Wrong_predecessor_hash {provided = _; expected} ->
           expected = commitment.predecessor
       | _ -> false)
  >>=? fun i ->
  ignore i ;
  return ()

let test_full_inbox () =
  let constants =
    {
      Tezos_protocol_alpha_parameters.Default_parameters.constants_test with
      consensus_threshold = 0;
      endorsing_reward_per_slot = Tez.zero;
      baking_reward_bonus_per_slot = Tez.zero;
      baking_reward_fixed_portion = Tez.zero;
      tx_rollup_enable = true;
      tx_rollup_max_inboxes_count = 15;
    }
  in
  Context.init_with_constants constants 1 >>=? fun (b, contracts) ->
  let contract =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  originate b contract >>=? fun (b, tx_rollup) ->
  let range start top =
    let rec aux n acc = if n < start then acc else aux (n - 1) (n :: acc) in
    aux top []
  in
  (* Transactions in blocks [2..17) *)
  make_transactions_in tx_rollup contract (range 2 17) b >>=? fun b ->
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_submit_batch (B b) contract tx_rollup "contents" >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:(check_proto_error Tx_rollup_errors.Too_many_inboxes)
  >>=? fun i ->
  ignore i ;
  return ()

(** [test_bond_finalization] tests that level retirement in fact
    allows bonds to be returned. *)
let test_bond_finalization () =
  context_init1 () >>=? fun (b, contract1) ->
  let pkh1 = is_implicit_exn contract1 in
  originate b contract1 >>=? fun (b, tx_rollup) ->
  Context.Contract.balance (B b) contract1 >>=? fun balance ->
  (* Transactions in block 2, 3, 4 *)
  make_transactions_in tx_rollup contract1 [2; 3; 4] b >>=? fun b ->
  (* Let’s try to remove the bond *)
  Incremental.begin_construction b >>=? fun i ->
  Context.get_constants (I i) >>=? fun constants ->
  let bond = constants.parametric.tx_rollup_commitment_bond in
  Op.tx_rollup_return_bond (I i) contract1 tx_rollup >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f @@ function
       | Tx_rollup_errors.Bond_does_not_exist a_pkh1 -> a_pkh1 = pkh1
       | _ -> false)
  >>=? fun i ->
  make_incomplete_commitment_for_batch i Tx_rollup_level.root tx_rollup []
  >>=? fun (commitment_a, _) ->
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment_a >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  Op.tx_rollup_return_bond (I i) contract1 tx_rollup >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f @@ function
       | Tx_rollup_errors.Bond_in_use a_pkh1 -> a_pkh1 = pkh1
       | _ -> false)
  >>=? fun i ->
  Incremental.finalize_block i >>=? fun b ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) contract1 balance bond
  >>=? fun () ->
  (* Finalize the commitment of level 0. *)
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_finalize (I i) contract1 tx_rollup >>=? fun operation ->
  Incremental.add_operation i operation >>=? fun i ->
  Incremental.finalize_block i >>=? fun b ->
  (* Bake enough block, and remove the commitment of level 0. *)
  Block.bake b ~operations:[] >>=? fun b ->
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_remove_commitment (I i) contract1 tx_rollup >>=? fun operation ->
  Incremental.add_operation i operation >>=? fun i ->
  Incremental.finalize_block i >>=? fun b ->
  (* Try to return the bond *)
  Context.Contract.balance (B b) contract1 >>=? fun balance ->
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_return_bond (I i) contract1 tx_rollup >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  Incremental.finalize_block i >>=? fun b ->
  (* Check the balance*)
  Assert.balance_was_credited ~loc:__LOC__ (B b) contract1 balance bond

(** [test_finalization_edge_cases] tests finalization correctly fails
    in various edge cases. *)
let test_finalization_edge_cases () =
  context_init1 ~tx_rollup_finality_period:2 () >>=? fun (b, contract1) ->
  originate b contract1 >>=? fun (b, tx_rollup) ->
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_finalize (I i) contract1 tx_rollup >>=? fun op ->
  (* With no inbox and no commitment *)
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error @@ Tx_rollup_errors.No_commitment_to_finalize)
  >>=? fun _i ->
  let message = "bogus" in
  Op.tx_rollup_submit_batch (B b) contract1 tx_rollup message >>=? fun op ->
  Block.bake ~operation:op b >>=? fun b ->
  Op.tx_rollup_submit_batch (B b) contract1 tx_rollup message >>=? fun op ->
  Block.bake ~operation:op b >>=? fun b ->
  Op.tx_rollup_finalize (B b) contract1 tx_rollup >>=? fun op ->
  (* With an inbox, but no commitment *)
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error @@ Tx_rollup_errors.No_commitment_to_finalize)
  >>=? fun _i ->
  make_incomplete_commitment_for_batch i (tx_level 0l) tx_rollup []
  >>=? fun (commitment, _) ->
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun op ->
  (* With a commitment, but too soon after the commitment *)
  Incremental.add_operation i op >>=? fun i ->
  Incremental.finalize_block i >>=? fun b ->
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_finalize (I i) contract1 tx_rollup >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error @@ Tx_rollup_errors.No_commitment_to_finalize)
  >>=? fun _i ->
  Incremental.finalize_block i >>=? fun b ->
  (* Now our finalization is valid *)
  Block.bake ~operation:op b >>=? fun _block -> return_unit

(** [test_too_many_commitments] tests that you can't submit new
      commitments if there are too many finalized commitments. *)
let test_too_many_commitments () =
  context_init1 () >>=? fun (b, contract1) ->
  originate b contract1 >>=? fun (b, tx_rollup) ->
  (* Transactions in block 2, 3, 4, 5 *)
  make_transactions_in tx_rollup contract1 [2; 3; 4; 5] b >>=? fun b ->
  Incremental.begin_construction b >>=? fun i ->
  let rec make_commitments i level n =
    if n = 0 then return (i, level)
    else
      make_incomplete_commitment_for_batch i level tx_rollup []
      >>=? fun (commitment, _) ->
      Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun op ->
      Incremental.add_operation i op >>=? fun i ->
      make_commitments i (Tx_rollup_level.succ level) (n - 1)
  in
  make_commitments i Tx_rollup_level.root 3 >>=? fun (i, level) ->
  (* Make sure all commitments can be finalized. *)
  bake_until i 10l >>=? fun i ->
  Op.tx_rollup_finalize (I i) contract1 tx_rollup >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  Op.tx_rollup_finalize (I i) contract1 tx_rollup >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  (* Fail to add a new commitment. *)
  make_incomplete_commitment_for_batch i level tx_rollup []
  >>=? fun (commitment, _) ->
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:(check_proto_error Tx_rollup_errors.Too_many_commitments)
  >>=? fun i ->
  (* Wait out the withdrawal period. *)
  bake_until i 12l >>=? fun i ->
  (* Remove one finalized commitment. *)
  Op.tx_rollup_remove_commitment (I i) contract1 tx_rollup >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  (* Now we can add a new commitment. *)
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  ignore i ;

  return ()

module Rejection = struct
  open Protocol

  exception Error of Environment.Error_monad.error

  module Prover_storage :
    Tx_rollup_l2_storage_sig.STORAGE
      with type t = Tezos_context_memory.Context_binary.tree
       and type 'a m = 'a Lwt.t = struct
    type t = Tezos_context_memory.Context_binary.tree

    type 'a m = 'a Lwt.t

    module Syntax = struct
      include Lwt.Syntax

      let return = Lwt.return

      let fail e = Lwt.fail (Error e)

      let catch (m : 'a m) k h =
        Lwt.catch
          (fun () -> m >>= k)
          (function Error e -> h e | e -> Lwt.fail e)

      let list_fold_left_m = Lwt_list.fold_left_s
    end

    let path k = [Bytes.to_string k]

    let get store key =
      Tezos_context_memory.Context_binary.Tree.find store (path key)

    let set store key value =
      Tezos_context_memory.Context_binary.Tree.add store (path key) value

    let remove store key =
      Tezos_context_memory.Context_binary.Tree.remove store (path key)
  end

  module Storage :
    Tx_rollup_l2_storage_sig.STORAGE
      with type t = Tezos_context_memory.Context_binary.t
       and type 'a m = 'a Lwt.t = struct
    type t = Tezos_context_memory.Context_binary.t

    type 'a m = 'a Lwt.t

    module Syntax = struct
      include Lwt.Syntax

      let return = Lwt.return

      let fail e = Lwt.fail (Error e)

      let catch (m : 'a m) k h =
        Lwt.catch
          (fun () -> m >>= k)
          (function Error e -> h e | e -> Lwt.fail e)

      let list_fold_left_m = Lwt_list.fold_left_s
    end

    let path k = [Bytes.to_string k]

    let get store key =
      Tezos_context_memory.Context_binary.find store (path key)

    let set store key value =
      Tezos_context_memory.Context_binary.add store (path key) value

    let remove store key =
      Tezos_context_memory.Context_binary.remove store (path key)
  end

  module Prover_context = Tx_rollup_l2_context.Make (Prover_storage)
  module Context = Tx_rollup_l2_context.Make (Storage)
  module Prover_apply = Tx_rollup_l2_apply.Make (Prover_context)
  module Apply = Tx_rollup_l2_apply.Make (Context)
  module C = Tezos_context_memory.Context_binary

  let previous_message_result : Tx_rollup_commitment.message_result =
    {
      context_hash = Tx_rollup_commitment.empty_l2_context_hash;
      withdrawals_merkle_root = Tx_rollup_withdraw.Merkle.empty;
    }

  let init_with_bogus_batch () =
    context_init1 () >>=? fun (b, contract1) ->
    originate b contract1 >>=? fun (b, tx_rollup) ->
    let message = "bogus" in
    Op.tx_rollup_submit_batch (B b) contract1 tx_rollup message
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    Incremental.begin_construction b >|=? fun i ->
    let level = Tx_rollup_level.root in
    (i, contract1, tx_rollup, level, message)

  let init_with_valid_commitment () =
    init_with_bogus_batch ()
    >>=? fun (i, contract1, tx_rollup, level, message) ->
    make_incomplete_commitment_for_batch i level tx_rollup []
    >>=? fun (commitment, _batches_result) ->
    Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun op ->
    Incremental.add_operation i op >|=? fun i ->
    (i, contract1, tx_rollup, level, message)

  let init_with_invalid_commitment () =
    init_with_bogus_batch ()
    >>=? fun (i, contract1, tx_rollup, level, message) ->
    make_incomplete_commitment_for_batch i level tx_rollup []
    >>=? fun (commitment, _batches_result) ->
    let commitment =
      {
        commitment with
        messages =
          [
            Tx_rollup_commitment.hash_message_result
              {
                context_hash =
                  Context_hash.of_b58check_exn
                    "CoUiEnajKeukmYFUgWTJF2z3v24MycpTaomF8a9hRzVy7as9hvgy";
                withdrawals_merkle_root = Tx_rollup_withdraw.Merkle.empty;
              };
          ];
      }
    in
    Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun op ->
    Incremental.add_operation i op >|=? fun i ->
    (i, contract1, tx_rollup, level, message)

  let run_transaction ctxt l2_parameters msg =
    let open Prover_context.Syntax in
    let* (ctxt, _) = Prover_apply.apply_message ctxt l2_parameters msg in
    return ctxt

  let time () =
    Time.System.now () |> Time.System.to_notation
    |> Time.Protocol.of_notation_exn

  (** [init_l2_store ()] creates the required empty storage and tree.
      It creates a tree where the counters are explicitly set (it's an Irmin
      restriction that we can not hash an empty tree). It returns the
      persistent store containing the tree.

      {!Tx_rollup_commitment.empty_l2_context_hash} is the hash of the
      tree produced in this function.
  *)
  let init_l2_store () =
    let open Context.Syntax in
    let store = C.empty in
    let time = time () in
    let tree = C.Tree.empty store in
    let* tree = Prover_context.Address_index.init_counter tree in
    let* tree = Prover_context.Ticket_index.init_counter tree in
    let* ctxt = C.add_tree store [] tree in
    let* h = C.commit ~time ctxt in
    let index = C.index ctxt in
    let* store = C.checkout_exn index h in
    return store

  let get_tree_from_store store =
    let open Context.Syntax in
    let* tree_opt = C.find_tree store [] in
    match tree_opt with Some x -> return x | None -> assert false

  let hash_tree_from_store store =
    let open Context.Syntax in
    let+ tree = get_tree_from_store store in
    C.Tree.hash tree

  let commit_store store =
    let open Context.Syntax in
    let time = time () in
    let* h = C.commit ~time store in
    let index = C.index store in
    let* store = C.checkout_exn index h in
    return store

  (** See {!Tx_rollup_commitment.empty_l2_context_hash} documentation.
      The constant is created from the hash of the underlying tree in
      the store generated by {!init_l2_store}. We then add a regression
      test to ensure these two are synchronized. *)
  let test_empty_l2_context_hash () =
    let open Context.Syntax in
    let* store = init_l2_store () in
    let* hash_tree = hash_tree_from_store store in
    assert (
      Context_hash.(hash_tree = Tx_rollup_commitment.empty_l2_context_hash)) ;
    return_unit

  (** [make_proof store msg] applies [msg] on [store] and returns the
      created proof. *)
  let make_proof store l2_parameters msg =
    let open Context.Syntax in
    let index = C.index store in
    let* hash = hash_tree_from_store store in
    let* (proof, ()) =
      C.produce_stream_proof index (`Node hash) (fun ctxt ->
          catch
            (run_transaction ctxt l2_parameters msg)
            (fun ctxt -> return (ctxt, ()))
            (fun _ -> return (ctxt, ())))
    in
    return proof

  let valid_empty_proof l2_parameters =
    let open Context.Syntax in
    let* l2_store = init_l2_store () in
    let (message, _) = Tx_rollup_message.make_batch "bogus" in
    make_proof l2_store l2_parameters message

  let invalid_proof : Tx_rollup_l2_proof.t =
    {
      version = 1;
      before = `Value Tx_rollup_commitment.empty_l2_context_hash;
      after = `Value Context_hash.zero;
      state = Seq.empty;
    }

  (** Takes a commitment and replaces the message results with valid results.

      FIXME/TORU: this overrides the withdrawals merkle roots. This is not good
      but there is yet not tests calling this function which uses these roots.
      Although, it shouldn't be to hard to fix this. *)
  let replace_commitment ~l2_parameters ~store ~commitment messages =
    let open Context in
    let open Syntax in
    let* (_, rev_results) =
      list_fold_left_m
        (fun (store, rev_results) msg ->
          let* store =
            catch
              (Apply.apply_message store l2_parameters msg)
              (fun (store, _) -> return store)
              (fun _reason -> return store)
          in
          let* hash_tree = hash_tree_from_store store in
          let result_hash =
            Tx_rollup_commitment.hash_message_result
              {
                context_hash = hash_tree;
                withdrawals_merkle_root = Tx_rollup_withdraw.Merkle.empty;
              }
          in
          return (store, result_hash :: rev_results))
        (store, [])
        messages
    in
    let results = List.rev rev_results in
    return Tx_rollup_commitment.{commitment with messages = results}

  (** Produce an invalid commitment with {!make_incomplete_commitment_for_batch},
      then changes the Merkle roots for each message result.

      FIXME/TORU: it is not perfectly valid, the withdrawals are still missing.
      see {!replace_commitment} documentation. *)
  let make_valid_commitment_for_messages ~i ~level ~tx_rollup
      ?(withdrawals = []) ~store messages =
    make_incomplete_commitment_for_batch i level tx_rollup withdrawals
    >>=? fun (commitment, _) ->
    l2_parameters (I i) >>=? fun l2_parameters ->
    replace_commitment ~l2_parameters ~commitment ~store messages
    >>= fun commitment -> return commitment

  (** Create a deposit on the layer1 side through the origination of a contract
      and return the associated deposit message to apply in the layer2. *)
  let make_deposit b tx_rollup account =
    let (sk, pk, addr) = gen_l2_account () in
    Contract_helpers.originate_contract
      "contracts/tx_rollup_deposit.tz"
      "Unit"
      account
      b
      (is_implicit_exn account)
    >>=? fun (contract, b) ->
    let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash addr) in
    let fee = Test_tez.of_int 10 in
    Op.transaction
      ~counter:(Z.of_int 2)
      ~fee
      (B b)
      account
      contract
      Tez.zero
      ~parameters
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    make_unit_ticket_key (B b) ~ticketer:contract tx_rollup
    >>=? fun ticket_hash ->
    let (deposit, _) =
      Tx_rollup_message.make_deposit
        (is_implicit_exn account)
        (Tx_rollup_l2_address.Indexable.value addr)
        ticket_hash
        (Tx_rollup_l2_qty.of_int64_exn 10L)
    in
    return (b, deposit, (sk, pk, addr), ticket_hash)

  let init_with_deposit () =
    init_l2_store () >>= fun store ->
    context_init1 () >>=? fun (b, account) ->
    originate b account >>=? fun (b, tx_rollup) ->
    make_deposit b tx_rollup account
    >>=? fun (b, deposit, l2_account, ticket_hash) ->
    let deposit_hash = Tx_rollup_message.hash_uncarbonated deposit in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [deposit_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Incremental.begin_construction b >>=? fun i ->
    make_valid_commitment_for_messages
      ~i
      ~level:Tx_rollup_level.root
      ~tx_rollup
      ~store
      [deposit]
    >>=? fun commitment ->
    Op.tx_rollup_commit (I i) account tx_rollup commitment >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* We try to reject the previous commitment, but as it was valid,
       we can not reject a valid state. We do not really need to test this
       here, as there are dedicated tests, but this behaves like a regression
       test. *)
    l2_parameters (I i) >>=? fun l2_parameters ->
    make_proof store l2_parameters deposit >>= fun proof ->
    Incremental.begin_construction b >>=? fun i ->
    Op.tx_rollup_reject
      (I i)
      account
      tx_rollup
      Tx_rollup_level.root
      deposit
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:
        (check_proto_error Tx_rollup_errors.Proof_produced_rejected_state)
    >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* Finally, we apply the deposit manually to have the good resulting store
       for next operations *)
    Apply.apply_message store l2_parameters deposit >>= fun (store, _) ->
    commit_store store >>= fun store ->
    return (b, account, tx_rollup, store, l2_account, ticket_hash)

  let operation_content destination ticket_hash qty =
    let open Tx_rollup_l2_batch.V1 in
    Transfer
      {
        destination = Indexable.from_value destination;
        ticket_hash = Indexable.from_value ticket_hash;
        qty = Tx_rollup_l2_qty.of_int64_exn qty;
      }

  let operation signer ?(counter = 1L) contents =
    let open Tx_rollup_l2_batch.V1 in
    {signer = Indexable.from_value signer; counter; contents}

  let make_transfers src counter transfers =
    let rec contents acc = function
      | [] -> acc
      | (destination, ticket_hash, qty) :: rst ->
          let acc = operation_content destination ticket_hash qty :: acc in
          contents acc rst
    in
    let contents = contents [] transfers in
    operation src ?counter contents

  let make_message_transfer ~signers all_transfers =
    let transaction =
      List.map
        (fun (src, counter, transfers) -> make_transfers src counter transfers)
        all_transfers
    in
    let signatures =
      Tx_rollup_l2_helpers.sign_transaction signers transaction
    in
    let signature =
      assert_some
      @@ Environment.Bls_signature.aggregate_signature_opt signatures
    in
    let batch =
      Tx_rollup_l2_batch.V1.
        {contents = [transaction]; aggregated_signature = signature}
    in
    let batch_bytes =
      Data_encoding.Binary.to_string_exn Tx_rollup_l2_batch.encoding (V1 batch)
    in
    let msg = Tx_rollup_message.make_batch batch_bytes |> fst in
    (msg, batch_bytes)

  (** Test that we can produce a simple but valid proof. *)
  let test_valid_proof_on_invalid_commitment () =
    init_with_deposit ()
    >>=? fun (b, account, tx_rollup, store, (sk, pk, _pkh), ticket_hash) ->
    hash_tree_from_store store >>= fun l2_context_hash ->
    (* Create a transfer from [pk] to a new address *)
    let (_, _, addr) = gen_l2_account () in
    let (message, batch_bytes) =
      make_message_transfer
        ~signers:[sk]
        [(Bls_pk pk, None, [(addr, ticket_hash, 1L)])]
    in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Op.tx_rollup_submit_batch (B b) account tx_rollup batch_bytes
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    (* Make an invalid commitment for the submitted transfer *)
    let level = Tx_rollup_level.(succ root) in
    Incremental.begin_construction b >>=? fun i ->
    make_incomplete_commitment_for_batch i level tx_rollup []
    >>=? fun (commitment, _) ->
    Op.tx_rollup_commit (I i) account tx_rollup commitment >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* Now we produce a valid proof rejecting the commitment *)
    l2_parameters (I i) >>=? fun l2_parameters ->
    make_proof store l2_parameters message >>= fun proof ->
    Op.tx_rollup_reject
      (B b)
      account
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result:
        {
          context_hash = l2_context_hash;
          withdrawals_merkle_root = Tx_rollup_withdraw.Merkle.empty;
        }
    >>=? fun op ->
    Incremental.add_operation i op >>=? fun _ -> return_unit

  (** It is really similar to {!test_valid_proof_on_invalid_commitment} but it
      tries to reject a valid commitment, thus, fails. *)
  let test_valid_proof_on_valid_commitment () =
    init_with_deposit ()
    >>=? fun (b, account, tx_rollup, store, (sk, pk, _pkh), ticket_hash) ->
    hash_tree_from_store store >>= fun l2_context_hash ->
    (* Create a transfer from [pk] to a new address *)
    let (_, _, addr) = gen_l2_account () in
    let (message, batch_bytes) =
      make_message_transfer
        ~signers:[sk]
        [(Bls_pk pk, None, [(addr, ticket_hash, 1L)])]
    in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Op.tx_rollup_submit_batch (B b) account tx_rollup batch_bytes
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    (* Make an invalid commitment for the submitted transfer *)
    let level = Tx_rollup_level.(succ root) in
    Incremental.begin_construction b >>=? fun i ->
    make_valid_commitment_for_messages ~i ~level ~tx_rollup ~store [message]
    >>=? fun commitment ->
    Op.tx_rollup_commit (I i) account tx_rollup commitment >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* Now we produce a valid proof rejecting the commitment *)
    l2_parameters (B b) >>=? fun l2_parameters ->
    make_proof store l2_parameters message >>= fun proof ->
    Op.tx_rollup_reject
      (B b)
      account
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result:
        {
          context_hash = l2_context_hash;
          withdrawals_merkle_root = Tx_rollup_withdraw.Merkle.empty;
        }
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.Proof_produced_rejected_state -> true
         | _ -> false)
    >>=? fun _ -> return_unit

  (** Test the proof production (used in this test file) and the proof
      verification handles a hard failure. For instance, we try to
      a proof with a ill-signed batch. *)
  let test_proof_with_hard_fail_message () =
    init_with_deposit ()
    >>=? fun (b, account, tx_rollup, store, (_sk, pk, addr), ticket_hash) ->
    hash_tree_from_store store >>= fun l2_context_hash ->
    (* We build a dummy transfer, we don't care about the content, it will hard
       fail on the check signature. *)
    let (random_sk, _, _) = gen_l2_account () in
    let (message, batch_bytes) =
      make_message_transfer
        ~signers:[random_sk]
        [(Bls_pk pk, None, [(addr, ticket_hash, 1L)])]
    in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Op.tx_rollup_submit_batch (B b) account tx_rollup batch_bytes
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    (* Make an invalid commitment for the submitted transfer *)
    let level = Tx_rollup_level.(succ root) in
    Incremental.begin_construction b >>=? fun i ->
    make_incomplete_commitment_for_batch i level tx_rollup []
    >>=? fun (commitment, _) ->
    Op.tx_rollup_commit (I i) account tx_rollup commitment >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* Now we produce a valid proof rejecting the commitment *)
    l2_parameters (B b) >>=? fun l2_parameters ->
    make_proof store l2_parameters message >>= fun proof ->
    Op.tx_rollup_reject
      (B b)
      account
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result:
        {
          context_hash = l2_context_hash;
          withdrawals_merkle_root = Tx_rollup_withdraw.Merkle.empty;
        }
    >>=? fun op ->
    Incremental.add_operation i op >>=? fun _ -> return_unit

  (** Test that an empty proof is enough to reject a commitment on an
      invalid message. Yhe committed message does not change the
      context at all (i.e. the message can not be decoded). *)
  let test_empty_proof_on_invalid_message () =
    init_with_invalid_commitment ()
    >>=? fun (i, contract, tx_rollup, level, message) ->
    let (msg, _) = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message.hash_uncarbonated msg in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    l2_parameters (I i) >>=? fun l2_parameters ->
    valid_empty_proof l2_parameters >>= fun proof ->
    Op.tx_rollup_reject
      (I i)
      contract
      tx_rollup
      level
      msg
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result
    >>=? fun op ->
    Incremental.add_operation i op >>=? fun _ -> return_unit

  (** Test that an empty proof is not able to reject a valid commitment. *)
  let test_invalid_proof_on_invalid_commitment () =
    init_with_valid_commitment ()
    >>=? fun (i, contract, tx_rollup, level, message) ->
    let (msg, _) = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message.hash_uncarbonated msg in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Op.tx_rollup_reject
      (I i)
      contract
      tx_rollup
      level
      msg
      ~message_position:0
      ~message_path
      ~proof:invalid_proof
      ~previous_message_result
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:
        (check_proto_error Tx_rollup_errors.Proof_failed_to_reject)
    >>=? fun _ -> return_unit

  (** Test that rejection successfully fails when there is a disagreement about
      the previous state. *)
  let test_invalid_agreed () =
    init_with_valid_commitment ()
    >>=? fun (i, contract, tx_rollup, level, message) ->
    let (msg, _) = Tx_rollup_message.make_batch message in
    (* This intentionally does not match  *)
    let previous_message_result : Tx_rollup_commitment.message_result =
      {
        (* Expected is Tx_rollup_commitment.empty_l2_context_hash *)
        context_hash = Context_hash.zero;
        withdrawals_merkle_root = Tx_rollup_withdraw.Merkle.empty;
      }
    in
    let message_hash = Tx_rollup_message.hash_uncarbonated msg in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Op.tx_rollup_reject
      (I i)
      contract
      tx_rollup
      level
      msg
      ~message_position:0
      ~message_path
      ~proof:invalid_proof (* doesn't matter -- we'll never check it*)
      ~previous_message_result
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:
        (check_proto_error
           (Tx_rollup_errors.Wrong_rejection_hashes
              {
                provided = previous_message_result;
                computed =
                  Tx_rollup_message_result_hash.of_b58check_exn
                    "txmr221XLNBX67yfP1bzr2Aa5GdnKCDMQJFQDhMVjJmiqMeLnNSX13";
                expected =
                  Tx_rollup_message_result_hash.of_b58check_exn
                    "txmr3BaLWnnLhhPsdH27okZpFG7urW4CyYbgsm3uiyNdFbRC8EtKTX";
              }))
    >>=? fun _ -> return_unit

  (** Test that rejection successfully fails when there's no commitment to
      reject *)
  let test_no_commitment () =
    context_init 1 >>=? fun (b, contracts) ->
    let contract =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
    in
    originate b contract >>=? fun (b, tx_rollup) ->
    let message = "bogus" in
    Op.tx_rollup_submit_batch (B b) contract tx_rollup message
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    Incremental.begin_construction b >>=? fun i ->
    let level = Tx_rollup_level.root in
    let (message, _size) = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    l2_parameters (I i) >>=? fun l2_parameters ->
    valid_empty_proof l2_parameters >>= fun proof ->
    Op.tx_rollup_reject
      (I i)
      contract
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:
        (check_proto_error
           (Tx_rollup_errors.Cannot_reject_level
              {provided = level; accepted_range = None}))
    >>=? fun _ -> return_unit

  (** Test that rejection successfully fails when the rejected commitment is
      already final *)
  let test_commitment_is_final () =
    init_with_valid_commitment ()
    >>=? fun (i, contract, tx_rollup, level, message) ->
    (* Create a new commitment so that once we have finalized the first one,
       we still have a range of valid final commitments *)
    Op.tx_rollup_submit_batch (I i) contract tx_rollup message >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    Incremental.begin_construction b >>=? fun i ->
    let level2 = Tx_rollup_level.succ level in
    make_incomplete_commitment_for_batch i level2 tx_rollup []
    >>=? fun (commitment2, _) ->
    Op.tx_rollup_commit (I i) contract tx_rollup commitment2 >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    Op.tx_rollup_finalize (I i) contract tx_rollup >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    let (message, _size) = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    l2_parameters (I i) >>=? fun l2_parameters ->
    valid_empty_proof l2_parameters >>= fun proof ->
    Op.tx_rollup_reject
      (I i)
      contract
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:
        (check_proto_error
           (Tx_rollup_errors.Cannot_reject_level
              {provided = level; accepted_range = Some (level2, level2)}))
    >>=? fun _ -> return_unit

  (** Test that rejection successfully fails when the message hash does not
      match the one stored in the inbox *)
  let test_wrong_message_hash () =
    init_with_valid_commitment ()
    >>=? fun (i, contract1, tx_rollup, level, prev_message) ->
    let (prev_message, _size) = Tx_rollup_message.make_batch prev_message in
    let prev_message_hash = Tx_rollup_message.hash_uncarbonated prev_message in
    let expected_root =
      Tx_rollup_inbox.Merkle.merklize_list [prev_message_hash]
    in
    let (message, _size) = Tx_rollup_message.make_batch "wrong message" in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    l2_parameters (I i) >>=? fun l2_parameters ->
    valid_empty_proof l2_parameters >>= fun proof ->
    Op.tx_rollup_reject
      (I i)
      contract1
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:
        (check_proto_error
           (Tx_rollup_errors.Wrong_message_path {expected = expected_root}))
    >>=? fun _ -> return_unit

  (** Test that rejection successfully fails when the message position does
      exist in the inbox. *)
  let test_wrong_message_position () =
    init_with_valid_commitment ()
    >>=? fun (i, contract1, tx_rollup, level, message) ->
    let (message, _size) = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    l2_parameters (I i) >>=? fun l2_parameters ->
    valid_empty_proof l2_parameters >>= fun proof ->
    Op.tx_rollup_reject
      (I i)
      contract1
      tx_rollup
      level
      message
      ~message_position:1
      ~message_path
      ~proof
      ~previous_message_result
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:
        (check_proto_error
           (Tx_rollup_errors.Wrong_message_position
              {level; position = 1; length = 1}))
    >>=? fun _ -> return_unit

  (** Test rejecting a commitment to a non-trivial message -- that is,
      not a no-op. *)
  let test_nontrivial_rejection () =
    let (_, _, addr) = gen_l2_account () in
    init_l2_store () >>= fun store ->
    context_init1 () >>=? fun (b, account) ->
    originate b account >>=? fun (b, tx_rollup) ->
    Contract_helpers.originate_contract
      "contracts/tx_rollup_deposit.tz"
      "Unit"
      account
      b
      (is_implicit_exn account)
    >>=? fun (contract, b) ->
    let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash addr) in
    let fee = Test_tez.of_int 10 in
    Op.transaction
      ~counter:(Z.of_int 2)
      ~fee
      (B b)
      account
      contract
      Tez.zero
      ~parameters
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    make_unit_ticket_key (B b) ~ticketer:contract tx_rollup
    >>=? fun ticket_hash ->
    let (deposit_message, _size) =
      Tx_rollup_message.make_deposit
        (is_implicit_exn account)
        (Tx_rollup_l2_address.Indexable.value addr)
        ticket_hash
        (Tx_rollup_l2_qty.of_int64_exn 10L)
    in
    let message_hash = Tx_rollup_message.hash_uncarbonated deposit_message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Incremental.begin_construction b >>=? fun i ->
    make_incomplete_commitment_for_batch i Tx_rollup_level.root tx_rollup []
    >>=? fun (commitment, _) ->
    Op.tx_rollup_commit (I i) account tx_rollup commitment >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    Incremental.begin_construction b >>=? fun i ->
    Op.tx_rollup_reject
      (I i)
      account
      tx_rollup
      Tx_rollup_level.root
      deposit_message
      ~message_position:0
      ~message_path
      ~proof:invalid_proof
      ~previous_message_result
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:
        (check_proto_error Tx_rollup_errors.Proof_failed_to_reject)
    >>=? fun i ->
    (* Check with a reasonable proof *)
    l2_parameters (I i) >>=? fun l2_parameters ->
    make_proof store l2_parameters deposit_message >>= fun proof ->
    Op.tx_rollup_reject
      (I i)
      account
      tx_rollup
      Tx_rollup_level.root
      deposit_message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result
    >>=? fun op ->
    Incremental.add_operation i op >>=? fun _ -> return_unit

  let add_store_to_ctxt ctxt store =
    let open Context.Syntax in
    let time = Time.Protocol.of_seconds 0L in
    let* ctxt = C.add_tree ctxt [] store in
    let* h = C.commit ~time ctxt in
    let index = C.index ctxt in
    let* ctxt = C.checkout_exn index h in
    return ctxt

  let test_large_rejection size =
    let (_sk, _pk, addr) = gen_l2_account () in
    init_l2_store () >>= fun store ->
    context_init1 ~tx_rollup_rejection_max_proof_size:size ()
    >>=? fun (b, account) ->
    originate b account >>=? fun (b, tx_rollup) ->
    Contract_helpers.originate_contract
      "contracts/tx_rollup_deposit.tz"
      "Unit"
      account
      b
      (is_implicit_exn account)
    >>=? fun (contract, b) ->
    let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash addr) in
    let fee = Test_tez.of_int 10 in
    Op.transaction
      ~counter:(Z.of_int 2)
      ~fee
      (B b)
      account
      contract
      Tez.zero
      ~parameters
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    make_unit_ticket_key (B b) ~ticketer:contract tx_rollup
    >>=? fun ticket_hash ->
    let (deposit, _) =
      Tx_rollup_message.make_deposit
        (is_implicit_exn account)
        (Tx_rollup_l2_address.Indexable.value addr)
        ticket_hash
        (Tx_rollup_l2_qty.of_int64_exn 10L)
    in
    let deposit_hash = Tx_rollup_message.hash_uncarbonated deposit in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [deposit_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Incremental.begin_construction b >>=? fun i ->
    make_valid_commitment_for_messages
      ~i
      ~level:Tx_rollup_level.root
      ~tx_rollup
      ~store
      [deposit]
    >>=? fun commitment ->
    Op.tx_rollup_commit (I i) account tx_rollup commitment >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* The previous commitment produced a valid after hash. However, the
       proof required is above [size], so the rejection should succeed. *)
    l2_parameters (I i) >>=? fun l2_parameters ->
    make_proof store l2_parameters deposit >>= fun proof ->
    Incremental.begin_construction b >>=? fun i ->
    Op.tx_rollup_reject
      (I i)
      account
      tx_rollup
      Tx_rollup_level.root
      deposit
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result
    >>=? fun op -> return (i, op)

  (** Test that a commitment which require a too-large proof can be rejected
      even if the after hash is correct. *)
  let test_too_large_rejection () =
    (* With a limit, the commitment is rejected because the required proof
       is above the limit. *)
    test_large_rejection 100 >>=? fun (i, op) ->
    Incremental.add_operation i op >>=? fun _ ->
    (* With a high limit, the commitment can not be rejected as it is valid *)
    test_large_rejection 10_000 >>=? fun (i, op) ->
    Incremental.add_operation
      i
      ~expect_failure:
        (check_proto_error Tx_rollup_errors.Proof_produced_rejected_state)
      op
    >>=? fun _ -> return_unit

  (** Drop the last element of a seq, that is, the last element of a proof *)
  let rec drop x =
    let open Seq in
    match x with
    | Cons (x, xs) -> (
        let node = xs () in
        match node with Nil -> Nil | n -> Cons (x, fun () -> drop n))
    | Nil -> assert false

  let test_valid_proof_truncated () =
    init_l2_store () >>= fun store ->
    context_init1 ~tx_rollup_rejection_max_proof_size:100 ()
    >>=? fun (b, account) ->
    originate b account >>=? fun (b, tx_rollup) ->
    make_deposit b tx_rollup account >>=? fun (b, deposit, _, _) ->
    let deposit_hash = Tx_rollup_message.hash_uncarbonated deposit in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [deposit_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Incremental.begin_construction b >>=? fun i ->
    let level = Tx_rollup_level.root in
    make_valid_commitment_for_messages ~i ~level ~store ~tx_rollup [deposit]
    >>=? fun commitment ->
    Op.tx_rollup_commit (I i) account tx_rollup commitment >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* The previous commitment produced a valid after hash. However, the
       proof required is above [size], so the rejection should succeed with
       a truncated proof. *)
    l2_parameters (I i) >>=? fun l2_parameters ->
    make_proof store l2_parameters deposit >>= fun proof ->
    let proof_truncated =
      let proof_node = proof.state () in
      let truncated_node = drop proof_node in
      {proof with state = (fun () -> truncated_node)}
    in
    (* We try to reject with the truncated proof which is already above the
       size limit. *)
    Incremental.begin_construction b >>=? fun i ->
    Op.tx_rollup_reject
      (I i)
      account
      tx_rollup
      Tx_rollup_level.root
      deposit
      ~message_position:0
      ~message_path
      ~proof:proof_truncated
      ~previous_message_result
    >>=? fun op ->
    Incremental.add_operation i op >>=? fun _ -> return_unit

  let tests =
    [
      Tztest.tztest
        "regression test empty_l2_context_hash"
        `Quick
        test_empty_l2_context_hash;
      Tztest.tztest
        "reject invalid commitment"
        `Quick
        test_valid_proof_on_invalid_commitment;
      Tztest.tztest
        "reject valid commitment fails"
        `Quick
        test_valid_proof_on_valid_commitment;
      Tztest.tztest
        "proof for a hard failing message"
        `Quick
        test_proof_with_hard_fail_message;
      Tztest.tztest
        "empty proof for an invalid message"
        `Quick
        test_empty_proof_on_invalid_message;
      Tztest.tztest
        "reject with invalid proof"
        `Quick
        test_invalid_proof_on_invalid_commitment;
      Tztest.tztest "reject with invalid agreed" `Quick test_invalid_agreed;
      Tztest.tztest "reject with no commitment" `Quick test_no_commitment;
      Tztest.tztest "reject with wrong message" `Quick test_wrong_message_hash;
      Tztest.tztest
        "reject with wrong message position"
        `Quick
        test_wrong_message_position;
      Tztest.tztest "reject too-large proof" `Quick test_too_large_rejection;
      Tztest.tztest "reject a final commitment" `Quick test_commitment_is_final;
      Tztest.tztest
        "test successful and unsuccessful rejection of nontrivial message"
        `Quick
        test_nontrivial_rejection;
      Tztest.tztest
        "reject with a truncated proof above the limit"
        `Quick
        test_valid_proof_truncated;
    ]
end

(** [test_state] tests some edge cases in state management around
    rejecting commitments. *)
let test_state () =
  context_init1 () >>=? fun (b, account1) ->
  originate b account1 >>=? fun (b, tx_rollup) ->
  (* let pkh = is_implicit_exn account1 in *)
  let contents = "bogus" in
  let (message, _) = Tx_rollup_message.make_batch contents in
  let message_hash = Tx_rollup_message.hash_uncarbonated message in
  (match Tx_rollup_inbox.Merkle.compute_path [message_hash] 0 with
  | Ok message_path -> return message_path
  | _ -> assert false)
  >>=? fun message_path ->
  let inbox_hash = Tx_rollup_inbox.Merkle.merklize_list [message_hash] in

  let submit b =
    Op.tx_rollup_submit_batch (B b) account1 tx_rollup contents
    >>=? fun operation -> Block.bake b ~operation
  in

  let reject ?expect_failure b level =
    l2_parameters (B b) >>=? fun l2_parameters ->
    Rejection.valid_empty_proof l2_parameters >>= fun proof ->
    Op.tx_rollup_reject
      (B b)
      account1
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result:Rejection.previous_message_result
    >>=? fun operation ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation i operation ?expect_failure >>=? fun i ->
    Incremental.finalize_block i
  in

  (* Submit bogus message three time to have three inboxes *)
  submit b >>=? fun b ->
  submit b >>=? fun b ->
  submit b >>=? fun b ->
  Context.Tx_rollup.state (B b) tx_rollup >>=? fun initial_state ->
  (* Commit to the first inbox with an incorrect commitment *)
  let commit1 =
    Tx_rollup_commitment.
      {
        level = Tx_rollup_level.root;
        messages = [Tx_rollup_message_result_hash.zero];
        predecessor = None;
        inbox_merkle_root = inbox_hash;
      }
  in
  Op.tx_rollup_commit (B b) account1 tx_rollup commit1 >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  (* Reject the commitment *)
  reject b Tx_rollup_level.root >>=? fun b ->
  (* Check that we went back to the initial state *)
  Context.Tx_rollup.state (B b) tx_rollup >>=? fun state_after_reject ->
  Alcotest.(
    check
      tx_rollup_state_testable_no_storage
      "state unchanged by commit/reject at root"
      initial_state
      state_after_reject) ;
  (* Commit an incorrect commitment again *)
  Op.tx_rollup_commit (B b) account1 tx_rollup commit1 >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  (* Commit a second time *)
  let commit2 =
    {
      commit1 with
      level = Tx_rollup_level.succ commit1.level;
      predecessor = Some (Tx_rollup_commitment.hash commit1);
    }
  in
  Op.tx_rollup_commit (B b) account1 tx_rollup commit2 >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  (* Reject the first commitment *)
  reject b Tx_rollup_level.root >>=? fun b ->
  (* Check that we went back to the initial state *)
  Context.Tx_rollup.state (B b) tx_rollup >>=? fun state_after_reject ->
  Alcotest.(
    check
      tx_rollup_state_testable_no_storage
      "state unchanged by commit/reject at root"
      initial_state
      state_after_reject) ;
  (* Commit twice *)
  let commit1 =
    Tx_rollup_commitment.
      {
        commit1 with
        messages =
          [
            Tx_rollup_commitment.hash_message_result
              Rejection.previous_message_result;
          ];
      }
  in
  let commit2 =
    Tx_rollup_commitment.
      {commit2 with predecessor = Some (Tx_rollup_commitment.hash commit1)}
  in
  Op.tx_rollup_commit (B b) account1 tx_rollup commit1 >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  Op.tx_rollup_commit (B b) account1 tx_rollup commit2 >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  (* committing empty blocks then finalizing *)
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Op.tx_rollup_finalize (B b) account1 tx_rollup >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  (* Check we cannot finalize root anymore *)
  reject
    b
    Tx_rollup_level.root
    ~expect_failure:
      (check_proto_error_f (function
          | Tx_rollup_errors.Cannot_reject_level _ -> true
          | _ -> false))
  >>=? fun b ->
  (* We can reject level 1 *)
  reject b Tx_rollup_level.(succ root) >>=? fun b ->
  (* There is no commitment to finalize anymore *)
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Op.tx_rollup_finalize (B b) account1 tx_rollup >>=? fun operation ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    i
    operation
    ~expect_failure:
      (check_proto_error Tx_rollup_errors.No_commitment_to_finalize)
  >>=? fun i ->
  ignore i ;
  return_unit

(** [test_state_with_deleted] tests an edge cases in state management
    when rejecting commitment whose predecessor has already been
    deleted. *)
let test_state_with_deleted () =
  context_init1 () >>=? fun (b, account1) ->
  originate b account1 >>=? fun (b, tx_rollup) ->
  let contents = "bogus" in
  let (message, _) = Tx_rollup_message.make_batch contents in
  let message_hash = Tx_rollup_message.hash_uncarbonated message in
  (match Tx_rollup_inbox.Merkle.compute_path [message_hash] 0 with
  | Ok message_path -> return message_path
  | _ -> assert false)
  >>=? fun message_path ->
  let inbox_hash = Tx_rollup_inbox.Merkle.merklize_list [message_hash] in
  let submit b =
    Op.tx_rollup_submit_batch (B b) account1 tx_rollup contents
    >>=? fun operation -> Block.bake b ~operation
  in
  (* Create three inboxes *)
  submit b >>=? fun b ->
  submit b >>=? fun b ->
  submit b >>=? fun b ->
  (* Commit to level 0 *)
  let commit0 =
    Tx_rollup_commitment.
      {
        level = Tx_rollup_level.root;
        messages =
          [
            Tx_rollup_commitment.hash_message_result
              Rejection.previous_message_result;
          ];
        predecessor = None;
        inbox_merkle_root = inbox_hash;
      }
  in
  Op.tx_rollup_commit (B b) account1 tx_rollup commit0 >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  (* Commit to level 1 *)
  let commit1 =
    Tx_rollup_commitment.
      {
        level = Tx_rollup_level.succ commit0.level;
        messages = [Tx_rollup_message_result_hash.zero];
        predecessor = Some (Tx_rollup_commitment.hash commit0);
        inbox_merkle_root = inbox_hash;
      }
  in
  Op.tx_rollup_commit (B b) account1 tx_rollup commit1 >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  (* Finalize *)
  Op.tx_rollup_finalize (B b) account1 tx_rollup >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  (* Wait for some blocks, then remove *)
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Op.tx_rollup_remove_commitment (B b) account1 tx_rollup >>=? fun operation ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation i operation >>=? fun i ->
  Incremental.finalize_block i >>=? fun b ->
  (* Reject *)
  let reject ?expect_failure b level =
    l2_parameters (B b) >>=? fun l2_parameters ->
    Rejection.valid_empty_proof l2_parameters >>= fun proof ->
    Op.tx_rollup_reject
      (B b)
      account1
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result:Rejection.previous_message_result
    >>=? fun operation ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation i operation ?expect_failure >>=? fun i ->
    Incremental.finalize_block i
  in
  reject b commit1.level >>=? fun b ->
  ignore b ;
  return_unit

(** [test_state_message_storage_preallocation] verifies that message
   commitment burn is charged upfront. *)
let test_state_message_storage_preallocation () =
  let open Error_monad_operators in
  context_init1 () >>=? fun (b, account1) ->
  originate b account1 >>=? fun (b, tx_rollup) ->
  Incremental.begin_construction b >>=? fun i ->
  let ctxt = Incremental.alpha_ctxt i in
  let (message, _) = Tx_rollup_message.make_batch "bogus" in
  let message_hash = Tx_rollup_message.hash_uncarbonated message in
  let _inbox_hash = Tx_rollup_inbox.Merkle.merklize_list [message_hash] in
  let state = Tx_rollup_state.initial_state ~pre_allocated_storage:Z.zero in
  let occupied_storage_before =
    Tx_rollup_state.Internal_for_tests.get_occupied_storage state
  in
  Tx_rollup_inbox.append_message ctxt tx_rollup state message
  >>=?? fun (ctxt, state, _) ->
  let occupied_storage_after =
    Tx_rollup_state.Internal_for_tests.get_occupied_storage state
  in
  (* The size an empty inbox as created in
     {!Tx_rollup_inbox_storage.prepare_inbox}. *)
  let inbox_preparation = 40 in
  Alcotest.check
    ~pos:__POS__
    zestable
    "the storage occupied by the first message is the size of the inbox plus \
     the preallocation for commiting the message"
    (Z.of_int
       (inbox_preparation + Tx_rollup_commitment_repr.Message_result_hash.size))
    (Z.sub occupied_storage_after occupied_storage_before) ;
  let occupied_storage_before =
    Tx_rollup_state.Internal_for_tests.get_occupied_storage state
  in
  Tx_rollup_inbox.append_message ctxt tx_rollup state message
  >>=?? fun (_ctxt, state, _) ->
  let occupied_storage_after =
    Tx_rollup_state.Internal_for_tests.get_occupied_storage state
  in
  Alcotest.check
    ~pos:__POS__
    zestable
    "the storage occupied by the second message is just the preallocation"
    (Z.of_int Tx_rollup_commitment_repr.Message_result_hash.size)
    (Z.sub occupied_storage_after occupied_storage_before) ;
  return_unit

module Withdraw = struct
  (** [context_init_withdraw n] initializes a context with [n + 1] accounts, one rollup and a
      withdrawal recipient contract. *)
  let context_init_withdraw ?tx_rollup_origination_size
      ?(amount = Z.of_int64 @@ Tx_rollup_l2_qty.to_int64 Nat_ticket.amount) n =
    context_init ?tx_rollup_origination_size (n + 1)
    >>=? fun (block, accounts) ->
    let account1 =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth accounts 0
    in
    originate block account1 >>=? fun (block, tx_rollup) ->
    Nat_ticket.init_deposit amount block tx_rollup account1
    >>=? fun (operation, block, deposit_contract) ->
    Block.bake ~operation block >>=? fun block ->
    Contract_helpers.originate_contract_from_string
      ~script:
        (Format.sprintf
           {| parameter (ticket %s);
              storage (option (ticket %s));
              code { CAR ; SOME ; NIL operation ; PAIR } ;|}
           Nat_ticket.ty_str
           Nat_ticket.ty_str)
      ~storage:"None"
      ~source_contract:account1
      ~baker:(is_implicit_exn account1)
      block
    >>=? fun (withdraw_contract, _script, block) ->
    return
      (account1, accounts, tx_rollup, deposit_contract, withdraw_contract, block)

  (** [context_init1_withdraw] initializes a context with one account, one rollup and a
      withdrawal recipient contract. *)
  let context_init1_withdraw () =
    context_init_withdraw 0
    >>=? fun ( account1,
               _accounts,
               tx_rollup,
               deposit_contract,
               withdraw_contract,
               b ) ->
    return (account1, tx_rollup, deposit_contract, withdraw_contract, b)

  (** [context_init2_withdraw] initializes a context with two accounts, one rollup and a
      withdrawal recipient contract. *)
  let context_init2_withdraw () =
    context_init_withdraw 1
    >>=? fun ( account1,
               accounts,
               tx_rollup,
               deposit_contract,
               withdraw_contract,
               b ) ->
    let account2 =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth accounts 1
    in
    return
      (account1, account2, tx_rollup, deposit_contract, withdraw_contract, b)

  (** [context_finalize_batch_with_withdrawals account tx_rollup batch withdrawals b]
      submits a batch containing the message [batch] to [tx_rollup] in the block [b].
      In the following block, it adds a commitment for that block containing
      [withdrawals] (same format as in [make_incomplete_commitment_for_batch]).
      In the third and final block, it finalizes the commitment.

      It returns the commitment and a list of dummy context hashes
      that was mocked as the result of the applying the batch.
   *)
  let context_finalize_batch_with_withdrawals ~account ~tx_rollup
      ?(batch = "batch") ~withdrawals b =
    Op.tx_rollup_submit_batch (B b) account tx_rollup batch
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    (* Make a commitment for the dummy batch. Mock the
       list of withdrawals as per
       [withdrawals]. Include the commitment in an operation and bake. *)
    Incremental.begin_construction b >>=? fun i ->
    make_incomplete_commitment_for_batch
      i
      Tx_rollup_level.root
      tx_rollup
      withdrawals
    >>=? fun (commitment, context_hash_list) ->
    Op.tx_rollup_commit (I i) account tx_rollup commitment >>=? fun operation ->
    Incremental.add_operation i operation >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* 3. Finalize the commitment *)
    Op.tx_rollup_finalize (B b) account tx_rollup >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    return (commitment, context_hash_list, b)

  (** [test_valid_withdraw] checks that a smart contract can deposit tickets to a
    transaction rollup. *)
  let test_valid_withdraw () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, block)
      ->
    Contract_helpers.originate_contract_from_string
      ~script:
        (Format.sprintf
           {| parameter (ticket %s);
              storage unit;
              code { CDR; NIL operation ; PAIR } ;|}
           Nat_ticket.ty_str)
      ~storage:"Unit"
      ~source_contract:account1
      ~baker:(is_implicit_exn account1)
      block
    >>=? fun (withdraw_dropping_contract, _script, block) ->
    let token_one = Nat_ticket.ex_token ~ticketer:deposit_contract in
    (* The Tx_rollup should own some tickets and the two contract none before
       calling withdraw.*)
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Contract deposit_contract)
      None
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Contract withdraw_contract)
      None
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Tx_rollup tx_rollup)
      (Some (Int64.to_int Nat_ticket.int64_amount))
    >>=? fun () ->
    (* The withdrawal execution operation must include proof that the
       level it specifies allows the withdrawal it executes.

       Currently, for a withdrawal execution [(level, rollup)]
       the protocol only verifies that:
       - at [level], there is a commitment for [rollup]

       It does not yet verify that the effects of the inbox at [level] actually
       enables a withdrawal.

       In this test, we simply add dummy batch and a commitment for that batch to
       to some level, which ensures that the withdrawal can be executed.

       Instead of a dummy batch, a more complete test would add:

       - A deposit operation
       - A L2->L1 operation

       This will result in a withdrawal that can be executed.
    *)

    (* 1. Create a ticket and two withdrawal to empty it *)
    let int64_half_amount = Int64.div Nat_ticket.int64_amount 2L in
    let half_amount = Tx_rollup_l2_qty.of_int64_exn int64_half_amount in
    Nat_ticket.withdrawal
      (B block)
      ~ticketer:deposit_contract
      ~claimer:account1
      ~amount:half_amount
      tx_rollup
    >>=? fun withdraw1 ->
    Nat_ticket.withdrawal
      (B block)
      ~ticketer:deposit_contract
      ~claimer:account1
      ~amount:half_amount
      tx_rollup
    >>=? fun withdraw2 ->
    (* 2 Add a batch message to [b], a commitment for that inbox
       containing the withdrawal at index 0, and finalize that
       commitment *)
    context_finalize_batch_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [withdraw1; withdraw2])]
      block
    >>=? fun (_commitment, context_hash_list, block) ->
    (* -- At this point, everything is in place for
       the user to execute the withdrawal -- *)

    (* 3. Now execute the withdrawal. The ticket should be received by
       withdraw_contract at the default entrypoint. *)
    let entrypoint = Entrypoint.default in
    let context_hash =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
    in
    let withdrawals_list = [withdraw1; withdraw2] in
    let withdraw_position = 0 in
    wrap
    @@ Tx_rollup_withdraw.Merkle.compute_path withdrawals_list withdraw_position
    >>?= fun withdraw_path1 ->
    let withdrawals_merkle_root =
      Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
    in
    occupied_storage_size (B block) tx_rollup
    >>=? fun storage_size_before_withdraw ->
    Op.tx_rollup_withdraw
      (B block)
      ~source:account1
      tx_rollup
      Tx_rollup_level.root
      ~context_hash
      ~contents:(Script.lazy_expr Nat_ticket.contents)
      ~ty:(Script.lazy_expr Nat_ticket.ty)
      ~ticketer:deposit_contract
      half_amount
      ~destination:withdraw_contract
      ~withdraw_position
      withdrawals_merkle_root
      withdraw_path1
      ~message_index:0
      entrypoint
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* 4.1 We assert that [withdraw_contract] has received the ticket as
       expected *)
    occupied_storage_size (B block) tx_rollup
    >>=? fun storage_size_after_withdraw ->
    let extra_storage_space =
      Z.(sub storage_size_after_withdraw storage_size_before_withdraw)
    in
    (* extra space should be allocated for withdraw*)
    assert (Z.zero < extra_storage_space) ;
    Incremental.begin_construction block >>=? fun i ->
    let ctxt = Incremental.alpha_ctxt i in
    wrap_lwt @@ Contract.get_storage ctxt withdraw_contract
    >>=? fun (_ctxt, found_storage) ->
    Format.printf
      "found_storage %s"
      (match found_storage with
      | Some storage -> Expr.to_string storage
      | None -> "None") ;
    let expected_storage =
      Format.sprintf
        "(Some (Pair 0x%s (Pair %d %s)))"
        (Hex.show
           (Hex.of_string
              (Data_encoding.Binary.to_string_exn
                 Contract.encoding
                 deposit_contract)))
        Nat_ticket.contents_nat
        (Tx_rollup_l2_qty.to_string half_amount)
      |> Expr.from_string |> Option.some
    in
    (if expected_storage = found_storage then return_unit
    else Alcotest.fail "Storage didn't match")
    >>=? fun () ->
    (* 4.2 The Tx_rollup should owns some tickets and the withdraw_contract half
       of it.*)
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Contract deposit_contract)
      None
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Contract withdraw_contract)
      (Some (Int64.to_int int64_half_amount))
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Tx_rollup tx_rollup)
      (Some (Int64.to_int int64_half_amount))
    >>=? fun () ->
    (* 5.1 And finally we try to drop the other half amount of ticket. *)
    let withdraw_position = 1 in
    wrap
    @@ Tx_rollup_withdraw.Merkle.compute_path withdrawals_list withdraw_position
    >>?= fun withdraw_path2 ->
    let withdrawals_merkle_root =
      Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
    in
    Op.tx_rollup_withdraw
      (B block)
      ~source:account1
      tx_rollup
      Tx_rollup_level.root
      ~context_hash
      ~contents:(Script.lazy_expr Nat_ticket.contents)
      ~ty:(Script.lazy_expr Nat_ticket.ty)
      ~ticketer:deposit_contract
      half_amount
      ~destination:withdraw_dropping_contract
      ~withdraw_position
      withdrawals_merkle_root
      withdraw_path2
      ~message_index:0
      entrypoint
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* 4. Finally, we assert that [withdraw_contract] has received the ticket as
       expected *)
    Incremental.begin_construction block >>=? fun i ->
    let ctxt = Incremental.alpha_ctxt i in
    wrap_lwt @@ Contract.get_storage ctxt withdraw_dropping_contract
    >>=? fun (_ctxt, found_storage) ->
    let expected_storage = "Unit" |> Expr.from_string |> Option.some in
    (if expected_storage = found_storage then return_unit
    else Alcotest.fail "Storage didn't match")
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Contract deposit_contract)
      None
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Contract withdraw_dropping_contract)
      None
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Tx_rollup tx_rollup)
      None

  (** [test_invalid_withdraw_no_commitment] checks that attempting to
   withdraw from a level with no committed inbox raises an error. *)
  let test_invalid_withdraw_no_commitment () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, b) ->
    Incremental.begin_construction b >>=? fun i ->
    let entrypoint = Entrypoint.default in
    let context_hash = Context_hash.hash_bytes [Bytes.make 20 'c'] in
    make_ticket_key
      (B b)
      ~ty:(Tezos_micheline.Micheline.root Nat_ticket.ty)
      ~contents:(Tezos_micheline.Micheline.root Nat_ticket.contents)
      ~ticketer:deposit_contract
      tx_rollup
    >>=? fun ticket_hash ->
    let dummy_withdraw : Tx_rollup_withdraw.t =
      {
        claimer = is_implicit_exn account1;
        ticket_hash;
        amount = Nat_ticket.amount;
      }
    in
    let withdrawals_list = [dummy_withdraw] in
    let withdraw_position = 0 in
    wrap
    @@ Tx_rollup_withdraw.Merkle.compute_path withdrawals_list withdraw_position
    >>?= fun dummy_withdraw_proof ->
    let withdrawals_merkle_root =
      Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
    in
    Op.tx_rollup_withdraw
      (I i)
      ~source:account1
      tx_rollup
      Tx_rollup_level.root
      ~context_hash
      ~message_index:0
      ~contents:(Script.lazy_expr Nat_ticket.contents)
      ~ty:(Script.lazy_expr Nat_ticket.ty)
      ~ticketer:deposit_contract
      Nat_ticket.amount
      ~destination:withdraw_contract
      ~withdraw_position
      withdrawals_merkle_root
      dummy_withdraw_proof
      entrypoint
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.No_finalized_commitment_for_level
             {level; window = None} ->
             Tx_rollup_level.(level = root)
         | _ -> false)
      i
      operation
    >>=? fun _ -> return_unit

  (** [test_invalid_withdraw_missing_withdraw_in_commitment] tries
     withdrawing when the commitment in question has no withdrawals
     associated. *)
  let test_invalid_withdraw_missing_withdraw_in_commitment () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, b) ->
    let batch = "batch" in
    Op.tx_rollup_submit_batch (B b) account1 tx_rollup batch
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun withdraw ->
    context_finalize_batch_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [])]
      b
    >>=? fun (_commitment, context_hash_list, b) ->
    Incremental.begin_construction b >>=? fun i ->
    (let entrypoint = Entrypoint.default in
     let context_hash =
       WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
     in
     let withdrawals_list = [withdraw] in
     let withdraw_position = 0 in
     wrap
     @@ Tx_rollup_withdraw.Merkle.compute_path
          withdrawals_list
          withdraw_position
     >>?= fun withdraw_path ->
     let withdrawals_merkle_root =
       Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
     in
     Op.tx_rollup_withdraw
       (I i)
       ~source:account1
       tx_rollup
       Tx_rollup_level.root
       ~context_hash
       ~message_index:0
       ~contents:(Script.lazy_expr Nat_ticket.contents)
       ~ty:(Script.lazy_expr Nat_ticket.ty)
       ~ticketer:deposit_contract
       Nat_ticket.amount
       ~destination:withdraw_contract
       ~withdraw_position
       withdrawals_merkle_root
       withdraw_path
       entrypoint)
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:(check_proto_error Tx_rollup_errors.Withdraw_invalid_path)
      i
      operation
    >>=? fun _ -> return_unit

  (** [test_invalid_withdraw_tickets] test withdrawing with tickets
     that do not correspond to the given proof and asserts that errors
     are raised. *)
  let test_invalid_withdraw_tickets () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, b) ->
    let batch = "batch" in
    Op.tx_rollup_submit_batch (B b) account1 tx_rollup batch
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun withdraw ->
    context_finalize_batch_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [withdraw])]
      b
    >>=? fun (_commitment, context_hash_list, b) ->
    (* Try executing the withdrawal with invalid amounts *)
    let entrypoint = Entrypoint.default in
    let context_hash =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
    in
    Incremental.begin_construction b >>=? fun i ->
    List.iter_es
      (fun amount ->
        (let withdrawals_list = [{withdraw with amount}] in
         let withdraw_position = 0 in
         wrap
         @@ Tx_rollup_withdraw.Merkle.compute_path
              withdrawals_list
              withdraw_position
         >>?= fun withdraw_path ->
         let withdrawals_merkle_root =
           Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
         in
         Op.tx_rollup_withdraw
           (I i)
           ~source:account1
           tx_rollup
           Tx_rollup_level.root
           ~context_hash
           ~message_index:0
           ~contents:(Script.lazy_expr Nat_ticket.contents)
           ~ty:(Script.lazy_expr Nat_ticket.ty)
           ~ticketer:deposit_contract
           amount
           ~destination:withdraw_contract
           ~withdraw_position
           withdrawals_merkle_root
           withdraw_path
           entrypoint)
        >>=? fun operation ->
        Incremental.add_operation
          ~expect_failure:
            (check_proto_error Tx_rollup_errors.Withdraw_invalid_path)
          i
          operation
        >>=? fun _i -> return_unit)
      [Tx_rollup_l2_qty.of_int64_exn 9L; Tx_rollup_l2_qty.of_int64_exn 11L]
    >>=? fun () ->
    (* Try with wrong type *)
    let withdrawals_list = [withdraw] in
    let withdraw_position = 0 in
    ( wrap
    @@ Tx_rollup_withdraw.Merkle.compute_path withdrawals_list withdraw_position
    >>?= fun withdraw_path ->
      let withdrawals_merkle_root =
        Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
      in
      Op.tx_rollup_withdraw
        (I i)
        ~source:account1
        tx_rollup
        Tx_rollup_level.root
        ~context_hash
        ~message_index:0
        ~contents:(Script.lazy_expr Nat_ticket.contents)
        ~ty:(Script.lazy_expr @@ Expr.from_string "unit")
        ~ticketer:deposit_contract
        Nat_ticket.amount
        ~destination:withdraw_contract
        withdrawals_merkle_root
        withdraw_path
        ~withdraw_position
        entrypoint )
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:(function
        | Environment.Ecoproto_error
            (Script_tc_errors.Invalid_constant (_, _, _))
          :: _ ->
            return_unit
        | _ -> Alcotest.fail "expected to fail with wrong type")
      i
      operation
    >>=? fun _i ->
    (* Try with wrong contents *)
    (let withdrawals_list = [withdraw] in
     let withdraw_position = 0 in
     wrap
     @@ Tx_rollup_withdraw.Merkle.compute_path
          withdrawals_list
          withdraw_position
     >>?= fun withdraw_path ->
     let withdrawals_merkle_root =
       Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
     in
     Op.tx_rollup_withdraw
       (I i)
       ~source:account1
       tx_rollup
       Tx_rollup_level.root
       ~context_hash
       ~message_index:0
       ~contents:(Script.lazy_expr @@ Expr.from_string "2")
       ~ty:(Script.lazy_expr Nat_ticket.ty)
       ~ticketer:deposit_contract
       Nat_ticket.amount
       ~destination:withdraw_contract
       withdrawals_merkle_root
       withdraw_path
       ~withdraw_position
       entrypoint)
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:(check_proto_error Tx_rollup_errors.Withdraw_invalid_path)
      i
      operation
    >>=? fun _i ->
    (* Try with wrong ticketer *)
    (let withdrawals_list = [withdraw] in
     let withdraw_position = 0 in
     wrap
     @@ Tx_rollup_withdraw.Merkle.compute_path
          withdrawals_list
          withdraw_position
     >>?= fun withdraw_path ->
     let withdrawals_merkle_root =
       Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
     in
     Op.tx_rollup_withdraw
       (I i)
       ~source:account1
       tx_rollup
       Tx_rollup_level.root
       ~context_hash
       ~message_index:0
       ~contents:(Script.lazy_expr Nat_ticket.contents)
       ~ty:(Script.lazy_expr Nat_ticket.ty)
       ~ticketer:account1
       Nat_ticket.amount
       ~destination:withdraw_contract
       withdrawals_merkle_root
       withdraw_path
       ~withdraw_position
       entrypoint)
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:(check_proto_error Tx_rollup_errors.Withdraw_invalid_path)
      i
      operation
    >>=? fun _i -> return_unit

  (** [test_invalid_withdraw_invalid_proof] tries withdrawing with
     an invalid proof. *)
  let test_invalid_withdraw_invalid_proof () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, b) ->
    let batch = "batch" in
    Op.tx_rollup_submit_batch (B b) account1 tx_rollup batch
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun withdrawal1 ->
    let withdrawal2 : Tx_rollup_withdraw.t =
      {withdrawal1 with amount = Tx_rollup_l2_qty.of_int64_exn 5L}
    in
    context_finalize_batch_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [withdrawal1; withdrawal2])]
      b
    >>=? fun (_commitment, context_hash_list, b) ->
    let entrypoint = Entrypoint.default in
    let context_hash =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
    in

    Incremental.begin_construction b >>=? fun i ->
    let withdrawals_list = [withdrawal1; withdrawal2] in
    let withdraw_position = 1 in
    wrap
    @@ Tx_rollup_withdraw.Merkle.compute_path withdrawals_list withdraw_position
    (* We're sending the parameters for withdrawal1, but we calculate
       the proof for withdrawal2 *)
    >>?=
    fun invalid_withdraw_path ->
    let withdrawals_merkle_root =
      Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
    in
    Op.tx_rollup_withdraw
      (I i)
      ~source:account1
      tx_rollup
      Tx_rollup_level.root
      ~context_hash
      ~message_index:0
      ~contents:(Script.lazy_expr Nat_ticket.contents)
      ~ty:(Script.lazy_expr Nat_ticket.ty)
      ~ticketer:deposit_contract
      Nat_ticket.amount
      ~destination:withdraw_contract
      withdrawals_merkle_root
      invalid_withdraw_path
      ~withdraw_position
      entrypoint
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:(check_proto_error Tx_rollup_errors.Withdraw_invalid_path)
      i
      operation
    >>=? fun _ ->
    let withdrawals_list = [withdrawal1] in
    let withdraw_position = 0 in
    ( (* We give the proof for a list of withdrawals that does not correspond
           to the list in the commitment *)
      wrap
    @@ Tx_rollup_withdraw.Merkle.compute_path withdrawals_list withdraw_position
    >>?= fun invalid_withdraw_path ->
      let withdrawals_merkle_root =
        Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
      in
      Op.tx_rollup_withdraw
        (I i)
        ~source:account1
        tx_rollup
        Tx_rollup_level.root
        ~context_hash
        ~message_index:0
        ~contents:(Script.lazy_expr Nat_ticket.contents)
        ~ty:(Script.lazy_expr Nat_ticket.ty)
        ~ticketer:deposit_contract
        Nat_ticket.amount
        ~destination:withdraw_contract
        withdrawals_merkle_root
        invalid_withdraw_path
        ~withdraw_position
        entrypoint )
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:(check_proto_error Tx_rollup_errors.Withdraw_invalid_path)
      i
      operation
    >>=? fun _ -> return_unit

  (** [test_invalid_withdraw_already_consumed] asserts that withdrawing the same
      withdrawal twice raises [Withdraw_already_consumed]. *)
  let test_invalid_withdraw_already_consumed () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, b) ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun withdraw ->
    context_finalize_batch_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [withdraw])]
      b
    >>=? fun (_commitment, context_hash_list, b) ->
    let entrypoint = Entrypoint.default in
    let context_hash =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
    in
    let withdrawals_list = [withdraw] in
    let withdraw_position = 0 in
    wrap
    @@ Tx_rollup_withdraw.Merkle.compute_path withdrawals_list withdraw_position
    >>?= fun withdraw_path ->
    (* Execute withdraw *)
    let withdrawals_merkle_root =
      Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
    in
    Op.tx_rollup_withdraw
      (B b)
      ~source:account1
      tx_rollup
      Tx_rollup_level.root
      ~context_hash
      ~contents:(Script.lazy_expr Nat_ticket.contents)
      ~ty:(Script.lazy_expr Nat_ticket.ty)
      ~ticketer:deposit_contract
      Nat_ticket.amount
      ~destination:withdraw_contract
      withdrawals_merkle_root
      withdraw_path
      ~message_index:0
      ~withdraw_position
      entrypoint
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    (* Execute again *)
    Incremental.begin_construction b >>=? fun i ->
    Op.tx_rollup_withdraw
      (I i)
      ~source:account1
      tx_rollup
      Tx_rollup_level.root
      ~context_hash
      ~contents:(Script.lazy_expr Nat_ticket.contents)
      ~ty:(Script.lazy_expr Nat_ticket.ty)
      ~ticketer:deposit_contract
      Nat_ticket.amount
      ~destination:withdraw_contract
      withdrawals_merkle_root
      withdraw_path
      ~withdraw_position
      ~message_index:0
      entrypoint
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:
        (check_proto_error Tx_rollup_errors.Withdraw_already_consumed)
      i
      operation
    >>=? fun _ -> return_unit

  (** [test_multiple_withdrawals_same_batch] checks that multiple
       withdrawals from the same batch are possible. *)
  let test_multiple_withdrawals_same_batch () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, b) ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      ~amount:(Tx_rollup_l2_qty.of_int64_exn 1L)
      tx_rollup
    >>=? fun withdraw1 ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      ~amount:(Tx_rollup_l2_qty.of_int64_exn 2L)
      tx_rollup
    >>=? fun withdraw2 ->
    let withdraws = [withdraw1; withdraw2] in
    context_finalize_batch_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, withdraws)]
      b
    >>=? fun (_commitment, context_hash_list, b) ->
    let entrypoint = Entrypoint.default in
    let context_hash =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
    in
    let withdraw_root = Tx_rollup_withdraw.Merkle.merklize_list withdraws in
    (* Execute withdraw *)
    let withdraw_op b withdraw_proof qty withdraw_position =
      Op.tx_rollup_withdraw
        (B b)
        ~source:account1
        tx_rollup
        Tx_rollup_level.root
        ~context_hash
        ~contents:(Script.lazy_expr Nat_ticket.contents)
        ~ty:(Script.lazy_expr Nat_ticket.ty)
        ~ticketer:deposit_contract
        qty
        ~destination:withdraw_contract
        ~message_index:0
        ~withdraw_position
        withdraw_root
        withdraw_proof
        entrypoint
    in
    let withdraw_proof =
      match Tx_rollup_withdraw.Merkle.compute_path withdraws 0 with
      | Ok x -> x
      | Error _ -> assert false
    in
    withdraw_op b withdraw_proof (Tx_rollup_l2_qty.of_int64_exn 1L) 0
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    (* Execute again *)
    Incremental.begin_construction b >>=? fun i ->
    withdraw_op b withdraw_proof (Tx_rollup_l2_qty.of_int64_exn 1L) 0
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:
        (check_proto_error Tx_rollup_errors.Withdraw_already_consumed)
      i
      operation
    >>=? fun _ ->
    let withdraw_proof =
      match Tx_rollup_withdraw.Merkle.compute_path withdraws 1 with
      | Ok x -> x
      | Error _ -> assert false
    in
    (* Execute second withdraw *)
    withdraw_op b withdraw_proof (Tx_rollup_l2_qty.of_int64_exn 2L) 1
    >>=? fun operation ->
    Incremental.add_operation i operation >>=? fun _i -> return_unit

  (** [test_multiple_withdrawals_same_inbox] checks that multiple
       withdrawals from the same inbox are possible. *)
  let test_multiple_withdrawals_same_inbox () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, b) ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      ~amount:(Tx_rollup_l2_qty.of_int64_exn 1L)
      tx_rollup
    >>=? fun withdraw1 ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      ~amount:(Tx_rollup_l2_qty.of_int64_exn 2L)
      tx_rollup
    >>=? fun withdraw2 ->
    Incremental.begin_construction b >>=? fun i ->
    (* 2. Create a commitment *)
    make_incomplete_commitment_for_batch i Tx_rollup_level.root tx_rollup []
    >>=? fun (commitment, _) ->
    Op.tx_rollup_commit (I i) account1 tx_rollup commitment
    >>=? fun operation ->
    Incremental.add_operation i operation >>=? fun i ->
    (* 1. Submit two batches *)
    Op.tx_rollup_submit_batch (I i) account1 tx_rollup "batch"
    >>=? fun operation ->
    Incremental.add_operation i operation >>=? fun i ->
    Op.tx_rollup_submit_batch (I i) account1 tx_rollup "batch2"
    >>=? fun operation ->
    Incremental.add_operation i operation >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    Incremental.begin_construction b >>=? fun i ->
    (* 2. Create a commitment *)
    make_incomplete_commitment_for_batch
      i
      (tx_level 1l)
      tx_rollup
      [(0, [withdraw1]); (1, [withdraw2])]
    >>=? fun (commitment, context_hash_list) ->
    Op.tx_rollup_commit (I i) account1 tx_rollup commitment
    >>=? fun operation ->
    Incremental.add_operation i operation >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* 3. Finalize the commitments *)
    Op.tx_rollup_finalize (B b) account1 tx_rollup >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    Op.tx_rollup_finalize (B b) account1 tx_rollup >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    let entrypoint = Entrypoint.default in
    (* Execute withdraw *)
    let withdraw_op b withdraw_root withdraw_proof qty message_index =
      let context_hash =
        WithExceptions.Option.get ~loc:__LOC__
        @@ List.nth context_hash_list message_index
      in
      Op.tx_rollup_withdraw
        (B b)
        ~source:account1
        tx_rollup
        (tx_level 1l)
        ~context_hash
        ~contents:(Script.lazy_expr Nat_ticket.contents)
        ~ty:(Script.lazy_expr Nat_ticket.ty)
        ~ticketer:deposit_contract
        qty
        ~destination:withdraw_contract
        ~withdraw_position:0
        ~message_index
        withdraw_root
        withdraw_proof
        entrypoint
    in
    let withdraw_root = Tx_rollup_withdraw.Merkle.merklize_list [withdraw1] in
    let withdraw_proof =
      match Tx_rollup_withdraw.Merkle.compute_path [withdraw1] 0 with
      | Ok x -> x
      | _ -> assert false
    in
    withdraw_op
      b
      withdraw_root
      withdraw_proof
      (Tx_rollup_l2_qty.of_int64_exn 1L)
      0
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    (* Execute again *)
    Incremental.begin_construction b >>=? fun i ->
    withdraw_op
      b
      withdraw_root
      withdraw_proof
      (Tx_rollup_l2_qty.of_int64_exn 1L)
      0
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:
        (check_proto_error Tx_rollup_errors.Withdraw_already_consumed)
      i
      operation
    >>=? fun _ ->
    (* Execute second withdraw *)
    let withdraw_root = Tx_rollup_withdraw.Merkle.merklize_list [withdraw2] in
    let withdraw_proof =
      match Tx_rollup_withdraw.Merkle.compute_path [withdraw2] 0 with
      | Ok x -> x
      | _ -> assert false
    in
    withdraw_op
      b
      withdraw_root
      withdraw_proof
      (Tx_rollup_l2_qty.of_int64_exn 2L)
      1
    >>=? fun operation ->
    Incremental.add_operation i operation >>=? fun _i -> return_unit

  (** [test_invalid_withdraw_someone_elses] asserts that attempting to
     execute a withdrawal with an erroneous [recipient] creates an
     incorrect proof.  *)
  let test_invalid_withdraw_someone_elses () =
    context_init2_withdraw ()
    >>=? fun ( account1,
               account2,
               tx_rollup,
               deposit_contract,
               withdraw_contract,
               b ) ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun withdraw ->
    context_finalize_batch_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [withdraw])]
      b
    >>=? fun (_commitment, context_hash_list, b) ->
    let entrypoint = Entrypoint.default in
    let context_hash =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
    in
    let withdrawals_list = [withdraw] in
    let withdraw_position = 0 in
    wrap
    @@ Tx_rollup_withdraw.Merkle.compute_path withdrawals_list withdraw_position
    >>?= fun withdraw_path ->
    (* Execute again *)
    Incremental.begin_construction b >>=? fun i ->
    let withdrawals_merkle_root =
      Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
    in
    Op.tx_rollup_withdraw
      (I i)
      (* The source of the withdrawal execution is not the recipient set in [withdraw] *)
      ~source:account2
      tx_rollup
      Tx_rollup_level.root
      ~context_hash
      ~contents:(Script.lazy_expr Nat_ticket.contents)
      ~ty:(Script.lazy_expr Nat_ticket.ty)
      ~ticketer:deposit_contract
      Nat_ticket.amount
      ~destination:withdraw_contract
      withdrawals_merkle_root
      withdraw_path
      ~withdraw_position
      ~message_index:0
      entrypoint
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:(check_proto_error Tx_rollup_errors.Withdraw_invalid_path)
      i
      operation
    >>=? fun _ -> return_unit

  (** [test_invalid_withdraw_illtyped_entrypoint] asserts that
     attempting to withdraw nat tickets to a contract taking unit
     tickets raises [Bad_contract_parameter]. *)
  let test_invalid_withdraw_illtyped_entrypoint () =
    context_init1_withdraw ()
    >>=? fun ( account1,
               tx_rollup,
               deposit_contract,
               _unused_withdraw_contract,
               b ) ->
    Contract_helpers.originate_contract
      "contracts/tx_rollup_withdraw_unit_tickets.tz"
      "None"
      account1
      b
      (is_implicit_exn account1)
    >>=? fun (withdraw_contract_unit_tickets, b) ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun withdraw ->
    context_finalize_batch_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [withdraw])]
      b
    >>=? fun (_commitment, context_hash_list, b) ->
    let entrypoint = Entrypoint.default in
    let context_hash =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
    in
    let withdrawals_list = [withdraw] in
    let withdraw_position = 0 in
    wrap
    @@ Tx_rollup_withdraw.Merkle.compute_path withdrawals_list withdraw_position
    >>?= fun withdraw_path ->
    Incremental.begin_construction b >>=? fun i ->
    let withdrawals_merkle_root =
      Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
    in
    Op.tx_rollup_withdraw
      (I i)
      ~source:account1
      tx_rollup
      Tx_rollup_level.root
      ~context_hash
      ~contents:(Script.lazy_expr Nat_ticket.contents)
      ~ty:(Script.lazy_expr Nat_ticket.ty)
      ~ticketer:deposit_contract
      Nat_ticket.amount
      ~destination:withdraw_contract_unit_tickets
      withdrawals_merkle_root
      withdraw_path
      ~message_index:0
      ~withdraw_position
      entrypoint
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:
        (check_proto_error
       @@ Script_interpreter.Bad_contract_parameter
            withdraw_contract_unit_tickets)
      i
      operation
    >>=? fun _ -> return_unit

  (** [test_invalid_withdraw_bad_entrypoint] asserts that
      attempting to withdraw nat tickets to a contract taking unit
      tickets raises [Bad_contract_parameter]. *)
  let test_invalid_withdraw_bad_entrypoint () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, b) ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun withdraw ->
    context_finalize_batch_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [withdraw])]
      b
    >>=? fun (_commitment, context_hash_list, b) ->
    let inexistant_entrypoint = Entrypoint.of_string_strict_exn "foobar" in
    let context_hash =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
    in
    let withdrawals_list = [withdraw] in
    let withdraw_position = 0 in
    wrap
    @@ Tx_rollup_withdraw.Merkle.compute_path withdrawals_list withdraw_position
    >>?= fun withdraw_path ->
    let withdrawals_merkle_root =
      Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
    in
    Incremental.begin_construction b >>=? fun i ->
    Op.tx_rollup_withdraw
      (I i)
      ~source:account1
      tx_rollup
      Tx_rollup_level.root
      ~context_hash
      ~contents:(Script.lazy_expr Nat_ticket.contents)
      ~ty:(Script.lazy_expr Nat_ticket.ty)
      ~ticketer:deposit_contract
      Nat_ticket.amount
      ~destination:withdraw_contract
      withdrawals_merkle_root
      withdraw_path
      ~message_index:0
      ~withdraw_position
      inexistant_entrypoint
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:
        (check_proto_error
       @@ Script_interpreter.Bad_contract_parameter withdraw_contract)
      i
      operation
    >>=? fun _ -> return_unit

  (** [test_invalid_message_index] checks that attempting to withdraw from a
      level with a wrong message index raises an error. *)
  let test_invalid_message_index () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, b) ->
    (* 1. Create and submit two dummy batch *)
    let batch1 = "batch" in
    Op.tx_rollup_submit_batch (B b) account1 tx_rollup batch1
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    (* 2.1 Create a ticket and its hash *)
    let ty = Expr.from_string "nat" in
    let contents_nat = 1 in
    let contents = Expr.from_string (string_of_int contents_nat) in
    let amount = Tx_rollup_l2_qty.of_int64_exn 10L in
    make_ticket_key
      (B b)
      ~ty:(Tezos_micheline.Micheline.root ty)
      ~contents:(Tezos_micheline.Micheline.root contents)
      ~ticketer:deposit_contract
      tx_rollup
    >>=? fun ticket_hash ->
    (* 2.2 Create a withdrawal for the ticket *)
    let withdraw : Tx_rollup_withdraw.t =
      {claimer = is_implicit_exn account1; ticket_hash; amount}
    in

    (* 2.3 Finally, make a commitment for the dummy batch.  mock the
       list of withdrawals to include the previously created
       [withdrawal]. Include the commitment in an operation and bake
       it. *)
    Incremental.begin_construction b >>=? fun i ->
    make_incomplete_commitment_for_batch
      i
      Tx_rollup_level.root
      tx_rollup
      [(0, [withdraw])]
    >>=? fun (commitment, context_hash_list) ->
    Op.tx_rollup_commit (I i) account1 tx_rollup commitment
    >>=? fun operation ->
    Incremental.add_operation i operation >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* 3. Finalize the commitment *)
    Op.tx_rollup_finalize (B b) account1 tx_rollup >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    (* -- At this point, everything is in place for
       the user to execute the withdrawal -- *)

    (* 4. Now execute the withdrawal. The ticket should be received
       by withdraw_contract at the default entrypoint. *)
    (let entrypoint = Entrypoint.default in
     let context_hash =
       WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
     in
     let withdrawals_list = [withdraw] in
     let withdraw_position = 0 in
     wrap
     @@ Tx_rollup_withdraw.Merkle.compute_path
          withdrawals_list
          withdraw_position
     >>?= fun withdraw_path ->
     let withdrawals_merkle_root =
       Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
     in
     Op.tx_rollup_withdraw
       (B b)
       ~source:account1
       tx_rollup
       Tx_rollup_level.root
       ~context_hash
       ~contents:(Script.lazy_expr contents)
       ~ty:(Script.lazy_expr ty)
       ~ticketer:deposit_contract
       amount
       ~destination:withdraw_contract
       withdrawals_merkle_root
       withdraw_path
       ~withdraw_position
       ~message_index:1
       entrypoint)
    >>=? fun operation ->
    Incremental.begin_construction b >>=? fun i ->
    (* 5. try with wrong message_index *)
    Incremental.add_operation
      ~expect_failure:(check_proto_error Tx_rollup_errors.Withdraw_invalid_path)
      i
      operation
    >>=? fun _i -> return_unit

  (** [test_too_late_withdrawal] checks that attempting to withdraw from a
      level of a commitment already removed fails. *)
  let test_too_late_withdrawal () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, b) ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun withdraw ->
    context_finalize_batch_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [withdraw])]
      b
    >>=? fun (_commitment, context_hash_list, b) ->
    (* Remove the commitment *)
    Op.tx_rollup_remove_commitment (B b) account1 tx_rollup
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    (* At this point, the withdrawal can no longer be executed *)
    (let entrypoint = Entrypoint.default in
     let context_hash =
       WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
     in
     let withdrawals_list = [withdraw] in
     let withdraw_position = 0 in
     wrap
     @@ Tx_rollup_withdraw.Merkle.compute_path
          withdrawals_list
          withdraw_position
     >>?= fun withdraw_path ->
     let withdrawals_merkle_root =
       Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
     in
     Op.tx_rollup_withdraw
       (B b)
       ~source:account1
       tx_rollup
       Tx_rollup_level.root
       ~context_hash
       ~contents:(Script.lazy_expr Nat_ticket.contents)
       ~ty:(Script.lazy_expr Nat_ticket.ty)
       ~ticketer:deposit_contract
       Nat_ticket.amount
       ~destination:withdraw_contract
       withdrawals_merkle_root
       withdraw_path
       ~withdraw_position
       ~message_index:0
       entrypoint)
    >>=? fun operation ->
    Incremental.begin_construction b >>=? fun i ->
    (* 5. try with correct withdraw but too late *)
    Incremental.add_operation
      ~expect_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.No_finalized_commitment_for_level
             {level; window = None} ->
             Tx_rollup_level.(level = root)
         | _error -> false)
      i
      operation
    >>=? fun _i -> return_unit

  (** [test_withdrawal_accounting_is_cleaned_up_after_removal]
      Check that withdrawal accounting is cleaned
      up along with the commitment.
   *)
  let test_withdrawal_accounting_is_cleaned_up_after_removal () =
    let open Error_monad_operators in
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, b) ->
    let assert_consumed b ~msg consumed_expected =
      Incremental.begin_construction b >>=? fun i ->
      let ctxt = Incremental.alpha_ctxt i in
      Alpha_context.Tx_rollup_withdraw.mem
        ctxt
        tx_rollup
        Tx_rollup_level.root
        ~message_index:0
        ~withdraw_position:0
      >>=?? fun (consumed_actual, _) ->
      Alcotest.(check bool msg consumed_expected consumed_actual) ;
      return_unit
    in

    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun withdraw ->
    context_finalize_batch_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [withdraw])]
      b
    >>=? fun (_commitment, context_hash_list, b) ->
    assert_consumed b ~msg:"should not be consumed before withdrawal" false
    >>=? fun () ->
    (* Exexute with withdrawal *)
    (let entrypoint = Entrypoint.default in
     let context_hash =
       WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
     in
     let withdrawals_list = [withdraw] in
     let withdraw_position = 0 in
     wrap
     @@ Tx_rollup_withdraw.Merkle.compute_path
          withdrawals_list
          withdraw_position
     >>?= fun withdraw_path ->
     let withdrawals_merkle_root =
       Tx_rollup_withdraw.Merkle.merklize_list withdrawals_list
     in
     Op.tx_rollup_withdraw
       (B b)
       ~source:account1
       tx_rollup
       Tx_rollup_level.root
       ~context_hash
       ~contents:(Script.lazy_expr Nat_ticket.contents)
       ~ty:(Script.lazy_expr Nat_ticket.ty)
       ~ticketer:deposit_contract
       Nat_ticket.amount
       ~destination:withdraw_contract
       withdrawals_merkle_root
       withdraw_path
       ~withdraw_position
       ~message_index:0
       entrypoint)
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    assert_consumed b ~msg:"should be consumed after withdrawal" true
    >>=? fun () ->
    (* Remove the commitment *)
    Op.tx_rollup_remove_commitment (B b) account1 tx_rollup
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    assert_consumed
      b
      ~msg:"consumtion memory should be removed with commitment"
      false
    >>=? fun () -> return_unit

  let tests =
    [
      Tztest.tztest "Test withdraw" `Quick test_valid_withdraw;
      Tztest.tztest
        "Test withdraw w/ missing commitment"
        `Quick
        test_invalid_withdraw_no_commitment;
      Tztest.tztest
        "Test withdraw w/ missing withdraw in commitment"
        `Quick
        test_invalid_withdraw_missing_withdraw_in_commitment;
      Tztest.tztest
        "Test withdraw w/ invalid amount"
        `Quick
        test_invalid_withdraw_tickets;
      Tztest.tztest
        "Test withdraw w/ invalid proof"
        `Quick
        test_invalid_withdraw_invalid_proof;
      Tztest.tztest
        "Test withdraw twice"
        `Quick
        test_invalid_withdraw_already_consumed;
      Tztest.tztest
        "Test multiple withdrawals from the same batch"
        `Quick
        test_multiple_withdrawals_same_batch;
      Tztest.tztest
        "Test multiple withdrawals from the same inbox (but different batches)"
        `Quick
        test_multiple_withdrawals_same_inbox;
      Tztest.tztest
        "Test withdraw someone elses's withdraw"
        `Quick
        test_invalid_withdraw_someone_elses;
      Tztest.tztest
        "Test withdraw with an ill-typed entrypoint"
        `Quick
        test_invalid_withdraw_illtyped_entrypoint;
      Tztest.tztest
        "Test withdraw with missing entrypoint"
        `Quick
        test_invalid_withdraw_bad_entrypoint;
      Tztest.tztest
        "Test withdraw w/ an invalid message index"
        `Quick
        test_invalid_message_index;
      Tztest.tztest "Test withdrawing too late" `Quick test_too_late_withdrawal;
      Tztest.tztest
        "Test withdrawing is cleaned up after removal"
        `Quick
        test_withdrawal_accounting_is_cleaned_up_after_removal;
    ]
end

let tests =
  [
    Tztest.tztest
      "check feature flag is disabled"
      `Quick
      test_disable_feature_flag;
    Tztest.tztest "check tx rollup origination and burn" `Quick test_origination;
    Tztest.tztest
      "check two originated tx rollup in one operation have different address"
      `Quick
      test_two_originations;
    Tztest.tztest
      "check the function that updates the burn per byte rate of a transaction \
       rollup"
      `Quick
      test_burn_per_byte_update;
    Tztest.tztest "add one batch to a rollup" `Quick test_add_batch;
    Tztest.tztest "add two batches to a rollup" `Quick test_add_two_batches;
    Tztest.tztest
      "add one batch and limit the burn"
      `Quick
      test_add_batch_with_limit;
    Tztest.tztest
      "Try to add a batch larger than the limit"
      `Quick
      test_batch_too_big;
    Tztest.tztest
      "Try to add several batches to reach the inbox size limit"
      `Quick
      test_inbox_size_too_big;
    Tztest.tztest
      "Try to add several batches to reach the inbox count limit"
      `Quick
      test_inbox_count_too_big;
    Tztest.tztest "Test deposit with valid contract" `Quick test_valid_deposit;
    Tztest.tztest
      "Test deposit with invalid parameter"
      `Quick
      test_invalid_deposit_not_ticket;
    Tztest.tztest
      "Test deposit with too big ticket"
      `Quick
      test_invalid_deposit_too_big_ticket;
    Tztest.tztest
      "Test deposit with too big ticket type"
      `Quick
      test_invalid_deposit_too_big_ticket_type;
    Tztest.tztest
      "Test valid deposit with big ticket"
      `Quick
      test_valid_deposit_big_ticket;
    Tztest.tztest
      "Test valid deposit to inexistant rollup"
      `Quick
      test_valid_deposit_inexistant_rollup;
    Tztest.tztest "Test invalid entrypoint" `Quick test_invalid_entrypoint;
    Tztest.tztest
      "Test valid deposit to invalid L2 address"
      `Quick
      test_invalid_l2_address;
    Tztest.tztest
      "Test valid deposit non internal operation"
      `Quick
      test_deposit_by_non_internal_operation;
    Tztest.tztest
      "Test valid deposit with non-zero amount"
      `Quick
      test_valid_deposit_invalid_amount;
    Tztest.tztest
      "Test depositing too many tickets"
      `Quick
      test_deposit_too_many_tickets;
    Tztest.tztest "Test finalization" `Quick test_finalization;
    Tztest.tztest "Smoke test commitment" `Quick test_commitment_duplication;
    Tztest.tztest
      "Test commitment predecessor edge cases"
      `Quick
      test_commitment_predecessor;
    Tztest.tztest "Test full inbox" `Quick test_full_inbox;
    Tztest.tztest
      "Test too many finalized commitments"
      `Quick
      test_too_many_commitments;
    Tztest.tztest
      "Test finalization edge cases"
      `Quick
      test_finalization_edge_cases;
    Tztest.tztest "Test bond finalization" `Quick test_bond_finalization;
    Tztest.tztest "Test state" `Quick test_state;
    Tztest.tztest
      "Try to commit to the current inbox and fail"
      `Quick
      test_commit_current_inbox;
    Tztest.tztest
      "Test state with deleted commitment"
      `Quick
      test_state_with_deleted;
    Tztest.tztest
      "Test upfront message preallocation"
      `Quick
      test_state_message_storage_preallocation;
    Tztest.tztest
      "Test storage burn for submitting batch"
      `Quick
      test_storage_burn_for_adding_batch;
    Tztest.tztest
      "Test additional space allocation for deposit"
      `Quick
      test_additional_space_allocation_for_valid_deposit;
    Tztest.tztest
      "Test additional space allocation for commitment"
      `Quick
      test_storage_burn_for_commitment;
  ]
  @ Withdraw.tests @ Rejection.tests @ parsing_tests
