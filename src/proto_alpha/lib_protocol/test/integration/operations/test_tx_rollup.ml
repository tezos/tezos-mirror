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

let wrap m = m >|= Environment.wrap_tzresult

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
let context_init ?(tx_rollup_max_unfinalized_levels = 2100)
    ?(tx_rollup_max_ticket_payload_size = 10_240) n =
  Context.init_with_constants
    {
      Context.default_test_contants with
      consensus_threshold = 0;
      tx_rollup_enable = true;
      tx_rollup_finality_period = 1;
      tx_rollup_withdraw_period = 1;
      tx_rollup_max_finalized_levels = 2;
      tx_rollup_max_unfinalized_levels;
      endorsing_reward_per_slot = Tez.zero;
      baking_reward_bonus_per_slot = Tez.zero;
      baking_reward_fixed_portion = Tez.zero;
      tx_rollup_max_ticket_payload_size;
    }
    n

(** [context_init1] initializes a context with no consensus rewards
    to not interfere with balances prediction. It returns the created
    context and 1 contract. *)
let context_init1 ?tx_rollup_max_unfinalized_levels
    ?tx_rollup_max_ticket_payload_size () =
  context_init
    ?tx_rollup_max_unfinalized_levels
    ?tx_rollup_max_ticket_payload_size
    1
  >|=? function
  | (b, contract_1 :: _) -> (b, contract_1)
  | (_, _) -> assert false

(** [context_init2] initializes a context with no consensus rewards
    to not interfere with balances prediction. It returns the created
    context and 2 contracts. *)
let context_init2 ?tx_rollup_max_unfinalized_levels
    ?tx_rollup_max_ticket_payload_size () =
  context_init
    ?tx_rollup_max_unfinalized_levels
    ?tx_rollup_max_ticket_payload_size
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
let init_originate_and_submit ?(batch = String.make 5 'c') () =
  context_init1 () >>=? fun (b, contract) ->
  originate b contract >>=? fun (b, tx_rollup) ->
  Context.Contract.balance (B b) contract >>=? fun balance ->
  Context.Tx_rollup.state (B b) tx_rollup >>=? fun state ->
  Op.tx_rollup_submit_batch (B b) contract tx_rollup batch >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  return ((contract, balance), state, tx_rollup, b)

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
  let public_key = Bls12_381.Signature.MinPk.derive_pk secret_key in
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
  wrap @@ Script_ir_translator.parse_comparable_data ctxt contents_type contents
  >>=? fun (contents, ctxt) ->
  wrap
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

let raw_level level = assert_ok @@ Raw_level.of_int32 level

let merkle_root_empty_withdraw_list = Tx_rollup_withdraw.merkelize_list []

(* Make a valid commitment for a batch.  TODO/TORU: roots are still wrong, of
   course, until we get Merkle proofs In the mean time provides the list of
   withdraw in a association list of [batch_index -> withdraw_list].
   Be careful not to provide a too big withdraw_list as the construction
   is expensive *)
let make_commitment_for_batch i level tx_rollup withdraw_list =
  let ctxt = Incremental.alpha_ctxt i in
  wrap (Alpha_context.Tx_rollup_inbox.get ctxt level tx_rollup)
  >>=? fun (ctxt, metadata) ->
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
              |> Tx_rollup_withdraw.merkelize_list;
          })
      batches_result
  in
  (match Tx_rollup_level.pred level with
  | None -> return_none
  | Some predecessor_level -> (
      wrap (Tx_rollup_commitment.find ctxt tx_rollup predecessor_level)
      >|=? function
      | (_, None) -> None
      | (_, Some {commitment; _}) -> Some (Tx_rollup_commitment.hash commitment)
      ))
  >>=? fun predecessor ->
  let inbox_merkle_root = metadata.merkle_root in
  let commitment : Tx_rollup_commitment.t =
    {level; messages; predecessor; inbox_merkle_root}
  in
  return (commitment, batches_result)

let check_bond ctxt tx_rollup contract count =
  let pkh = is_implicit_exn contract in
  wrap (Tx_rollup_commitment.pending_bonded_commitments ctxt tx_rollup pkh)
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
  wrap @@ Ticket_balance_key.of_ex_token ctxt ~owner token
  >>=? fun (key_hash, ctxt) ->
  wrap (Ticket_balance.get_balance ctxt key_hash) >>=? fun (balance, _) ->
  match (balance, expected) with
  | (Some b, Some e) -> Assert.equal_int ~loc (Z.to_int b) e
  | (Some b, None) ->
      failwith "%s: Expected no balance but got some %d" loc (Z.to_int b)
  | (None, Some b) -> failwith "%s: Expected balance %d but got none" loc b
  | (None, None) -> return ()

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
  Context.get_constants (B b) >>=? fun constant ->
  let message_count = constant.parametric.tx_rollup_max_messages_per_inbox in
  let contents = "some contents" in
  originate b contract >>=? fun (b, tx_rollup) ->
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
  Incremental.begin_construction b >|=? Incremental.alpha_ctxt >>=? fun _ctxt ->
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
  context_init ~tx_rollup_max_unfinalized_levels:5_000 2
  >>=? fun (b, contracts) ->
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
  inbox_burn state contents_size >>?= fun cost ->
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
  make_commitment_for_batch i Tx_rollup_level.root tx_rollup []
  >>=? fun (commitment, _) ->
  (* Successfully fail to submit a different commitment from contract2 *)
  let batches2 : Tx_rollup_message_result_hash.t list =
    [Bytes.make 20 '1'; Bytes.make 20 '2']
    |> List.map (fun hash ->
           let context_hash = Context_hash.hash_bytes [hash] in
           Tx_rollup_commitment.hash_message_result
             {
               context_hash;
               withdrawals_merkle_root = merkle_root_empty_withdraw_list;
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
  let submitted_level = (Level.current (Incremental.alpha_ctxt i)).level in
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  let cost = Constants.tx_rollup_commitment_bond (Incremental.alpha_ctxt i) in
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
  let ctxt = Incremental.alpha_ctxt i in
  wrap (Tx_rollup_commitment.find ctxt tx_rollup Tx_rollup_level.root)
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
  make_commitment_for_batch i Tx_rollup_level.root tx_rollup []
  >>=? fun (commitment, _) ->
  let commitment_for_invalid_inbox = {commitment with level = tx_level 10l} in
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment_for_invalid_inbox
  >>=? fun op ->
  let error =
    Tx_rollup_errors.Commitment_too_early
      {provided = tx_level 10l; expected = tx_level 0l}
  in
  Incremental.add_operation i op ~expect_apply_failure:(check_proto_error error)
  >>=? fun _ ->
  (* Now we submit a real commitment *)
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  (* Commitment without predecessor for block with predecessor*)
  make_commitment_for_batch i Tx_rollup_level.(succ root) tx_rollup []
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
      tx_rollup_max_unfinalized_levels = 15;
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
  let bond = Constants.tx_rollup_commitment_bond (Incremental.alpha_ctxt i) in
  Op.tx_rollup_return_bond (I i) contract1 tx_rollup >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error_f @@ function
       | Tx_rollup_errors.Bond_does_not_exist a_pkh1 -> a_pkh1 = pkh1
       | _ -> false)
  >>=? fun i ->
  make_commitment_for_batch i Tx_rollup_level.root tx_rollup []
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
      make_commitment_for_batch i level tx_rollup [] >>=? fun (commitment, _) ->
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
  make_commitment_for_batch i level tx_rollup [] >>=? fun (commitment, _) ->
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_failure:
      (check_proto_error Tx_rollup_errors.Too_many_finalized_commitments)
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
  let init_with_valid_commitment () =
    context_init1 () >>=? fun (b, contract1) ->
    originate b contract1 >>=? fun (b, tx_rollup) ->
    let message = "bogus" in
    Op.tx_rollup_submit_batch (B b) contract1 tx_rollup message
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    Incremental.begin_construction b >>=? fun i ->
    let level = Tx_rollup_level.root in
    make_commitment_for_batch i level tx_rollup []
    >>=? fun (commitment, _batches_result) ->
    Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    return (i, contract1, tx_rollup, level, message)

  (** [test_success] tests that rejection succeeds if the commitment is
      wrong and the proof is correct. *)
  let test_success () =
    init_with_valid_commitment ()
    >>=? fun (i, contract1, tx_rollup, level, message) ->
    let proof = true in
    let (message, _size) = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Op.tx_rollup_reject
      (I i)
      contract1
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result:
        {
          context_hash = Tx_rollup_commitment.empty_l2_context_hash;
          withdrawals_merkle_root =
            Tx_rollup_withdraw.empty_withdrawals_merkle_root;
        }
    >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    ignore i ;

    return ()

  (** [test_invalid_proof] tests that rejection successfully fails
      with an invalid proof. *)
  let test_invalid_proof () =
    init_with_valid_commitment ()
    >>=? fun (i, contract1, tx_rollup, level, message) ->
    let proof = false in
    let (message, _size) = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Op.tx_rollup_reject
      (I i)
      contract1
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result:
        {
          context_hash = Tx_rollup_commitment.empty_l2_context_hash;
          withdrawals_merkle_root =
            Tx_rollup_withdraw.empty_withdrawals_merkle_root;
        }
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:(check_proto_error Tx_rollup_errors.Invalid_proof)
    >>=? fun i ->
    ignore i ;

    return ()

  (** [test_invalid_agreed] tests that rejection successfully fails
      when there is a disagreement about the previous state. *)
  let test_invalid_agreed () =
    init_with_valid_commitment ()
    >>=? fun (i, contract1, tx_rollup, level, message) ->
    let proof = false in
    let (message, _size) = Tx_rollup_message.make_batch message in
    (* This intentionally does not match  *)
    let previous_message_result : Tx_rollup_commitment.message_result =
      {
        (* Expected is Tx_rollup_commitment.empty_l2_context_hash *)
        context_hash = Context_hash.zero;
        withdrawals_merkle_root =
          Tx_rollup_withdraw.empty_withdrawals_merkle_root;
      }
    in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
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
           (Tx_rollup_errors.Wrong_rejection_hashes
              {
                provided = previous_message_result;
                computed =
                  Tx_rollup_message_result_hash.of_b58check_exn
                    "txmr3jXfJ6zu4AAxg6VEnDAXxDYyucP3ZPoLuzxLn4QcRsyArSHMmX";
                expected =
                  Tx_rollup_message_result_hash.of_b58check_exn
                    "txmr2RQL6pMQMkjZwL28kEeyAGpmaorNx2nT6G9JpQj81ER4XqDpD7";
              }))
    >>=? fun i ->
    ignore i ;

    return ()

  (** [test_no_commitment] tests that rejection successfully fails
      when there's no commitment to reject *)
  let test_no_commitment () =
    context_init 2 >>=? fun (b, contracts) ->
    let contract1 =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
    in
    originate b contract1 >>=? fun (b, tx_rollup) ->
    let message = "bogus" in
    Op.tx_rollup_submit_batch (B b) contract1 tx_rollup message
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    Incremental.begin_construction b >>=? fun i ->
    let level = Tx_rollup_level.root in
    let proof = true in
    let (message, _size) = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Op.tx_rollup_reject
      (I i)
      contract1
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result:
        {
          context_hash = Tx_rollup_commitment.empty_l2_context_hash;
          withdrawals_merkle_root =
            Tx_rollup_withdraw.empty_withdrawals_merkle_root;
        }
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:
        (check_proto_error
           (Tx_rollup_errors.Cannot_reject_level
              {provided = level; accepted_range = None}))
    >>=? fun i ->
    ignore i ;

    return ()

  (** [test_commitment_is_final] tests that rejection successfully fails
      when the rejected commitment is already final *)
  let test_commitment_is_final () =
    init_with_valid_commitment ()
    >>=? fun (i, contract1, tx_rollup, level, message) ->
    (* Create a new commitment so that once we have finalized the fist one,
       we still have a range of valid final commitments *)
    Op.tx_rollup_submit_batch (I i) contract1 tx_rollup message >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    Incremental.begin_construction b >>=? fun i ->
    let level2 = Tx_rollup_level.succ level in
    make_commitment_for_batch i level2 tx_rollup []
    >>=? fun (commitment2, _batches_result) ->
    Op.tx_rollup_commit (I i) contract1 tx_rollup commitment2 >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    Op.tx_rollup_finalize (I i) contract1 tx_rollup >>=? fun op ->
    Incremental.add_operation i op >>=? fun i ->
    let proof = true in
    let (message, _size) = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Op.tx_rollup_reject
      (I i)
      contract1
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result:
        {
          context_hash = Tx_rollup_commitment.empty_l2_context_hash;
          withdrawals_merkle_root =
            Tx_rollup_withdraw.empty_withdrawals_merkle_root;
        }
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:
        (check_proto_error
           (Tx_rollup_errors.Cannot_reject_level
              {provided = level; accepted_range = Some (level2, level2)}))
    >>=? fun i ->
    ignore i ;

    return ()

  (** [test_wrong_message_hash] tests that rejection successfully fails
      when the message hash does not match the one stored in the inbox *)
  let test_wrong_message_hash () =
    init_with_valid_commitment ()
    >>=? fun (i, contract1, tx_rollup, level, _message) ->
    let proof = true in
    let (message, _size) = Tx_rollup_message.make_batch "wrong message" in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Op.tx_rollup_reject
      (I i)
      contract1
      tx_rollup
      level
      message
      ~message_position:0
      ~message_path
      ~proof
      ~previous_message_result:
        {
          context_hash = Tx_rollup_commitment.empty_l2_context_hash;
          withdrawals_merkle_root =
            Tx_rollup_withdraw.empty_withdrawals_merkle_root;
        }
    >>=? fun op ->
    let expected =
      Tx_rollup_inbox.Merkle.root_of_b58check_opt
        "txi1pvHiq799LSL2SXnRipQoCmSHnJ3SYH6SqjvJEQEKyfCLj3hCh"
      |> Option.value_f ~default:(fun () -> assert false)
    in
    let error = Tx_rollup_errors.Wrong_message_path {expected} in
    Incremental.add_operation i op ~expect_failure:(check_proto_error error)
    >>=? fun i ->
    ignore i ;
    return ()

  (** [test_wrong_message_position] tests that rejection successfully fails
      when the message position does exist in the inbox *)
  let test_wrong_message_position () =
    init_with_valid_commitment ()
    >>=? fun (i, contract1, tx_rollup, level, message) ->
    let proof = true in
    let (message, _size) = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message.hash_uncarbonated message in
    let message_path =
      match Tx_rollup_inbox.Merkle.(compute_path [message_hash] 0) with
      | Error _ -> assert false
      | Ok path -> path
    in
    Op.tx_rollup_reject
      (I i)
      contract1
      tx_rollup
      level
      message
      ~message_position:1
      ~message_path
      ~proof
      ~previous_message_result:
        {
          context_hash = Tx_rollup_commitment.empty_l2_context_hash;
          withdrawals_merkle_root =
            Tx_rollup_withdraw.empty_withdrawals_merkle_root;
        }
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_failure:
        (check_proto_error
           (Tx_rollup_errors.Wrong_message_position
              {level; position = 1; length = 1}))
    >>=? fun i ->
    ignore i ;

    return ()

  let tests =
    [
      Tztest.tztest "Test rejection happy path" `Quick test_success;
      Tztest.tztest
        "Test rejection with invalid agreed"
        `Quick
        test_invalid_agreed;
      Tztest.tztest
        "Test rejection with invalid proof"
        `Quick
        test_invalid_proof;
      Tztest.tztest
        "Test rejection with no commitment"
        `Quick
        test_no_commitment;
      Tztest.tztest
        "Test rejection with final commitment"
        `Quick
        test_commitment_is_final;
      Tztest.tztest
        "Test rejection with wrong message"
        `Quick
        test_wrong_message_hash;
      Tztest.tztest
        "Test rejection with wrong message position"
        `Quick
        test_wrong_message_position;
    ]
end

(** [test_state] tests some edge cases in state management around
    rejecting commitments. *)
let test_state () =
  context_init1 () >>=? fun (b, account1) ->
  originate b account1 >>=? fun (b, tx_rollup) ->
  let pkh = is_implicit_exn account1 in
  Incremental.begin_construction b >>=? fun i ->
  let ctxt = Incremental.alpha_ctxt i in
  let (message, _) = Tx_rollup_message.make_batch "bogus" in
  let state = Tx_rollup_state.initial_state in
  wrap (Tx_rollup_inbox.append_message ctxt tx_rollup state message)
  >>=? fun (ctxt, state) ->
  let append_inbox i ctxt state =
    let i = Incremental.set_alpha_ctxt i ctxt in
    (* need to increment state so that the second message goes into a new inbox *)
    Incremental.finalize_block i >>=? fun b ->
    Incremental.begin_construction b >>=? fun i ->
    let ctxt = Incremental.alpha_ctxt i in
    wrap (Tx_rollup_inbox.append_message ctxt tx_rollup state message)
    >|=? fun (ctxt, state) -> (i, ctxt, state)
  in
  append_inbox i ctxt state >>=? fun (i, ctxt, state) ->
  append_inbox i ctxt state >>=? fun (i, ctxt, state) ->
  let level0 = Tx_rollup_level.root in
  let inbox_hash = Tx_rollup_inbox.hash_inbox [message] in
  let add_commitment ctxt state level predecessor =
    let commitment =
      Tx_rollup_commitment.
        {
          level;
          messages = [Tx_rollup_message_result_hash.zero];
          predecessor;
          inbox_hash;
        }
    in
    wrap
      (Tx_rollup_commitment.add_commitment ctxt tx_rollup state pkh commitment)
    >|=? fun (ctxt, state) -> (ctxt, state, commitment)
  in
  let state_before = state in
  (* Create and reject a commitment at level 0 *)
  add_commitment ctxt state Tx_rollup_level.root None
  >>=? fun (ctxt, state, _) ->
  wrap (Tx_rollup_commitment.reject_commitment ctxt tx_rollup state level0)
  >>=? fun (ctxt, state) ->
  Alcotest.(
    check
      tx_rollup_state_testable
      "state unchanged by commit/reject at root"
      state_before
      state) ;
  (* Create a commitment at level 0; create and reject a commitment at level 1 *)
  add_commitment ctxt state Tx_rollup_level.root None
  >>=? fun (ctxt, state, commitment0) ->
  let level1 = Tx_rollup_level.succ level0 in
  let commitment0_hash = Tx_rollup_commitment.hash commitment0 in
  let state_before_reject_1 = state in
  add_commitment ctxt state level1 (Some commitment0_hash)
  >>=? fun (ctxt, state, _) ->
  wrap (Tx_rollup_commitment.reject_commitment ctxt tx_rollup state level1)
  >>=? fun (ctxt, state) ->
  Alcotest.(
    check
      tx_rollup_state_testable
      "state unchanged by commit/reject at l1"
      state_before_reject_1
      state) ;
  wrap
    (Tx_rollup_commitment.reject_commitment
       ctxt
       tx_rollup
       state
       Tx_rollup_level.root)
  >>=? fun (ctxt, state) ->
  Alcotest.(
    check
      tx_rollup_state_testable
      "state unchanged from initial after rejecting all commitments"
      state_before
      state) ;
  (* Now let's try commitments and rejections when there exist some finalized commits. *)
  add_commitment ctxt state level0 None >>=? fun (ctxt, state, commitment0) ->
  let commitment0_hash = Tx_rollup_commitment.hash commitment0 in
  wrap
    (Lwt.return
    @@ Tx_rollup_state.Internal_for_tests.record_inbox_deletion state level0)
  >>=? fun state ->
  let state_before = state in
  add_commitment ctxt state level1 (Some commitment0_hash)
  >>=? fun (ctxt, state, _commitment1) ->
  wrap (Tx_rollup_commitment.reject_commitment ctxt tx_rollup state level1)
  >>=? fun (ctxt, state) ->
  Alcotest.(
    check
      tx_rollup_state_testable
      "state unchanged after add/reject with one final commitment"
      state_before
      state) ;
  (* Add two commitments and reject the first *)
  add_commitment ctxt state level1 (Some commitment0_hash)
  >>=? fun (ctxt, state, commitment1) ->
  let commitment1_hash = Tx_rollup_commitment.hash commitment1 in
  let level2 = Tx_rollup_level.succ level1 in
  add_commitment ctxt state level2 (Some commitment1_hash)
  >>=? fun (ctxt, state, _commitment2) ->
  wrap (Tx_rollup_commitment.reject_commitment ctxt tx_rollup state level1)
  >>=? fun (ctxt, state) ->
  Alcotest.(
    check
      tx_rollup_state_testable
      "state unchanged after add/reject with one final commitment"
      state_before
      state) ;
  ignore state ;
  ignore ctxt ;
  ignore i ;
  return ()

module Withdraw = struct
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
      Tx_rollup_withdraw.
        {claimer = is_implicit_exn claimer; ticket_hash; amount}
  end

  (** [context_init_withdraw n] initializes a context with [n + 1] accounts, one rollup and a
      withdrawal recipient contract. *)
  let context_init_withdraw n =
    context_init (n + 1) >>=? fun (block, accounts) ->
    let account1 =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth accounts 0
    in
    originate block account1 >>=? fun (block, tx_rollup) ->
    let script =
      Format.sprintf
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
                  PUSH nat %Ld;
                  PUSH %s %d;
                  TICKET;
                  PAIR ;
                  TRANSFER_TOKENS;
                  PUSH unit Unit;
                  NIL operation;
                  DIG 2 ;
                  CONS;
                  PAIR } |}
        (Tx_rollup_l2_qty.to_int64 Nat_ticket.amount)
        Nat_ticket.ty_str
        Nat_ticket.contents_nat
    in
    Contract_helpers.originate_contract_from_string
      ~baker:(is_implicit_exn account1)
      ~source_contract:account1
      ~script
      ~storage:"Unit"
      block
    >>=? fun (deposit_contract, _script, block) ->
    Op.transaction
      (B block)
      ~entrypoint:Entrypoint.default
      ~parameters:
        (Script.lazy_expr @@ Expr.from_string
        @@ Printf.sprintf
             {| Pair %S %S |}
             (Tx_rollup.to_b58check tx_rollup)
             "tz4MSfZsn6kMDczShy8PMeB628TNukn9hi2K")
      ~fee:Tez.one
      account1
      deposit_contract
      (Tez.of_mutez_exn 0L)
    >>=? fun operation ->
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
        [withdrawals] (same format as in [make_commitment_for_batch]).
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
    make_commitment_for_batch i Tx_rollup_level.root tx_rollup withdrawals
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
    let withdraw_proof1 =
      Tx_rollup_withdraw.compute_path [withdraw1; withdraw2] 0
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
      ~destination:withdraw_contract
      withdraw_proof1
      ~message_index:0
      entrypoint
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* 4.1 We assert that [withdraw_contract] has received the ticket as
       expected *)
    Incremental.begin_construction block >>=? fun i ->
    let ctxt = Incremental.alpha_ctxt i in
    wrap @@ Contract.get_storage ctxt withdraw_contract
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
    let withdraw_path2 =
      Tx_rollup_withdraw.compute_path [withdraw1; withdraw2] 1
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
      withdraw_path2
      ~message_index:0
      entrypoint
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* 4. Finally, we assert that [withdraw_contract] has received the ticket as
       expected *)
    Incremental.begin_construction block >>=? fun i ->
    let ctxt = Incremental.alpha_ctxt i in
    wrap @@ Contract.get_storage ctxt withdraw_dropping_contract
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
   withdraw from a level with no commited inbox raises an error. *)
  let test_invalid_withdraw_no_commitment () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, b) ->
    Incremental.begin_construction b >>=? fun i ->
    let entrypoint = Entrypoint.default in
    let context_hash = Context_hash.hash_bytes [Bytes.make 20 'c'] in
    (* A dummy path *)
    let dummy_withdraw_proof =
      let ticket_hash = Ticket_hash.zero in
      let dummy_withdraw : Tx_rollup_withdraw.t =
        {
          claimer = is_implicit_exn account1;
          ticket_hash;
          amount = Nat_ticket.amount;
        }
      in
      Tx_rollup_withdraw.compute_path [dummy_withdraw] 0
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
     let withdraw_path = Tx_rollup_withdraw.compute_path [withdraw] 0 in
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
        (let withdraw_path =
           Tx_rollup_withdraw.compute_path [{withdraw with amount}] 0
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
    (let withdraw_path = Tx_rollup_withdraw.compute_path [withdraw] 0 in
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
       withdraw_path
       entrypoint)
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
    (let withdraw_path = Tx_rollup_withdraw.compute_path [withdraw] 0 in
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
       withdraw_path
       entrypoint)
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:(check_proto_error Tx_rollup_errors.Withdraw_invalid_path)
      i
      operation
    >>=? fun _i ->
    (* Try with wrong ticketer *)
    (let withdraw_path = Tx_rollup_withdraw.compute_path [withdraw] 0 in
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
       withdraw_path
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
    (let invalid_withdraw_path =
       (* We're sending the parameters for withdrawal1, but we calculate
          the proof for withdrawal2 *)
       Tx_rollup_withdraw.compute_path [withdrawal1; withdrawal2] 1
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
       invalid_withdraw_path
       entrypoint)
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:(check_proto_error Tx_rollup_errors.Withdraw_invalid_path)
      i
      operation
    >>=? fun _ ->
    (let invalid_withdraw_path =
       (* We give the proof for a list of withdrawals that does not correspond
          to the list in the commitment *)
       Tx_rollup_withdraw.compute_path [withdrawal1] 0
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
       invalid_withdraw_path
       entrypoint)
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
    let withdraw_proof = Tx_rollup_withdraw.compute_path [withdraw] 0 in
    (* Execute withdraw *)
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
      withdraw_proof
      ~message_index:0
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
      withdraw_proof
      ~message_index:0
      entrypoint
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:
        (check_proto_error Tx_rollup_errors.Withdraw_already_consumed)
      i
      operation
    >>=? fun _ -> return_unit

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
    let withdraw_proof = Tx_rollup_withdraw.compute_path [withdraw] 0 in
    (* Execute again *)
    Incremental.begin_construction b >>=? fun i ->
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
      withdraw_proof
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
    let withdraw_proof = Tx_rollup_withdraw.compute_path [withdraw] 0 in
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
      ~destination:withdraw_contract_unit_tickets
      withdraw_proof
      ~message_index:0
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
    let withdraw_proof = Tx_rollup_withdraw.compute_path [withdraw] 0 in
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
      withdraw_proof
      ~message_index:0
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
      ~ticketer:withdraw_contract
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
    make_commitment_for_batch i Tx_rollup_level.root tx_rollup [(0, [withdraw])]
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
     let withdraw_proof = Tx_rollup_withdraw.compute_path [withdraw] 0 in
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
       withdraw_proof
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
     let withdraw_proof = Tx_rollup_withdraw.compute_path [withdraw] 0 in
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
       withdraw_proof
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
        ~withdraw_index:0
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
     let withdraw_proof = Tx_rollup_withdraw.compute_path [withdraw] 0 in
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
       withdraw_proof
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
    Tztest.tztest "Test bond finalization" `Quick test_bond_finalization;
    Tztest.tztest "Test state" `Quick test_state;
  ]
  @ Withdraw.tests @ Rejection.tests @ parsing_tests
