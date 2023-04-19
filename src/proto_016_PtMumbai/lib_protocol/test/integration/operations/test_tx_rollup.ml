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
    Invocation:   dune exec src/proto_016_PtMumbai/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_tx_rollup.ml
    Subject:      Test rollup
*)

open Protocol
open Alpha_context
open Test_tez
open Error_monad_operators

(* Similar to [Block.bake] but ensure that the operation [op] is applied
   in the block *)
let add_operation b op =
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation i op >>=? fun i -> Incremental.finalize_block i

(** [check_tx_rollup_exists ctxt tx_rollup] returns [()] iff [tx_rollup]
    is a valid address for a transaction rollup. Otherwise, it fails. *)
let check_tx_rollup_exists ctxt tx_rollup =
  Context.Tx_rollup.state ctxt tx_rollup >|=? fun (_ : Tx_rollup_state.t) -> ()

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

(** [check_runtime_error e t] checks that the first error of [t] is the
    Michelson runtime error and the second one equals [e]. *)
let check_runtime_error e = function
  | Environment.Ecoproto_error (Script_interpreter.Runtime_contract_error _)
    :: Environment.Ecoproto_error second
    :: _
    when second = e ->
      Assert.test_error_encodings e ;
      return_unit
  | t -> failwith "Expected runtime error, got: %a" Error_monad.pp_print_trace t

(** [test_disable_feature_flag] try to originate a tx rollup with the feature
    flag is deactivated and check it fails *)
let test_disable_feature_flag () =
  Context.init_with_constants1
    {
      Context.default_test_constants with
      tx_rollup = {Context.default_test_constants.tx_rollup with enable = false};
    }
  >>=? fun (b, contract) ->
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_origination (I i) contract >>=? fun (op, _tx_rollup) ->
  Incremental.add_operation
    ~expect_failure:
      (check_proto_error Validate_errors.Manager.Tx_rollup_feature_disabled)
    i
    op
  >>=? fun (_i : Incremental.t) -> return_unit

(** [test_sunset] try to originate a tx rollup after the sunset and check
    that it fails *)
let test_sunset () =
  Context.init_with_constants1
    {
      Context.default_test_constants with
      tx_rollup =
        {
          Context.default_test_constants.tx_rollup with
          enable = true;
          sunset_level = 0l;
        };
    }
  >>=? fun (b, contract) ->
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_origination (I i) contract >>=? fun (op, _tx_rollup) ->
  Incremental.add_operation
    ~expect_failure:
      (check_proto_error Validate_errors.Manager.Tx_rollup_feature_disabled)
    i
    op
  >>=? fun (_i : Incremental.t) -> return_unit

let path =
  project_root
  // "src/proto_016_PtMumbai/lib_protocol/test/integration/operations"

(** [parsing_tests] try originating contracts using the
    type [tx_rollup_l2_address], test that it only works
    when rollups are enabled.
 *)
let parsing_tests =
  let test_origination ~tx_rollup_enable script_path initial_storage =
    Context.init1 ~tx_rollup_enable ~tx_rollup_sunset_level:Int32.max_int ()
    >>=? fun (b, contract) ->
    Contract_helpers.originate_contract
      script_path
      initial_storage
      contract
      b
      (Context.Contract.pkh contract)
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
      ("deposit", path // "contracts/tx_rollup_deposit.tz");
      ("type", path // "contracts/tx_rollup_parse_type.tz");
      ("comparable_type", path // "contracts/tx_rollup_parse_comparable_type.tz");
      ("data", path // "contracts/tx_rollup_parse_data.tz");
    ]

let message_hash_testable : Tx_rollup_message_hash.t Alcotest.testable =
  Alcotest.testable Tx_rollup_message_hash.pp ( = )

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

(** [context_init tup] initializes a context with no consensus rewards
    to not interfere with balances prediction. It returns the created
    context and contracts. *)
let context_init ?(tx_rollup_max_inboxes_count = 2100)
    ?(tx_rollup_rejection_max_proof_size = 30_000)
    ?(tx_rollup_max_ticket_payload_size = 10_240)
    ?(tx_rollup_finality_period = 1) ?(tx_rollup_origination_size = 60_000)
    ?(cost_per_byte = Tez.zero) ?(tx_rollup_hard_size_limit_per_message = 5_000)
    ?(tx_max_messages_per_inbox = 1010) tup =
  Context.init_with_constants_gen
    tup
    {
      Context.default_test_constants with
      consensus_threshold = 0;
      tx_rollup =
        {
          Context.default_test_constants.tx_rollup with
          enable = true;
          sunset_level = Int32.max_int;
          withdraw_period = 2;
          max_commitments_count = 3;
          finality_period = tx_rollup_finality_period;
          origination_size = tx_rollup_origination_size;
          rejection_max_proof_size = tx_rollup_rejection_max_proof_size;
          max_inboxes_count = tx_rollup_max_inboxes_count;
          hard_size_limit_per_message = tx_rollup_hard_size_limit_per_message;
          max_ticket_payload_size = tx_rollup_max_ticket_payload_size;
          max_messages_per_inbox = tx_max_messages_per_inbox;
        };
      endorsing_reward_per_slot = Tez.zero;
      baking_reward_bonus_per_slot = Tez.zero;
      baking_reward_fixed_portion = Tez.zero;
      cost_per_byte;
    }

(** [context_init1] initializes a context with no consensus rewards
    to not interfere with balances prediction. It returns the created
    context and 1 contract. *)
let context_init1 ?tx_rollup_max_inboxes_count
    ?tx_rollup_max_ticket_payload_size ?tx_rollup_rejection_max_proof_size
    ?tx_rollup_finality_period ?tx_rollup_origination_size ?cost_per_byte
    ?tx_rollup_hard_size_limit_per_message ?tx_max_messages_per_inbox () =
  context_init
    ?tx_rollup_max_inboxes_count
    ?tx_rollup_max_ticket_payload_size
    ?tx_rollup_rejection_max_proof_size
    ?tx_rollup_finality_period
    ?tx_rollup_origination_size
    ?cost_per_byte
    ?tx_rollup_hard_size_limit_per_message
    ?tx_max_messages_per_inbox
    Context.T1

(** [context_init2] initializes a context with no consensus rewards
    to not interfere with balances prediction. It returns the created
    context and 2 contracts. *)
let context_init2 ?tx_rollup_max_inboxes_count
    ?tx_rollup_max_ticket_payload_size ?cost_per_byte
    ?tx_rollup_hard_size_limit_per_message () =
  context_init
    ?tx_rollup_max_inboxes_count
    ?tx_rollup_max_ticket_payload_size
    ?cost_per_byte
    ?tx_rollup_hard_size_limit_per_message
    Context.T2

(** [originate b contract] originates a tx_rollup from [contract],
    and returns the new block and the tx_rollup address. *)
let originate b contract =
  Op.tx_rollup_origination (B b) contract >>=? fun (operation, tx_rollup) ->
  Block.bake ~operation b >>=? fun b -> return (b, tx_rollup)

(** Initializes the context, originates a tx_rollup and submits a batch.

    Returns the first contract and its balance, the originated tx_rollup,
    the state with the tx_rollup, and the baked block with the batch submitted.
*)
let init_originate_and_submit ?(batch = "batch") ?tx_rollup_origination_size ()
    =
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
    constants.parametric.tx_rollup.max_withdrawals_per_batch
  in
  return Tx_rollup_l2_apply.{tx_rollup_max_withdrawals_per_batch}

let commitment_compact_testable =
  Alcotest.testable Tx_rollup_commitment.Compact.pp (fun r1 r2 ->
      Tx_rollup_commitment_hash.equal
        (Tx_rollup_commitment.Compact.hash r1)
        (Tx_rollup_commitment.Compact.hash r2))

let commitment_hash_testable =
  Alcotest.testable Tx_rollup_commitment_hash.pp Tx_rollup_commitment_hash.( = )

let public_key_hash_testable =
  Alcotest.testable
    Tezos_crypto.Signature.Public_key_hash.pp
    Tezos_crypto.Signature.Public_key_hash.( = )

let raw_level_testable = Alcotest.testable Raw_level.pp Raw_level.( = )

let inbox_testable = Alcotest.testable Tx_rollup_inbox.pp Tx_rollup_inbox.( = )

let rng_state = Random.State.make_self_init ()

let gen_l2_account ?rng_state () =
  let seed =
    Option.map
      (fun rng_state ->
        Bytes.init 32 (fun _ -> char_of_int @@ Random.State.int rng_state 255))
      rng_state
  in
  let pkh, public_key, secret_key =
    Tezos_crypto.Signature.Bls.generate_key ?seed ()
  in
  (secret_key, public_key, pkh)

(** [make_ticket_key ty contents ticketer tx_rollup] computes the ticket hash
    of the ticket containing [contents] of type [ty], crafted by [ticketer] and
    owned by [tx_rollup]. *)
let make_ticket_key ctxt ~ty ~contents ~ticketer tx_rollup =
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
    ~owner:(Tx_rollup tx_rollup)
    (Ticket_token.Ex_token {ticketer; contents_type; contents})
  >|=?? fst

(** [make_unit_ticket_key ticketer tx_rollup] computes the ticket hash of
    the unit ticket crafted by [ticketer] and owned by [tx_rollup]. *)
let make_unit_ticket_key ctxt ~ticketer tx_rollup =
  let open Tezos_micheline.Micheline in
  let open Michelson_v1_primitives in
  let ty = Prim (0, T_unit, [], []) in
  let contents = Prim (0, D_Unit, [], []) in
  make_ticket_key ctxt ~ty ~contents ~ticketer tx_rollup

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
  |> fun x -> x |> Expr.from_string |> lazy_expr

let assert_ok res = match res with Ok r -> r | Error _ -> assert false

let assert_some res = match res with Some r -> r | None -> assert false

let raw_level level = assert_ok @@ Raw_level.of_int32 level

(** Create a deposit on the layer1 side through the origination of a contract
    and return the associated deposit message to apply in the layer2. *)
let make_deposit b tx_rollup l1_src addr =
  let script = path // "contracts/tx_rollup_deposit.tz" in
  Contract_helpers.originate_contract
    script
    "Unit"
    l1_src
    b
    (Context.Contract.pkh l1_src)
  >>=? fun (contract, b) ->
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash addr) in
  let fee = Test_tez.of_int 10 in
  Op.transaction
    ~counter:(Manager_counter.Internal_for_tests.of_int 2)
    ~fee
    (B b)
    l1_src
    contract
    Tez.zero
    ~parameters
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  make_unit_ticket_key (B b) ~ticketer:contract tx_rollup
  >>=? fun ticket_hash ->
  let deposit, cumulated_size =
    Tx_rollup_message.make_deposit
      (Context.Contract.pkh l1_src)
      (Tx_rollup_l2_address.Indexable.value addr)
      ticket_hash
      (Tx_rollup_l2_qty.of_int64_exn 100_000L)
  in
  return (b, (deposit, cumulated_size), ticket_hash)

(** Create an incomplete (but valid) commitment for a given level. It is
    incomplete in the sense that the Merkle roots for each message are generated
    with [Context_hash.hash_string message_index]. In the meantime provides the
    list of withdraw in a association list of [batch_index -> withdraw_list].
    Be careful not to provide a too-big withdraw_list as the construction is
    expensive *)
let make_incomplete_commitment_for_batch context level tx_rollup withdraw_list =
  Context.Tx_rollup.inbox context tx_rollup level >>=? fun metadata ->
  let metadata = assert_some metadata in
  let str_for_context_hash =
    Data_encoding.Binary.to_string_exn Tx_rollup_inbox.encoding metadata
  in
  List.init ~when_negative_length:[] metadata.inbox_length (fun i ->
      Context_hash.hash_string [str_for_context_hash ^ string_of_int i])
  >>?= fun batches_result ->
  let messages =
    List.mapi
      (fun i v ->
        Tx_rollup_message_result_hash.hash_uncarbonated
          {
            context_hash = v;
            withdraw_list_hash =
              List.assq i withdraw_list |> Option.value ~default:[]
              |> Tx_rollup_withdraw_list_hash.hash_uncarbonated;
          })
      batches_result
  in
  (match Tx_rollup_level.pred level with
  | None -> return_none
  | Some predecessor_level ->
      Context.Tx_rollup.commitment context tx_rollup predecessor_level
      >|=? fun commitment_opt ->
      Option.map
        (fun Tx_rollup_commitment.Submitted_commitment.{commitment; _} ->
          Tx_rollup_commitment.Compact.hash commitment)
        commitment_opt)
  >>=? fun predecessor ->
  let inbox_merkle_root = metadata.merkle_root in
  let commitment : Tx_rollup_commitment.Full.t =
    {level; messages; predecessor; inbox_merkle_root}
  in
  return (commitment, batches_result)

(** Check that the given contract has [count] pending bonded commitments *)
let check_bond ctxt tx_rollup contract count =
  let pkh = Context.Contract.pkh contract in
  Tx_rollup_commitment.pending_bonded_commitments ctxt tx_rollup pkh
  >>=?? fun (_, pending) ->
  Alcotest.(check int "Pending bonded commitment count correct" count pending) ;
  return ()

let check_bond_from_block b tx_rollup contract count =
  Incremental.begin_construction b >>=? fun i ->
  check_bond (Incremental.alpha_ctxt i) tx_rollup contract count

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
  Ticket_balance_key.of_ex_token ctxt ~owner token >>=?? fun (key_hash, ctxt) ->
  Ticket_balance.get_balance ctxt key_hash >>=?? fun (balance, _) ->
  match (balance, expected) with
  | Some b, Some e -> Assert.equal_int ~loc (Z.to_int b) e
  | Some b, None ->
      failwith "%s: Expected no balance but got some %d" loc (Z.to_int b)
  | None, Some b -> failwith "%s: Expected balance %d but got none" loc b
  | None, None -> return ()

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
      {ticketer; contents_type = Script_typed_ir.nat_t; contents}

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

  let withdrawal ctxt ~ticketer ~claimer ?(amount = amount) tx_rollup :
      (Tx_rollup_withdraw.t * Tx_rollup_reveal.t) tzresult Lwt.t =
    ticket_hash ctxt ~ticketer ~tx_rollup >|=? fun ticket_hash ->
    let claimer = Context.Contract.pkh claimer in
    ( Tx_rollup_withdraw.{claimer; ticket_hash; amount},
      Tx_rollup_reveal.
        {
          contents = Script.lazy_expr contents;
          ty = Script.lazy_expr ty;
          ticketer;
          amount;
          claimer;
        } )

  let init_deposit_contract amount block account =
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
                ASSERT_SOME;
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
      ~baker:(Context.Contract.pkh account)
      ~source_contract:account
      ~script
      ~storage:"Unit"
      block

  let deposit_op block tx_rollup pkh account deposit_contract =
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

  (** Return an operation to originate a contract that will deposit [amount]
      tickets to l2 address [pkh] on [tx_rollup] *)
  let init_deposit amount ?(pkh = "tz4MSfZsn6kMDczShy8PMeB628TNukn9hi2K") block
      tx_rollup account =
    init_deposit_contract amount block account
    >>=? fun (deposit_contract, _script, block) ->
    deposit_op block tx_rollup pkh account deposit_contract >|=? fun op ->
    (op, block, deposit_contract)
end

let message_result_hash_in_commitment (commitment : Tx_rollup_commitment.Full.t)
    ~message_position =
  WithExceptions.Option.get
    ~loc:__LOC__
    (List.nth commitment.messages message_position)

let compute_message_result_path (commitment : Tx_rollup_commitment.Full.t)
    ~message_position =
  let tree =
    List.fold_left
      (fun tree m -> Tx_rollup_commitment.Merkle.snoc tree m)
      Tx_rollup_commitment.Merkle.nil
      commitment.messages
  in
  assert_ok @@ Tx_rollup_commitment.Merkle.compute_path tree message_position

let message_result_hash_and_path (commitment : Tx_rollup_commitment.Full.t)
    ~message_position =
  ( message_result_hash_in_commitment commitment ~message_position,
    compute_message_result_path commitment ~message_position )

let single_message_path message_hash =
  match Tx_rollup_inbox.Merkle.compute_path [message_hash] 0 with
  | Ok message_path -> message_path
  | _ -> raise (Invalid_argument "Single_message_inbox.message_path")

let message_result context_hash withdraws =
  Tx_rollup_message_result.
    {
      context_hash;
      withdraw_list_hash =
        Tx_rollup_withdraw_list_hash.hash_uncarbonated withdraws;
    }

(** ---- TESTS -------------------------------------------------------------- *)

(** [test_origination] originates a transaction rollup and checks that
    it burns the expected quantity of xtz. *)
let test_origination () =
  Context.init1 ~tx_rollup_enable:true ~tx_rollup_sunset_level:Int32.max_int ()
  >>=? fun (b, contract) ->
  Context.get_constants (B b)
  >>=? fun {
             parametric = {tx_rollup = {origination_size; _}; cost_per_byte; _};
             _;
           } ->
  Context.Contract.balance (B b) contract >>=? fun balance ->
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_origination (I i) contract >>=? fun (op, tx_rollup) ->
  Incremental.add_operation i op >>=? fun i ->
  check_tx_rollup_exists (I i) tx_rollup >>=? fun () ->
  cost_per_byte *? Int64.of_int origination_size
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
  Context.init1 ~tx_rollup_enable:true ~tx_rollup_sunset_level:Int32.max_int ()
  >>=? fun (b, contract) ->
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
    Tx_rollup_message.make_batch contents
    |> fst |> Tx_rollup_message_hash.hash_uncarbonated
  in
  let merkle_root = Tx_rollup_inbox.Merkle.merklize_list [contents_hash] in
  let expected_inbox =
    Tx_rollup_inbox.
      {inbox_length = 1; cumulated_size = contents_size; merkle_root}
  in
  Alcotest.(
    check
      (option inbox_testable)
      "Expected inbox is not the computed one"
      (Some expected_inbox)
      inbox) ;
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
  let contents = "batch" in
  context_init1 () >>=? fun (b, contract) ->
  originate b contract >>=? fun (b, tx_rollup) ->
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_submit_batch (I i) contract tx_rollup contents ~burn_limit
  >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_apply_failure:
      (check_proto_error_f (function
          | Tx_rollup_errors.Submit_batch_burn_exceeded _ -> true
          | _ -> false))
  >>=? fun (_ : Incremental.t) -> return_unit

(** [test_add_two_batches] originates a tx rollup and adds two
    arbitrary batches to one of its inboxes. Ensure that their order
    is correct. *)
let test_add_two_batches () =
  (*
    TODO: https://gitlab.com/tezos/tezos/-/issues/2331
    This test can be generalized using a property-based approach.
   *)
  let contents1 = "batch" in
  init_originate_and_submit ~batch:contents1 ()
  >>=? fun ((contract, balance), state, tx_rollup, b) ->
  Op.tx_rollup_submit_batch (B b) contract tx_rollup contents1 >>=? fun op1 ->
  Context.Contract.counter (B b) contract >>=? fun counter ->
  let contents2 = "batch2" in
  Op.tx_rollup_submit_batch
    ~counter:Manager_counter.(succ counter)
    (B b)
    contract
    tx_rollup
    contents2
  >>=? fun op2 ->
  Op.batch_operations ~source:contract (B b) [op1; op2] >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  (* There were a first inbox with one message, and we are looking for
     its successor. *)
  Context.Tx_rollup.inbox (B b) tx_rollup Tx_rollup_level.(succ root)
  >>=? fun inbox ->
  let contents1_hash =
    Tx_rollup_message_hash.hash_uncarbonated
      (Tx_rollup_message.make_batch contents1 |> fst)
  in
  let contents2_hash =
    Tx_rollup_message_hash.hash_uncarbonated
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
      (option inbox_testable)
      "The expected inbox is not the computed one"
      inbox
      (Some expected_inbox)) ;
  inbox_burn state expected_inbox.cumulated_size >>?= fun cost ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) contract balance cost

(** Try to add a batch too large in an inbox. *)
let test_batch_too_big () =
  context_init1 () >>=? fun (b, contract) ->
  originate b contract >>=? fun (b, tx_rollup) ->
  Context.get_constants (B b) >>=? fun constant ->
  let contents =
    String.make
      (constant.parametric.tx_rollup.hard_size_limit_per_message + 1)
      'd'
  in
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_submit_batch (I i) contract tx_rollup contents >>=? fun op ->
  Incremental.add_operation
    i
    ~expect_failure:
      (check_proto_error Tx_rollup_errors.Message_size_exceeds_limit)
    op
  >>=? fun (_ : Incremental.t) -> return_unit

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
    constant.parametric.tx_rollup.hard_size_limit_per_inbox
  in
  Context.Contract.counter (B b) contract >>=? fun counter ->
  let rec fill_inbox inbox_size counter operations =
    (* We set an arbitrary gas limit to be able to reach the size
       limit of an operation. *)
    Op.tx_rollup_submit_batch
      ~gas_limit:(Custom_gas (Gas.Arith.integral_of_int_exn 20_000))
      ~counter
      (B b)
      contract
      tx_rollup
      contents
    >>=? fun operation ->
    let new_inbox_size = inbox_size + message_size in
    if new_inbox_size < tx_rollup_inbox_limit then
      fill_inbox
        new_inbox_size
        (Manager_counter.succ counter)
        (operation :: operations)
    else
      Incremental.begin_construction b >>=? fun i ->
      k i inbox_size (operation, operations)
  in
  fill_inbox 0 counter []

(** Try to add enough large batches to reach the size limit of an inbox. *)
let test_inbox_size_too_big () =
  context_init1 () >>=? fun (b, contract) ->
  Context.get_constants (B b) >>=? fun constant ->
  let tx_rollup_batch_limit =
    constant.parametric.tx_rollup.hard_size_limit_per_message - 1
  in
  let contents = String.make tx_rollup_batch_limit 'd' in
  originate b contract >>=? fun (b, tx_rollup) ->
  fill_inbox b tx_rollup contract contents (fun i _ (op, ops) ->
      Op.batch_operations
        ~recompute_counters:true
        ~source:contract
        (I i)
        (ops @ [op])
      >>=? fun op ->
      Incremental.add_operation
        i
        op
        ~check_size:false
        ~expect_apply_failure:
          (check_proto_error_f (function
              | Tx_rollup_errors.Inbox_size_would_exceed_limit _ -> true
              | _ -> false))
      >>=? fun (_i : Incremental.t) -> return_unit)

(** Try to add enough batches to reach the batch count limit of an inbox. *)
let test_inbox_count_too_big () =
  context_init1 ~tx_max_messages_per_inbox:505 () >>=? fun (b, contract) ->
  let _, _, pkh = gen_l2_account () in
  Context.get_constants (B b) >>=? fun constant ->
  let message_count = constant.parametric.tx_rollup.max_messages_per_inbox in
  let contents = "some contents" in
  originate b contract >>=? fun (b, tx_rollup) ->
  let script = path // "contracts/tx_rollup_deposit.tz" in
  Contract_helpers.originate_contract
    script
    "Unit"
    contract
    b
    (Context.Contract.pkh contract)
  >>=? fun (deposit_contract, b) ->
  let rec fill_inbox b counter n batch =
    (* By default, the [gas_limit] is the maximum gas that can be
       consumed by an operation. We set a lower (arbitrary) limit to
       be able to reach the size limit of an operation. *)
    Op.tx_rollup_submit_batch
      ~gas_limit:(Custom_gas (Gas.Arith.integral_of_int_exn 3_500))
      ~counter
      (B b)
      contract
      tx_rollup
      contents
    >>=? fun op ->
    (match batch with
    | None -> return op
    | Some batch ->
        Op.batch_operations
          ~recompute_counters:true
          ~source:contract
          (B b)
          [batch; op])
    >>=? fun op ->
    if n > 0 then fill_inbox b (Manager_counter.succ counter) (n - 1) (Some op)
    else return (op, counter)
  in
  Context.Contract.counter (B b) contract >>=? fun counter ->
  fill_inbox b counter message_count None >>=? fun (batch, counter) ->
  Op.tx_rollup_submit_batch
    ~gas_limit:(Custom_gas (Gas.Arith.integral_of_int_exn 2_500))
    ~counter
    (B b)
    contract
    tx_rollup
    contents
  >>=? fun op1 ->
  Op.batch_operations
    ~recompute_counters:true
    ~source:contract
    (B b)
    [batch; op1]
  >>=? fun op ->
  (* Submitting a new batch to a full inbox fails *)
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    i
    op
    ~check_size:false
    ~expect_apply_failure:
      (check_proto_error_f @@ function
       | Tx_rollup_errors.Inbox_count_would_exceed_limit rollup ->
           rollup = tx_rollup
       | _ -> false)
  >>=? fun (_i : Incremental.t) ->
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
  >>=? fun op2 ->
  Op.batch_operations
    ~recompute_counters:true
    ~source:contract
    (B b)
    [batch; op2]
  >>=? fun op ->
  (* Submitting a new deposit to a full inbox fails *)
  Incremental.add_operation
    i
    op
    ~check_size:false
    ~expect_apply_failure:
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
  let _, _, addr = gen_l2_account () in
  context_init1 () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  make_deposit b tx_rollup account addr
  >>=? fun (b, (deposit, cumulated_size), _) ->
  Context.Tx_rollup.inbox (B b) tx_rollup Tx_rollup_level.root >>=? fun inbox ->
  let merkle_root =
    Tx_rollup_inbox.Merkle.merklize_list
      [Tx_rollup_message_hash.hash_uncarbonated deposit]
  in
  let expected_inbox =
    Tx_rollup_inbox.{inbox_length = 1; cumulated_size; merkle_root}
  in
  Alcotest.(
    check
      (option inbox_testable)
      "Expected inbox different from the computed one"
      inbox
      (Some expected_inbox)) ;
  return_unit

(** [test_additional_space_allocation_for_valid_deposit] originates a tx rollup with small [tx_rollup_origination_size], make a valid deposit and check additional space allocation *)
let test_additional_space_allocation_for_valid_deposit () =
  let _, _, pkh = gen_l2_account () in
  let tx_rollup_origination_size = 1 in
  context_init1 ~tx_rollup_origination_size () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  let script = path // "contracts/tx_rollup_deposit.tz" in
  Contract_helpers.originate_contract
    script
    "Unit"
    account
    b
    (Context.Contract.pkh account)
  >>=? fun (contract, b) ->
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash pkh) in
  let fee = Test_tez.of_int 10 in
  Op.transaction
    ~counter:(Manager_counter.Internal_for_tests.of_int 2)
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
  let _, _, pkh = gen_l2_account () in
  context_init1 () >>=? fun (b, account) ->
  let script = path // "contracts/tx_rollup_deposit.tz" in
  Contract_helpers.originate_contract
    script
    "Unit"
    account
    b
    (Context.Contract.pkh account)
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
    ~expect_apply_failure:
      (check_proto_error_f (function
          | Script_interpreter.Runtime_contract_error _ -> true
          | _ -> false))
  >>=? fun (_ : Incremental.t) -> return_unit

(** [test_invalid_deposit_not_contract] checks a smart contract cannot
    deposit something that is not a ticket. *)
let test_invalid_deposit_not_ticket () =
  let _, _, pkh = gen_l2_account () in

  context_init1 () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  let script = path // "contracts/tx_rollup_deposit_incorrect_param.tz" in
  Contract_helpers.originate_contract
    script
    "Unit"
    account
    b
    (Context.Contract.pkh account)
  >>=? fun (contract, b) ->
  Incremental.begin_construction b >>=? fun i ->
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash pkh) in
  let fee = Test_tez.of_int 10 in
  Op.transaction ~fee (I i) account contract Tez.zero ~parameters >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_apply_failure:
      (check_proto_error_f (function
          | Script_interpreter.Bad_contract_parameter _ -> true
          | _ -> false))
  >>=? fun (_ : Incremental.t) -> return_unit

let string_ticket_of_size expected_size =
  if expected_size < 0 && expected_size mod 8 <> 0 then
    Alcotest.fail
      (Format.asprintf
         "string_ticket_of_size: argument [expected_size] must be positive and \
          a multiple of 8") ;
  let ticket_contents_ty =
    Tezos_micheline.Micheline.Prim (0, Michelson_v1_primitives.T_string, [], [])
  in
  let _, ticket_contents_ty_size =
    Script_typed_ir_size.node_size ticket_contents_ty
  in
  Alcotest.(
    check
      (option sint_testable)
      "Expected size of ticket_contents type"
      (Saturation_repr.of_int_opt 40)
      (Some ticket_contents_ty_size)) ;
  let _, empty_string_size =
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
  let _, ticket_contents_size =
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
  let _, _, pkh = gen_l2_account () in
  context_init1 () >>=? fun (b, account) ->
  Context.get_constants (B b) >>=? fun constant ->
  let tx_rollup_max_ticket_payload_size =
    constant.parametric.tx_rollup.max_ticket_payload_size
  in
  originate b account >>=? fun (b, tx_rollup) ->
  let script = path // "contracts/tx_rollup_deposit_string.tz" in
  Contract_helpers.originate_contract
    script
    "Unit"
    account
    b
    (Context.Contract.pkh account)
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
    ~counter:(Manager_counter.Internal_for_tests.of_int 2)
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
    ~expect_apply_failure:
      (check_proto_error_f (function
          | Tx_rollup_errors_repr.Ticket_payload_size_limit_exceeded _ -> true
          | _ -> false))
  >>=? fun (_ : Incremental.t) -> return_unit

(** [test_invalid_deposit_too_big_ticket_type] tests that depositing a
    ticket that has a content and type whose summed size exceeds
    [tx_rollup_max_ticket_payload_size] fails.*)
let test_invalid_deposit_too_big_ticket_type () =
  let _, _, pkh = gen_l2_account () in
  context_init1 () >>=? fun (b, account) ->
  Context.get_constants (B b) >>=? fun constant ->
  let tx_rollup_max_ticket_payload_size =
    constant.parametric.tx_rollup.max_ticket_payload_size
  in
  originate b account >>=? fun (b, tx_rollup) ->
  let script = path // "contracts/tx_rollup_deposit_pair_string.tz" in
  Contract_helpers.originate_contract
    script
    "Unit"
    account
    b
    (Context.Contract.pkh account)
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
    ~counter:(Manager_counter.Internal_for_tests.of_int 2)
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
    ~expect_apply_failure:
      (check_proto_error_f (function
          | Tx_rollup_errors_repr.Ticket_payload_size_limit_exceeded _ -> true
          | _ -> false))
  >>=? fun (_ : Incremental.t) -> return_unit

(** [test_valid_deposit_big_ticket] tests that depositing a ticket whose size is exactly
    [tx_rollup_max_ticket_payload_size] succeeds.*)
let test_valid_deposit_big_ticket () =
  let _, _, pkh = gen_l2_account () in
  (* [overhead] is the number of bytes introduced by the wrapping of a
     string in a ticket. This encompasses the ticketer, amount and ty
     fields.

     This value has been fetched from the failing test, and acts as a
     regression value. *)
  let overhead = 136 in
  context_init1 () >>=? fun (b, account) ->
  Context.get_constants (B b) >>=? fun constant ->
  let tx_rollup_max_ticket_payload_size =
    constant.parametric.tx_rollup.max_ticket_payload_size
  in
  originate b account >>=? fun (b, tx_rollup) ->
  let script = path // "contracts/tx_rollup_deposit_string.tz" in
  Contract_helpers.originate_contract
    script
    "Unit"
    account
    b
    (Context.Contract.pkh account)
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
    ~counter:(Manager_counter.Internal_for_tests.of_int 2)
    ~fee
    (B b)
    account
    contract
    Tez.zero
    ~parameters
  >>=? fun op ->
  Incremental.add_operation i op >>=? fun (_ : Incremental.t) -> return_unit

(** [test_invalid_entrypoint] checks that a transaction to an invalid entrypoint
    of a transaction rollup fails. *)
let test_invalid_entrypoint () =
  let _, _, pkh = gen_l2_account () in

  context_init1 () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  let script = path // "contracts/tx_rollup_deposit_incorrect_param.tz" in
  Contract_helpers.originate_contract
    script
    "Unit"
    account
    b
    (Context.Contract.pkh account)
  >>=? fun (contract, b) ->
  Incremental.begin_construction b >>=? fun i ->
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash pkh) in
  let fee = Test_tez.of_int 10 in
  Op.transaction ~fee (I i) account contract Tez.zero ~parameters >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_apply_failure:
      (check_proto_error_f (function
          | Script_interpreter.Bad_contract_parameter _ -> true
          | _ -> false))
  >>=? fun (_ : Incremental.t) -> return_unit

(** [test_invalid_l2_address] checks that a smart contract cannot make
    a deposit order to something that is not a valid layer-2 address. *)
let test_invalid_l2_address () =
  context_init1 () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  let script = path // "contracts/tx_rollup_deposit.tz" in
  Contract_helpers.originate_contract
    script
    "Unit"
    account
    b
    (Context.Contract.pkh account)
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
    ~expect_apply_failure:
      (check_proto_error_f (function
          | Script_interpreter.Bad_contract_parameter _ -> true
          | _ -> false))
  >>=? fun (_ : Incremental.t) -> return_unit

(** [test_valid_deposit_invalid_amount] checks that a transaction to a
    transaction rollup fails if the [amount] parameter is not null. *)
let test_valid_deposit_invalid_amount () =
  let _, _, pkh = gen_l2_account () in
  context_init1 () >>=? fun (b, account) ->
  originate b account >>=? fun (b, tx_rollup) ->
  let script = path // "contracts/tx_rollup_deposit_one_mutez.tz" in
  Contract_helpers.originate_contract
    script
    "Unit"
    account
    b
    (Context.Contract.pkh account)
  >>=? fun (contract, b) ->
  Incremental.begin_construction b >>=? fun i ->
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash pkh) in
  let fee = Test_tez.of_int 10 in
  Op.transaction ~fee (I i) account contract Tez.zero ~parameters >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_apply_failure:
      (check_runtime_error
         Script_interpreter_defs.Rollup_invalid_transaction_amount)
  >>=? fun (_ : Incremental.t) -> return_unit

(** [test_deposit_too_many_tickets] checks that a deposit of
     too many tickets is rejected *)
let test_deposit_too_many_tickets () =
  let too_many = Z.succ (Z.of_int64 Int64.max_int) in
  let _, _, pkh = gen_l2_account () in
  context_init1 () >>=? fun (block, account1) ->
  originate block account1 >>=? fun (block, tx_rollup) ->
  Nat_ticket.init_deposit too_many block tx_rollup account1
  >>=? fun (operation, b, deposit_contract) ->
  Block.bake ~allow_manager_failures:true ~operation b >>=? fun b ->
  let fee = Test_tez.of_int 10 in
  let parameters = print_deposit_arg (`Typed tx_rollup) (`Hash pkh) in
  Op.transaction ~fee (B b) account1 deposit_contract Tez.zero ~parameters
  >>=? fun operation ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    i
    operation
    ~expect_apply_failure:
      (check_proto_error Apply.Tx_rollup_invalid_transaction_ticket_amount)
  >>=? fun i ->
  ignore i ;
  return_unit

(** Test that block finalization changes gas rates *)
let test_finalization () =
  context_init2 ~tx_rollup_max_inboxes_count:5_000 () >>=? fun (b, contracts) ->
  let contract, _ = contracts in
  let filler = contract in
  originate b contract >>=? fun (b, tx_rollup) ->
  Context.get_constants (B b)
  >>=? fun {parametric = {tx_rollup = {hard_size_limit_per_inbox; _}; _}; _} ->
  (* Get the initial burn_per_byte. *)
  Context.Tx_rollup.state (B b) tx_rollup >>=? fun state ->
  burn_per_byte state >>?= fun cost ->
  Assert.equal_tez ~loc:__LOC__ Tez.zero cost >>=? fun () ->
  (* Fill the inbox. *)
  Context.get_constants (B b) >>=? fun constant ->
  let tx_rollup_batch_limit =
    constant.parametric.tx_rollup.hard_size_limit_per_message - 1
  in
  let contents = String.make tx_rollup_batch_limit 'd' in
  (* Repeating fill inbox and finalize block to increase EMA
     until EMA is enough to provoke a change of fees. *)
  let rec increase_ema n b tx_rollup f =
    f b tx_rollup >>=? fun (inbox_size, i, (_operation, ops)) ->
    Op.batch_operations ~recompute_counters:true ~source:filler (I i) ops
    >>=? fun op ->
    Incremental.add_operation ~check_size:false i op >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    Context.Tx_rollup.state (B b) tx_rollup >>=? fun state ->
    let inbox_ema =
      Alpha_context.Tx_rollup_state.Internal_for_tests.get_inbox_ema state
    in
    if hard_size_limit_per_inbox * 91 / 100 < inbox_ema then
      return (b, n, inbox_size)
    else increase_ema (n + 1) b tx_rollup f
  in
  ( increase_ema 1 b tx_rollup @@ fun b tx_rollup ->
    fill_inbox b tx_rollup filler contents (fun i size ops ->
        return (size, i, ops)) )
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
          ~hard_limit:hard_size_limit_per_inbox
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
  cost_per_byte *? Int64.of_int Tx_rollup_prefixes.message_result_hash.hash_size
  >>?= fun upfront_cost ->
  upfront_cost +? cost >>?= fun cost ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) contract balance cost

(** [test_commitment_duplication] originates a rollup, and makes a
    commitment. It attempts to add a second commitment for the same
    level, and ensures that this fails.  It adds a commitment with
    the wrong batch count and ensures that that fails. *)
let test_commitment_duplication () =
  context_init2 () >>=? fun (b, (contract1, contract2)) ->
  let pkh1 = Context.Contract.pkh contract1 in
  originate b contract1 >>=? fun (b, tx_rollup) ->
  Context.Contract.balance (B b) contract1 >>=? fun balance ->
  Context.Contract.balance (B b) contract2 >>=? fun balance2 ->
  (* In order to have a permissible commitment, we need a transaction. *)
  let contents = "batch" in
  Op.tx_rollup_submit_batch (B b) contract1 tx_rollup contents
  >>=? fun operation ->
  add_operation b operation >>=? fun b ->
  make_incomplete_commitment_for_batch (B b) Tx_rollup_level.root tx_rollup []
  >>=? fun (commitment, _) ->
  (* Successfully fail to submit a different commitment from contract2 *)
  let batches2 : Tx_rollup_message_result_hash.t list =
    [Bytes.make 20 '1'; Bytes.make 20 '2']
    |> List.map (fun hash ->
           let context_hash = Context_hash.hash_bytes [hash] in
           Tx_rollup_message_result_hash.hash_uncarbonated
             {
               context_hash;
               withdraw_list_hash = Tx_rollup_withdraw_list_hash.empty;
             })
  in
  let commitment_with_wrong_count : Tx_rollup_commitment.Full.t =
    {commitment with messages = batches2}
  in
  Op.tx_rollup_commit (B b) contract2 tx_rollup commitment_with_wrong_count
  >>=? fun op ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    i
    op
    ~expect_apply_failure:(check_proto_error Tx_rollup_errors.Wrong_batch_count)
  >>=? fun i ->
  (* Submit the correct one *)
  Context.get_level (I i) >>?= fun level ->
  let submitted_level = Raw_level.succ level in
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment >>=? fun operation ->
  add_operation b operation >>=? fun b ->
  Context.get_constants (B b) >>=? fun constants ->
  let cost = constants.parametric.tx_rollup.commitment_bond in
  Assert.balance_was_debited ~loc:__LOC__ (B b) contract1 balance cost
  >>=? fun () ->
  (* Successfully fail to submit a duplicate commitment *)
  Op.tx_rollup_commit (B b) contract2 tx_rollup commitment >>=? fun op ->
  Incremental.begin_construction b >>=? fun i ->
  (Incremental.add_operation i op >>= function
   | Ok _ -> failwith "an error was expected"
   | Error e ->
       check_proto_error_f
         (function Tx_rollup_errors.No_uncommitted_inbox -> true | _ -> false)
         e)
  >>=? fun () ->
  (* No charge. *)
  Assert.balance_was_debited ~loc:__LOC__ (I i) contract2 balance2 Tez.zero
  >>=? fun () ->
  Context.Tx_rollup.state (I i) tx_rollup >>=? fun state ->
  let ctxt = Incremental.alpha_ctxt i in
  Tx_rollup_commitment.find ctxt tx_rollup state Tx_rollup_level.root
  >>=?? fun (_, commitment_opt) ->
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
      let compact_commitment = Tx_rollup_commitment.Full.compact commitment in
      Alcotest.(
        check
          commitment_compact_testable
          "Commitment"
          expected_commitment
          compact_commitment) ;
      Alcotest.(
        check commitment_hash_testable "Commitment hash" expected_hash
        @@ Tx_rollup_commitment.Compact.hash compact_commitment) ;
      Alcotest.(check public_key_hash_testable "Committer" pkh1 committer) ;
      Alcotest.(
        check raw_level_testable "Submitted" submitted_level submitted_at) ;
      Alcotest.(check (option raw_level_testable) "Finalized" None finalized_at)) ;
  check_bond ctxt tx_rollup contract1 1 >>=? fun () ->
  check_bond ctxt tx_rollup contract2 0 >>=? fun () ->
  ignore i ;
  return ()

let test_commit_current_inbox () =
  context_init2 () >>=? fun (b, (contract1, contract2)) ->
  originate b contract1 >>=? fun (b, tx_rollup) ->
  (* In order to have a permissible commitment, we need a transaction. *)
  Incremental.begin_construction b >>=? fun i ->
  let contents = "batch" in
  let message, _ = Tx_rollup_message.make_batch contents in
  let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
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
    ~expect_apply_failure:
      (check_proto_error Tx_rollup_errors.No_uncommitted_inbox)
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
    Alcotest.(check zestable msg expected_delta Z.(sub size_after size_before))
  in
  let tx_rollup_origination_size = 1 in
  context_init1 ~tx_rollup_origination_size () >>=? fun (b, contract) ->
  originate b contract >>=? fun (b, tx_rollup) ->
  make_transactions_in tx_rollup contract [2; 3; 4] b >>=? fun b ->
  (* test allocated storage size and balance before/after submit commitment *)
  Incremental.begin_construction b >>=? fun i ->
  occupied_storage_size (B b) tx_rollup >>=? fun storage_size_before_commit ->
  make_incomplete_commitment_for_batch (I i) Tx_rollup_level.root tx_rollup []
  >>=? fun (commitment, _) ->
  Op.tx_rollup_commit (I i) contract tx_rollup commitment >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  occupied_storage_size (I i) tx_rollup >>=? fun storage_size_after_commit ->
  (* adding a commitment frees the inbox storage *)
  let commitment_add_delta = Z.neg Tx_rollup_inbox.size in
  (* removing a commitment doesn't change anything storage burn related *)
  let commitment_remove_delta = Z.zero in
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
  (* the inbox does not allocate anything, because it was already
     allocated by a freed inboxes *)
  let inbox_delta = Z.zero in
  check_storage_delta
    ~__POS__
    "Storage space is freed after finalize"
    ~size_before:storage_size_after_commit
    ~size_after:freed_space_after_finalize
    ~expected_delta:inbox_delta ;
  (* bake one more block so the commitment may be removed *)
  Block.bake b >>=? fun b ->
  (* test freed storage space after remove commitment *)
  Op.tx_rollup_remove_commitment (B b) contract tx_rollup >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  occupied_storage_size (B b) tx_rollup
  >>=? fun freed_space_after_remove_commitment ->
  let commitment_remove_delta = Z.neg commitment_remove_delta in
  check_storage_delta
    ~__POS__
    "Storage space is freed after removing commitment"
    ~size_before:freed_space_after_finalize
    ~size_after:freed_space_after_remove_commitment
    ~expected_delta:commitment_remove_delta ;
  (* test freed storage space after return bond *)
  Op.tx_rollup_return_bond (B b) contract tx_rollup >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  occupied_storage_size (B b) tx_rollup
  >>=? fun freed_space_after_return_bond ->
  let bond_remove_delta = Z.zero in
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
  make_incomplete_commitment_for_batch (I i) Tx_rollup_level.root tx_rollup []
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
  add_operation b operation >>=? fun b ->
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
      int
      "The delta of adding and removing a commitment is zero (modulo \
       preallocation)"
      (-commitment_add_delta - Tx_rollup_prefixes.message_result_hash.hash_size)
      commitment_remove_delta) ;
  (* test freed storage space after return bond *)
  Op.tx_rollup_return_bond (B b) contract tx_rollup >>=? fun operation ->
  add_operation b operation >>=? fun b ->
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
  (* Check error: Commitment for nonexistent block *)
  let bogus_hash =
    Tx_rollup_commitment_hash.of_bytes_exn
      (Bytes.of_string "tcu1deadbeefdeadbeefdeadbeefdead")
  in
  make_incomplete_commitment_for_batch (B b) Tx_rollup_level.root tx_rollup []
  >>=? fun (commitment, _) ->
  let commitment_for_invalid_inbox = {commitment with level = tx_level 10l} in
  Op.tx_rollup_commit (B b) contract1 tx_rollup commitment_for_invalid_inbox
  >>=? fun op ->
  let error =
    Tx_rollup_errors.Commitment_too_early
      {provided = tx_level 10l; expected = tx_level 0l}
  in
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation i op ~expect_apply_failure:(check_proto_error error)
  >>=? fun (_ : Incremental.t) ->
  (* Now we submit a real commitment *)
  Op.tx_rollup_commit (B b) contract1 tx_rollup commitment >>=? fun operation ->
  add_operation b operation >>=? fun b ->
  (* Commitment without predecessor for block with predecessor*)
  make_incomplete_commitment_for_batch
    (B b)
    Tx_rollup_level.(succ root)
    tx_rollup
    []
  >>=? fun (commitment, _) ->
  let commitment_with_missing_predecessor =
    {commitment with predecessor = None}
  in
  Op.tx_rollup_commit
    (B b)
    contract1
    tx_rollup
    commitment_with_missing_predecessor
  >>=? fun op ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    i
    op
    ~expect_apply_failure:
      (check_proto_error_f @@ function
       | Tx_rollup_errors.Wrong_predecessor_hash {provided = None; expected} ->
           expected = commitment.predecessor
       | _ -> false)
  >>=? fun (_i : Incremental.t) ->
  (* Commitment refers to a predecessor which does not exist *)
  let commitment_with_wrong_pred =
    {commitment with predecessor = Some bogus_hash}
  in
  Op.tx_rollup_commit (I i) contract1 tx_rollup commitment_with_wrong_pred
  >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_apply_failure:
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
      Tezos_protocol_016_PtMumbai_parameters.Default_parameters.constants_test with
      consensus_threshold = 0;
      endorsing_reward_per_slot = Tez.zero;
      baking_reward_bonus_per_slot = Tez.zero;
      baking_reward_fixed_portion = Tez.zero;
      tx_rollup =
        {
          Tezos_protocol_016_PtMumbai_parameters.Default_parameters
          .constants_test
            .tx_rollup
          with
          enable = true;
          sunset_level = Int32.max_int;
          max_inboxes_count = 15;
        };
    }
  in
  Context.init_with_constants1 constants >>=? fun (b, contract) ->
  originate b contract >>=? fun (b, tx_rollup) ->
  let range start top =
    let rec aux n acc = if n < start then acc else aux (n - 1) (n :: acc) in
    aux top []
  in
  (* Transactions in blocks [2..16) *)
  make_transactions_in tx_rollup contract (range 2 16) b >>=? fun b ->
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_submit_batch (B b) contract tx_rollup "contents" >>=? fun op ->
  Incremental.add_operation
    i
    op
    ~expect_apply_failure:(check_proto_error Tx_rollup_errors.Too_many_inboxes)
  >>=? fun i ->
  ignore i ;
  return ()

(** [test_bond_finalization] tests that level retirement in fact
    allows bonds to be returned. *)
let test_bond_finalization () =
  context_init1 () >>=? fun (b, contract1) ->
  let pkh1 = Context.Contract.pkh contract1 in
  originate b contract1 >>=? fun (b, tx_rollup) ->
  Context.Contract.balance (B b) contract1 >>=? fun balance ->
  (* Transactions in block 2, 3, 4 *)
  make_transactions_in tx_rollup contract1 [2; 3; 4] b >>=? fun b ->
  (* Let’s try to remove the bond *)
  Context.get_constants (B b) >>=? fun constants ->
  let bond = constants.parametric.tx_rollup.commitment_bond in
  Op.tx_rollup_return_bond (B b) contract1 tx_rollup >>=? fun op ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    i
    op
    ~expect_apply_failure:
      (check_proto_error_f @@ function
       | Tx_rollup_errors.Bond_does_not_exist a_pkh1 -> a_pkh1 = pkh1
       | _ -> false)
  >>=? fun i ->
  Incremental.finalize_block i >>=? fun b ->
  make_incomplete_commitment_for_batch (B b) Tx_rollup_level.root tx_rollup []
  >>=? fun (commitment_a, _) ->
  Op.tx_rollup_commit (B b) contract1 tx_rollup commitment_a
  >>=? fun operation ->
  add_operation b operation >>=? fun b ->
  Op.tx_rollup_return_bond (B b) contract1 tx_rollup >>=? fun op ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    i
    op
    ~expect_apply_failure:
      (check_proto_error_f @@ function
       | Tx_rollup_errors.Bond_in_use a_pkh1 -> a_pkh1 = pkh1
       | _ -> false)
  >>=? fun i ->
  Incremental.finalize_block i >>=? fun b ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) contract1 balance bond
  >>=? fun () ->
  (* Finalize the commitment of level 0. *)
  Op.tx_rollup_finalize (B b) contract1 tx_rollup >>=? fun operation ->
  add_operation b operation >>=? fun b ->
  (* Bake enough block, and remove the commitment of level 0. *)
  Block.bake b ~operations:[] >>=? fun b ->
  Op.tx_rollup_remove_commitment (B b) contract1 tx_rollup >>=? fun operation ->
  add_operation b operation >>=? fun b ->
  (* Try to return the bond *)
  Context.Contract.balance (B b) contract1 >>=? fun balance ->
  Op.tx_rollup_return_bond (B b) contract1 tx_rollup >>=? fun operation ->
  add_operation b operation >>=? fun b ->
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
    ~expect_apply_failure:
      (check_proto_error @@ Tx_rollup_errors.No_commitment_to_finalize)
  >>=? fun (_i : Incremental.t) ->
  let message = "bogus" in
  Op.tx_rollup_submit_batch (B b) contract1 tx_rollup message >>=? fun op ->
  add_operation b op >>=? fun b ->
  Op.tx_rollup_submit_batch (B b) contract1 tx_rollup message >>=? fun op ->
  add_operation b op >>=? fun b ->
  Op.tx_rollup_finalize (B b) contract1 tx_rollup >>=? fun op ->
  (* With an inbox, but no commitment *)
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    i
    op
    ~expect_apply_failure:
      (check_proto_error @@ Tx_rollup_errors.No_commitment_to_finalize)
  >>=? fun (_i : Incremental.t) ->
  make_incomplete_commitment_for_batch (I i) (tx_level 0l) tx_rollup []
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
    ~expect_apply_failure:
      (check_proto_error @@ Tx_rollup_errors.No_commitment_to_finalize)
  >>=? fun (_i : Incremental.t) ->
  Incremental.finalize_block i >>=? fun b ->
  (* Now our finalization is valid *)
  add_operation b op >>=? fun (_block : Block.t) -> return_unit

(** [test_too_many_commitments] tests that you can't submit new
      commitments if there are too many finalized commitments. *)
let test_too_many_commitments () =
  context_init1 () >>=? fun (b, contract1) ->
  originate b contract1 >>=? fun (b, tx_rollup) ->
  (* Transactions in block 2, 3, 4, 5 *)
  make_transactions_in tx_rollup contract1 [2; 3; 4; 5] b >>=? fun b ->
  let rec make_commitments b level n =
    if n = 0 then return (b, level)
    else
      make_incomplete_commitment_for_batch (B b) level tx_rollup []
      >>=? fun (commitment, _) ->
      Op.tx_rollup_commit (B b) contract1 tx_rollup commitment
      >>=? fun operation ->
      add_operation b operation >>=? fun b ->
      make_commitments b (Tx_rollup_level.succ level) (n - 1)
  in
  make_commitments b Tx_rollup_level.root 3 >>=? fun (b, level) ->
  (* Make sure all commitments can be finalized. *)
  Incremental.begin_construction b >>=? fun i ->
  bake_until i 10l >>=? fun i ->
  Incremental.finalize_block i >>=? fun b ->
  Op.tx_rollup_finalize (B b) contract1 tx_rollup >>=? fun operation ->
  add_operation b operation >>=? fun b ->
  Op.tx_rollup_finalize (B b) contract1 tx_rollup >>=? fun operation ->
  add_operation b operation >>=? fun b ->
  (* Fail to add a new commitment. *)
  make_incomplete_commitment_for_batch (B b) level tx_rollup []
  >>=? fun (commitment, _) ->
  Op.tx_rollup_commit (B b) contract1 tx_rollup commitment >>=? fun op ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    i
    op
    ~expect_apply_failure:
      (check_proto_error Tx_rollup_errors.Too_many_commitments)
  >>=? fun i ->
  (* Wait out the withdrawal period. *)
  bake_until i 12l >>=? fun i ->
  Incremental.finalize_block i >>=? fun b ->
  (* Remove one finalized commitment. *)
  Op.tx_rollup_remove_commitment (B b) contract1 tx_rollup >>=? fun operation ->
  add_operation b operation >>=? fun b ->
  (* Now we can add a new commitment. *)
  Op.tx_rollup_commit (B b) contract1 tx_rollup commitment >>=? fun operation ->
  add_operation b operation >>=? fun b ->
  ignore b ;

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
  module L2_Context = Tx_rollup_l2_context.Make (Storage)
  module Prover_apply = Tx_rollup_l2_apply.Make (Prover_context)
  module Apply = Tx_rollup_l2_apply.Make (L2_Context)
  module C = Tezos_context_memory.Context_binary

  let previous_message_result : Tx_rollup_message_result.t =
    {
      context_hash = Tx_rollup_message_result.empty_l2_context_hash;
      withdraw_list_hash = Tx_rollup_withdraw_list_hash.empty;
    }

  let init_with_bogus_batch () =
    context_init1 () >>=? fun (b, contract1) ->
    originate b contract1 >>=? fun (b, tx_rollup) ->
    let message = "bogus" in
    Op.tx_rollup_submit_batch (B b) contract1 tx_rollup message
    >>=? fun operation ->
    add_operation b operation >|=? fun b ->
    let level = Tx_rollup_level.root in
    (b, contract1, tx_rollup, level, message)

  let init_with_valid_commitment () =
    init_with_bogus_batch ()
    >>=? fun (b, contract1, tx_rollup, level, message) ->
    make_incomplete_commitment_for_batch (B b) level tx_rollup []
    >>=? fun (commitment, _batches_result) ->
    Op.tx_rollup_commit (B b) contract1 tx_rollup commitment
    >>=? fun operation ->
    add_operation b operation >|=? fun b ->
    (b, contract1, tx_rollup, level, message, commitment)

  let init_with_invalid_commitment () =
    init_with_bogus_batch ()
    >>=? fun (b, contract1, tx_rollup, level, message) ->
    make_incomplete_commitment_for_batch (B b) level tx_rollup []
    >>=? fun (commitment, _batches_result) ->
    let commitment =
      {
        commitment with
        messages =
          [
            Tx_rollup_message_result_hash.hash_uncarbonated
              {
                context_hash =
                  Context_hash.of_b58check_exn
                    "CoUiEnajKeukmYFUgWTJF2z3v24MycpTaomF8a9hRzVy7as9hvgy";
                withdraw_list_hash = Tx_rollup_withdraw_list_hash.empty;
              };
          ];
      }
    in
    Op.tx_rollup_commit (B b) contract1 tx_rollup commitment >>=? fun op ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation i op >|=? fun i ->
    (i, contract1, tx_rollup, level, message, commitment)

  let run_transaction ctxt l2_parameters msg =
    let open Prover_context.Syntax in
    let* ctxt, _ = Prover_apply.apply_message ctxt l2_parameters msg in
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
    let open L2_Context.Syntax in
    let* index = C.init "/tmp" in
    let store = C.empty index in
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
    let open L2_Context.Syntax in
    let* tree_opt = C.find_tree store [] in
    return @@ assert_some tree_opt

  let hash_tree_from_store store =
    let open L2_Context.Syntax in
    let+ tree = get_tree_from_store store in
    C.Tree.hash tree

  let commit_store store =
    let open L2_Context.Syntax in
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
    let open L2_Context.Syntax in
    let* store = init_l2_store () in
    let* hash_tree = hash_tree_from_store store in
    assert (
      Context_hash.(hash_tree = Tx_rollup_message_result.empty_l2_context_hash)) ;
    return_unit

  (** [make_proof store msg] applies [msg] on [store] and returns the
      created proof. *)
  let make_proof store l2_parameters msg =
    let open L2_Context.Syntax in
    let index = C.index store in
    let* hash = hash_tree_from_store store in
    let* proof, () =
      C.produce_stream_proof index (`Node hash) (fun ctxt ->
          catch
            (run_transaction ctxt l2_parameters msg)
            (fun ctxt -> return (ctxt, ()))
            (fun _ -> return (ctxt, ())))
    in
    return proof

  let valid_empty_proof l2_parameters =
    let open L2_Context.Syntax in
    let* l2_store = init_l2_store () in
    let message, _ = Tx_rollup_message.make_batch "bogus" in
    make_proof l2_store l2_parameters message

  let invalid_proof : Tx_rollup_l2_proof.t =
    {
      version = 1;
      before = `Value Tx_rollup_message_result.empty_l2_context_hash;
      after = `Value Context_hash.zero;
      state = Seq.empty;
    }

  (** Takes a commitment and replaces the message results with valid
      results. *)
  let replace_commitment ~l2_parameters ~store ~commitment messages =
    let open L2_Context in
    let open Syntax in
    let* _, rev_results =
      list_fold_left_m
        (fun (store, rev_results) msg ->
          let* store, withdraws =
            catch
              (Apply.apply_message store l2_parameters msg)
              (fun (store, (_, withdraws)) -> return (store, withdraws))
              (fun _reason -> return (store, []))
          in
          let* hash_tree = hash_tree_from_store store in
          let result_hash =
            Tx_rollup_message_result_hash.hash_uncarbonated
              {
                context_hash = hash_tree;
                withdraw_list_hash =
                  Tx_rollup_withdraw_list_hash.hash_uncarbonated withdraws;
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
  let make_valid_commitment_for_messages ctxt ~level ~tx_rollup
      ?(withdrawals = []) ~store messages =
    make_incomplete_commitment_for_batch ctxt level tx_rollup withdrawals
    >>=? fun (commitment, _) ->
    l2_parameters ctxt >>=? fun l2_parameters ->
    replace_commitment ~l2_parameters ~commitment ~store messages
    >>= fun commitment -> return commitment

  let init_with_deposit ?tx_rollup_hard_size_limit_per_message addr =
    init_l2_store () >>= fun store ->
    context_init2 ?tx_rollup_hard_size_limit_per_message ()
    >>=? fun (b, (account, account2)) ->
    originate b account >>=? fun (b, tx_rollup) ->
    make_deposit b tx_rollup account addr
    >>=? fun (b, (deposit, _), ticket_hash) ->
    let deposit_hash = Tx_rollup_message_hash.hash_uncarbonated deposit in
    let message_path = single_message_path deposit_hash in
    Incremental.begin_construction b >>=? fun i ->
    make_valid_commitment_for_messages
      (I i)
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
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (I i)
      account
      tx_rollup
      Tx_rollup_level.root
      deposit
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_apply_failure:
        (check_proto_error Tx_rollup_errors.Proof_produced_rejected_state)
    >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* Finally, we apply the deposit manually to have the good resulting store
       for next operations *)
    Apply.apply_message store l2_parameters deposit >>= fun (store, _) ->
    commit_store store >>= fun store ->
    return (b, account, account2, tx_rollup, store, ticket_hash)

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

  let bls_pk pk = Tx_rollup_l2_batch.Bls_pk pk

  let make_and_sign_transaction ~signers transaction =
    let signatures =
      Tx_rollup_l2_helpers.sign_transaction signers transaction
    in
    let signature =
      assert_some
      @@ Tezos_crypto.Signature.Bls.aggregate_signature_opt signatures
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

  let make_message_transfer ~signers all_transfers =
    let transaction =
      List.map
        (fun (src, counter, transfers) -> make_transfers src counter transfers)
        all_transfers
    in
    make_and_sign_transaction ~signers transaction

  (** Test that undecodable proofs are rejected by the protocol. *)
  let test_undecodable_proof () =
    let sk, pk, addr = gen_l2_account () in
    init_with_deposit addr
    >>=? fun (b, account, _, tx_rollup, store, ticket_hash) ->
    hash_tree_from_store store >>= fun l2_context_hash ->
    (* Create a transfer from [pk] to a new address *)
    let _, _, addr2 = gen_l2_account () in
    let message, batch_bytes =
      make_message_transfer
        ~signers:[sk]
        [(bls_pk pk, None, [(addr2, ticket_hash, 1L)])]
    in
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
    let message_path = single_message_path message_hash in
    Op.tx_rollup_submit_batch (B b) account tx_rollup batch_bytes
    >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    (* Make a commitment for the submitted transfer *)
    let level = Tx_rollup_level.(succ root) in
    make_incomplete_commitment_for_batch (B b) level tx_rollup []
    >>=? fun (commitment, _) ->
    Op.tx_rollup_commit (B b) account tx_rollup commitment >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    (* Now we produce an invalid serialized proof that cannot be
       decoded by the protocol. *)
    let proof =
      Tx_rollup_l2_proof.Internal_for_tests.of_bytes @@ Bytes.make 1_000 'c'
    in
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_raw_reject
      (B b)
      account
      tx_rollup
      level
      message
      ~gas_limit:Max
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result:(message_result l2_context_hash [])
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun operation ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Tx_rollup_errors.Proof_undecodable)
      i
      operation
    >>=? fun (_i : Incremental.t) -> return_unit

  (** Test that we can produce a simple but valid proof. *)
  let test_valid_proof_on_invalid_commitment () =
    let sk, pk, addr = gen_l2_account () in
    init_with_deposit addr
    >>=? fun (b, account, _, tx_rollup, store, ticket_hash) ->
    hash_tree_from_store store >>= fun l2_context_hash ->
    (* Create a transfer from [pk] to a new address *)
    let _, _, addr2 = gen_l2_account () in
    let message, batch_bytes =
      make_message_transfer
        ~signers:[sk]
        [(bls_pk pk, None, [(addr2, ticket_hash, 1L)])]
    in
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
    let message_path = single_message_path message_hash in
    Op.tx_rollup_submit_batch (B b) account tx_rollup batch_bytes
    >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    (* Make an invalid commitment for the submitted transfer *)
    let level = Tx_rollup_level.(succ root) in
    make_incomplete_commitment_for_batch (B b) level tx_rollup []
    >>=? fun (commitment, _) ->
    Op.tx_rollup_commit (B b) account tx_rollup commitment >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    (* Now we produce a valid proof rejecting the commitment *)
    l2_parameters (B b) >>=? fun l2_parameters ->
    make_proof store l2_parameters message >>= fun proof ->
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      account
      tx_rollup
      level
      message
      ~gas_limit:Max
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result:(message_result l2_context_hash [])
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun operation ->
    add_operation b operation >>=? fun (_ : Block.t) -> return_unit

  (** It is really similar to {!test_valid_proof_on_invalid_commitment} but it
      tries to reject a valid commitment, thus, fails. *)
  let test_valid_proof_on_valid_commitment () =
    let sk, pk, addr = gen_l2_account () in
    init_with_deposit addr
    >>=? fun (b, account, _, tx_rollup, store, ticket_hash) ->
    (* init_with_deposit creates a commitment -- we'll just check the bond
       here so that this test is easier to read. *)
    check_bond_from_block b tx_rollup account 1 >>=? fun () ->
    hash_tree_from_store store >>= fun l2_context_hash ->
    (* Create a transfer from [pk] to a new address *)
    let _, _, addr2 = gen_l2_account () in
    let message, batch_bytes =
      make_message_transfer
        ~signers:[sk]
        [(bls_pk pk, None, [(addr2, ticket_hash, 1L)])]
    in
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
    let message_path = single_message_path message_hash in
    Op.tx_rollup_submit_batch (B b) account tx_rollup batch_bytes
    >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    (* Make an invalid commitment for the submitted transfer *)
    let level = Tx_rollup_level.(succ root) in
    make_valid_commitment_for_messages (B b) ~level ~tx_rollup ~store [message]
    >>=? fun commitment ->
    Op.tx_rollup_commit (B b) account tx_rollup commitment >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    (* Now we produce a valid proof rejecting the commitment *)
    l2_parameters (B b) >>=? fun l2_parameters ->
    make_proof store l2_parameters message >>= fun proof ->
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      account
      tx_rollup
      level
      message
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result:(message_result l2_context_hash [])
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation
      i
      op
      ~expect_apply_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.Proof_produced_rejected_state -> true
         | _ -> false)
    >>=? fun i ->
    check_bond (Incremental.alpha_ctxt i) tx_rollup account 2 >>=? fun () ->
    return_unit

  (** Test that rejection rewards and slashing work:
      1. Create two messages and two commitments
      2. Reject the second commitment
      3. Ensure that slashing and rewards happen
      4. Reject the first commitment
      5. Ensure that there is no further slashing or reward
    *)
  let test_rejection_rewards () =
    let open Error_monad_operators in
    let _, _, addr = gen_l2_account () in
    init_l2_store () >>= fun store ->
    context_init2 () >>=? fun (b, (contract1, contract2)) ->
    originate b contract1 >>=? fun (b, tx_rollup) ->
    make_deposit b tx_rollup contract1 addr
    >>=? fun (b, (deposit_message, _), _ticket_hash) ->
    Context.Contract.balance (B b) contract1 >>=? fun balance ->
    Context.Contract.balance (B b) contract2 >>=? fun balance2 ->
    (* [check_frozen] checks that contract1 has [expect] frozen tez. *)
    let check_frozen ~loc b expect =
      Incremental.begin_construction b >>=? fun i ->
      Contract.get_frozen_bonds (Incremental.alpha_ctxt i) contract1
      >>=?? fun frozen -> Assert.equal_tez ~loc expect frozen
    in
    (* Nothing frozen to start *)
    check_frozen ~loc:__LOC__ b Tez.zero >>=? fun () ->
    (* No-op batch for second inbox *)
    Op.tx_rollup_submit_batch (B b) contract1 tx_rollup "fake"
    >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    l2_parameters (B b) >>=? fun l2_parameters ->
    let message, _ = Tx_rollup_message.make_batch "fake" in
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
    let message_path = single_message_path message_hash in
    hash_tree_from_store store >>= fun l2_context_hash ->
    let make_invalid_commitment b level h =
      (* Make some invalid commitments for the submitted messages *)
      make_incomplete_commitment_for_batch (B b) level tx_rollup []
      >>=? fun (commitment, _) ->
      (* Make this commitment bogus *)
      let message_result =
        Tx_rollup_message_result.
          {
            context_hash = h;
            withdraw_list_hash = Tx_rollup_withdraw_list_hash.empty;
          }
      in
      let message_result_hash =
        Tx_rollup_message_result_hash.hash_uncarbonated message_result
      in
      let commitment = {commitment with messages = [message_result_hash]} in
      Op.tx_rollup_commit (B b) contract1 tx_rollup commitment
      >>=? fun operation ->
      add_operation b operation >|=? fun b -> (b, commitment)
    in
    let level0 = tx_level 0l in
    let level1 = tx_level 1l in
    make_invalid_commitment b level0 l2_context_hash
    >>=? fun (b, commitment0) ->
    make_invalid_commitment b level1 Context_hash.zero
    >>=? fun (b, commitment1) ->
    Context.get_constants (B b) >>=? fun constants ->
    let bond_cost = constants.parametric.tx_rollup.commitment_bond in
    Assert.balance_was_debited ~loc:__LOC__ (B b) contract1 balance bond_cost
    >>=? fun () ->
    check_frozen ~loc:__LOC__ b bond_cost >>=? fun () ->
    (* Now we produce a valid proof rejecting the second commitment *)
    make_proof store l2_parameters message >>= fun proof ->
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment1 ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      contract2
      tx_rollup
      level1
      message
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result:(message_result l2_context_hash [])
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    check_bond_from_block b tx_rollup contract1 0 >>=? fun () ->
    Assert.balance_was_debited ~loc:__LOC__ (B b) contract1 balance bond_cost
    >>=? fun () ->
    (* Now we need to check that the tez is really gone -- not just frozen *)
    check_frozen ~loc:__LOC__ b Tez.zero >>=? fun () ->
    let reward = assert_ok Tez.(bond_cost /? 2L) in
    Assert.balance_was_credited ~loc:__LOC__ (B b) contract2 balance2 reward
    >>=? fun () ->
    (* Now, we can still reject the root commitment, but we won't get a reward *)
    Context.Contract.balance (B b) contract1 >>=? fun balance ->
    Context.Contract.balance (B b) contract2 >>=? fun balance2 ->
    make_proof store l2_parameters deposit_message >>= fun proof ->
    let message_hash =
      Tx_rollup_message_hash.hash_uncarbonated deposit_message
    in
    let message_position = 0 in
    let message_path = single_message_path message_hash in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment0 ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      contract2
      tx_rollup
      level0
      deposit_message
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result:(message_result l2_context_hash [])
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    check_bond_from_block b tx_rollup contract1 0 >>=? fun () ->
    Assert.balance_was_debited ~loc:__LOC__ (B b) contract1 balance Tez.zero
    >>=? fun () ->
    (* Now we need to check that the tez still really gone -- not just frozen *)
    check_frozen ~loc:__LOC__ b Tez.zero >>=? fun () ->
    Assert.balance_was_credited ~loc:__LOC__ (B b) contract2 balance2 Tez.zero

  (** Test the proof production (used in this test file) and the proof
      verification handles a hard failure. [make_bad_message] makes a
      message whose l2 apply will fail in whatever specific way we
      wish to test. *)
  let do_test_proof_with_hard_fail_message make_bad_message =
    let sk, pk, addr = gen_l2_account () in
    init_with_deposit addr
    >>=? fun (b, account, _, tx_rollup, store, ticket_hash) ->
    hash_tree_from_store store >>= fun l2_context_hash ->
    let message, batch_bytes = make_bad_message sk pk addr ticket_hash in
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
    let message_path = single_message_path message_hash in
    Op.tx_rollup_submit_batch (B b) account tx_rollup batch_bytes
    >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    (* Make an invalid commitment for the submitted transfer *)
    let level = Tx_rollup_level.(succ root) in
    make_incomplete_commitment_for_batch (B b) level tx_rollup []
    >>=? fun (commitment, _) ->
    Op.tx_rollup_commit (B b) account tx_rollup commitment >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    (* Now we produce a valid proof rejecting the commitment *)
    l2_parameters (B b) >>=? fun l2_parameters ->
    make_proof store l2_parameters message >>= fun proof ->
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      account
      tx_rollup
      level
      message
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result:(message_result l2_context_hash [])
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation i op >>=? fun i ->
    check_bond (Incremental.alpha_ctxt i) tx_rollup account 0 >>=? fun () ->
    return_unit

  (** Test that proof production and verification can handle an invalid
      signature *)
  let test_proof_with_invalid_signature () =
    do_test_proof_with_hard_fail_message (fun _sk pk addr ticket_hash ->
        (* We build a dummy transfer, we don't care about the content, it will hard
           fail on the check signature. *)
        let random_sk, _, _ = gen_l2_account () in
        make_message_transfer
          ~signers:[random_sk]
          [(Bls_pk pk, None, [(addr, ticket_hash, 1L)])])

  (** Test that proof production and verification can handle an unparseable
      message *)
  let test_proof_with_unparsable_batch () =
    do_test_proof_with_hard_fail_message (fun _sk _pk _addr _ticket_hash ->
        let message = "wrong" in
        let batch, _ = Tx_rollup_message.make_batch message in
        (batch, message))

  (** Test that proof production and verification can handle an invalid
      counter *)
  let test_proof_with_invalid_counter () =
    do_test_proof_with_hard_fail_message (fun sk pk _addr ticket_hash ->
        let _, _, addr = gen_l2_account () in
        make_message_transfer
          ~signers:[sk]
          [(Bls_pk pk, Some 42L, [(addr, ticket_hash, 1L)])])

  (** Test that proof production and verification can handle an invalid
      transfer of zero tickets *)
  let test_proof_with_zero_transfer () =
    do_test_proof_with_hard_fail_message (fun sk pk addr ticket_hash ->
        make_message_transfer
          ~signers:[sk]
          [(Bls_pk pk, None, [(addr, ticket_hash, 0L)])])

  (** Test that proof production and verification can handle an invalid
      transfer with multiple operations from the same signer *)
  let test_proof_with_multiple_operations () =
    do_test_proof_with_hard_fail_message (fun sk pk addr ticket_hash ->
        make_message_transfer
          ~signers:[sk; sk]
          [
            (Bls_pk pk, None, [(addr, ticket_hash, 1L)]);
            (Bls_pk pk, None, [(addr, ticket_hash, 2L)]);
          ])

  (** Test that an empty proof is not able to reject a valid commitment. *)
  let test_empty_proof_on_invalid_message () =
    init_with_valid_commitment ()
    >>=? fun (b, contract, tx_rollup, level, message, commitment) ->
    let msg, _ = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated msg in
    let message_path = single_message_path message_hash in
    l2_parameters (B b) >>=? fun l2_parameters ->
    valid_empty_proof l2_parameters >>= fun proof ->
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      contract
      tx_rollup
      level
      msg
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun operation ->
    add_operation b operation >>=? fun (_ : Block.t) -> return_unit

  (** Test that an empty proof is not able to reject a valid commitment. *)
  let test_invalid_proof_on_invalid_commitment () =
    init_with_valid_commitment ()
    >>=? fun (b, contract, tx_rollup, level, message, commitment) ->
    let msg, _ = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated msg in
    let message_path = single_message_path message_hash in
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      contract
      tx_rollup
      level
      msg
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof:invalid_proof
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation
      i
      op
      ~expect_apply_failure:
        (check_proto_error Tx_rollup_errors.Proof_failed_to_reject)
    >>=? fun (_ : Incremental.t) -> return_unit

  (** Test that rejection successfully fails when there is a disagreement about
      the previous state. *)
  let test_invalid_agreed () =
    init_with_valid_commitment ()
    >>=? fun (b, contract, tx_rollup, level, message, commitment) ->
    let msg, _ = Tx_rollup_message.make_batch message in
    (* This intentionally does not match  *)
    let previous_message_result : Tx_rollup_message_result.t =
      {
        (* Expected is Tx_rollup_commitment.empty_l2_context_hash *)
        context_hash = Context_hash.zero;
        withdraw_list_hash = Tx_rollup_withdraw_list_hash.empty;
      }
    in
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated msg in
    let message_path = single_message_path message_hash in
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      contract
      tx_rollup
      level
      msg
      ~message_position
      ~message_path
      ~proof:invalid_proof (* doesn't matter -- we'll never check it*)
      ~message_result_hash
      ~message_result_path
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation
      i
      op
      ~expect_apply_failure:
        (check_proto_error
           (Tx_rollup_errors.Wrong_rejection_hash
              {
                provided =
                  Tx_rollup_message_result_hash.hash_uncarbonated
                    previous_message_result;
                expected =
                  `Hash
                    (Tx_rollup_message_result_hash.of_b58check_exn
                       "txmr344vtdPzvWsfnoSd3mJ3MCFA5ehKLQs1pK9WGcX4FEACg1rVgC");
              }))
    >>=? fun (_ : Incremental.t) -> return_unit

  (** Test that rejection successfully fails when there's no commitment to
      reject *)
  let test_no_commitment () =
    context_init1 () >>=? fun (b, contract) ->
    originate b contract >>=? fun (b, tx_rollup) ->
    let message = "bogus" in
    Op.tx_rollup_submit_batch (B b) contract tx_rollup message
    >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    Incremental.begin_construction b >>=? fun i ->
    let level = Tx_rollup_level.root in
    let message, _size = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
    let message_path = single_message_path message_hash in
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
      ~message_result_hash:Tx_rollup_message_result_hash.zero
      ~message_result_path:Tx_rollup_commitment.Merkle.dummy_path
      ~proof
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_apply_failure:
        (check_proto_error
           (Tx_rollup_errors.Cannot_reject_level
              {provided = level; accepted_range = None}))
    >>=? fun (_ : Incremental.t) -> return_unit

  (** Test that rejection successfully fails when the rejected commitment is
      already final *)
  let test_commitment_is_final () =
    init_with_valid_commitment ()
    >>=? fun (b, contract, tx_rollup, level, message, commitment) ->
    (* Create a new commitment so that once we have finalized the first one,
       we still have a range of valid final commitments *)
    Op.tx_rollup_submit_batch (B b) contract tx_rollup message
    >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    let level2 = Tx_rollup_level.succ level in
    make_incomplete_commitment_for_batch (B b) level2 tx_rollup []
    >>=? fun (commitment2, _) ->
    Op.tx_rollup_commit (B b) contract tx_rollup commitment2
    >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    Op.tx_rollup_finalize (B b) contract tx_rollup >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    let message, _size = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
    let message_path = single_message_path message_hash in
    l2_parameters (B b) >>=? fun l2_parameters ->
    valid_empty_proof l2_parameters >>= fun proof ->
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      contract
      tx_rollup
      level
      message
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation
      i
      op
      ~expect_apply_failure:
        (check_proto_error
           (Tx_rollup_errors.Cannot_reject_level
              {provided = level; accepted_range = Some (level2, level2)}))
    >>=? fun (_ : Incremental.t) -> return_unit

  (** Test that rejection successfully fails when the message hash does not
      match the one stored in the inbox *)
  let test_wrong_message_hash () =
    init_with_valid_commitment ()
    >>=? fun (b, contract1, tx_rollup, level, prev_message, commitment) ->
    let prev_message, _size = Tx_rollup_message.make_batch prev_message in
    let prev_message_hash =
      Tx_rollup_message_hash.hash_uncarbonated prev_message
    in
    let expected_root =
      Tx_rollup_inbox.Merkle.merklize_list [prev_message_hash]
    in
    let message, _size = Tx_rollup_message.make_batch "wrong message" in
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
    let message_path = single_message_path message_hash in
    l2_parameters (B b) >>=? fun l2_parameters ->
    valid_empty_proof l2_parameters >>= fun proof ->
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      contract1
      tx_rollup
      level
      message
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation
      i
      op
      ~expect_apply_failure:
        (check_proto_error
           (Tx_rollup_errors.Wrong_message_path {expected = expected_root}))
    >>=? fun (_ : Incremental.t) -> return_unit

  (** Test that rejection successfully fails when the message position does
      exist in the inbox. *)
  let test_wrong_message_position () =
    init_with_valid_commitment ()
    >>=? fun (b, contract1, tx_rollup, level, message, _commitment) ->
    let message, _size = Tx_rollup_message.make_batch message in
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
    let message_path = single_message_path message_hash in
    l2_parameters (B b) >>=? fun l2_parameters ->
    valid_empty_proof l2_parameters >>= fun proof ->
    Op.tx_rollup_reject
      (B b)
      contract1
      tx_rollup
      level
      message
      ~message_position:1
      ~message_path
      ~message_result_hash:Tx_rollup_message_result_hash.zero
      ~message_result_path:Tx_rollup_commitment.Merkle.dummy_path
      ~proof
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation
      i
      op
      ~expect_apply_failure:
        (check_proto_error
           (Tx_rollup_errors.Wrong_message_position
              {level; position = 1; length = 1}))
    >>=? fun (_ : Incremental.t) -> return_unit

  (** Test rejecting a commitment to a non-trivial message -- that is,
      not a no-op. *)
  let test_nontrivial_rejection () =
    let _, _, addr = gen_l2_account () in
    init_l2_store () >>= fun store ->
    context_init1 () >>=? fun (b, account) ->
    originate b account >>=? fun (b, tx_rollup) ->
    make_deposit b tx_rollup account addr >>=? fun (b, (deposit, _), _) ->
    let message_hash = Tx_rollup_message_hash.hash_uncarbonated deposit in
    let message_path = single_message_path message_hash in
    make_incomplete_commitment_for_batch (B b) Tx_rollup_level.root tx_rollup []
    >>=? fun (commitment, _) ->
    Op.tx_rollup_commit (B b) account tx_rollup commitment >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      account
      tx_rollup
      Tx_rollup_level.root
      deposit
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof:invalid_proof
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation
      i
      op
      ~expect_apply_failure:
        (check_proto_error Tx_rollup_errors.Proof_failed_to_reject)
    >>=? fun (_i : Incremental.t) ->
    (* Check with a reasonable proof *)
    l2_parameters (B b) >>=? fun l2_parameters ->
    make_proof store l2_parameters deposit >>= fun proof ->
    Op.tx_rollup_reject
      (B b)
      account
      tx_rollup
      Tx_rollup_level.root
      deposit
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun operation ->
    add_operation b operation >>=? fun (_ : Block.t) -> return_unit

  let add_store_to_ctxt ctxt store =
    let open L2_Context.Syntax in
    let time = Time.Protocol.of_seconds 0L in
    let* ctxt = C.add_tree ctxt [] store in
    let* h = C.commit ~time ctxt in
    let index = C.index ctxt in
    let* ctxt = C.checkout_exn index h in
    return ctxt

  let test_large_rejection size =
    let _, _, addr = gen_l2_account () in
    init_l2_store () >>= fun store ->
    context_init1 ~tx_rollup_rejection_max_proof_size:size ()
    >>=? fun (b, account) ->
    originate b account >>=? fun (b, tx_rollup) ->
    make_deposit b tx_rollup account addr >>=? fun (b, (deposit, _), _) ->
    let deposit_hash = Tx_rollup_message_hash.hash_uncarbonated deposit in
    let message_path = single_message_path deposit_hash in
    Incremental.begin_construction b >>=? fun i ->
    make_valid_commitment_for_messages
      (I i)
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
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (I i)
      account
      tx_rollup
      Tx_rollup_level.root
      deposit
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op -> return (i, op)

  (** Test that a commitment which require a too-large proof can be rejected
      even if the after hash is correct. *)
  let test_too_large_rejection () =
    (* With a limit, the commitment is rejected because the required proof
       is above the limit. *)
    test_large_rejection 100 >>=? fun (i, op) ->
    Incremental.add_operation i op >>=? fun (_ : Incremental.t) ->
    (* With a high limit, the commitment can not be rejected as it is valid *)
    test_large_rejection 10_000 >>=? fun (i, op) ->
    Incremental.add_operation
      i
      ~expect_apply_failure:
        (check_proto_error Tx_rollup_errors.Proof_produced_rejected_state)
      op
    >>=? fun (_ : Incremental.t) -> return_unit

  (** Drop the last element of a seq, that is, the last element of a proof *)
  let rec drop_last x =
    let open Seq in
    match x with
    | Cons (x, xs) -> (
        let node = xs () in
        match node with Nil -> Nil | n -> Cons (x, fun () -> drop_last n))
    | Nil -> assert false

  let rec drop_n x n = if n <= 0 then x else drop_n (drop_last x) (n - 1)

  let test_valid_proof_truncated () =
    let _, _, addr = gen_l2_account () in
    init_l2_store () >>= fun store ->
    context_init1 ~tx_rollup_rejection_max_proof_size:100 ()
    >>=? fun (b, account) ->
    originate b account >>=? fun (b, tx_rollup) ->
    make_deposit b tx_rollup account addr >>=? fun (b, (deposit, _), _) ->
    let deposit_hash = Tx_rollup_message_hash.hash_uncarbonated deposit in
    let message_path = single_message_path deposit_hash in
    Incremental.begin_construction b >>=? fun i ->
    let level = Tx_rollup_level.root in
    make_valid_commitment_for_messages (I i) ~level ~store ~tx_rollup [deposit]
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
      let truncated_node = drop_last proof_node in
      {proof with state = (fun () -> truncated_node)}
    in
    (* We try to reject with the truncated proof which is already above the
       size limit. *)
    Incremental.begin_construction b >>=? fun i ->
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (I i)
      account
      tx_rollup
      Tx_rollup_level.root
      deposit
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof:proof_truncated
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.add_operation i op >>=? fun (_ : Incremental.t) -> return_unit

  (** Create a context where the batcher submitted and committed for a layer2
      batch that produces [n_withdraw] withdrawals.
      The commitment is created without respecting the constant in the protocol.
      Therefore, a valid rejection just has to respect this limit and
      send its correct state (which may have returned
      [Maximum_withdraws_per_message_exceeded])

      In other terms, the rejection created in this function must fail
      if [n_withdraw <= tx_rollup_max_withdrawals_per_batch] but also must
      succeed to reject if [n_withdraw > tx_rollup_max_withdrawals_per_batch]. *)
  let test_reject_withdrawals_helper ?expect_apply_failure n_withdraw =
    let sk, pk, addr = gen_l2_account () in
    init_with_deposit ~tx_rollup_hard_size_limit_per_message:20_000 addr
    >>=? fun (b, account, _, tx_rollup, store, ticket_hash) ->
    hash_tree_from_store store >>= fun l2_context_hash ->
    (* 1. Create a batch with [n_withdraw] withdrawals. *)
    let destination = Context.Contract.pkh account in
    let qty = Tx_rollup_l2_qty.one in
    let operation =
      let open Tx_rollup_l2_batch.V1 in
      let withdraws =
        Stdlib.List.init n_withdraw (fun _ ->
            Withdraw {destination; ticket_hash; qty})
      in
      {
        signer = Indexable.from_value (bls_pk pk);
        counter = 1L;
        contents = withdraws;
      }
    in
    let message, batch_bytes =
      make_and_sign_transaction ~signers:[sk] [operation]
    in

    let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
    let message_path = single_message_path message_hash in
    (* 2. Submit and commit the batch. *)
    Op.tx_rollup_submit_batch (B b) account tx_rollup batch_bytes
    >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    let level = Tx_rollup_level.(succ root) in
    let withdrawals =
      Stdlib.List.init n_withdraw (fun _ ->
          Tx_rollup_withdraw.{claimer = destination; ticket_hash; amount = qty})
    in
    make_incomplete_commitment_for_batch
      (B b)
      level
      tx_rollup
      [(0, withdrawals)]
    >>=? fun (commitment, _) ->
    (* In the case where [n_withdraw] is above the limit, we want to create
       a commitment that did not respect the limit. Therefore, we use a
       a special [apply_message] with tweaked parameters. *)
    Apply.apply_message
      store
      Tx_rollup_l2_apply.{tx_rollup_max_withdrawals_per_batch = n_withdraw + 42}
      message
    >>= fun (store', _) ->
    hash_tree_from_store store' >>= fun to_commit_hash ->
    let result_hash =
      Tx_rollup_message_result_hash.hash_uncarbonated
        {
          context_hash = to_commit_hash;
          withdraw_list_hash =
            Tx_rollup_withdraw_list_hash.hash_uncarbonated withdrawals;
        }
    in
    let commitment = {commitment with messages = [result_hash]} in
    Op.tx_rollup_commit (B b) account tx_rollup commitment >>=? fun operation ->
    add_operation b operation >>=? fun b ->
    (* 4. Now we create a proof that used the correct layer2 apply with
        the correct parameters. *)
    l2_parameters (B b) >>=? fun l2_parameters ->
    make_proof store l2_parameters message >>= fun proof ->
    let previous_message_result : Tx_rollup_message_result.t =
      {
        context_hash = l2_context_hash;
        withdraw_list_hash = Tx_rollup_withdraw_list_hash.empty;
      }
    in
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      account
      tx_rollup
      level
      message
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation i op ?expect_apply_failure
    >>=? fun (_ : Incremental.t) -> return_unit

  let test_reject_withdrawals_limit () =
    context_init1 () >>=? fun (b, _) ->
    l2_parameters (B b) >>=? fun l2_parameters ->
    let limit =
      l2_parameters.Tx_rollup_l2_apply.tx_rollup_max_withdrawals_per_batch
    in
    let expect_apply_failure =
      check_proto_error Tx_rollup_errors.Proof_produced_rejected_state
    in
    (* It must not be rejected: (limit - 1) is below the limit *)
    test_reject_withdrawals_helper ~expect_apply_failure (limit - 1)
    >>=? fun () ->
    (* It must not be rejected: limit is the limit :p. *)
    test_reject_withdrawals_helper ~expect_apply_failure limit >>=? fun () ->
    (* It must be rejected: (limit + 1) is above the limit *)
    test_reject_withdrawals_helper (limit + 1)

  (** Fill a storage with [l2_accounts], they are all credited [100] of
      [Ticket_hash.zero]. *)
  let fill_store store l2_accounts =
    let open L2_Context.Syntax in
    let* store, _, tidx =
      L2_Context.Ticket_index.get_or_associate_index store Ticket_hash.zero
    in
    let* store =
      list_fold_left_m
        (fun store (_, pk, addr) ->
          let* store, _, aidx =
            L2_Context.Address_index.get_or_associate_index store addr
          in
          let* store =
            L2_Context.Address_metadata.init_with_public_key store aidx pk
          in
          let* store =
            L2_Context.Ticket_ledger.credit
              store
              tidx
              aidx
              (Tx_rollup_l2_qty.of_int64_exn 100L)
          in
          return store)
        store
        l2_accounts
    in
    let time = time () in
    let* (_ : Context_hash.t) = C.commit ~time store in
    return store

  (** Regression test to ensure that we can reject a commitment where the
      proof is truncated to be close to the maximum size limit (i.e. 32Kb) and
      the rejected message size is close to the maximum size (i.e. 5Kb).

      It also test that all required parameters for the rejection fits in a
      Tezos operation even in the worst cases.
  *)
  let test_rejection_size_limit () =
    let rng_state = Random.State.make [|42|] in
    context_init1 () >>=? fun (b, account) ->
    originate b account >>=? fun (b, tx_rollup) ->
    Context.get_constants (B b) >>=? fun constant ->
    (* We begin by creating a context for the first message, the context is
       obviously invalid but we will use it as the supposedly valid base for
       the second message which we will reject. *)
    init_l2_store () >>= fun store ->
    (* 200 accounts with this fixed random state is enough to create a proof
       larger than the current max proof size (i.e. 30Kb). The more accounts
       we add in the context, bigger the proofs becomes. It needs to be adjusted
       so the following [message2] in this context produces a proof that
       is larger to 30Kb. *)
    List.init ~when_negative_length:[] 200 (fun _ ->
        gen_l2_account ~rng_state ())
    >>?= fun l2_accounts ->
    (* The context is filled with the generated l2 accounts. *)
    fill_store store l2_accounts >>= fun store ->
    hash_tree_from_store store >>= fun l2_context_hash ->
    let message0_result : Tx_rollup_message_result.t =
      {
        context_hash = l2_context_hash;
        withdraw_list_hash = Tx_rollup_withdraw_list_hash.empty;
      }
    in
    let message0_result_hash =
      Tx_rollup_message_result_hash.hash_uncarbonated message0_result
    in
    (* Then, we build a real message which is close to the maximum message size
       limit and produces a proof also close to the maximum proof size limit. *)
    let _sk, _pk, addr = gen_l2_account ~rng_state () in
    let signers, transfers =
      List.map
        (fun (sk, pk, _) ->
          (sk, (bls_pk pk, None, [(addr, Ticket_hash.zero, 1L)])))
        (* 45 with this fixed random state is enough to create message
           close to the maximum message size (i.e. 5Kb), but small enough so
           it is not rejected by the protocol. The more we put transfers in
           the operation, bigger the message becomes. *)
        (List.take_n 45 l2_accounts)
      |> List.split
    in
    l2_parameters (B b) >>=? fun l2_parameters ->
    let message1, batch_bytes = make_message_transfer ~signers transfers in
    let message1_hash = Tx_rollup_message_hash.hash_uncarbonated message1 in
    Incremental.begin_construction b >>=? fun i ->
    (* Submit the two first hand-crafted messages. *)
    let message0, _ = Tx_rollup_message.make_batch "xoxo" in
    let message0_hash = Tx_rollup_message_hash.hash_uncarbonated message0 in
    Op.tx_rollup_submit_batch
      ~gas_limit:(Custom_gas (Gas.Arith.integral_of_int_exn 2_500))
      (I i)
      account
      tx_rollup
      "xoxo"
    >>=? fun op1 ->
    Op.tx_rollup_submit_batch (I i) account tx_rollup batch_bytes
    >>=? fun op2 ->
    let message_count =
      constant.parametric.tx_rollup.max_messages_per_inbox - 2
    in
    (* Fill the inbox at tx_level 0. Trying to reject a commitment for a full
       inbox has the benefit of having large message paths. Thus, making the
       rejection operation size even larger. *)
    let ops = List.repeat message_count op1 in
    Op.combine_operations ~source:account (I i) ([op1; op2] @ ops)
    >>=? fun op ->
    Incremental.add_operation ~check_size:false i op >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* Prepare a commitment for the tx_level 0, only the very first message
       result hash will be a real value (in order to start the proof verification
       from a valid state). *)
    let message2_result_hash =
      Tx_rollup_message_result_hash.hash_uncarbonated previous_message_result
    in
    let messages = [message0; message1] @ List.repeat message_count message0 in
    let message_hashes =
      [message0_hash; message1_hash] @ List.repeat message_count message0_hash
    in
    let message_result_hashes =
      [message0_result_hash; message2_result_hash]
      @ List.repeat message_count message0_result_hash
    in
    let commitment : Tx_rollup_commitment.Full.t =
      {
        level = Tx_rollup_level.root;
        messages = message_result_hashes;
        predecessor = None;
        inbox_merkle_root = Tx_rollup_inbox.Merkle.merklize_list message_hashes;
      }
    in
    Op.tx_rollup_commit (B b) account tx_rollup commitment >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    (* Prepare the rejection for the second message. *)
    make_proof store l2_parameters message1 >>= fun proof ->
    let message_hashes =
      List.map Tx_rollup_message_hash.hash_uncarbonated messages
    in
    let message_path =
      assert_ok @@ Tx_rollup_inbox.Merkle.compute_path message_hashes 1
    in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position:1
    in
    let _, previous_message_result_path =
      message_result_hash_and_path commitment ~message_position:0
    in
    (* The actual proof size is almost 32Kb, after the drop the truncated
       proof size is 26Kb. *)
    let proof_truncated =
      let proof_node = proof.state () in
      (* 100 is the magic number of nodes to drop in the proof. The more nodes
         we drop, smaller the proof becomes. The goal here is to drop enough
         nodes to have a proof close to 26Kb. *)
      let truncated_node = drop_n proof_node 100 in
      {proof with state = (fun () -> truncated_node)}
    in
    Op.tx_rollup_reject
      ~gas_limit:(Custom_gas (Gas.Arith.integral_of_int_exn 100_000))
      (B b)
      account
      tx_rollup
      Tx_rollup_level.root
      message1
      ~message_position:1
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof:proof_truncated
      ~previous_message_result:message0_result
      ~previous_message_result_path
    >>=? fun op ->
    Incremental.begin_construction b >>=? fun i ->
    (* Finally, we reject the commitment and check that the size fits in
       a Tezos operation. *)
    Incremental.add_operation i op >>=? fun (_i : Incremental.t) -> return_unit

  let tests =
    [
      Tztest.tztest
        "regression test empty_l2_context_hash"
        `Quick
        test_empty_l2_context_hash;
      Tztest.tztest "unencodable proofs fail" `Quick test_undecodable_proof;
      Tztest.tztest
        "reject invalid commitment"
        `Quick
        test_valid_proof_on_invalid_commitment;
      Tztest.tztest
        "reject valid commitment fails"
        `Quick
        test_valid_proof_on_valid_commitment;
      Tztest.tztest "rejection rewards" `Quick test_rejection_rewards;
      Tztest.tztest
        "proof for a hard failing message: invalid signature"
        `Quick
        test_proof_with_invalid_signature;
      Tztest.tztest
        "proof for a hard failing message: unparseable message"
        `Quick
        test_proof_with_unparsable_batch;
      Tztest.tztest
        "proof for a hard failing message: invalid counter"
        `Quick
        test_proof_with_invalid_counter;
      Tztest.tztest
        "proof for a hard failing message: zero-ticket transfer"
        `Quick
        test_proof_with_zero_transfer;
      Tztest.tztest
        "proof for a hard failing message: multiple ops from one signer"
        `Quick
        test_proof_with_multiple_operations;
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
      Tztest.tztest
        "reject withdrawals when out of bound"
        `Quick
        test_reject_withdrawals_limit;
      Tztest.tztest
        "regression test rejection max proof size and message"
        `Quick
        test_rejection_size_limit;
    ]
end

module Single_message_inbox = struct
  let contents = "bogus"

  let message, _ = Tx_rollup_message.make_batch contents

  let message_hash = Tx_rollup_message_hash.hash_uncarbonated message

  let message_path = single_message_path message_hash

  let inbox_hash = Tx_rollup_inbox.Merkle.merklize_list [message_hash]

  let submit b tx_rollup account =
    Op.tx_rollup_submit_batch (B b) account tx_rollup contents
    >>=? fun operation -> Block.bake b ~operation

  let reject ?expect_apply_failure b tx_rollup account level commitment =
    Format.printf
      "Rejecting level %a (%s)\n"
      Tx_rollup_level.pp
      level
      (if Option.is_some expect_apply_failure then "x" else "√") ;
    l2_parameters (B b) >>=? fun l2_parameters ->
    Rejection.valid_empty_proof l2_parameters >>= fun proof ->
    let message_position = 0 in
    let message_result_hash, message_result_path =
      message_result_hash_and_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (B b)
      account
      tx_rollup
      level
      message
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result:Rejection.previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun operation ->
    Incremental.begin_construction b >>=? fun i ->
    Incremental.add_operation i operation ?expect_apply_failure >>=? fun i ->
    Incremental.finalize_block i

  let make_commit predecessor_commit messages =
    let level =
      Option.fold
        ~none:Tx_rollup_level.root
        ~some:(fun (x : 'a Tx_rollup_commitment.template) ->
          Tx_rollup_level.succ x.level)
        predecessor_commit
    in
    let predecessor =
      Option.map
        (fun x -> Tx_rollup_commitment.(Compact.hash @@ Full.compact x))
        predecessor_commit
    in
    Tx_rollup_commitment.
      {level; messages; predecessor; inbox_merkle_root = inbox_hash}

  let wrong_commit ?predecessor_commit () =
    make_commit predecessor_commit [Tx_rollup_message_result_hash.zero]

  let correct_commit ?predecessor_commit () =
    make_commit
      predecessor_commit
      [
        Tx_rollup_message_result_hash.hash_uncarbonated
          Rejection.previous_message_result;
      ]

  let commit ?expect_apply_failure b tx_rollup account commit =
    Format.printf
      "Commiting for level %a (%s)\n"
      Tx_rollup_level.pp
      commit.Tx_rollup_commitment.level
      (if Option.is_some expect_apply_failure then "x" else "√") ;
    Incremental.begin_construction b >>=? fun i ->
    Op.tx_rollup_commit (B b) account tx_rollup commit >>=? fun operation ->
    Incremental.add_operation i operation ?expect_apply_failure >>=? fun i ->
    Incremental.finalize_block i
end

(** [test_state] tests some edge cases in state management around
    rejecting commitments. *)
let test_state () =
  let open Single_message_inbox in
  context_init2 () >>=? fun (b, (account1, account2)) ->
  originate b account1 >>=? fun (b, tx_rollup) ->
  (* Submit bogus message three time to have three inboxes *)
  submit b tx_rollup account1 >>=? fun b ->
  submit b tx_rollup account1 >>=? fun b ->
  submit b tx_rollup account1 >>=? fun b ->
  (*
     Inboxes:    [  ] - [  ] - [  ]
     Commits:
  *)
  Context.Tx_rollup.state (B b) tx_rollup >>=? fun initial_state ->
  (* Commit to the first inbox with an incorrect commitment *)
  let commit1 = wrong_commit () in
  commit b tx_rollup account1 commit1 >>=? fun b ->
  (*
     Inboxes:    [  ] - [  ] - [  ]
     Commits:    [A1]
  *)
  let commit2 = wrong_commit ~predecessor_commit:commit1 () in
  commit b tx_rollup account2 commit2 >>=? fun b ->
  (*
     Inboxes:    [  ] - [  ] - [  ]
     Commits:    [A1]   [A2]
  *)
  (* Reject the commitment *)
  reject b tx_rollup account1 Tx_rollup_level.root commit1 >>=? fun b ->
  (*
     Inboxes:    [  ] - [  ] - [  ]
     Commits:    <A1>   <A2>
  *)
  (* Check that we went back to the initial state *)
  Context.Tx_rollup.state (B b) tx_rollup >>=? fun state_after_reject ->
  Alcotest.(
    check
      tx_rollup_state_testable_no_storage
      "state unchanged by commit/reject at root"
      initial_state
      (Tx_rollup_state.Internal_for_tests.reset_commitments_watermark
         state_after_reject)) ;
  assert (
    Tx_rollup_state.Internal_for_tests.get_commitments_watermark
      state_after_reject
    = Some Tx_rollup_level.(succ root)) ;
  (* Show that you cannot commit with [account1] again *)
  commit
    b
    tx_rollup
    account1
    commit1
    ~expect_apply_failure:(check_proto_error Tx_rollup_errors.Invalid_committer)
  >>=? fun b ->
  (* Commit an incorrect commitment again. *)
  commit b tx_rollup account2 commit1 >>=? fun b ->
  (*
     Inboxes:    [  ] - [  ] - [  ]
     Commits:    [A2]   <A2>
  *)
  Context.Contract.balance_and_frozen_bonds (B b) account2
  >>=? fun before_slashing ->
  let commit2 = wrong_commit ~predecessor_commit:commit1 () in
  commit b tx_rollup account1 commit2 >>=? fun b ->
  Context.get_constants (B b) >>=? fun constants ->
  Assert.balance_was_debited
    ~loc:__LOC__
    (B b)
    account2
    before_slashing
    constants.parametric.tx_rollup.commitment_bond
  >>=? fun () ->
  (*
     Inboxes:    [  ] - [  ] - [  ]
     Commits:    [A2]   [A1]
  *)
  (* Reject the first commitment *)
  reject b tx_rollup account2 Tx_rollup_level.root commit1 >>=? fun b ->
  (* Check that we went back to the initial state *)
  (*
     Inboxes:    [  ] - [  ] - [  ]
     Commits:    <A2>   <A1>
  *)
  Context.Tx_rollup.state (B b) tx_rollup >>=? fun state_after_reject ->
  Alcotest.(
    check
      tx_rollup_state_testable_no_storage
      "state unchanged by commit/reject at root"
      initial_state
      (Tx_rollup_state.Internal_for_tests.reset_commitments_watermark
         state_after_reject)) ;
  (* Commit twice *)
  let commit1 = correct_commit () in
  let commit2 = wrong_commit ~predecessor_commit:commit1 () in
  commit b tx_rollup account1 commit1 >>=? fun b ->
  (*
     Inboxes:    [  ] - [  ] - [  ]
     Commits:    [A1]   <A1>
  *)
  commit b tx_rollup account2 commit2 >>=? fun b ->
  (*
     Inboxes:    [  ] - [  ] - [  ]
     Commits:    [A1]   [A2]
  *)
  (* committing empty blocks then finalizing *)
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Op.tx_rollup_finalize (B b) account1 tx_rollup >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  (* Check we cannot finalize root anymore *)
  reject
    b
    tx_rollup
    account2
    Tx_rollup_level.root
    commit1
    ~expect_apply_failure:
      (check_proto_error_f (function
          | Tx_rollup_errors.Cannot_reject_level _ -> true
          | _ -> false))
  >>=? fun b ->
  (* We can reject level 1 *)
  reject b tx_rollup account2 Tx_rollup_level.(succ root) commit2 >>=? fun b ->
  (* There is no commitment to finalize anymore *)
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Op.tx_rollup_finalize (B b) account1 tx_rollup >>=? fun operation ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    i
    operation
    ~expect_apply_failure:
      (check_proto_error Tx_rollup_errors.No_commitment_to_finalize)
  >>=? fun i ->
  ignore i ;
  return_unit

(** [test_state_with_deleted] tests an edge cases in state management
    when rejecting commitment whose predecessor has already been
    deleted. *)
let test_state_with_deleted () =
  let open Single_message_inbox in
  context_init1 () >>=? fun (b, account1) ->
  originate b account1 >>=? fun (b, tx_rollup) ->
  (* Create three inboxes *)
  submit b tx_rollup account1 >>=? fun b ->
  submit b tx_rollup account1 >>=? fun b ->
  submit b tx_rollup account1 >>=? fun b ->
  (* Commit to level 0 *)
  let commit0 =
    Tx_rollup_commitment.
      {
        level = Tx_rollup_level.root;
        messages =
          [
            Tx_rollup_message_result_hash.hash_uncarbonated
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
        predecessor =
          Some Tx_rollup_commitment.(Compact.hash @@ Full.compact commit0);
        inbox_merkle_root = inbox_hash;
      }
  in
  Op.tx_rollup_commit (B b) account1 tx_rollup commit1 >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  (* Finalize *)
  Op.tx_rollup_finalize (B b) account1 tx_rollup >>=? fun operation ->
  Block.bake b ~operation >>=? fun b ->
  (* fail to remove too early *)
  Incremental.begin_construction b >>=? fun i ->
  Op.tx_rollup_remove_commitment (I i) account1 tx_rollup >>=? fun operation ->
  Incremental.add_operation
    i
    operation
    ~expect_apply_failure:
      (check_proto_error Tx_rollup_errors.Remove_commitment_too_early)
  >>=? fun (_ : Incremental.t) ->
  (* Wait for some blocks, then remove *)
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Block.bake b ~operations:[] >>=? fun b ->
  Op.tx_rollup_remove_commitment (B b) account1 tx_rollup >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  (* Reject level 1, it works *)
  reject b tx_rollup account1 commit1.level commit1 >>=? fun b ->
  ignore b ;
  return_unit

(** [test_state_message_storage_preallocation] verifies that message
   commitment burn is charged upfront. *)
let test_state_message_storage_preallocation () =
  context_init1 () >>=? fun (b, account1) ->
  originate b account1 >>=? fun (b, tx_rollup) ->
  Incremental.begin_construction b >>=? fun i ->
  let ctxt = Incremental.alpha_ctxt i in
  let message, _ = Tx_rollup_message.make_batch "bogus" in
  let message_hash = Tx_rollup_message_hash.hash_uncarbonated message in
  let (_inbox_hash : Tx_rollup_inbox.Merkle.root) =
    Tx_rollup_inbox.Merkle.merklize_list [message_hash]
  in
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
  let inbox_preparation = Tx_rollup_inbox.size in
  Alcotest.check
    zestable
    "the storage occupied by the first message is the size of the inbox plus \
     the preallocation for commiting the message"
    inbox_preparation
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
    zestable
    "the storage occupied by the second message null thanks to the merklisation"
    Z.zero
    (Z.sub occupied_storage_after occupied_storage_before) ;
  return_unit

module Withdraw = struct
  (** [context_init_withdraw tup] initializes a context with [tup] accounts, one rollup and a
      withdrawal recipient contract. *)
  let context_init_withdraw ?tx_rollup_origination_size
      ?(amount = Z.of_int64 @@ Tx_rollup_l2_qty.to_int64 Nat_ticket.amount) tup
      =
    context_init ?tx_rollup_origination_size tup >>=? fun (block, accounts) ->
    let account1 = Context.tup_hd tup accounts in
    originate block account1 >>=? fun (block, tx_rollup) ->
    Nat_ticket.init_deposit amount block tx_rollup account1
    >>=? fun (operation, block, deposit_contract) ->
    Block.bake ~operation block >>=? fun block ->
    Contract_helpers.originate_contract_from_string_hash
      ~script:
        (Format.sprintf
           {| parameter (ticket %s);
              storage (option (ticket %s));
              code { CAR ; SOME ; NIL operation ; PAIR } ;|}
           Nat_ticket.ty_str
           Nat_ticket.ty_str)
      ~storage:"None"
      ~source_contract:account1
      ~baker:(Context.Contract.pkh account1)
      block
    >>=? fun (withdraw_contract, _script, block) ->
    return
      (account1, accounts, tx_rollup, deposit_contract, withdraw_contract, block)

  (** [context_init1_withdraw] initializes a context with one account, one rollup and a
      withdrawal recipient contract. *)
  let context_init1_withdraw () =
    context_init_withdraw Context.T1
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
    context_init_withdraw Context.T2
    >>=? fun ( account1,
               (_, account2),
               tx_rollup,
               deposit_contract,
               withdraw_contract,
               b ) ->
    return
      (account1, account2, tx_rollup, deposit_contract, withdraw_contract, b)

  let originate_forge_withdraw_deposit_contract account block =
    Contract_helpers.originate_contract_from_string_hash
      ~script:
        {| parameter (or (pair %default nat nat)
                         (or (ticket %withdraw nat)
                             (pair %deposit address tx_rollup_l2_address)));
           storage (list (ticket nat));
           code
             {
               UNPAIR;
               IF_LEFT
                 {
                   UNPAIR;
                   TICKET;
                   ASSERT_SOME;
                   CONS;
                   NIL operation;
                   PAIR
                 }
                 {
                   IF_LEFT
                     {
                       CONS;
                       NIL operation;
                       PAIR;
                     }
                     {
                       UNPAIR;
                       CONTRACT %deposit (pair (ticket nat) tx_rollup_l2_address);
                       ASSERT_SOME;
                       DUG 2;
                       SWAP;
                       IF_CONS
                         {
                           SWAP;
                           DUG 3;
                           PAIR;
                           PUSH mutez 0;
                           SWAP;
                           TRANSFER_TOKENS;
                           NIL operation;
                           SWAP;
                           CONS;
                           PAIR
                         }
                         { FAIL }
                     }
                }
             } |}
      ~storage:"{}"
      ~source_contract:account
      ~baker:(Context.Contract.pkh account)
      block
    >>=? fun (forge_withdraw_deposit_contract, _script, block) ->
    return (forge_withdraw_deposit_contract, block)

  (** [finalize_all_commitment_with_withdrawals ~account ~tx_rollup ~withdrawals
      b] commit and finalize all uncommitted inboxes for a tx_rollup and a
      commitment containing [withdrawals] for the last unfinalized commitments
      (same format as in [make_incomplete_commitment_for_batch]).

      It returns the commitment and a list of dummy context hashes for
      the last inboxes committed.  When [batches] is set, first it
      submits [batches] in a combined manager operation*)
  let finalize_all_commitment_with_withdrawals ?batches ~account ~tx_rollup
      ~withdrawals b =
    (match batches with
    | None -> return b
    | Some batches ->
        List.fold_right_es
          (fun batch operations ->
            Op.tx_rollup_submit_batch (B b) account tx_rollup batch
            >>=? fun operation -> return (operation :: operations))
          batches
          []
        >>=? fun operations ->
        Op.combine_operations ~source:account (B b) operations
        >>=? fun operation -> Block.bake ~operation b)
    >>=? fun b ->
    Context.get_level (B b) >>?= fun current_level ->
    Context.Tx_rollup.state (B b) tx_rollup >>=? fun state ->
    Alpha_context.Tx_rollup_state.Internal_for_tests.next_commitment_level
      state
      (Raw_level.succ current_level)
    >>??= fun next_commitment_level ->
    let uncommitted_inboxes =
      Alpha_context.Tx_rollup_state.Internal_for_tests.uncommitted_inboxes_count
        state
    in
    let uncommitted_inboxes = max 0 (uncommitted_inboxes - 1) in
    let rec aux b committed_inbox next_commitment_level =
      if committed_inbox >= uncommitted_inboxes then
        return (b, next_commitment_level)
      else
        (* Make a commitment for the dummy batch. Mock the list of withdrawals as
              per [withdrawals]. Include the commitment in an operation and bake. *)
        make_incomplete_commitment_for_batch
          (B b)
          next_commitment_level
          tx_rollup
          []
        >>=? fun (commitment, _context_hash_list) ->
        Op.tx_rollup_commit (B b) account tx_rollup commitment
        >>=? fun operation ->
        Block.bake ~operation b >>=? fun b ->
        (* 3. Finalize the commitment *)
        Op.tx_rollup_finalize (B b) account tx_rollup >>=? fun operation ->
        Block.bake ~operation b >>=? fun b ->
        aux b (committed_inbox + 1) (Tx_rollup_level.succ next_commitment_level)
    in
    aux b 0 next_commitment_level >>=? fun (b, next_commitment_level) ->
    make_incomplete_commitment_for_batch
      (B b)
      next_commitment_level
      tx_rollup
      withdrawals
    >>=? fun (commitment, context_hash_list) ->
    Op.tx_rollup_commit (B b) account tx_rollup commitment >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    (* 3. Finalize the commitment *)
    Op.tx_rollup_finalize (B b) account tx_rollup >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    return (commitment, context_hash_list, next_commitment_level, b)

  (** [test_valid_withdraw] checks that a smart contract can deposit tickets to a
    transaction rollup. *)
  let test_valid_withdraw () =
    context_init2_withdraw ()
    >>=? fun ( account1,
               account2,
               tx_rollup,
               deposit_contract,
               withdraw_contract,
               block ) ->
    Contract_helpers.originate_contract_from_string_hash
      ~script:
        (Format.sprintf
           {| parameter (ticket %s);
              storage unit;
              code { CDR; NIL operation ; PAIR } ;|}
           Nat_ticket.ty_str)
      ~storage:"Unit"
      ~source_contract:account1
      ~baker:(Context.Contract.pkh account1)
      block
    >>=? fun (withdraw_dropping_contract, _script, block) ->
    let token_one = Nat_ticket.ex_token ~ticketer:deposit_contract in
    (* The Tx_rollup should own some tickets and the four contract none before
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
      (Contract (Originated withdraw_contract))
      None
    >>=? fun () ->
    assert_ticket_balance ~loc:__LOC__ block token_one (Contract account1) None
    >>=? fun () ->
    assert_ticket_balance ~loc:__LOC__ block token_one (Contract account2) None
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
    >>=? fun (withdraw1, ticket_info1) ->
    Nat_ticket.withdrawal
      (B block)
      ~ticketer:deposit_contract
      ~claimer:account2
      ~amount:half_amount
      tx_rollup
    >>=? fun (withdraw2, ticket_info2) ->
    (* 2 Add a batch message to [b], a commitment for that inbox
       containing the withdrawal at index 0, and finalize that
       commitment *)
    finalize_all_commitment_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [withdraw1; withdraw2])]
      block
    >>=? fun (commitment, context_hash_list, committed_level, block) ->
    let message_index = 0 in
    let context_hash =
      WithExceptions.Option.get
        ~loc:__LOC__
        (List.nth context_hash_list message_index)
    in
    occupied_storage_size (B block) tx_rollup
    >>=? fun storage_size_before_withdraw ->
    (* -- At this point, everything is in place for the user to execute the
           withdrawal -- *)
    let message_result_path =
      compute_message_result_path commitment ~message_position:message_index
    in
    Op.tx_rollup_dispatch_tickets
      (B block)
      ~source:account1
      ~message_index
      ~message_result_path
      tx_rollup
      committed_level
      context_hash
      [ticket_info1; ticket_info2]
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* Now the Tx_rollup should own no tickets and the two implicit contract half before
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
      (Contract (Originated withdraw_contract))
      None
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Contract account1)
      (Some (Int64.to_int int64_half_amount))
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Contract account2)
      (Some (Int64.to_int int64_half_amount))
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Tx_rollup tx_rollup)
      None
    >>=? fun () ->
    (* 3. Now execute the withdrawal. The ticket should be received by
       withdraw_contract at the default entrypoint. *)
    let entrypoint = Entrypoint.default in
    Op.transfer_ticket
      (B block)
      ~source:account1
      ~contents:(Script.lazy_expr Nat_ticket.contents)
      ~ty:(Script.lazy_expr Nat_ticket.ty)
      ~ticketer:deposit_contract
      ~amount:
        (WithExceptions.Option.get ~loc:__LOC__
        @@ Ticket_amount.of_z
        @@ Script_int.of_int64 int64_half_amount)
      ~destination:(Originated withdraw_contract)
      ~entrypoint
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* 4.1 We assert that [withdraw_contract] has received the ticket as
       expected *)
    occupied_storage_size (B block) tx_rollup
    >>=? fun storage_size_after_withdraw ->
    let extra_storage_space =
      Z.(sub storage_size_after_withdraw storage_size_before_withdraw)
    in
    (* no extra space should be allocated for withdraw*)
    assert (extra_storage_space = Z.zero) ;
    Incremental.begin_construction block >>=? fun i ->
    let ctxt = Incremental.alpha_ctxt i in
    Contract.get_storage ctxt withdraw_contract
    >>=?? fun (_ctxt, found_storage) ->
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
      (Contract (Originated withdraw_contract))
      (Some (Int64.to_int int64_half_amount))
    >>=? fun () ->
    assert_ticket_balance ~loc:__LOC__ block token_one (Contract account1) None
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Contract account2)
      (Some (Int64.to_int int64_half_amount))
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Tx_rollup tx_rollup)
      None
    >>=? fun () ->
    (* 5.1 And finally we try to drop the other half amount of ticket. *)
    Op.transfer_ticket
      (B block)
      ~source:account2
      ~contents:(Script.lazy_expr Nat_ticket.contents)
      ~ty:(Script.lazy_expr Nat_ticket.ty)
      ~ticketer:deposit_contract
      ~amount:
        (WithExceptions.Option.get ~loc:__LOC__
        @@ Ticket_amount.of_z
        @@ Script_int.of_int64 int64_half_amount)
      ~destination:(Originated withdraw_dropping_contract)
      ~entrypoint
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* 4. Finally, we assert that [withdraw_contract] has received the ticket as
       expected *)
    Incremental.begin_construction block >>=? fun i ->
    let ctxt = Incremental.alpha_ctxt i in
    Contract.get_storage ctxt withdraw_dropping_contract
    >>=?? fun (_ctxt, found_storage) ->
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
      (Contract (Originated withdraw_contract))
      (Some (Int64.to_int int64_half_amount))
    >>=? fun () ->
    assert_ticket_balance ~loc:__LOC__ block token_one (Contract account1) None
    >>=? fun () ->
    assert_ticket_balance ~loc:__LOC__ block token_one (Contract account2) None
    >>=? fun () ->
    assert_ticket_balance
      ~loc:__LOC__
      block
      token_one
      (Tx_rollup tx_rollup)
      None

  (** [test_invalid_reveal_withdrawals_no_commitment] checks that attempting to
      reveal withdrawals from a level with no committed inbox raises an
      error. *)
  let test_invalid_reveal_withdrawals_no_commitment () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, _withdraw_contract, block)
      ->
    Nat_ticket.withdrawal
      (B block)
      ~ticketer:deposit_contract
      ~claimer:account1
      ~amount:Nat_ticket.amount
      tx_rollup
    >>=? fun (_withdraw, ticket_info) ->
    Incremental.begin_construction block >>=? fun incr ->
    Op.tx_rollup_dispatch_tickets
      (I incr)
      ~source:account1
      ~message_index:0 (* any indexes will fail *)
      ~message_result_path:Tx_rollup_commitment.Merkle.dummy_path
      tx_rollup
      Tx_rollup_level.root
      (Context_hash.hash_bytes [Bytes.make 20 'c'])
      (* any context hash will fail *)
      [ticket_info]
    (* any non-empty list will fail *)
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.No_finalized_commitment_for_level
             {level; window = None} ->
             Tx_rollup_level.(level = root)
         | _ -> false)
      incr
      operation
    >>=? fun (_ : Incremental.t) -> return_unit

  (** [test_invalid_reveal_withdrawals_missing_withdraw_in_commitment] tries to
      reveal withdrawals when the commitment in question has no withdrawals
      associated. *)
  let test_invalid_reveal_withdrawals_missing_withdraw_in_commitment () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, _deposit_contract, _withdraw_contract, block)
      ->
    let batch = "batch" in
    Op.tx_rollup_submit_batch (B block) account1 tx_rollup batch
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    finalize_all_commitment_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[]
      block
    >>=? fun (commitment, context_hash_list, committed_level, block) ->
    let context_hash =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.nth context_hash_list 0
    in
    Incremental.begin_construction block >>=? fun incr ->
    let message_index = 0 in
    let message_result_path =
      compute_message_result_path commitment ~message_position:message_index
    in
    Op.tx_rollup_dispatch_tickets
      (I incr)
      ~source:account1
      ~message_index
      ~message_result_path
      tx_rollup
      committed_level
      context_hash
      []
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_failure:
        (check_proto_error Tx_rollup_errors.No_withdrawals_to_dispatch)
      incr
      operation
    >>=? fun (_ : Incremental.t) -> return_unit

  (** [test_reveal_withdrawals_invalid_tickets_info] test to reveal withdrawals with
      tickets that do not correspond to the given proof and asserts that errors
      are raised. *)
  let test_reveal_withdrawals_invalid_tickets_info () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, withdraw_contract, block)
      ->
    let batch = "batch" in
    Op.tx_rollup_submit_batch (B block) account1 tx_rollup batch
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    let message_index = 0 in
    Nat_ticket.withdrawal
      (B block)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun (withdraw, ticket_info) ->
    finalize_all_commitment_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [withdraw])]
      block
    >>=? fun (commitment, context_hash_list, committed_level, block) ->
    let context_hash =
      WithExceptions.Option.get
        ~loc:__LOC__
        (List.nth context_hash_list message_index)
    in
    Incremental.begin_construction block >>=? fun incr ->
    (* Try with invalid amounts *)
    let message_result_path =
      compute_message_result_path commitment ~message_position:message_index
    in
    Op.tx_rollup_dispatch_tickets
      (I incr)
      ~source:account1
      ~message_index
      ~message_result_path
      tx_rollup
      committed_level
      context_hash
      [{ticket_info with amount = Tx_rollup_l2_qty.of_int64_exn 9L}]
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.Wrong_rejection_hash
             {provided = _; expected = `Valid_path (_, 0)} ->
             true
         | _ -> false)
      incr
      operation
    >>=? fun (_incr : Incremental.t) ->
    (* Try with twice the same withdrawal *)
    Op.tx_rollup_dispatch_tickets
      (I incr)
      ~source:account1
      ~message_index
      ~message_result_path
      tx_rollup
      committed_level
      context_hash
      [ticket_info; ticket_info]
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.Wrong_rejection_hash
             {provided = _; expected = `Valid_path (_, 0)} ->
             true
         | _ -> false)
      incr
      operation
    >>=? fun (_incr : Incremental.t) ->
    (* Try with wrong type *)
    Op.tx_rollup_dispatch_tickets
      (I incr)
      ~source:account1
      ~message_index
      ~message_result_path
      tx_rollup
      committed_level
      context_hash
      [{ticket_info with ty = Script.lazy_expr @@ Expr.from_string "unit"}]
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_apply_failure:(function
        | Environment.Ecoproto_error
            (Script_tc_errors.Invalid_constant (_, _, _))
          :: _ ->
            return_unit
        | _ -> Alcotest.fail "expected to fail with wrong type")
      incr
      operation
    >>=? fun (_incr : Incremental.t) ->
    (* Try with wrong contents *)
    Op.tx_rollup_dispatch_tickets
      (I incr)
      ~source:account1
      ~message_index
      ~message_result_path
      tx_rollup
      committed_level
      context_hash
      [{ticket_info with contents = Script.lazy_expr @@ Expr.from_string "2"}]
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.Wrong_rejection_hash
             {provided = _; expected = `Valid_path (_, 0)} ->
             true
         | _ -> false)
      incr
      operation
    >>=? fun (_incr : Incremental.t) ->
    (* Try with wrong ticketer *)
    Op.tx_rollup_dispatch_tickets
      (I incr)
      ~source:account1
      ~message_index
      ~message_result_path
      tx_rollup
      committed_level
      context_hash
      [{ticket_info with ticketer = Originated withdraw_contract}]
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.Wrong_rejection_hash
             {provided = _; expected = `Valid_path (_, 0)} ->
             true
         | _ -> false)
      incr
      operation
    >>=? fun (_incr : Incremental.t) -> return_unit

  (** [test_reveal_withdrawals_twice] asserts that withdrawing the same
      withdrawal twice raises an error with the ticket table accounting. *)
  let test_reveal_withdrawals_twice () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, _withdraw_contract, block)
      ->
    Nat_ticket.withdrawal
      (B block)
      ~amount:
        (Tx_rollup_l2_qty.of_int64_exn
           (Int64.div (Tx_rollup_l2_qty.to_int64 Nat_ticket.amount) 2L))
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun (withdraw, ticket_info) ->
    finalize_all_commitment_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(0, [withdraw])]
      block
    >>=? fun (commitment, context_hash_list, committed_level, block) ->
    let message_index = 0 in
    let context_hash =
      WithExceptions.Option.get
        ~loc:__LOC__
        (List.nth context_hash_list message_index)
    in

    let message_result_path =
      compute_message_result_path commitment ~message_position:message_index
    in
    Op.tx_rollup_dispatch_tickets
      (B block)
      ~source:account1
      ~message_index
      ~message_result_path
      tx_rollup
      committed_level
      context_hash
      [ticket_info]
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* Execute again *)
    Op.tx_rollup_dispatch_tickets
      (B block)
      ~source:account1
      ~message_index
      ~message_result_path
      tx_rollup
      committed_level
      context_hash
      [ticket_info]
    >>=? fun operation ->
    Incremental.begin_construction block >>=? fun incr ->
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Tx_rollup_errors.Withdrawals_already_dispatched)
      incr
      operation
    >>=? fun (_ : Incremental.t) -> return_unit

  (** [test_multiple_withdrawals_multiple_batches] checks that multiple withdrawals
      from the same batch are possible. *)
  let test_multiple_withdrawals_multiple_batches () =
    context_init2_withdraw ()
    >>=? fun ( account1,
               account2,
               tx_rollup,
               deposit_contract,
               withdraw_contract,
               block ) ->
    Nat_ticket.withdrawal
      (B block)
      ~ticketer:deposit_contract
      ~claimer:account1
      ~amount:(Tx_rollup_l2_qty.of_int64_exn 1L)
      tx_rollup
    >>=? fun (withdraw1, ticket_info1) ->
    Nat_ticket.withdrawal
      (B block)
      ~ticketer:deposit_contract
      ~claimer:account1
      ~amount:(Tx_rollup_l2_qty.of_int64_exn 1L)
      tx_rollup
    >>=? fun (withdraw2, ticket_info2) ->
    Nat_ticket.withdrawal
      (B block)
      ~ticketer:deposit_contract
      ~claimer:account1
      ~amount:(Tx_rollup_l2_qty.of_int64_exn 2L)
      tx_rollup
    >>=? fun (withdraw3, ticket_info3) ->
    Nat_ticket.withdrawal
      (B block)
      ~ticketer:deposit_contract
      ~claimer:account2
      ~amount:(Tx_rollup_l2_qty.of_int64_exn 2L)
      tx_rollup
    >>=? fun (withdraw4, ticket_info4) ->
    let first_message_index = 0 in
    let second_message_index = 1 in
    let first_withdrawals = [withdraw1; withdraw2] in
    let second_withdrawals = [withdraw3; withdraw4] in
    finalize_all_commitment_with_withdrawals
      ~batches:["batch1"; "batch2"]
      ~account:account1
      ~tx_rollup
      ~withdrawals:
        [
          (first_message_index, first_withdrawals);
          (second_message_index, second_withdrawals);
        ]
      block
    >>=? fun (commitment, context_hash_list, committed_level, block) ->
    let first_context_hash =
      WithExceptions.Option.get
        ~loc:__LOC__
        (List.nth context_hash_list first_message_index)
    in
    let second_context_hash =
      WithExceptions.Option.get
        ~loc:__LOC__
        (List.nth context_hash_list second_message_index)
    in
    let path1 =
      compute_message_result_path
        commitment
        ~message_position:first_message_index
    in
    let path2 =
      compute_message_result_path
        commitment
        ~message_position:second_message_index
    in
    Op.tx_rollup_dispatch_tickets
      (B block)
      ~source:account1
      ~message_index:first_message_index
      ~message_result_path:path1
      tx_rollup
      committed_level
      first_context_hash
      [ticket_info1; ticket_info2]
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* Execute withdraw *)
    let withdraw_op source b qty =
      Op.transfer_ticket
        (B b)
        ~source
        ~contents:(Script.lazy_expr Nat_ticket.contents)
        ~ty:(Script.lazy_expr Nat_ticket.ty)
        ~ticketer:deposit_contract
        ~amount:qty
        ~destination:(Originated withdraw_contract)
        ~entrypoint:Entrypoint.default
    in
    (* Execute withdraw with half amount *)
    withdraw_op account1 block Ticket_amount.one >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* Execute withdraw with the rest amount *)
    withdraw_op account1 block Ticket_amount.one >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* Execute again, now should fail with a ticket table error *)
    withdraw_op account1 block Ticket_amount.one >>=? fun operation ->
    Incremental.begin_construction block >>=? fun incr ->
    Incremental.add_operation
      ~expect_apply_failure:(function
        | Environment.Ecoproto_error
            (Ticket_balance.Negative_ticket_balance {key = _; balance})
          :: _ ->
            if Z.(balance = neg @@ of_int64 1L) then return_unit
              (*  key is ticket hash with account1 as owner *)
            else Alcotest.fail "failed  with wrong value"
        | _ ->
            Alcotest.fail "expected to fail with ticket table accounting error")
      incr
      operation
    >>=? fun (_incr : Incremental.t) ->
    (* Execute second reveal *)
    Op.tx_rollup_dispatch_tickets
      (B block)
      ~source:account1
      ~message_index:second_message_index
      ~message_result_path:path2
      tx_rollup
      committed_level
      second_context_hash
      [ticket_info3; ticket_info4]
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    withdraw_op account1 block Ticket_amount.(add one one)
    >>=? fun operation1 ->
    withdraw_op account2 block Ticket_amount.(add one one)
    >>=? fun operation2 ->
    Block.bake ~operations:[operation1; operation2] block
    >>=? fun (_block : Block.t) -> return_unit

  (** [test_invalid_index_or_context] checks that attempting to reveal withdrawal
      from a level with a wrong message index or context hash raises an
      error. *)
  let test_invalid_index_or_context () =
    context_init2_withdraw ()
    >>=? fun ( account1,
               account2,
               tx_rollup,
               deposit_contract,
               _withdraw_contract,
               block ) ->
    let batch = "batch" in
    Op.tx_rollup_submit_batch (B block) account1 tx_rollup batch
    >>=? fun operation1 ->
    Op.tx_rollup_submit_batch (B block) account2 tx_rollup batch
    >>=? fun operation2 ->
    Block.bake ~operations:[operation1; operation2] block >>=? fun block ->
    Nat_ticket.withdrawal
      (B block)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun (withdraw, ticket_info) ->
    make_incomplete_commitment_for_batch
      (B block)
      Tx_rollup_level.root
      tx_rollup
      []
    >>=? fun (commitment, _context_hash_list) ->
    Op.tx_rollup_commit (B block) account1 tx_rollup commitment
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    Op.tx_rollup_finalize (B block) account1 tx_rollup >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    let valid_message_index = 0 in
    let wrong_message_index = 1 in
    make_incomplete_commitment_for_batch
      (B block)
      Tx_rollup_level.(succ root)
      tx_rollup
      [(valid_message_index, [withdraw])]
    >>=? fun (commitment, context_hash_list) ->
    Op.tx_rollup_commit (B block) account1 tx_rollup commitment
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* 3. Finalize the commitment *)
    Op.tx_rollup_finalize (B block) account1 tx_rollup >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    let valid_context_hash =
      WithExceptions.Option.get
        ~loc:__LOC__
        (List.nth context_hash_list valid_message_index)
    in
    let wrong_context_hash =
      WithExceptions.Option.get
        ~loc:__LOC__
        (List.nth context_hash_list wrong_message_index)
    in
    Incremental.begin_construction block >>=? fun incr ->
    (* try with wrong context hash *)
    let valid_path =
      compute_message_result_path
        commitment
        ~message_position:valid_message_index
    in
    let wrong_path =
      compute_message_result_path
        commitment
        ~message_position:wrong_message_index
    in
    Op.tx_rollup_dispatch_tickets
      (I incr)
      ~source:account1
      ~message_index:valid_message_index
      ~message_result_path:valid_path
      tx_rollup
      Tx_rollup_level.(succ root)
      wrong_context_hash
      [ticket_info]
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.Wrong_rejection_hash
             {provided = _; expected = `Valid_path (_, idx)} ->
             Compare.Int.(idx = valid_message_index)
         | _ -> false)
      incr
      operation
    >>=? fun (_i : Incremental.t) ->
    (* try with wrong message_index *)
    Op.tx_rollup_dispatch_tickets
      (I incr)
      ~source:account1
      ~message_index:wrong_message_index
      ~message_result_path:wrong_path
      tx_rollup
      Tx_rollup_level.(succ root)
      valid_context_hash
      [ticket_info]
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.Wrong_rejection_hash
             {provided = _; expected = `Valid_path (_, idx)} ->
             Compare.Int.(idx = wrong_message_index)
         | _ -> false)
      incr
      operation
    >>=? fun (_i : Incremental.t) ->
    (* 5. try with a hilariously-large message_index.  If permitted,
         this could cause a stack overflow. *)
    let wrong_message_index = 1_000_000_000 in
    Op.tx_rollup_dispatch_tickets
      (I incr)
      ~source:account1
      ~message_index:wrong_message_index
      ~message_result_path:wrong_path
      tx_rollup
      Tx_rollup_level.(succ root)
      valid_context_hash
      [ticket_info]
    >>=? fun operation ->
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.Wrong_rejection_hash
             {provided = _; expected = `Valid_path (_, idx)} ->
             Compare.Int.(idx = wrong_message_index)
         | _ -> false)
      incr
      operation
    >>=? fun (_i : Incremental.t) ->
    (* valid reveal *)
    Op.tx_rollup_dispatch_tickets
      (I incr)
      ~source:account1
      ~message_index:valid_message_index
      ~message_result_path:valid_path
      tx_rollup
      Tx_rollup_level.(succ root)
      valid_context_hash
      [ticket_info]
    >>=? fun operation ->
    Incremental.add_operation incr operation >>=? fun (_i : Incremental.t) ->
    return_unit

  (** [test_too_late_withdrawal] checks that attempting to withdraw from a level
      of a commitment already removed fails. *)
  let test_too_late_withdrawal () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, _withdraw_contract, block)
      ->
    Nat_ticket.withdrawal
      (B block)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun (withdraw, ticket_info) ->
    let message_index = 0 in
    (* Make a commitment for the dummy batch. Mock the list of withdrawals as
       per [withdrawals]. Include the commitment in an operation and bake. *)
    finalize_all_commitment_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(message_index, [withdraw])]
      block
    >>=? fun (commitment, context_hash_list, committed_level, block) ->
    let context_hash =
      WithExceptions.Option.get ~loc:__LOC__
      @@ List.nth context_hash_list message_index
    in
    Block.bake block >>=? fun block ->
    (* Remove the commitment *)
    Op.tx_rollup_remove_commitment (B block) account1 tx_rollup
    >>=? fun operation ->
    Block.bake ~operation block >>=? fun block ->
    (* At this point, the reveal can no longer be executed *)
    Incremental.begin_construction block >>=? fun incr ->
    let message_result_path =
      compute_message_result_path commitment ~message_position:message_index
    in
    Op.tx_rollup_dispatch_tickets
      (I incr)
      ~source:account1
      ~message_index
      ~message_result_path
      tx_rollup
      committed_level
      context_hash
      [ticket_info]
    >>=? fun operation ->
    (* try with correct withdraw but too late *)
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error_f @@ function
         | Tx_rollup_errors.No_finalized_commitment_for_level
             {level; window = None} ->
             Tx_rollup_level.(level = committed_level)
         | _error -> false)
      incr
      operation
    >>=? fun (_i : Incremental.t) -> return_unit

  (** [test_withdrawal_accounting_is_cleaned_up_after_removal]
      Check that withdrawal accounting is cleaned
      up along with the commitment.
   *)
  let test_withdrawal_accounting_is_cleaned_up_after_removal () =
    context_init1_withdraw ()
    >>=? fun (account1, tx_rollup, deposit_contract, _withdraw_contract, b) ->
    let message_position = 0 in
    let assert_consumed b ~msg committed_level consumed_expected =
      Incremental.begin_construction b >>=? fun i ->
      let ctxt = Incremental.alpha_ctxt i in
      Alpha_context.Tx_rollup_reveal.mem
        ctxt
        tx_rollup
        committed_level
        ~message_position
      >>=?? fun (_, consumed_actual) ->
      Alcotest.(check bool msg consumed_expected consumed_actual) ;
      return_unit
    in

    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      tx_rollup
    >>=? fun (withdraw, ticket_info) ->
    finalize_all_commitment_with_withdrawals
      ~account:account1
      ~tx_rollup
      ~withdrawals:[(message_position, [withdraw])]
      b
    >>=? fun (commitment, context_hash_list, committed_level, b) ->
    let context_hash =
      WithExceptions.Option.get ~loc:__LOC__ (List.nth context_hash_list 0)
    in
    assert_consumed
      b
      ~msg:"should not be consumed before withdrawal"
      committed_level
      false
    >>=? fun () ->
    (* Execute with withdrawal *)
    let message_result_path =
      compute_message_result_path commitment ~message_position
    in
    Op.tx_rollup_dispatch_tickets
      (B b)
      ~source:account1
      ~message_index:message_position
      ~message_result_path
      tx_rollup
      committed_level
      context_hash
      [ticket_info]
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    assert_consumed
      b
      ~msg:"should be consumed after withdrawal"
      committed_level
      true
    >>=? fun () ->
    (* Remove the commitment *)
    Op.tx_rollup_remove_commitment (B b) account1 tx_rollup
    >>=? fun operation ->
    Block.bake ~operation b >>=? fun b ->
    assert_consumed
      b
      committed_level
      ~msg:"consumtion memory should be removed with commitment"
      false
    >>=? fun () -> return_unit

  (** Confirm that executing a deposit message produces the correct withdraws.
      In order to check that the withdrawals are actually correct, we fail to
      reject them. *)
  let make_and_check_correct_commitment ctxt tx_rollup account store message
      level withdrawals ~previous_message_result =
    make_incomplete_commitment_for_batch ctxt level tx_rollup withdrawals
    >>=? fun (commitment, _) ->
    l2_parameters ctxt >>=? fun l2_parameters ->
    Rejection.make_proof store l2_parameters message >>= fun proof ->
    let after =
      match proof.after with `Value hash -> hash | `Node hash -> hash
    in
    let m1_withdrawals =
      match List.nth withdrawals 0 with
      | Some (_idx, withdrawals) -> withdrawals
      | None -> []
    in
    let message_result =
      Tx_rollup_message_result.
        {
          context_hash = after;
          withdraw_list_hash =
            Tx_rollup_withdraw_list_hash.hash_uncarbonated m1_withdrawals;
        }
    in
    let message_result_hash =
      Tx_rollup_message_result_hash.hash_uncarbonated message_result
    in
    let commitment = {commitment with messages = [message_result_hash]} in
    Op.tx_rollup_commit ctxt account tx_rollup commitment >>=? fun operation ->
    (match ctxt with
    | B b -> Incremental.begin_construction b
    | I i -> return i)
    >>=? fun i ->
    Incremental.add_operation i operation >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    Incremental.begin_construction b >>=? fun i ->
    let message_position = 0 in
    let message_path =
      single_message_path @@ Tx_rollup_message_hash.hash_uncarbonated message
    in
    let message_result_path =
      compute_message_result_path commitment ~message_position
    in
    Op.tx_rollup_reject
      (I i)
      account
      tx_rollup
      level
      message
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~proof
      ~previous_message_result
      ~previous_message_result_path:Tx_rollup_commitment.Merkle.dummy_path
    >>=? fun op ->
    Incremental.add_operation
      i
      op
      ~expect_apply_failure:
        (check_proto_error Tx_rollup_errors.Proof_produced_rejected_state)
    >>=? fun (_i : Incremental.t) -> return (i, message_result)

  (** [test_deposit_overflow_to_withdrawal] checks that a deposit that
      overflows causes withdrawals to be generated. *)
  let test_deposit_overflow_to_withdrawal () =
    (* We deposit one less than the max, so that we can prove that the
       withdraw is equal to the deposit, rather than the remainder after
       we overflow. *)
    let max = Int64.(sub max_int 1L) in
    let _, _, pkh = gen_l2_account () in
    context_init1 () >>=? fun (b, account1) ->
    originate b account1 >>=? fun (b, tx_rollup) ->
    let pkh_str = Tx_rollup_l2_address.to_b58check pkh in
    Nat_ticket.init_deposit_contract (Z.of_int64 max) b account1
    >>=? fun (deposit_contract, _script, b) ->
    let deposit_pkh = Context.Contract.pkh account1 in
    let deposit b =
      Nat_ticket.deposit_op b tx_rollup pkh_str account1 deposit_contract
      >>=? fun operation -> Block.bake ~operation b
    in
    deposit b >>=? fun b ->
    deposit b >>=? fun b ->
    deposit b >>=? fun b ->
    Nat_ticket.withdrawal
      (B b)
      ~ticketer:deposit_contract
      ~claimer:account1
      ~amount:(Tx_rollup_l2_qty.of_int64_exn max)
      tx_rollup
    >>=? fun (withdraw, _) ->
    Nat_ticket.ticket_hash (B b) ~ticketer:deposit_contract ~tx_rollup
    >>=? fun ticket_hash ->
    let deposit1, _ =
      Tx_rollup_message.make_deposit
        deposit_pkh
        (Tx_rollup_l2_address.Indexable.value pkh)
        ticket_hash
        (Tx_rollup_l2_qty.of_int64_exn max)
    in
    Rejection.init_l2_store () >>= fun store ->
    (* For the first deposit, we have no withdraws *)
    make_and_check_correct_commitment
      (B b)
      tx_rollup
      account1
      store
      deposit1
      (tx_level 0l)
      []
      ~previous_message_result:Rejection.previous_message_result
    >>=? fun (i, previous_message_result) ->
    l2_parameters (I i) >>=? fun l2_parameters ->
    (* Finally, we apply the deposit manually to have the good resulting store
       for next operations *)
    Rejection.Apply.apply_message store l2_parameters deposit1
    >>= fun (store, _) ->
    Rejection.commit_store store >>= fun store ->
    (* For the second deposit, we have one. *)
    make_and_check_correct_commitment
      (I i)
      tx_rollup
      account1
      store
      deposit1
      (tx_level 1l)
      [(0, [withdraw])]
      ~previous_message_result
    >>=? fun (i, previous_message_result) ->
    Rejection.Apply.apply_message store l2_parameters deposit1
    >>= fun (store, _) ->
    Rejection.commit_store store >>= fun store ->
    (* For the third deposit, we have one. *)
    make_and_check_correct_commitment
      (I i)
      tx_rollup
      account1
      store
      deposit1
      (tx_level 2l)
      [(0, [withdraw])]
      ~previous_message_result
    >>=? fun (_, _) -> return_unit

  (** [test_deposit_multiple_destinations_at_limit] checks that we can
      deposit the maximum number of tickets to multiple destinations ]
      without overflowing. *)
  let test_deposit_multiple_destinations_at_limit () =
    let max = Int64.max_int in
    let _, _, pkh1 = gen_l2_account () in
    let _, _, pkh2 = gen_l2_account () in
    context_init1 () >>=? fun (b, account1) ->
    originate b account1 >>=? fun (b, tx_rollup) ->
    Nat_ticket.init_deposit_contract (Z.of_int64 max) b account1
    >>=? fun (deposit_contract, _script, b) ->
    let deposit_pkh = Context.Contract.pkh account1 in
    let deposit b pkh =
      let pkh_str = Tx_rollup_l2_address.to_b58check pkh in
      Nat_ticket.deposit_op b tx_rollup pkh_str account1 deposit_contract
      >>=? fun operation -> Block.bake ~operation b
    in
    deposit b pkh1 >>=? fun b ->
    deposit b pkh2 >>=? fun b ->
    Nat_ticket.ticket_hash (B b) ~ticketer:deposit_contract ~tx_rollup
    >>=? fun ticket_hash ->
    let make_deposit pkh =
      Tx_rollup_message.make_deposit
        deposit_pkh
        (Tx_rollup_l2_address.Indexable.value pkh)
        ticket_hash
        (Tx_rollup_l2_qty.of_int64_exn max)
    in
    let deposit1, _ = make_deposit pkh1 in
    let deposit2, _ = make_deposit pkh2 in
    Rejection.init_l2_store () >>= fun store ->
    (* For the first deposit, we have no withdraws *)
    make_and_check_correct_commitment
      (B b)
      tx_rollup
      account1
      store
      deposit1
      (tx_level 0l)
      []
      ~previous_message_result:Rejection.previous_message_result
    >>=? fun (i, previous_message_result) ->
    l2_parameters (I i) >>=? fun l2_parameters ->
    (* Finally, we apply the deposit manually to have the good resulting store
       for next operations *)
    Rejection.Apply.apply_message store l2_parameters deposit1
    >>= fun (store, _) ->
    Rejection.commit_store store >>= fun store ->
    (* For the second deposit, still no withdraws. *)
    make_and_check_correct_commitment
      (I i)
      tx_rollup
      account1
      store
      deposit2
      (tx_level 1l)
      []
      ~previous_message_result
    >>=? fun (i, _) ->
    ignore i ;
    return_unit

  module Forge_deposit_withdraw (Ctxt : sig
    val forge_withdraw_deposit_contract : Contract.t

    val account : Contract.t

    val tx_rollup : Tx_rollup.t
  end) =
  struct
    open Lwt_result_syntax

    let forge_ticket block =
      let* operation =
        Op.transaction
          (B block)
          ~entrypoint:Entrypoint.default
          ~parameters:
            (Expr_common.(
               pair_n
                 [
                   int (Z.of_int Nat_ticket.contents_nat);
                   int (Tx_rollup_l2_qty.to_z Nat_ticket.amount);
                 ])
            |> Tezos_micheline.Micheline.strip_locations |> Script.lazy_expr)
          ~fee:Tez.one
          Ctxt.account
          Ctxt.forge_withdraw_deposit_contract
          (Tez.of_mutez_exn 0L)
      in
      Block.bake ~operation block

    let deposit_ticket block =
      let* operation =
        Op.transaction
          (B block)
          ~entrypoint:(Entrypoint.of_string_strict_exn "deposit")
          ~parameters:
            (Expr_common.(
               pair_n
                 [
                   string (Tx_rollup.to_b58check Ctxt.tx_rollup);
                   string "tz4MSfZsn6kMDczShy8PMeB628TNukn9hi2K";
                 ])
            |> Tezos_micheline.Micheline.strip_locations |> Script.lazy_expr)
          ~fee:Tez.one
          Ctxt.account
          Ctxt.forge_withdraw_deposit_contract
          (Tez.of_mutez_exn 0L)
      in
      Block.bake ~operation block

    let dispatch_ticket block =
      let* withdraw, ticket_info =
        Nat_ticket.withdrawal
          (B block)
          ~ticketer:Ctxt.forge_withdraw_deposit_contract
          ~claimer:Ctxt.account
          Ctxt.tx_rollup
      in
      let message_index = 0 in
      let* commitment, context_hash_list, committed_level, block =
        finalize_all_commitment_with_withdrawals
          ~batches:["batch"]
          ~account:Ctxt.account
          ~tx_rollup:Ctxt.tx_rollup
          ~withdrawals:[(message_index, [withdraw])]
          block
      in
      let context_hash =
        WithExceptions.Option.get ~loc:__LOC__
        @@ List.nth context_hash_list message_index
      in
      let message_result_path =
        compute_message_result_path commitment ~message_position:message_index
      in
      let* operation =
        Op.tx_rollup_dispatch_tickets
          (B block)
          ~source:Ctxt.account
          ~message_index
          ~message_result_path
          Ctxt.tx_rollup
          committed_level
          context_hash
          [ticket_info]
      in
      Block.bake ~operation block

    let transfer_ticket block =
      let* operation =
        Op.transfer_ticket
          (B block)
          ~source:Ctxt.account
          ~contents:(Script.lazy_expr Nat_ticket.contents)
          ~ty:(Script.lazy_expr Nat_ticket.ty)
          ~ticketer:Ctxt.forge_withdraw_deposit_contract
          ~amount:
            (WithExceptions.Option.get ~loc:__LOC__
            @@ Ticket_amount.of_zint
            @@ Tx_rollup_l2_qty.to_z Nat_ticket.amount)
          ~destination:Ctxt.forge_withdraw_deposit_contract
          ~entrypoint:(Entrypoint.of_string_strict_exn "withdraw")
      in
      Block.bake ~operation block

    let token_one =
      Nat_ticket.ex_token ~ticketer:Ctxt.forge_withdraw_deposit_contract

    let assert_contract_ticket_balance ~__LOC__ block balance =
      assert_ticket_balance
        ~loc:__LOC__
        block
        token_one
        (Contract Ctxt.forge_withdraw_deposit_contract)
        balance

    let assert_account_ticket_balance ~__LOC__ block balance =
      assert_ticket_balance
        ~loc:__LOC__
        block
        token_one
        (Contract Ctxt.account)
        balance

    let assert_tx_rollup_ticket_balance ~__LOC__ block balance =
      assert_ticket_balance
        ~loc:__LOC__
        block
        token_one
        (Tx_rollup Ctxt.tx_rollup)
        balance
  end

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4083
     Remove and replace the following test with transfers between
     originated and implicit accounts directly. *)

  (** [test_forge_deposit_withdraw_deposit ()] checks the following scenario:
      1. forges new tickets and stores it in contract's storage.
      2. deposits these tickets into a tx_rollup
      3. dispatches them to an account
      4. forges new tickets
      3. transfers the tickets from the account to the contract
      4. deposits the just received tickets into the tx_rollup *)
  let test_forge_deposit_withdraw_deposit () =
    let open Lwt_result_syntax in
    let* block, account = context_init1 () in
    let* block, tx_rollup = originate block account in
    let* forge_withdraw_deposit_contract, block =
      originate_forge_withdraw_deposit_contract account block
    in
    let open Forge_deposit_withdraw (struct
      let forge_withdraw_deposit_contract =
        Contract.Originated forge_withdraw_deposit_contract

      let account = account

      let tx_rollup = tx_rollup
    end) in
    (* forge tickets and store them in the contract storage. *)
    let* block = forge_ticket block in
    let* () = assert_contract_ticket_balance ~__LOC__ block (Some 10) in
    let* () = assert_tx_rollup_ticket_balance ~__LOC__ block None in
    let* () = assert_account_ticket_balance ~__LOC__ block None in
    (* deposit tickets from the contract storage into the tx_rollup. *)
    let* block = deposit_ticket block in
    let* () = assert_contract_ticket_balance ~__LOC__ block None in
    let* () = assert_tx_rollup_ticket_balance ~__LOC__ block (Some 10) in
    let* () = assert_account_ticket_balance ~__LOC__ block None in
    (* add withdrawals, then transfer the tickets from tx_rollup to account. *)
    let* block = dispatch_ticket block in
    let* () = assert_contract_ticket_balance ~__LOC__ block None in
    let* () = assert_tx_rollup_ticket_balance ~__LOC__ block None in
    let* () = assert_account_ticket_balance ~__LOC__ block (Some 10) in
    (* forge new tickets and store them in the contract storage. *)
    let* block = forge_ticket block in
    let* () = assert_contract_ticket_balance ~__LOC__ block (Some 10) in
    let* () = assert_tx_rollup_ticket_balance ~__LOC__ block None in
    let* () = assert_account_ticket_balance ~__LOC__ block (Some 10) in
    (* transfer tickets from account to the contract. *)
    let* block = transfer_ticket block in
    let* () = assert_contract_ticket_balance ~__LOC__ block (Some 20) in
    let* () = assert_tx_rollup_ticket_balance ~__LOC__ block None in
    let* () = assert_account_ticket_balance ~__LOC__ block None in
    (* deposit back the tickets that was just transfered. *)
    let* block = deposit_ticket block in
    let* () = assert_contract_ticket_balance ~__LOC__ block (Some 10) in
    let* () = assert_tx_rollup_ticket_balance ~__LOC__ block (Some 10) in
    assert_account_ticket_balance ~__LOC__ block None

  (** [test_forge_deposit_withdraw_implicit_transfer ()] checks the following scenario:
      1. forges new tickets and stores them in contract's storage
      2. deposits these tickets into a tx_rollup
      3. dispatches them to an account
      4. transfers the dispatched tickets to another implicit account *)
  let test_forge_deposit_withdraw_implicit_transfer () =
    let open Lwt_result_syntax in
    let* block, (account, another_account) = context_init2 () in
    let* block, tx_rollup = originate block account in
    let* forge_withdraw_deposit_contract, block =
      originate_forge_withdraw_deposit_contract account block
    in
    let open Forge_deposit_withdraw (struct
      let forge_withdraw_deposit_contract =
        Contract.Originated forge_withdraw_deposit_contract

      let account = account

      let tx_rollup = tx_rollup
    end) in
    let transfer_ticket_to_implicit_with_wrong_type block =
      let* operation =
        Op.transfer_ticket
          (B block)
          ~source:account
          ~contents:(Script.lazy_expr Nat_ticket.contents)
          ~ty:(Script.lazy_expr @@ Expr.from_string "string")
          ~ticketer:(Originated forge_withdraw_deposit_contract)
          ~amount:
            (WithExceptions.Option.get ~loc:__LOC__
            @@ Ticket_amount.of_zint
            @@ Tx_rollup_l2_qty.to_z Nat_ticket.amount)
          ~destination:another_account
          ~entrypoint:Entrypoint.default
      in
      Block.bake ~operation block
    in
    let transfer_ticket_to_implicit_with_too_large_amount block =
      let* operation =
        Op.transfer_ticket
          (B block)
          ~source:account
          ~contents:(Script.lazy_expr Nat_ticket.contents)
          ~ty:(Script.lazy_expr @@ Expr.from_string "string")
          ~ticketer:(Originated forge_withdraw_deposit_contract)
          ~amount:
            (WithExceptions.Option.get ~loc:__LOC__
            @@ Ticket_amount.of_zint @@ Z.of_int64
            @@ Int64.add Nat_ticket.int64_amount 1L)
          ~destination:another_account
          ~entrypoint:Entrypoint.default
      in
      Block.bake ~operation block
    in
    let transfer_ticket_to_self_implicit block =
      let* operation =
        Op.transfer_ticket
          (B block)
          ~source:account
          ~contents:(Script.lazy_expr Nat_ticket.contents)
          ~ty:(Script.lazy_expr @@ Expr.from_string "string")
          ~ticketer:(Originated forge_withdraw_deposit_contract)
          ~amount:
            (WithExceptions.Option.get ~loc:__LOC__
            @@ Ticket_amount.of_zint
            @@ Tx_rollup_l2_qty.to_z Nat_ticket.amount)
          ~destination:account
          ~entrypoint:Entrypoint.default
      in
      Block.bake ~operation block
    in
    let transfer_ticket_to_implicit block =
      let* operation =
        Op.transfer_ticket
          (B block)
          ~source:account
          ~contents:(Script.lazy_expr Nat_ticket.contents)
          ~ty:(Script.lazy_expr Nat_ticket.ty)
          ~ticketer:(Originated forge_withdraw_deposit_contract)
          ~amount:
            (WithExceptions.Option.get ~loc:__LOC__
            @@ Ticket_amount.of_zint
            @@ Tx_rollup_l2_qty.to_z Nat_ticket.amount)
          ~destination:another_account
          ~entrypoint:Entrypoint.default
      in
      Block.bake ~operation block
    in
    let assert_another_account_ticket_balance ~__LOC__ block balance =
      assert_ticket_balance
        ~loc:__LOC__
        block
        token_one
        (Contract another_account)
        balance
    in
    (* forge tickets and store them in the contract storage. *)
    let* block = forge_ticket block in
    let* () = assert_contract_ticket_balance ~__LOC__ block (Some 10) in
    let* () = assert_tx_rollup_ticket_balance ~__LOC__ block None in
    let* () = assert_account_ticket_balance ~__LOC__ block None in
    (* deposit tickets from the contract storage into the tx_rollup. *)
    let* block = deposit_ticket block in
    let* () = assert_contract_ticket_balance ~__LOC__ block None in
    let* () = assert_tx_rollup_ticket_balance ~__LOC__ block (Some 10) in
    let* () = assert_account_ticket_balance ~__LOC__ block None in
    (* add withdrawals, then transfer the tickets from tx_rollup to account. *)
    let* block = dispatch_ticket block in
    let* () = assert_contract_ticket_balance ~__LOC__ block None in
    let* () = assert_tx_rollup_ticket_balance ~__LOC__ block None in
    let* () = assert_account_ticket_balance ~__LOC__ block (Some 10) in
    (* transfer the tickets dispatched from tx_rollup to another implicit account (error cases). *)
    let*! result = transfer_ticket_to_implicit_with_wrong_type block in
    let* () =
      Assert.is_error
        ~pp:(fun pp _ ->
          Format.pp_print_string
            pp
            "tickets with wrong type should not be accepted")
        ~loc:__LOC__
        result
    in
    let*! result = transfer_ticket_to_implicit_with_too_large_amount block in
    let* () =
      Assert.is_error
        ~pp:(fun pp _ ->
          Format.pp_print_string
            pp
            "over-drafting tickets should not be acceptable")
        ~loc:__LOC__
        result
    in
    let*! result = transfer_ticket_to_self_implicit block in
    let* () =
      Assert.is_error
        ~pp:(fun pp _ ->
          Format.pp_print_string
            pp
            "transferring tickets to oneself should not be acceptable")
        ~loc:__LOC__
        result
    in
    let* () = assert_account_ticket_balance ~__LOC__ block (Some 10) in
    let* () = assert_another_account_ticket_balance ~__LOC__ block None in
    (* transfer the tickets dispatched from tx_rollup to another implicit account (success case). *)
    let* block = transfer_ticket_to_implicit block in
    let* () = assert_account_ticket_balance ~__LOC__ block None in
    assert_another_account_ticket_balance ~__LOC__ block (Some 10)

  let tests =
    [
      Tztest.tztest "withdraw" `Quick test_valid_withdraw;
      Tztest.tztest
        "reveal withdrawals w/ missing commitment"
        `Quick
        test_invalid_reveal_withdrawals_no_commitment;
      Tztest.tztest
        "reveal withdrawals w/ missing withdraw in commitment"
        `Quick
        test_invalid_reveal_withdrawals_missing_withdraw_in_commitment;
      Tztest.tztest
        "reveal withdrawals w/ incorrect tickets info"
        `Quick
        test_reveal_withdrawals_invalid_tickets_info;
      Tztest.tztest
        "reveal withdrawals twice"
        `Quick
        test_reveal_withdrawals_twice;
      Tztest.tztest
        "withdraw w/ an invalid message index"
        `Quick
        test_invalid_index_or_context;
      Tztest.tztest "withdrawing too late" `Quick test_too_late_withdrawal;
      Tztest.tztest
        "storage clean up"
        `Quick
        test_withdrawal_accounting_is_cleaned_up_after_removal;
      Tztest.tztest
        "deposits overflowing to withdrawals"
        `Quick
        test_deposit_overflow_to_withdrawal;
      Tztest.tztest
        "deposit to multiple destinations don't overflow"
        `Quick
        test_deposit_multiple_destinations_at_limit;
      Tztest.tztest
        "multiple withdrawals from the same batch and from different batches"
        `Quick
        test_multiple_withdrawals_multiple_batches;
      Tztest.tztest
        "deposit, followed by withdrawal, followed by deposit"
        `Quick
        test_forge_deposit_withdraw_deposit;
      Tztest.tztest
        "deposit, followed by withdrawal, followed by transfer to an implicit \
         account"
        `Quick
        test_forge_deposit_withdraw_implicit_transfer;
    ]
end

let tests =
  [
    Tztest.tztest "feature flag is disabled" `Quick test_disable_feature_flag;
    Tztest.tztest "sunset" `Quick test_sunset;
    Tztest.tztest "tx rollup origination and burn" `Quick test_origination;
    Tztest.tztest
      "two originated tx rollup in one operation have different address"
      `Quick
      test_two_originations;
    Tztest.tztest
      "the function that updates the burn per byte rate of a transaction rollup"
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
    Tztest.tztest "deposit with valid contract" `Quick test_valid_deposit;
    Tztest.tztest
      "deposit with invalid parameter"
      `Quick
      test_invalid_deposit_not_ticket;
    Tztest.tztest
      "deposit with too big ticket"
      `Quick
      test_invalid_deposit_too_big_ticket;
    Tztest.tztest
      "deposit with too big ticket type"
      `Quick
      test_invalid_deposit_too_big_ticket_type;
    Tztest.tztest
      "valid deposit with big ticket"
      `Quick
      test_valid_deposit_big_ticket;
    Tztest.tztest
      "valid deposit to inexistant rollup"
      `Quick
      test_valid_deposit_inexistant_rollup;
    Tztest.tztest "invalid entrypoint" `Quick test_invalid_entrypoint;
    Tztest.tztest
      "valid deposit to invalid L2 address"
      `Quick
      test_invalid_l2_address;
    Tztest.tztest
      "valid deposit with non-zero amount"
      `Quick
      test_valid_deposit_invalid_amount;
    Tztest.tztest
      "depositing too many tickets"
      `Quick
      test_deposit_too_many_tickets;
    Tztest.tztest "finalization" `Quick test_finalization;
    Tztest.tztest "Smoke test commitment" `Quick test_commitment_duplication;
    Tztest.tztest
      "commitment predecessor edge cases"
      `Quick
      test_commitment_predecessor;
    Tztest.tztest "full inbox" `Quick test_full_inbox;
    Tztest.tztest
      "too many finalized commitments"
      `Quick
      test_too_many_commitments;
    Tztest.tztest "finalization edge cases" `Quick test_finalization_edge_cases;
    Tztest.tztest "bond finalization" `Quick test_bond_finalization;
    Tztest.tztest "state" `Quick test_state;
    Tztest.tztest
      "Try to commit to the current inbox and fail"
      `Quick
      test_commit_current_inbox;
    Tztest.tztest "state with deleted commitment" `Quick test_state_with_deleted;
    Tztest.tztest
      "upfront message preallocation"
      `Quick
      test_state_message_storage_preallocation;
    Tztest.tztest
      "storage burn for submitting batch"
      `Quick
      test_storage_burn_for_adding_batch;
    Tztest.tztest
      "additional space allocation for deposit"
      `Quick
      test_additional_space_allocation_for_valid_deposit;
    Tztest.tztest
      "additional space allocation for commitment"
      `Quick
      test_storage_burn_for_commitment;
  ]
  @ Withdraw.tests @ Rejection.tests @ parsing_tests

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("tx rollup", tests)]
  |> Lwt_main.run
