(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
open Validate_errors.Manager

(** Testing
    -------
    Component:    Host operation
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                   -- --file test_host_operation.ml

    Subject:      Test the host manager operation, which allows a manager
                  operation batch to contain multiple sources.
*)

(** Helpers *)

let default_constants =
  let open Constants_helpers.Set in
  Context.default_test_constants
  |> sponsored_operations_enable true
  |> consensus_threshold 0 |> origination_size 0
  |> Issuance_weights.base_total_issued_per_minute Tez.zero
  |> liquidity_baking_subsidy Tez.zero

let context_init ?(constants = default_constants) n =
  Context.init_with_constants_n constants n

let context_init_1 ?constants () =
  let open Lwt_result_syntax in
  let* r = context_init ?constants 1 in
  match r with b, [c1] -> return (b, c1) | _ -> assert false

let context_init_2 ?constants () =
  let open Lwt_result_syntax in
  let* r = context_init ?constants 2 in
  match r with b, [c1; c2] -> return (b, (c1, c2)) | _ -> assert false

let context_init_3 ?constants () =
  let open Lwt_result_syntax in
  let* r = context_init ?constants 3 in
  match r with b, [c1; c2; c3] -> return (b, (c1, c2, c3)) | _ -> assert false

let context_init_4 ?constants () =
  let open Lwt_result_syntax in
  let* r = context_init ?constants 4 in
  match r with
  | b, [c1; c2; c3; c4] -> return (b, (c1, c2, c3, c4))
  | _ -> assert false

let originate_contract ~b source =
  let open Lwt_result_syntax in
  let local_fact_path = "../michelson/contracts/rec_fact.tz" in
  let root_fact_path =
    "./src/proto_alpha/lib_protocol/test/integration/michelson/contracts/rec_fact.tz"
  in
  let resolved_fact_path =
    if Sys.file_exists local_fact_path then local_fact_path else root_fact_path
  in
  let contract = Contract_helpers.load_script ~storage:"0" resolved_fact_path in
  let* op, c = Op.contract_origination (B b) ~script:contract source in
  let* b = Block.bake b ~operation:op in
  return (b, c)

let validate_single_operation ?expect_failure c op =
  let open Lwt_result_syntax in
  let* inc =
    match c with
    | Context.B b -> Incremental.begin_construction b
    | I i -> return i
  in
  Incremental.validate_operation ?expect_failure inc op

let pp_operation fmt (op : packed_operation) =
  Format.fprintf
    fmt
    "%a@."
    Data_encoding.Json.pp
    (Data_encoding.Json.construct Operation.encoding op)

(** Injects a standalone [Host] operation (with no guest operations):
    {
    Hosted by [host]
    }

    Expects: [Sponsored_transaction_feature_disabled].
    Note: this batch would NOT be valid even if the feature flag were enabled
    (but the validation error would be different).
 *)
let test_disable_feature_flag () =
  let open Lwt_result_syntax in
  let* b, (host, guest) =
    Context.init2 ~consensus_threshold:0 ~sponsored_operations_enable:false ()
  in
  let ctxt = Context.B b in
  let* guest = Context.Contract.manager ctxt guest in
  let* to_sign_op =
    Op.manager_operation
      ~source:host
      ctxt
      (Host {guest = guest.pkh; guest_signature = Signature.zero})
  in
  let* account = Context.Contract.manager ctxt host in
  let op = Op.sign account.sk (Context.branch ctxt) to_sign_op in
  let*! b = Block.bake ~operation:op b in
  Assert.proto_error ~loc:__LOC__ b (function
      | Sponsored_transaction_feature_disabled -> true
      | _ -> false)

(** Injects one [batch] :
    {
    Hosted by [host]
      {
        [guest] calls [Op.dummy_script]
      }
    }

    Expects: [Sponsored_transaction_feature_disabled].
    Note: this operation would be valid if the feature flag were enabled.
 *)
let test_disable_feature_flag_simple_batch () =
  let open Lwt_result_syntax in
  let* b, (host, guest) =
    Context.init2 ~consensus_threshold:0 ~sponsored_operations_enable:false ()
  in
  let fee = Tez_helpers.of_int 10 in
  let* op, contract =
    Op.contract_origination (B b) host ~fee ~script:Op.dummy_script
  in
  let* b = Block.bake b ~operation:op in
  let* op1 = Op.transaction (B b) ~fee guest contract Tez.zero in
  let* batch = Op.host (B b) ~host ~guest ~ops:[op1] in
  let*! b = Block.bake ~operation:batch b in
  Assert.proto_error ~loc:__LOC__ b (function
      | Sponsored_transaction_feature_disabled -> true
      | _ -> false)

(** Host operation is not the first operation of a batch
    { // the following operations are baked in one block
    [other_contract] calls [Op.dummy_script];
    { // the batch for [host] starts here
    [host] transfers 50 tez to [other_contract];
    Hosted by [host]
     {
        [guest] calls [Op.dummy_script]
     }
    }
    }

    Expects: [Sponsored_transaction_feature_disabled].
    Note: the sponsored batch would be valid if the feature flag were enabled.
 *)
let test_disable_feature_flag_complex_batch () =
  let open Lwt_result_syntax in
  let* b, (host, guest, other_contract) =
    Context.init3 ~consensus_threshold:0 ~sponsored_operations_enable:false ()
  in
  let fee = Tez_helpers.of_int 10 in
  let* op, contract =
    Op.contract_origination (B b) host ~fee ~script:Op.dummy_script
  in
  let* b = Block.bake b ~operation:op in
  let* op1 =
    Op.transaction (B b) ~fee host other_contract (Tez_helpers.of_int 50)
  in
  let* op2 = Op.transaction (B b) ~fee other_contract contract Tez.zero in
  let* op3 = Op.transaction (B b) ~fee guest contract Tez.zero in
  let* op4 = Op.host (B b) ~host ~guest ~ops:[op3] in
  let* batch = Op.batch_operations ~source:host (B b) [op1; op4] in
  let*! b = Block.bake ~operations:[op2; batch] b in
  Assert.proto_error ~loc:__LOC__ b (function
      | Sponsored_transaction_feature_disabled -> true
      | _ -> false)

(** Validation KO: Source consistency *)

(** Guest = Sponsor

    Test that a sponsor cannot be a guest.

    Expects: [Guest_is_sponsor].
 *)
let test_guest_equal_sponsor () =
  let open Lwt_result_syntax in
  let* b, host = context_init_1 () in
  let* b, fact = originate_contract ~b host in
  let* host_counter = Context.Contract.counter (B b) host in
  let* op1 =
    Op.transaction
      (B b)
      ~counter:Manager_counter.(succ host_counter)
      host
      fact
      Tez.zero
  in
  let* batch = Op.host (B b) ~host ~guest:host ~ops:[op1] in
  let*! r = validate_single_operation (B b) batch in
  Assert.proto_error ~loc:__LOC__ r (function
      | Guest_is_sponsor pkh ->
          Signature.Public_key_hash.(pkh = Account.pkh_of_contract_exn host)
      | _ -> false)

(** Same guest twice

    Test that a sponsor cannot include two batches from a same guest.

    Expects: [Sponsored_transaction_source_hosted_twice].
 *)
let test_same_guest_twice () =
  let open Lwt_result_syntax in
  let* b, (host, guest) = context_init_2 () in
  let* b, fact = originate_contract ~b host in
  let* op1 =
    Op.transaction (B b) guest fact Tez.zero ~fee:Tez_helpers.one_mutez
  in
  let* batch1 = Op.host (B b) ~host ~guest ~ops:[op1] in
  let* op2 =
    Op.transaction (B b) guest fact Tez.zero ~fee:Tez_helpers.one_cent
  in
  let* batch2 = Op.host (B b) ~host ~guest ~ops:[op2] in
  let* operation =
    Op.batch_operations
      ~recompute_counters:true
      ~source:host
      (B b)
      [batch1; batch2]
  in
  let guest_pkh = Account.pkh_of_contract_exn guest in
  let*! r = validate_single_operation (B b) operation in
  Assert.proto_error ~loc:__LOC__ r (function
      | Guest_hosted_twice {guest} ->
          Signature.Public_key_hash.(guest = guest_pkh)
      | _ -> false)

(** Wrong source for the host

    Tests that an operation cannot have two different sponsors.

    Expects: [Inconsistent_sources].
 *)
let test_wrong_source_for_host () =
  let open Lwt_result_syntax in
  let* b, (host1, host2, guest1, guest2) = context_init_4 () in
  let* b, fact = originate_contract ~b host1 in
  let* op1 =
    Op.transaction (B b) guest1 fact Tez.zero ~fee:Tez_helpers.one_mutez
  in
  let* batch1 = Op.host (B b) ~host:host1 ~guest:guest1 ~ops:[op1] in
  let* op2 =
    Op.transaction (B b) guest2 fact Tez.zero ~fee:Tez_helpers.one_cent
  in
  let* batch2 = Op.host (B b) ~host:host2 ~guest:guest2 ~ops:[op2] in
  let* operation =
    Op.batch_operations
      ~recompute_counters:true
      ~source:host1
      (B b)
      [batch1; batch2]
  in
  let* (_ : Incremental.t) =
    let expect_failure =
      Error_helpers.expect_inconsistent_sources
        ~loc:__LOC__
        ~first_source:host1
        ~source:host2
    in
    validate_single_operation ~expect_failure (B b) operation
  in
  (* Now signed by host2. *)
  let* operation =
    Op.batch_operations
      ~recompute_counters:true
      ~source:host2
      (B b)
      [batch1; batch2]
  in
  let* (_ : Incremental.t) =
    let expect_failure =
      Error_helpers.expect_inconsistent_sources
        ~loc:__LOC__
        ~first_source:host1
        ~source:host2
    in
    validate_single_operation ~expect_failure (B b) operation
  in
  return_unit

(** Guest operation before host operation

    Tests that a guest operation cannot occur before a host operation.

    Expects: [Inconsistent_sources].
 *)
let test_guest_operation_before_host () =
  let open Lwt_result_syntax in
  let* b, (host, guest) = context_init_2 () in
  let* b, fact = originate_contract ~b host in
  let* op1 =
    Op.transaction (B b) guest fact Tez.zero ~fee:Tez_helpers.one_mutez
  in
  let* op2 =
    Op.transaction (B b) guest fact Tez.zero ~fee:Tez_helpers.one_cent
  in
  let* batch = Op.host (B b) ~host ~guest ~ops:[op2] in
  (* Whole batch is signed by the host *)
  let* operation =
    Op.batch_operations ~recompute_counters:true ~source:host (B b) [op1; batch]
  in
  let* (_ : Incremental.t) =
    let expect_failure =
      Error_helpers.expect_inconsistent_sources
        ~loc:__LOC__
        ~first_source:guest
        ~source:host
    in
    validate_single_operation ~expect_failure (B b) operation
  in
  (* Whole batch is signed by the guest *)
  let* operation =
    Op.batch_operations
      ~recompute_counters:true
      ~source:guest
      (B b)
      [op1; batch]
  in
  let* (_ : Incremental.t) =
    let expect_failure =
      Error_helpers.expect_inconsistent_sources
        ~loc:__LOC__
        ~first_source:guest
        ~source:host
    in
    validate_single_operation ~expect_failure (B b) operation
  in
  return_unit

(** Unexpected guest in hosted guest batch

    Tests that an unexpected guest is not allowed.

    Expects: [Guest_operation_wrong_source].
 *)
let test_unexpected_guest_in_batch () =
  let open Lwt_result_syntax in
  let* b, (host, unexpected_guest, expected_guest) = context_init_3 () in
  let* b, fact = originate_contract ~b host in
  let* op =
    Op.transaction
      (B b)
      unexpected_guest
      fact
      Tez.zero
      ~fee:Tez_helpers.one_mutez
  in
  let* batch = Op.host (B b) ~host ~guest:expected_guest ~ops:[op] in
  let* (_ : Incremental.t) =
    let expect_failure =
      Error_helpers.expect_guest_operation_wrong_source
        ~loc:__LOC__
        ~guest:expected_guest
        ~unexpected_source:unexpected_guest
    in
    validate_single_operation ~expect_failure (B b) batch
  in
  return_unit

(** No sponsor operation in a guest subbatch

    Tests when a sponsor operation is the first or last operation in a guest subbatch.

    Expects: [Guest_operation_wrong_source].
*)
let test_no_sponsor_operation_in_guest_subbatch () =
  let open Lwt_result_syntax in
  let* b, (host, guest) = context_init_2 () in
  let* b, fact = originate_contract ~b host in
  let* op_host = Op.transaction (B b) host fact Tez.zero in
  let* op_guest = Op.transaction (B b) guest fact Tez.zero in
  let* batch = Op.host (B b) ~host ~guest ~ops:[op_host; op_guest] in
  let* batch =
    Op.batch_operations (B b) ~recompute_counters:true ~source:host [batch]
  in
  let* (_ : Incremental.t) =
    let expect_failure =
      Error_helpers.expect_guest_operation_wrong_source
        ~loc:__LOC__
        ~guest
        ~unexpected_source:host
    in
    validate_single_operation ~expect_failure (B b) batch
  in
  let* batch = Op.host (B b) ~host ~guest ~ops:[op_guest; op_host] in
  let* batch =
    Op.batch_operations (B b) ~recompute_counters:true ~source:host [batch]
  in
  let* (_ : Incremental.t) =
    let expect_failure =
      Error_helpers.expect_guest_operation_wrong_source
        ~loc:__LOC__
        ~guest
        ~unexpected_source:host
    in
    validate_single_operation ~expect_failure (B b) batch
  in
  return_unit

let tests =
  let open Tztest in
  List.map
    (fun (s, f) -> tztest s `Quick f)
    [
      ( "Check feature flag is disabled (Host operation alone)",
        test_disable_feature_flag );
      ( "Check feature flag is disabled (simple valid batch)",
        test_disable_feature_flag_simple_batch );
      ( "Check feature flag is disabled (complex valid batch)",
        test_disable_feature_flag_complex_batch );
      ("Validation KO: guest equal sponsor", test_guest_equal_sponsor);
      ("Validation KO: same guest twice", test_same_guest_twice);
      ("Validation KO: test wrong source for host", test_wrong_source_for_host);
      ( "Validation KO: test guest operation before host",
        test_guest_operation_before_host );
      ( "Validation KO: test unexpected guest in batch",
        test_unexpected_guest_in_batch );
      ( "Validation KO: no sponsor operation in guest subbatch",
        test_no_sponsor_operation_in_guest_subbatch );
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("Host operation", tests)]
  |> Lwt_main.run
