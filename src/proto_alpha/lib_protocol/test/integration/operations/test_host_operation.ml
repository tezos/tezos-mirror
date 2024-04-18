(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context

(** Testing
    -------
    Component:    Host operation
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                   -- --file test_host_operation.ml
    Subject:      Test the host manager operation.
*)

(** Injects a standalone [Host] operation (with no guest operations):
    {
    Hosted by [host]
    }

    Expects: [Validate_errors.Manager.Sponsored_transaction_feature_disabled].
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
      | Validate_errors.Manager.Sponsored_transaction_feature_disabled -> true
      | _ -> false)

(** Injects one [batch] :
    {
    Hosted by [host]
      {
        [guest] calls [Op.dummy_script]
      }
    }

    Expects: [Validate_errors.Manager.Sponsored_transaction_feature_disabled].
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
      | Validate_errors.Manager.Sponsored_transaction_feature_disabled -> true
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

    Expects: [Validate_errors.Manager.Sponsored_transaction_feature_disabled].
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
      | Validate_errors.Manager.Sponsored_transaction_feature_disabled -> true
      | _ -> false)

let tests =
  [
    Tztest.tztest
      "Check feature flag is disabled (Host operation alone)"
      `Quick
      test_disable_feature_flag;
    Tztest.tztest
      "Check feature flag is disabled (simple valid batch)"
      `Quick
      test_disable_feature_flag_simple_batch;
    Tztest.tztest
      "Check feature flag is disabled (complex valid batch)"
      `Quick
      test_disable_feature_flag_complex_batch;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("Host operation", tests)]
  |> Lwt_main.run
