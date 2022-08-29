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
    Invocation:   dune exec \
                  src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                  -- test "^zk rollup$"
    Subject:      Test zk rollup
*)

open Protocol
open Alpha_context
open Lwt_result_syntax

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

let of_plonk_smap s =
  Zk_rollup.Account.SMap.of_seq @@ Plonk.Main_protocol.SMap.to_seq s

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
      ~public_parameters:Operator.public_parameters
      ~circuits_info:(of_plonk_smap Operator.circuits)
      ~init_state:Operator.init_state
      ~nb_ops:1
  in
  let* _i =
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
  let* ctxt, contracts = context_init 1 in
  let contract = Stdlib.List.hd contracts in
  let expected_size =
    let init_account =
      Zk_rollup.Account.
        {
          static =
            {
              public_parameters = Operator.public_parameters;
              state_length = 1;
              circuits_info = of_plonk_smap Operator.circuits;
              nb_ops = 1;
            };
          dynamic = {state = Operator.init_state};
        }
    in
    let init_pl = Zk_rollup.(Empty {next_index = 0L}) in
    Zk_rollup.Address.size
    + Data_encoding.Binary.length Zk_rollup.Account.encoding init_account
    + Data_encoding.Binary.length Zk_rollup.pending_list_encoding init_pl
  in
  let* constants = Context.get_constants (B ctxt) in
  let expected_fees =
    Tez.mul_exn constants.parametric.cost_per_byte expected_size
  in
  let* operation, _rollup =
    Op.zk_rollup_origination
      (B ctxt)
      contract
      ~public_parameters:Operator.public_parameters
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
  let* ctxt, contracts = context_init 1 in
  let contract = Stdlib.List.hd contracts in
  let* operation, _rollup =
    Op.zk_rollup_origination
      (B ctxt)
      contract
      ~public_parameters:Operator.public_parameters
      ~circuits_info:(of_plonk_smap Operator.circuits)
      ~init_state:Operator.init_state
      ~nb_ops:(-1)
  in
  let* i = Incremental.begin_construction ctxt in
  let* _i =
    Incremental.add_operation
      ~expect_apply_failure:
        (check_proto_error Zk_rollup_apply.Zk_rollup_negative_nb_ops)
      i
      operation
  in
  return_unit

(** Initializes the context and originates a ZKRU. *)
let init_and_originate n =
  let* ctxt, contracts = context_init n in
  let contract = Stdlib.List.hd contracts in
  let* operation, rollup =
    Op.zk_rollup_origination
      (B ctxt)
      contract
      ~public_parameters:Operator.public_parameters
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
  let* ctxt, contracts, zk_rollup1 = init_and_originate 1 in
  let contract = Stdlib.List.hd contracts in
  let* operation, zk_rollup2 =
    Op.zk_rollup_origination
      (B ctxt)
      contract
      ~public_parameters:Operator.public_parameters
      ~circuits_info:(of_plonk_smap Operator.circuits)
      ~init_state:Operator.init_state
      ~nb_ops:1
  in
  let* _b = Block.bake ~operation ctxt in
  assert (zk_rollup1 <> zk_rollup2) ;
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
  ]
