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

(* Testing
   -------
   Component:    Data-availability layer
   Invocation:   dune exec tezt/tests/main.exe -- --file dal.ml
   Subject: Integration tests related to the data-availability layer
*)

let test_feature_flag =
  (* This test ensures the feature flag works:

     - 1. It checks the feature flag is not enabled by default

     - 2. It checks the new operations added by the feature flag
     cannot be propagated by checking their classification in the
     mempool. *)
  let open Tezt_tezos in
  Protocol.register_test
    ~__FILE__
    ~title:"dal feature flag"
    ~tags:["dal"]
    ~supports:Protocol.(From_protocol (Protocol.number Alpha))
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let* protocol_parameters = RPC_legacy.get_constants client in
  let feature_flag =
    JSON.(
      protocol_parameters |-> "dal_parametric" |-> "feature_enable" |> as_bool)
  in
  let number_of_slots =
    JSON.(
      protocol_parameters |-> "dal_parametric" |-> "number_of_slots" |> as_int)
  in
  Check.(
    (feature_flag = false)
      bool
      ~error_msg:"Feature flag for the DAL should be disabled") ;
  let* (`OpHash oph1) =
    Operation.Consensus.(
      inject
        ~force:true
        ~signer:Constant.bootstrap1
        (slot_availability ~endorsement:(Array.make number_of_slots false))
        client)
  in
  let* (`OpHash oph2) =
    Operation.Manager.(
      inject
        ~force:true
        [make @@ dal_publish_slot_header ~index:0 ~level:1 ~header:0]
        client)
  in
  let* mempool = Mempool.get_mempool client in
  let expected_mempool = Mempool.{empty with refused = [oph1; oph2]} in
  Check.(
    (mempool = expected_mempool)
      Mempool.classified_typ
      ~error_msg:"Expected mempool: %R. Got: %L. (Order does not matter)") ;
  let* () = Client.bake_for_and_wait client in
  let* block_metadata = RPC.(call node @@ get_chain_block_metadata ()) in
  if block_metadata.dal_slot_availability <> None then
    Test.fail "Did not expect to find \"dal_slot_availibility\"" ;
  unit

open Tezt_tezos.Rollup.Dal

let publish_slot ~source ?fee ~index node client =
  let level = Node.get_level node in
  let header = 0 in
  Operation.Manager.(
    inject
      [make ~source ?fee @@ dal_publish_slot_header ~index ~level ~header]
      client)

let slot_availability ~signer availability client =
  let default_size = 256 in
  let endorsement = Array.make default_size false in
  List.iter (fun i -> endorsement.(i) <- true) availability ;
  Operation.Consensus.(inject ~signer (slot_availability ~endorsement) client)

type status = Applied | Failed of {error_id : string}

let pp fmt = function
  | Applied -> Format.fprintf fmt "applied"
  | Failed {error_id} -> Format.fprintf fmt "failed: %s" error_id

let status_typ = Check.equalable pp ( = )

let check_manager_operation_status result expected_status oph =
  let manager_operations = JSON.(result |=> 3 |> as_list) in
  let op =
    try
      List.find
        (fun op -> JSON.(op |-> "hash" |> as_string) = oph)
        manager_operations
    with Not_found ->
      Test.fail
        "Test expecting operation %s to be included into the last block."
        oph
  in
  let op_result =
    JSON.(op |-> "contents" |=> 0 |-> "metadata" |-> "operation_result")
  in
  let status_kind = JSON.(op_result |-> "status" |> as_string) in
  let status =
    match status_kind with
    | "applied" -> Applied
    | "failed" ->
        let error_id =
          JSON.(op_result |-> "errors" |=> 0 |-> "id" |> as_string)
        in
        Failed {error_id}
    | s -> Test.fail "Unexpected status: %s" s
  in
  let prefix_msg = sf "Unexpected operation result for %s." oph in
  Check.(expected_status = status)
    status_typ
    ~error_msg:(prefix_msg ^ " Expected: %L. Got: %R.")

let test_slot_management_logic =
  Protocol.register_test
    ~__FILE__
    ~title:"dal basic logic"
    ~tags:["dal"]
    ~supports:Protocol.(From_protocol (Protocol.number Alpha))
  @@ fun protocol ->
  let* parameter_file = Parameters.parameter_file protocol in
  let* node, client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let* (`OpHash oph1) =
    publish_slot ~source:Constant.bootstrap1 ~fee:1_000 ~index:0 node client
  in
  let* (`OpHash oph2) =
    publish_slot ~source:Constant.bootstrap2 ~fee:1_500 ~index:1 node client
  in
  let* (`OpHash oph3) =
    publish_slot ~source:Constant.bootstrap3 ~fee:2_000 ~index:0 node client
  in
  let* (`OpHash oph4) =
    publish_slot ~source:Constant.bootstrap4 ~fee:1_200 ~index:1 node client
  in
  let* mempool = Mempool.get_mempool client in
  let expected_mempool =
    Mempool.{empty with applied = [oph1; oph2; oph3; oph4]}
  in
  Check.(
    (mempool = expected_mempool)
      Mempool.classified_typ
      ~error_msg:"Expected all the operations to be applied. Got %L") ;
  let* () = Client.bake_for_and_wait client in
  let* bytes = RPC_legacy.raw_bytes client in
  if JSON.(bytes |-> "dal" |> is_null) then
    Test.fail "Expected the context to contain some information about the DAL" ;
  let* operations_result = RPC_legacy.get_operations client in
  let fees_error =
    Failed {error_id = "proto.alpha.dal_publish_slot_heade_duplicate"}
  in
  (* The baker sorts operations fee wise. Consequently order of
     application for the operations will be: oph3 > oph2 > oph4 > oph1

     For slot 0, oph3 is applied first.

     Flor slot1, oph2 is applied first. *)
  check_manager_operation_status operations_result fees_error oph1 ;
  check_manager_operation_status operations_result fees_error oph4 ;
  check_manager_operation_status operations_result Applied oph3 ;
  check_manager_operation_status operations_result Applied oph2 ;
  let* _ = slot_availability ~signer:Constant.bootstrap1 [1; 0] client in
  let* _ = slot_availability ~signer:Constant.bootstrap2 [1; 0] client in
  let* _ = slot_availability ~signer:Constant.bootstrap3 [1] client in
  let* _ = slot_availability ~signer:Constant.bootstrap4 [1] client in
  let* _ = slot_availability ~signer:Constant.bootstrap5 [1] client in
  let* () = Client.bake_for_and_wait client in
  let* metadata = RPC.call node (RPC.get_chain_block_metadata ()) in
  let dal_slot_availability =
    match metadata.dal_slot_availability with
    | None ->
        assert false
        (* Field is part of the encoding when the feature flag is true *)
    | Some x -> x
  in
  Check.(
    (dal_slot_availability.(0) = false)
      bool
      ~error_msg:"Expected slot 0 to be unavailable") ;
  Check.(
    (dal_slot_availability.(1) = true)
      bool
      ~error_msg:"Expected slot 1 to be available") ;
  let* bytes = RPC_legacy.raw_bytes client in
  if not JSON.(bytes |-> "dal" |> is_null) then
    Test.fail
      "Expected the context to contain no more information about the DAL \
       anymore." ;
  unit

let register ~protocols =
  test_feature_flag protocols ;
  test_slot_management_logic protocols
