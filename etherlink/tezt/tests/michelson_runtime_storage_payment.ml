(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Etherlink: TezosX storage payment
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- \
                   --file tezosx_storage_payment.ml
   Subject:      End-to-end scenarios exercising storage-payment receipt
                 fields on Tezos X manager operations: `storage_size` and
                 `paid_storage_size_diff` after origination, after a
                 storage growth, and after a storage shrink.
*)

open Rpc.Syntax

module Setup = struct
  let runtime_tags = List.map Tezosx_runtime.tag

  let register_sandbox_test ?uses_client ~title ~tags ~with_runtimes
      ?tez_bootstrap_accounts =
    Test_helpers.register_sandbox
      ~__FILE__
      ?uses_client
      ?tez_bootstrap_accounts
      ~kernel:Latest
      ~title
      ~tags:(["tezosx"; "storage_payment"] @ runtime_tags with_runtimes @ tags)
      ~with_runtimes
end

let tezlink_foreign_endpoint_from_evm_node evm_node =
  let evm_node_endpoint = Evm_node.rpc_endpoint_record evm_node in
  {evm_node_endpoint with path = "/tezlink"}

let tezlink_endpoint_from_evm_node evm_node =
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node evm_node in
  Client.Foreign_endpoint tezlink_endpoint

let tezlink_client_from_evm_node evm_node =
  let endpoint = tezlink_endpoint_from_evm_node evm_node in
  Client.init ~endpoint

module Contract = struct
  let contract_prg path =
    Michelson_script.find path Michelson_contracts.tezlink_protocol
    |> Michelson_script.path

  module Store_input = struct
    let prg = contract_prg ["opcodes"; "store_input"]

    let originate ~initial_storage =
      let init = sf {|"%s"|} initial_storage in
      Client.originate_contract ~init ~prg

    let call ~new_storage store_input_address =
      let arg = sf {|"%s"|} new_storage in
      Client.transfer ~arg ~receiver:store_input_address
  end
end

module Tezos_JSON = struct
  open JSON

  let get_manager_operations operations = operations |=> 3 |> as_list

  let get_first_content operations =
    Check.(
      (List.length operations > 0)
        int
        ~error_msg:"Expected at least one operation in head, got %L") ;
    List.nth operations 0 |-> "contents" |=> 0

  let get_operation_content_storage_size content =
    let open JSON in
    content |-> "metadata" |-> "operation_result" |-> "storage_size" |> as_opt
    |> Option.map as_int |> Option.value ~default:0

  let get_operation_content_paid_storage_size_diff content =
    let open JSON in
    content |-> "metadata" |-> "operation_result" |-> "paid_storage_size_diff"
    |> as_opt |> Option.map as_int |> Option.value ~default:0
end

let get_first_manager_operations_content ~tez_endpoint =
  let* operations =
    RPC_core.call tez_endpoint @@ RPC.get_chain_block_operations ()
  in
  let content =
    let open Tezos_JSON in
    operations |> get_manager_operations |> get_first_content
  in
  return content

(** Scenario: an origination receipt surfaces `storage_size` and
    `paid_storage_size_diff` matching what L1 produces — both equal to
    the contract's full initial size, since `paid_bytes` starts at zero
    on a fresh contract. *)
let test_origination_receipt_exposes_storage_fields () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"origination receipt: storage_size = paid_storage_size_diff = total"
    ~tags:["origination"; "receipt"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint_from_evm_node evm_node in
  let* tez_client = tezlink_client_from_evm_node evm_node () in

  let initial_storage = "hello" in
  let* _kt1 =
    Contract.Store_input.originate
      ~alias:"store_input"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ~initial_storage
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* content = get_first_manager_operations_content ~tez_endpoint in
  let storage_size = Tezos_JSON.get_operation_content_storage_size content in
  let paid_storage_size_diff =
    Tezos_JSON.get_operation_content_paid_storage_size_diff content
  in

  Check.(
    (storage_size > String.length initial_storage)
      int
      ~error_msg:"Expected origination receipt's storage_size > 0, got %L") ;
  Check.(
    (storage_size = paid_storage_size_diff)
      int
      ~error_msg:
        "Expected storage_size %L to equal paid_storage_size_diff %R at \
         origination") ;
  unit

(** Scenario: a transfer that grows the destination's storage produces
    a receipt where `storage_size` reflects the post-op `used_bytes` and
    `paid_storage_size_diff` is the byte-length growth of the storage
    past the watermark.

    The `store_input.tz` contract replaces its string storage with the
    parameter, so the storage growth equals the byte-length difference
    between the new and the old string. *)
let test_transfer_with_growth_exposes_delta () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"transfer receipt: growth exposes delta"
    ~tags:["transfer"; "receipt"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint_from_evm_node evm_node in
  let* tez_client = tezlink_client_from_evm_node evm_node () in

  let initial_storage = "x" in
  let new_storage = "hello" in
  let* kt1 =
    Contract.Store_input.originate
      ~alias:"store_input"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ~initial_storage
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* origination_storage_size =
    let* content = get_first_manager_operations_content ~tez_endpoint in
    return (Tezos_JSON.get_operation_content_storage_size content)
  in

  let* () =
    Contract.Store_input.call
      ~amount:Tez.zero
      ~fee:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~burn_cap:Tez.one
      ~new_storage
      kt1
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* content = get_first_manager_operations_content ~tez_endpoint in
  let storage_size = Tezos_JSON.get_operation_content_storage_size content in
  let paid_storage_size_diff =
    Tezos_JSON.get_operation_content_paid_storage_size_diff content
  in

  let growth = String.length new_storage - String.length initial_storage in
  Check.(
    (paid_storage_size_diff = growth)
      int
      ~error_msg:"Expected paid_storage_size_diff %R, got %L") ;
  Check.(
    (storage_size = origination_storage_size + growth)
      int
      ~error_msg:"Expected storage_size %R (origination + growth), got %L") ;
  unit

(** Scenario: a transfer that shrinks the destination's storage past
    the watermark produces a receipt where `storage_size` is the new
    (smaller) `used_bytes` and `paid_storage_size_diff` is zero — the
    sender does not pay for an allocation that happened earlier. *)
let test_transfer_with_shrink_exposes_zero_diff () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"transfer receipt: shrink exposes zero delta"
    ~tags:["transfer"; "receipt"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint_from_evm_node evm_node in
  let* tez_client = tezlink_client_from_evm_node evm_node () in

  let initial_storage = "hello" in
  let new_storage = "x" in
  let* kt1 =
    Contract.Store_input.originate
      ~alias:"store_input"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ~initial_storage
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* origination_storage_size =
    let* content = get_first_manager_operations_content ~tez_endpoint in
    return (Tezos_JSON.get_operation_content_storage_size content)
  in

  let* () =
    Contract.Store_input.call
      ~amount:Tez.zero
      ~fee:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~burn_cap:Tez.one
      ~new_storage
      kt1
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* content = get_first_manager_operations_content ~tez_endpoint in
  let storage_size = Tezos_JSON.get_operation_content_storage_size content in
  let paid_storage_size_diff =
    Tezos_JSON.get_operation_content_paid_storage_size_diff content
  in

  let shrink = String.length initial_storage - String.length new_storage in
  Check.(
    (paid_storage_size_diff = 0)
      int
      ~error_msg:"Expected paid_storage_size_diff = 0 on shrink, got %L") ;
  Check.(
    (storage_size = origination_storage_size - shrink)
      int
      ~error_msg:"Expected storage_size %R (origination - shrink), got %L") ;
  unit

let () =
  test_origination_receipt_exposes_storage_fields () ;
  test_transfer_with_growth_exposes_delta () ;
  test_transfer_with_shrink_exposes_zero_diff ()
