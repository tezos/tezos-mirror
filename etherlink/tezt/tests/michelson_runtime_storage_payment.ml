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

  module Double_send = struct
    let prg = contract_prg ["mini_scenarios"; "double_send"]

    let originate ~initial_storage =
      let init = sf {|"%s"|} initial_storage in
      Client.originate_contract ~init ~prg

    let call ~dest1 ~dest2 ~new_storage address =
      let arg = sf {|Pair (Pair "%s" "%s") "%s"|} dest1 dest2 new_storage in
      Client.transfer ~arg ~receiver:address
  end
end

module BalanceUpdate = struct
  type t =
    | Contract_change of {pkh : string; change : int}
    | Burned_change of {category : string; change : int}

  let from_json bu =
    let open JSON in
    let kind = bu |-> "kind" |> as_string in
    let change = bu |-> "change" |> as_int in
    match kind with
    | "contract" ->
        Contract_change {pkh = bu |-> "contract" |> as_string; change}
    | "burned" ->
        Burned_change {category = bu |-> "category" |> as_string; change}
    | _ -> Test.fail "Unexpected balance_update kind: %s" kind

  let pp fmt = function
    | Contract_change {pkh; change} ->
        Format.fprintf fmt "Contract(%s, %d)" pkh change
    | Burned_change {category; change} ->
        Format.fprintf fmt "Burned(%s, %d)" category change

  let typ = Check.equalable pp ( = )

  (** Look for the dual storage-fees burn pair in a list of balance
      updates. *)
  let has_storage_fees_burn_pair ~payer ~burn balance_updates =
    let debit = Contract_change {pkh = payer; change = -burn} in
    let credit = Burned_change {category = "storage fees"; change = burn} in
    let debit_present = List.exists (( = ) debit) balance_updates in
    let credit_present = List.exists (( = ) credit) balance_updates in
    debit_present && credit_present
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

  let get_operation_result content =
    content |-> "metadata" |-> "operation_result"

  let get_operation_content_storage_size content =
    get_operation_result content
    |-> "storage_size" |> as_opt |> Option.map as_int |> Option.value ~default:0

  let get_operation_content_paid_storage_size_diff content =
    get_operation_result content
    |-> "paid_storage_size_diff" |> as_opt |> Option.map as_int
    |> Option.value ~default:0

  let get_internal_operation_results content =
    content |-> "metadata" |-> "internal_operation_results" |> as_list

  let status_of_result result = result |-> "status" |> as_string

  let balance_updates_of_result result =
    result |-> "balance_updates" |> as_opt |> Option.map as_list
    |> Option.value ~default:[]
    |> List.map BalanceUpdate.from_json

  let allocated_destination_contract_of_result result =
    result |-> "allocated_destination_contract" |> as_opt |> Option.map as_bool
    |> Option.value ~default:false
end

(** Cost of one byte burned to the storage-fees sink (`COST_PER_BYTES`
    in the kernel; matches L1's `cost_per_byte`). *)
let cost_per_byte = 250

(** Slot size of an originated contract or a freshly allocated
    implicit account (`ORIGINATION_SIZE` in the kernel). *)
let origination_size = 257

(** Mutez burned to allocate one such slot. *)
let allocation_slot_burn = cost_per_byte * origination_size

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

  let operation_result = Tezos_JSON.get_operation_result content in
  let balance_updates = Tezos_JSON.balance_updates_of_result operation_result in
  let variable_burn = paid_storage_size_diff * cost_per_byte in
  Check.is_true
    (BalanceUpdate.has_storage_fees_burn_pair
       ~payer:Constant.bootstrap1.public_key_hash
       ~burn:variable_burn
       balance_updates)
    ~error_msg:
      (sf
         "Expected origination receipt to contain a variable burn pair of %d \
          mutez on bootstrap1"
         variable_burn) ;
  Check.is_true
    (BalanceUpdate.has_storage_fees_burn_pair
       ~payer:Constant.bootstrap1.public_key_hash
       ~burn:allocation_slot_burn
       balance_updates)
    ~error_msg:
      "Expected origination receipt to contain the slot burn pair on bootstrap1" ;
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

  let operation_result = Tezos_JSON.get_operation_result content in
  let balance_updates = Tezos_JSON.balance_updates_of_result operation_result in
  let variable_burn = growth * cost_per_byte in
  Check.is_true
    (BalanceUpdate.has_storage_fees_burn_pair
       ~payer:Constant.bootstrap1.public_key_hash
       ~burn:variable_burn
       balance_updates)
    ~error_msg:
      (sf
         "Expected transfer-growth receipt to contain a variable burn pair of \
          %d mutez on bootstrap1"
         variable_burn) ;
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

(** Scenario: a transfer to a fresh (not yet allocated) implicit
    account triggers the slot burn for the destination's allocation.
    The receipt exposes the dual `Debited(source)` / `Credited(storage
    fees)` balance updates, and `allocated_destination_contract` is
    `true`. *)
let test_transfer_to_fresh_implicit_burns_slot () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "transfer to unallocated implicit: receipt has storage-fees burn pair"
    ~tags:["transfer"; "implicit"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint_from_evm_node evm_node in
  let* tez_client = tezlink_client_from_evm_node evm_node () in

  let* () =
    Client.transfer
      ~amount:(Tez.of_int 1)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* content = get_first_manager_operations_content ~tez_endpoint in
  let operation_result = Tezos_JSON.get_operation_result content in
  let status = Tezos_JSON.status_of_result operation_result in
  Check.(
    (status = "applied")
      string
      ~error_msg:"Expected operation status %R, got %L") ;

  let balance_updates = Tezos_JSON.balance_updates_of_result operation_result in
  Check.is_true
    (BalanceUpdate.has_storage_fees_burn_pair
       ~payer:Constant.bootstrap1.public_key_hash
       ~burn:allocation_slot_burn
       balance_updates)
    ~error_msg:"Expected receipt to contain the slot burn pair on bootstrap1" ;

  let allocated =
    Tezos_JSON.allocated_destination_contract_of_result operation_result
  in
  Check.is_true allocated ~error_msg:"Expected allocated_destination_contract" ;
  unit

(** Scenario: a Michelson contract emits two internal transfers
    crediting two fresh implicit accounts *and* grows its own storage
    on the top-level call. Three storage burns are charged to the
    source (bootstrap1): one variable burn at top-level for the
    storage growth, and one slot burn per internal for the fresh
    implicit allocation. Each receipt carries its expected set of
    balance updates and nothing else. Acts as the positive control
    for the under-funded scenario that follows. *)
let test_two_internal_transfers_burn_each () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "top-level growth + two internal allocations: every receipt has its \
       exact balance-update set"
    ~tags:["transfer"; "internal"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint_from_evm_node evm_node in
  let* tez_client = tezlink_client_from_evm_node evm_node () in

  let initial_storage = "" in
  let* kt1 =
    Contract.Double_send.originate
      ~alias:"double_send"
      ~amount:(Tez.of_mutez_int 10)
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ~initial_storage
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let new_storage = "bigger storage" in
  let* () =
    Contract.Double_send.call
      ~amount:Tez.zero
      ~fee:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~burn_cap:Tez.one
      ~dest1:Constant.bootstrap2.public_key_hash
      ~dest2:Constant.bootstrap3.public_key_hash
      ~new_storage
      kt1
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* content = get_first_manager_operations_content ~tez_endpoint in
  let operation_result = Tezos_JSON.get_operation_result content in
  let status = Tezos_JSON.status_of_result operation_result in
  Check.(
    (status = "applied")
      string
      ~error_msg:"Expected main operation status %R, got %L") ;

  let paid_storage_size_diff =
    Tezos_JSON.get_operation_content_paid_storage_size_diff content
  in
  let variable_burn = paid_storage_size_diff * cost_per_byte in
  Check.(
    (variable_burn > 0)
      int
      ~error_msg:"Expected top-level variable burn > 0 (storage grew), got %L") ;

  let top_level_op_balance_updates =
    Tezos_JSON.balance_updates_of_result operation_result
  in
  let expected_top_level_op_balance_updates =
    BalanceUpdate.
      [
        Contract_change
          {pkh = Constant.bootstrap1.public_key_hash; change = -variable_burn};
        Burned_change {category = "storage fees"; change = variable_burn};
      ]
  in
  Check.(top_level_op_balance_updates = expected_top_level_op_balance_updates)
    (Check.list BalanceUpdate.typ)
    ~error_msg:"Expected top-level operation balance updates %R, got %L" ;

  let internal_ops = Tezos_JSON.get_internal_operation_results content in
  Check.(
    (List.length internal_ops = 2)
      int
      ~error_msg:"Expected 2 internal operations, got %L") ;
  List.iteri
    (fun i (internal_op, dest) ->
      let result = JSON.(internal_op |-> "result") in
      let st = Tezos_JSON.status_of_result result in
      Check.(
        (st = "applied")
          string
          ~error_msg:(sf "Expected internal op %d status %%R, got %%L" i)) ;

      let internal_op_balance_updates =
        Tezos_JSON.balance_updates_of_result result
      in
      let expected_internal_op_balance_updates =
        BalanceUpdate.
          [
            Contract_change {pkh = kt1; change = -1};
            Contract_change {pkh = dest; change = 1};
            Contract_change
              {
                pkh = Constant.bootstrap1.public_key_hash;
                change = -allocation_slot_burn;
              };
            Burned_change
              {category = "storage fees"; change = allocation_slot_burn};
          ]
      in
      Check.(internal_op_balance_updates = expected_internal_op_balance_updates)
        (Check.list BalanceUpdate.typ)
        ~error_msg:(sf "Expected internal op %d balance updates %%R, got %%L" i) ;
      let allocated =
        Tezos_JSON.allocated_destination_contract_of_result result
      in
      Check.is_true
        allocated
        ~error_msg:
          (sf "Expected internal op %d's allocated_destination_contract" i))
    (List.combine
       internal_ops
       [
         Constant.bootstrap2.public_key_hash; Constant.bootstrap3.public_key_hash;
       ]) ;
  unit

(** Scenario: same setup as
    {!test_two_internal_transfers_burn_each}, but the source of the
    call is bootstrap5 funded with just enough balance to cover the
    top-level variable burn plus a single slot burn — not two. The
    second internal burn must therefore fail with
    [CannotPayStorageFee], rolling the whole batch back via
    [SafeStorage] (bootstrap5's balance loses only the call fee). *)
let test_partial_internal_burn_failure_backtracks_all () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "partial internal burn failure: top-level keeps burn pair, internals \
       lose theirs"
    ~tags:["transfer"; "internal"; "backtracked"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint_from_evm_node evm_node in
  let* tez_client = tezlink_client_from_evm_node evm_node () in

  let initial_storage = "" in
  let* kt1 =
    Contract.Double_send.originate
      ~alias:"double_send"
      ~amount:(Tez.of_mutez_int 10)
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ~initial_storage
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  (* Setup an account with not enough fund to pay the next operation *)
  let initial_balance = Tez.of_mutez_int 100_000 in
  let reveal_fee = Tez.of_mutez_int 200 in
  let* () =
    Client.transfer
      ~amount:Tez.(initial_balance + reveal_fee)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap5.alias
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in
  (* Reveal manually not prevent the `reveal_fee` to shifted the balance delta. *)
  let*! () =
    Client.reveal ~fee:reveal_fee ~src:Constant.bootstrap5.alias tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let new_storage = "bigger storage" in
  let call_fee = Tez.of_mutez_int 200 in
  let* () =
    Contract.Double_send.call
      ~amount:Tez.zero
      ~fee:call_fee
      ~gas_limit:5000
      ~storage_limit:600
      ~giver:Constant.bootstrap5.alias
      ~burn_cap:Tez.one
      ~force:true
      ~dest1:Constant.bootstrap2.public_key_hash
      ~dest2:Constant.bootstrap3.public_key_hash
      ~new_storage
      kt1
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* balance_after_call =
    Client.get_balance_for
      ~account:Constant.bootstrap5.public_key_hash
      tez_client
  in
  let delta_balance = Tez.(initial_balance - balance_after_call) in

  let* content = get_first_manager_operations_content ~tez_endpoint in
  let operation_result = Tezos_JSON.get_operation_result content in
  let status = Tezos_JSON.status_of_result operation_result in
  Check.(
    (status = "backtracked")
      string
      ~error_msg:"Expected main op status %R, got %L") ;

  let paid_storage_size_diff =
    Tezos_JSON.get_operation_content_paid_storage_size_diff content
  in
  let variable_burn = paid_storage_size_diff * cost_per_byte in
  Check.(
    (variable_burn > 0)
      int
      ~error_msg:"Expected top-level variable burn > 0 (storage grew), got %L") ;

  let top_level_op_balance_updates =
    Tezos_JSON.balance_updates_of_result operation_result
  in
  let expected_top_level_op_balance_updates =
    BalanceUpdate.
      [
        Contract_change
          {pkh = Constant.bootstrap5.public_key_hash; change = -variable_burn};
        Burned_change {category = "storage fees"; change = variable_burn};
      ]
  in
  Check.(top_level_op_balance_updates = expected_top_level_op_balance_updates)
    (Check.list BalanceUpdate.typ)
    ~error_msg:"Expected top-level operation balance updates %R, got %L" ;

  let internal_ops = Tezos_JSON.get_internal_operation_results content in
  Check.(
    (List.length internal_ops = 2)
      int
      ~error_msg:"Expected 2 internal operations, got %L") ;
  List.iteri
    (fun i (internal_op, dest) ->
      let result = JSON.(internal_op |-> "result") in
      let st = Tezos_JSON.status_of_result result in
      Check.(
        (st = "backtracked")
          string
          ~error_msg:(sf "Expected internal op %d to be backtracked, got %%L" i)) ;

      let internal_op_balance_updates =
        Tezos_JSON.balance_updates_of_result result
      in
      let expected_internal_op_balance_updates =
        BalanceUpdate.
          [
            Contract_change {pkh = kt1; change = -1};
            Contract_change {pkh = dest; change = 1};
          ]
      in
      Check.(internal_op_balance_updates = expected_internal_op_balance_updates)
        (Check.list BalanceUpdate.typ)
        ~error_msg:(sf "Expected internal op %d balance updates %%R, got %%L" i))
    (List.combine
       internal_ops
       [
         Constant.bootstrap2.public_key_hash; Constant.bootstrap3.public_key_hash;
       ]) ;

  Check.(
    (delta_balance = call_fee)
      Tez.typ
      ~error_msg:
        "Expected only the call fee to be consumed, but lost %L mutez \
         (expected %R)") ;
  unit

let () =
  test_origination_receipt_exposes_storage_fields () ;
  test_transfer_with_growth_exposes_delta () ;
  test_transfer_with_shrink_exposes_zero_diff () ;
  test_transfer_to_fresh_implicit_burns_slot () ;
  test_two_internal_transfers_burn_each () ;
  test_partial_internal_burn_failure_backtracks_all ()
