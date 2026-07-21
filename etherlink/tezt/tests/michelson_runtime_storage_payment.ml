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
open Test_helpers

(** Tezlink's per-operation hard gas limit (Michelson gas units). Mirrors
    [hard_gas_limit_per_operation] in
    [etherlink/bin_node/lib_dev/tezlink/tezlink_constants.ml]. Octez-client
    defaults to 3_000_000 gas for [originate]/[transfer], which the node now
    rejects with [GasLimitSetError]; cap every Tezlink-targeted call here. *)
let michelson_hard_gas_limit_per_operation = 660_000

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

module Contract = struct
  let contract_prg path =
    Michelson_script.find path Michelson_contracts.tezlink_protocol
    |> Michelson_script.path

  module Store_input = struct
    let prg = contract_prg ["opcodes"; "store_input"]

    let originate ?(gas_limit = michelson_hard_gas_limit_per_operation)
        ~initial_storage =
      let init = sf {|"%s"|} initial_storage in
      Client.originate_contract ~init ~prg ~gas_limit

    let call ?(gas_limit = michelson_hard_gas_limit_per_operation) ~new_storage
        store_input_address =
      let arg = sf {|"%s"|} new_storage in
      Client.transfer ~arg ~receiver:store_input_address ~gas_limit

    let read_storage ~tez_client kt1 =
      let open JSON in
      let* json =
        Client.RPC.call tez_client
        @@ RPC.get_chain_block_context_contract_storage ~id:kt1 ()
      in
      return (json |-> "string" |> as_string)
  end

  module Double_send = struct
    let prg = contract_prg ["mini_scenarios"; "double_send"]

    let originate ?(gas_limit = michelson_hard_gas_limit_per_operation)
        ~initial_storage =
      let init = sf {|"%s"|} initial_storage in
      Client.originate_contract ~init ~prg ~gas_limit

    let call ?(gas_limit = michelson_hard_gas_limit_per_operation) ~dest1 ~dest2
        ~new_storage address =
      let arg = sf {|Pair (Pair "%s" "%s") "%s"|} dest1 dest2 new_storage in
      Client.transfer ~arg ~receiver:address ~gas_limit
  end

  module Failing_cross_runtime_http_call_tez_callback = struct
    let prg =
      contract_prg
        ["mini_scenarios"; "failing_cross_runtime_http_call_tez_callback"]

    let originate ?(gas_limit = michelson_hard_gas_limit_per_operation)
        ~destination =
      let init = sf {|Pair 0 (Pair "%s" None)|} destination in
      Client.originate_contract ~init ~prg ~gas_limit

    let run ?(gas_limit = michelson_hard_gas_limit_per_operation) ?storage_limit
        ~body_hex address =
      Client.transfer
        ?storage_limit
        ~arg:body_hex
        ~entrypoint:"run"
        ~receiver:address
        ~gas_limit
  end

  module Update_big_map = struct
    let prg = contract_prg ["opcodes"; "update_big_map"]

    let originate ?(gas_limit = michelson_hard_gas_limit_per_operation)
        ~initial_storage =
      Client.originate_contract ~init:initial_storage ~prg ~gas_limit

    let call ?(gas_limit = michelson_hard_gas_limit_per_operation) ~updates
        address =
      Client.transfer ~arg:updates ~receiver:address ~gas_limit
  end

  module Big_map_write = struct
    let prg = contract_prg ["mini_scenarios"; "big_map_write"]

    let originate ?(gas_limit = michelson_hard_gas_limit_per_operation) () =
      Client.originate_contract ~init:"Unit" ~prg ~gas_limit

    let call ?(gas_limit = michelson_hard_gas_limit_per_operation) ~param
        address =
      Client.transfer ~arg:param ~receiver:address ~gas_limit
  end

  module Big_map_store = struct
    let prg = contract_prg ["mini_scenarios"; "big_map_store"]

    let originate ?(gas_limit = michelson_hard_gas_limit_per_operation)
        ?(init = "{}") () =
      Client.originate_contract ~init ~prg ~gas_limit

    let call ?(gas_limit = michelson_hard_gas_limit_per_operation) address =
      Client.transfer ~arg:"Unit" ~receiver:address ~gas_limit
  end

  module Receiver_store = struct
    let prg = contract_prg ["big_maps"; "receiver_store"]

    let originate ?(gas_limit = michelson_hard_gas_limit_per_operation) () =
      Client.originate_contract ~init:"{}" ~prg ~gas_limit

    let call ?(gas_limit = michelson_hard_gas_limit_per_operation) ~param
        address =
      Client.transfer ~arg:param ~receiver:address ~gas_limit
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

  (** Look for any storage-fees burn in a list of balance updates. *)
  let any_storage_fees_burn balance_updates =
    List.exists
      (function
        | Burned_change {category = "storage fees"; _} -> true | _ -> false)
      balance_updates
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

  let internal_balance_updates content =
    get_internal_operation_results content
    |> List.concat_map (fun iop -> balance_updates_of_result (iop |-> "result"))

  let allocated_destination_contract_of_result result =
    result |-> "allocated_destination_contract" |> as_opt |> Option.map as_bool
    |> Option.value ~default:false

  let error_messages_of_result result =
    result |-> "errors" |> as_opt |> Option.map as_list
    |> Option.value ~default:[]
    |> List.map (fun err -> err |-> "error_message" |> as_string)
end

(** [revm/src/precompiles/constants.rs:RUNTIME_GATEWAY_PRECOMPILE_ADDRESS] *)
let evm_gateway_address = "0xff00000000000000000000000000000000000007"

(** [tezos_execution/src/enshrined_contracts.rs:GATEWAY_ADDRESS] *)
let michelson_gateway_address = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw"

(** Cost of one byte burned to the storage-fees sink (`COST_PER_BYTES`
    in the kernel). *)
let cost_per_byte = 1

(** Slot size of an originated contract or a freshly allocated
    implicit account (`ORIGINATION_SIZE` in the kernel). *)
let origination_size = 257

(** Mutez burned to allocate one such slot. *)
let allocation_slot_burn = cost_per_byte * origination_size

(** Content size of the alias forwarder. *)
let alias_forwarder_content_size = 47

(** Cost dedicated to the alias forwarder origination. *)
let alias_materialization_cost =
  (origination_size + alias_forwarder_content_size) * cost_per_byte

(** [kernel/src/fees.rs:DEFAULT_MICHELSON_TO_EVM_GAS_MULTIPLIER] *)
let michelson_to_evm_gas_multiplier = 45

(** At the default [base_fee_per_gas = 10^9 wei] (kernel default
    [MINIMUM_BASE_FEE_PER_GAS]), 1 mutez translates to 1000 EVM
    gas via [mutez_to_evm_gas] (= 10^12 wei/mutez ÷ 10^9 wei/gas). *)
let mutez_to_evm_gas_factor = 1000

let get_first_manager_operations_content ~tez_endpoint =
  let* operations =
    RPC_core.call tez_endpoint @@ RPC.get_chain_block_operations ()
  in
  let content =
    let open Tezos_JSON in
    operations |> get_manager_operations |> get_first_content
  in
  return content

let encode_michelson_string ~tez_client s =
  let* hex =
    Client.convert_data
      ~data:(sf {|"%s"|} s)
      ~src_format:`Michelson
      ~dst_format:`Binary
      tez_client
  in
  return (String.trim hex)

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
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

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
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

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
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

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
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

  let* () =
    Client.transfer
      ~amount:(Tez.of_int 1)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      ~gas_limit:michelson_hard_gas_limit_per_operation
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
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

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
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

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

  let initial_balance = Tez.of_mutez_int 800 in
  let reveal_fee = Tez.of_mutez_int 200 in
  let* () =
    Client.transfer
      ~amount:Tez.(initial_balance + reveal_fee)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap5.alias
      ~burn_cap:Tez.one
      ~gas_limit:michelson_hard_gas_limit_per_operation
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in
  (* Reveal manually to prevent the [reveal_fee] from shifting the balance
     delta of the subsequent call.  Use the low-level [Operation.Manager]
     API rather than [Client.reveal]: octez-client's [reveal] subcommand
     does not expose [--gas-limit] and its simulation defaults to 3M gas,
     which the Tezlink node rejects since the per-op cap dropped to 660k. *)
  let* reveal_op =
    Operation.Manager.(
      operation
        [
          make
            ~source:Constant.bootstrap5
            ~fee:(Tez.to_mutez reveal_fee)
            (reveal Constant.bootstrap5 ());
        ])
      tez_client
  in
  let* _hash = Operation.inject ~dont_wait:true reveal_op tez_client in
  let*@ _ = Rpc.produce_block evm_node in

  let new_storage = "bigger storage" in
  (* gas_limit 10000 requires an execution gas fee of
     10000 * 45 * 1Gwei / 10^12 = 450 mutez, so the call fee must cover it. *)
  let call_fee = Tez.of_mutez_int 500 in
  let* () =
    Contract.Double_send.call
      ~amount:Tez.zero
      ~fee:call_fee
      ~gas_limit:10000
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

  let top_level_errors = Tezos_JSON.error_messages_of_result operation_result in
  Check.(
    (List.length top_level_errors = 1)
      int
      ~error_msg:"Expected exactly one error on top-level, got %L") ;
  let error_msg = List.hd top_level_errors in
  Check.is_true
    (String.starts_with ~prefix:"CannotPayStorageFee" error_msg)
    ~error_msg:
      (sf
         "Expected top-level error to start with CannotPayStorageFee, got: %s"
         error_msg) ;

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

(** Scenario: same setup as
    {!test_two_internal_transfers_burn_each}, but the call passes a
    `storage_limit` that fits the top-level burn and the first slot
    allocation, yet trips [OperationQuotaExceeded] on the second
    internal. Receipt shape mirrors
    {!test_partial_internal_burn_failure_backtracks_all} (source
    loses only the call fee). *)
let test_partial_internal_storage_limit_overshoot_backtracks_all () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "partial internal storage_limit overshoot: top-level keeps burn pair, \
       internals lose theirs"
    ~tags:["transfer"; "internal"; "backtracked"; "quota"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

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

  let* initial_balance =
    Client.get_balance_for
      ~account:Constant.bootstrap1.public_key_hash
      tez_client
  in

  let new_storage = "bigger storage" in
  (* This storage-limit will trigger the fail on the second internal operation because it need `origination_size` more *)
  let storage_limit = String.length new_storage + origination_size in
  (* gas_limit 10000 requires an execution gas fee of
     10000 * 45 * 1Gwei / 10^12 = 450 mutez, so the call fee must cover it. *)
  let call_fee = Tez.of_mutez_int 500 in
  let* () =
    Contract.Double_send.call
      ~amount:Tez.zero
      ~fee:call_fee
      ~gas_limit:10000
      ~storage_limit
      ~giver:Constant.bootstrap1.alias
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
      ~account:Constant.bootstrap1.public_key_hash
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

  let top_level_errors = Tezos_JSON.error_messages_of_result operation_result in
  Check.(
    (List.length top_level_errors = 1)
      int
      ~error_msg:"Expected exactly one error on top-level, got %L") ;
  let error_msg = List.hd top_level_errors in
  Check.is_true
    (String.starts_with ~prefix:"OperationQuotaExceeded" error_msg)
    ~error_msg:
      (sf
         "Expected top-level error to start with OperationQuotaExceeded, got: \
          %s"
         error_msg) ;

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

(** Scenario: a single call applies three [UPDATE] branches (insert,
    overwrite, delete) to the storage big-map. The receipt's
    [paid_storage_size_diff] reflects only the lazy-storage delta of
    this call:

    - insert  "a" → Some "new":      +(65 + encoded("new"))     = +73
    - delete  "d" → None:            −(65 + encoded("x"))       = −71
    - overw.  "k" "old" → "longer":  encoded("longer") − encoded("old") = +3

    Net = +5. Values are Micheline-encoded strings (tag + 4-byte
    length + bytes), so encoded("s") = 5 + len(s). *)
let test_big_map_update_insert_overwrite_delete () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"big_map UPDATE: insert + overwrite + delete in one call"
    ~tags:["transfer"; "big_map"; "receipt"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

  let initial_storage = {|Pair { Elt "d" "x" ; Elt "k" "old" } Unit|} in
  let* kt1 =
    Contract.Update_big_map.originate
      ~alias:"update_big_map"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ~initial_storage
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let updates =
    {|{ Elt "a" (Some "new") ; Elt "d" None ; Elt "k" (Some "longer") }|}
  in
  let* () =
    Contract.Update_big_map.call
      ~amount:Tez.zero
      ~fee:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~burn_cap:Tez.one
      ~updates
      kt1
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* content = get_first_manager_operations_content ~tez_endpoint in
  let paid_storage_size_diff =
    Tezos_JSON.get_operation_content_paid_storage_size_diff content
  in
  let expected_delta = 73 + 3 - 71 in
  Check.(
    (paid_storage_size_diff = expected_delta)
      int
      ~error_msg:
        "Expected paid_storage_size_diff %R (insert 73 + overwrite 3 − delete \
         71), got %L") ;

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
         "Expected the big-map delta to burn %d mutez to the storage-fees sink \
          on bootstrap1"
         variable_burn) ;
  unit

(** Scenario: a contract takes a big-map as parameter, [UPDATE]s it,
    then [DROP]s the result. The parameter big-map is allocated as a
    temporary inside the contract; the [UPDATE] also runs on a
    temporary id. Neither operation should contribute to the
    accumulator, so [paid_storage_size_diff] is zero and [storage_size]
    is unchanged from origination. *)
let test_temp_big_map_dropped_no_charge () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"big_map temp: parameter big-map dropped does not charge"
    ~tags:["transfer"; "big_map"; "receipt"; "temporary"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

  let* kt1 =
    Contract.Big_map_write.originate
      ~alias:"big_map_write"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ()
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* origination_storage_size =
    let* content = get_first_manager_operations_content ~tez_endpoint in
    return (Tezos_JSON.get_operation_content_storage_size content)
  in

  let* () =
    Contract.Big_map_write.call
      ~amount:Tez.zero
      ~fee:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~burn_cap:Tez.one
      ~param:"{ Elt 1 1 }"
      kt1
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* content = get_first_manager_operations_content ~tez_endpoint in
  let paid_storage_size_diff =
    Tezos_JSON.get_operation_content_paid_storage_size_diff content
  in
  let storage_size = Tezos_JSON.get_operation_content_storage_size content in
  Check.(
    (paid_storage_size_diff = 0)
      int
      ~error_msg:"Expected paid_storage_size_diff = 0 on temp big-map, got %L") ;
  Check.(
    (storage_size = origination_storage_size)
      int
      ~error_msg:"Expected storage_size %R (unchanged by temp big-map), got %L") ;
  unit

(** Scenario: a contract drops its storage big-map and returns a
    freshly built [EMPTY_BIG_MAP] as the new storage. MIR emits a
    [big_map_remove] of the old (perm) id and a
    [big_map_copy(temp, perm)] of the new — when the old is empty,
    these two cancel: −(33 + 0) + (33 + 0) = 0.

    Receipt: [paid_storage_size_diff = 0] and [storage_size] unchanged
    from origination. *)
let test_big_map_replacement_symmetric () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"big_map replacement: remove + temp→perm promotion cancel on empty"
    ~tags:["transfer"; "big_map"; "receipt"; "promotion"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

  let* kt1 =
    Contract.Big_map_store.originate
      ~alias:"big_map_store"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ()
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* origination_storage_size =
    let* content = get_first_manager_operations_content ~tez_endpoint in
    return (Tezos_JSON.get_operation_content_storage_size content)
  in

  let* () =
    Contract.Big_map_store.call
      ~amount:Tez.zero
      ~fee:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~burn_cap:Tez.one
      kt1
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* content = get_first_manager_operations_content ~tez_endpoint in
  let paid_storage_size_diff =
    Tezos_JSON.get_operation_content_paid_storage_size_diff content
  in
  let storage_size = Tezos_JSON.get_operation_content_storage_size content in
  Check.(
    (paid_storage_size_diff = 0)
      int
      ~error_msg:
        "Expected paid_storage_size_diff = 0 on empty-empty replacement, got %L") ;
  Check.(
    (storage_size = origination_storage_size)
      int
      ~error_msg:
        "Expected storage_size %R (unchanged by empty-empty replacement), got \
         %L") ;
  unit

(** Scenario: a contract stores the non-empty big-map it receives as
    parameter, dropping the empty big-map it was originated with. The
    parameter map is materialised as a temporary; storing it promotes it
    to permanent via [big_map_copy(temp, perm)] — contributing
    [+(33 + total_bytes)] — while the old empty map is dropped via
    [big_map_remove] [−(33 + 0)]. The two 33-byte slot forfaits cancel,
    so [paid_storage_size_diff] equals the aggregated entry size of the
    stored map:

    - "a" → 0x01:   65 + encoded(0x01)   = 65 + 6 = 71
    - "b" → 0x0203: 65 + encoded(0x0203) = 65 + 7 = 72

    Net = +143. Bytes are Micheline-encoded (tag + 4-byte length + bytes),
    so encoded(b) = 5 + len(b). *)
let test_big_map_nonempty_promotion_burns_content () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "big_map promotion: storing a non-empty parameter map bills its content"
    ~tags:["transfer"; "big_map"; "receipt"; "promotion"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

  let* kt1 =
    Contract.Receiver_store.originate
      ~alias:"receiver_store"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ()
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let param = {|{ Elt "a" 0x01 ; Elt "b" 0x0203 }|} in
  let* () =
    Contract.Receiver_store.call
      ~amount:Tez.zero
      ~fee:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~burn_cap:Tez.one
      ~param
      kt1
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* content = get_first_manager_operations_content ~tez_endpoint in
  let paid_storage_size_diff =
    Tezos_JSON.get_operation_content_paid_storage_size_diff content
  in
  let expected_delta = 65 + 6 + (65 + 7) in
  Check.(
    (paid_storage_size_diff = expected_delta)
      int
      ~error_msg:
        "Expected paid_storage_size_diff %R (promotion of a 2-entry map; the \
         33-byte slot forfaits cancel), got %L") ;

  (* The promoted content is burned to the storage-fees sink, exactly as
     L1 charges lazy storage. *)
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
         "Expected the promoted big-map content to burn %d mutez to the \
          storage-fees sink on bootstrap1"
         variable_burn) ;
  unit

(** Originating with a non-empty initial big-map bills the inherited
    content; replacing that big-map with a fresh [EMPTY_BIG_MAP] in a
    follow-up call refunds the content symmetrically. *)
let test_big_map_replacement_nonempty_burns_content () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "big_map replacement: non-empty initial big-map billed at origination, \
       content burned on replacement"
    ~tags:["transfer"; "big_map"; "receipt"; "origination"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

  let* _kt1_empty =
    Contract.Big_map_store.originate
      ~alias:"big_map_store_empty"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ()
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in
  let* empty_origination_storage_size =
    let* content = get_first_manager_operations_content ~tez_endpoint in
    return (Tezos_JSON.get_operation_content_storage_size content)
  in

  let* kt1 =
    Contract.Big_map_store.originate
      ~init:"{ Elt 1 1 }"
      ~alias:"big_map_store_nonempty"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ()
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in
  let* nonempty_origination_storage_size =
    let* content = get_first_manager_operations_content ~tez_endpoint in
    return (Tezos_JSON.get_operation_content_storage_size content)
  in

  (* The 33-byte big-map slot forfait is present in both originations
     and cancels in the delta; only the per-entry forfait (65) and the
     encoded size of [nat 1] (2 bytes) remain. *)
  let lazy_entry_delta = 65 + 2 in
  Check.(
    (nonempty_origination_storage_size - empty_origination_storage_size
    = lazy_entry_delta)
      int
      ~error_msg:"Expected nonempty origination storage_size delta = %R, got %L") ;

  let net_delta = -(33 + (65 + 2)) + 33 in
  let* () =
    Contract.Big_map_store.call
      ~amount:Tez.zero
      ~fee:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~burn_cap:Tez.one
      kt1
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  let* content = get_first_manager_operations_content ~tez_endpoint in
  let paid_storage_size_diff =
    Tezos_JSON.get_operation_content_paid_storage_size_diff content
  in
  let storage_size = Tezos_JSON.get_operation_content_storage_size content in
  Check.(
    (paid_storage_size_diff = 0)
      int
      ~error_msg:"Expected paid_storage_size_diff = 0, got %L") ;
  Check.(
    (storage_size = nonempty_origination_storage_size + net_delta)
      int
      ~error_msg:"Expected storage_size = %R, got %L") ;
  unit

(* =============================================================== *)
(* CRAC scenarios: storage-payment delegation at the runtime       *)
(* boundary.                                                       *)
(* =============================================================== *)

(** [send_crac_to_michelson ~sequencer ~sender ~destination ~nonce ()] sends
    a CRAC from [sender] to the Michelson contract [destination]
    through the gateway's [callMichelson] entrypoint, produces a
    block, and checks the transaction succeeded. *)
let send_crac_to_michelson ~sequencer ~sender ~nonce ?(amount = Wei.zero)
    ?(expected_status = true) ?(gas = 3_000_000) ~destination ?(entrypoint = "")
    ?(parameters = "0x") () =
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:sender.Eth_account.private_key
      ~chain_id:1337
      ~nonce
      ~gas
      ~gas_price:1_000_000_000
      ~value:amount
      ~address:evm_gateway_address
      ~signature:"callMichelson(string,string,bytes)"
      ~arguments:[destination; entrypoint; parameters]
      ()
  in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@ _block_number = Rpc.produce_block sequencer in
  let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  match receipt with
  | Some r ->
      Check.(
        (r.status = expected_status)
          bool
          ~error_msg:"Expected EVM receipt status %R but got %L") ;
      return r
  | None -> Test.fail "No receipt for EVM transaction to %s" evm_gateway_address

(** Sends a CRAC from EVM to a Michelson contract whose storage is
    not modified, and checks that the EVM caller pays nothing extra. *)
let test_evm_to_michelson_no_storage_growth_no_payment () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "EVM->Michelson CRAC: no Michelson allocation triggers no storage payment"
    ~tags:["cross_runtime"; "delegated_storage"; "no_growth"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in
  let initial_storage = "hello" in
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

  let sender = Eth_account.bootstrap_accounts.(0) in
  let* initial_storage_hex =
    encode_michelson_string ~tez_client initial_storage
  in
  (* Pre-originate the alias. *)
  let* _ =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender
      ~nonce:0
      ~destination:kt1
      ~parameters:initial_storage_hex
      ()
  in

  (* Witness: call with no storage growth. *)
  let*@ balance_before_w =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in
  let* _ =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender
      ~nonce:1
      ~destination:kt1
      ~parameters:initial_storage_hex
      ()
  in
  let*@ balance_after_w =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in

  (* Measure: call with no storage growth, identical to witness. *)
  let balance_before_m = balance_after_w in
  let* _ =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender
      ~nonce:2
      ~destination:kt1
      ~parameters:initial_storage_hex
      ()
  in
  let* content = get_first_manager_operations_content ~tez_endpoint in
  let*@ balance_after_m =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in

  (* Check the storage stayed at initial *)
  let* current_storage = Contract.Store_input.read_storage ~tez_client kt1 in
  Check.(
    (current_storage = initial_storage)
      string
      ~error_msg:"Expected Michelson storage to stay at %R, got %L") ;

  (* Check sender isn't paying more than the witness *)
  let balance_delta_w = Wei.(balance_before_w - balance_after_w) in
  let balance_delta_m = Wei.(balance_before_m - balance_after_m) in
  Check.(
    (balance_delta_m = balance_delta_w)
      Wei.typ
      ~error_msg:
        "Expected sender to pay no more than the witness on no-growth, got %L \
         vs %R") ;

  (* Check the Michelson receipt carries no storage-fees burn. *)
  let top_bus =
    Tezos_JSON.balance_updates_of_result
      (Tezos_JSON.get_operation_result content)
  in
  let internal_bus = Tezos_JSON.internal_balance_updates content in
  Check.is_false
    (BalanceUpdate.any_storage_fees_burn (top_bus @ internal_bus))
    ~error_msg:"Expected no storage-fees burn on the Michelson receipt" ;

  unit

(** Sends a CRAC from EVM to a Michelson contract that grows its
    storage, and checks that the storage cost is paid in EVM gas by
    the EVM caller. *)
let test_evm_to_michelson_storage_growth_evm_pays () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"EVM->Michelson CRAC: storage growth is paid by the EVM caller"
    ~tags:["cross_runtime"; "delegated_storage"; "growth"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in
  let initial_storage = "hello" in
  let new_storage = "hello world, this storage is now substantially longer" in
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

  let sender = Eth_account.bootstrap_accounts.(0) in
  let* initial_storage_hex =
    encode_michelson_string ~tez_client initial_storage
  in
  (* Pre-originate the alias. *)
  let* _ =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender
      ~nonce:0
      ~destination:kt1
      ~parameters:initial_storage_hex
      ()
  in

  (* Witness: call with no storage growth. *)
  let*@ balance_before_w =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in
  let* witness_receipt =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender
      ~nonce:1
      ~destination:kt1
      ~parameters:initial_storage_hex
      ()
  in
  let*@ balance_after_w =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in

  (* Measure: call with storage growth. *)
  let balance_before_m = balance_after_w in
  let* new_storage_hex = encode_michelson_string ~tez_client new_storage in
  let* measure_receipt =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender
      ~nonce:2
      ~destination:kt1
      ~parameters:new_storage_hex
      ()
  in
  let* content = get_first_manager_operations_content ~tez_endpoint in
  let*@ balance_after_m =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in

  (* Check the update of the storage *)
  let* current_storage = Contract.Store_input.read_storage ~tez_client kt1 in
  Check.(
    (current_storage = new_storage)
      string
      ~error_msg:"Expected Michelson storage to be %R, got %L") ;

  (* Check gas_used include the storage-fees *)
  let growth_bytes =
    String.length new_storage - String.length initial_storage
  in
  let gas_used_w = Int64.to_int witness_receipt.gasUsed in
  let gas_used_m = Int64.to_int measure_receipt.gasUsed in
  let diff_gas_used = gas_used_m - gas_used_w in
  Check.(
    (diff_gas_used > growth_bytes * cost_per_byte * mutez_to_evm_gas_factor)
      int
      ~error_msg:"Expected EVM gasUsed delta > %R, got %L") ;

  (* Check sender isn't paying more than the that *)
  let balance_delta_w = Wei.(balance_before_w - balance_after_w) in
  let balance_delta_m = Wei.(balance_before_m - balance_after_m) in
  let diff_balance_delta = Wei.(balance_delta_m - balance_delta_w) in
  let gas_price = Wei.to_wei_z (Z.of_int64 measure_receipt.effectiveGasPrice) in
  Check.(
    (diff_balance_delta = Wei.(gas_price * Z.of_int diff_gas_used))
      Wei.typ
      ~error_msg:
        "Expected sender to pays nothing more than the storage cost %R, got %L") ;

  (* Check the Michelson receipt carries no storage-fees burn. *)
  let top_bus =
    Tezos_JSON.balance_updates_of_result
      (Tezos_JSON.get_operation_result content)
  in
  let internal_bus = Tezos_JSON.internal_balance_updates content in
  Check.is_false
    (BalanceUpdate.any_storage_fees_burn (top_bus @ internal_bus))
    ~error_msg:"Expected no storage-fees burn on the Michelson receipt" ;

  unit

(** Sends a CRAC from EVM to a Michelson contract that would grow
    its storage, with a gas budget too low to cover the storage
    cost, and checks that the call OOGs, the Michelson storage is
    rolled back, and the EVM caller pays nothing extra. *)
let test_evm_to_michelson_storage_growth_evm_oog_at_g2 () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "EVM->Michelson CRAC: insufficient gas for g2 reverts the storage \
       allocation"
    ~tags:["cross_runtime"; "delegated_storage"; "oog"; "g2"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in
  let initial_storage = "hello" in
  let new_storage = "hello world, this storage is now substantially longer" in
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

  let sender = Eth_account.bootstrap_accounts.(0) in
  let* initial_storage_hex =
    encode_michelson_string ~tez_client initial_storage
  in
  (* Pre-originate the alias. *)
  let* _ =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender
      ~nonce:0
      ~destination:kt1
      ~parameters:initial_storage_hex
      ()
  in

  (* Witness: call with no storage growth. *)
  let*@ balance_before_w =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in
  let* witness_receipt =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender
      ~nonce:1
      ~destination:kt1
      ~parameters:initial_storage_hex
      ()
  in
  let*@ balance_after_w =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in
  let gas_used_w = Int64.to_int witness_receipt.gasUsed in

  (* Measure: call with storage growth, capped at the witness gas. *)
  let balance_before_m = balance_after_w in
  let* new_storage_hex = encode_michelson_string ~tez_client new_storage in
  let* _ =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender
      ~nonce:2
      ~destination:kt1
      ~parameters:new_storage_hex
      ~gas:gas_used_w
      ~expected_status:false
      ()
  in
  let* content = get_first_manager_operations_content ~tez_endpoint in
  let*@ balance_after_m =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in

  (* Check the storage was rolled back *)
  let* current_storage = Contract.Store_input.read_storage ~tez_client kt1 in
  Check.(
    (current_storage = initial_storage)
      string
      ~error_msg:
        "Expected Michelson storage to be rolled back to %R after OOG, got %L") ;

  (* Check sender isn't paying more than the witness *)
  let balance_delta_w = Wei.(balance_before_w - balance_after_w) in
  let balance_delta_m = Wei.(balance_before_m - balance_after_m) in
  Check.(
    (balance_delta_m <= balance_delta_w)
      Wei.typ
      ~error_msg:
        "Expected sender to pay no more than the witness on OOG, got %L vs %R") ;

  (* Check the Michelson receipt carries no storage-fees burn. *)
  let top_bus =
    Tezos_JSON.balance_updates_of_result
      (Tezos_JSON.get_operation_result content)
  in
  let internal_bus = Tezos_JSON.internal_balance_updates content in
  Check.is_false
    (BalanceUpdate.any_storage_fees_burn (top_bus @ internal_bus))
    ~error_msg:"Expected no storage-fees burn on the Michelson receipt" ;

  unit

(** Sends a CRAC from EVM to a Michelson contract that grows its
    storage, then the enclosing EVM frame reverts unconditionally.
    Checks that the storage growth is paid in EVM gas (the revert
    does not refund it) and that the Michelson allocation is rolled
    back. *)
let test_evm_to_michelson_storage_growth_evm_reverts_after_g2 () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "EVM->Michelson CRAC: enclosing EVM revert rolls back Michelson \
       allocation"
    ~tags:["cross_runtime"; "delegated_storage"; "revert"; "asymmetry"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in
  let initial_storage = "hello" in
  let new_storage = "hello world, this storage is now substantially longer" in
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

  let sender = Eth_account.bootstrap_accounts.(0) in
  (* Pre-originate the alias. *)
  let* initial_storage_hex =
    encode_michelson_string ~tez_client initial_storage
  in
  let* _ =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender
      ~nonce:0
      ~destination:kt1
      ~parameters:initial_storage_hex
      ()
  in

  (* Deploy CracStoreThenRevert: its callAndRevert calls the gateway then
     reverts the enclosing EVM frame. *)
  let* contract =
    Solidity_contracts.crac_store_then_revert Evm_version.Cancun
  in
  let* contract_address =
    let bytecode = Tezt.Base.read_file contract.bin in
    let* raw_tx =
      Cast.craft_deploy_tx
        ~source_private_key:sender.Eth_account.private_key
        ~chain_id:1337
        ~nonce:1
        ~gas:2_000_000
        ~gas_price:1_000_000_000
        ~data:("0x" ^ bytecode)
        ()
    in
    let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx evm_node in
    let*@ _ = Rpc.produce_block evm_node in
    let*@ receipt = Rpc.get_transaction_receipt ~tx_hash evm_node in
    match receipt with
    | Some {contractAddress = Some addr; status = true; _} -> return addr
    | _ -> Test.fail "Failed to deploy CracStoreThenRevert"
  in

  let call_and_revert ~nonce ~storage_hex =
    let* raw_tx =
      Cast.craft_tx
        ~source_private_key:sender.Eth_account.private_key
        ~chain_id:1337
        ~nonce
        ~gas:3_000_000
        ~gas_price:1_000_000_000
        ~value:Wei.zero
        ~address:contract_address
        ~signature:"callAndRevert(string,bytes)"
        ~arguments:[kt1; storage_hex]
        ()
    in
    let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx evm_node in
    let*@ _ = Rpc.produce_block evm_node in
    let*@ receipt = Rpc.get_transaction_receipt ~tx_hash evm_node in
    match receipt with
    | Some ({status = false; _} as r) -> return r
    | Some {status = true; _} ->
        Test.fail "Expected callAndRevert to revert, got status=true"
    | None -> Test.fail "No receipt for callAndRevert tx"
  in

  (* Witness: call with no storage growth, EVM frame reverts. *)
  let*@ balance_before_w =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in
  let* witness_receipt =
    call_and_revert ~nonce:2 ~storage_hex:initial_storage_hex
  in
  let*@ balance_after_w =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in

  (* Measure: call with storage growth, EVM frame reverts. *)
  let balance_before_m = balance_after_w in
  let* new_storage_hex = encode_michelson_string ~tez_client new_storage in
  let* measure_receipt =
    call_and_revert ~nonce:3 ~storage_hex:new_storage_hex
  in
  let* content = get_first_manager_operations_content ~tez_endpoint in
  let*@ balance_after_m =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in

  (* Check the storage was rolled back *)
  let* current_storage = Contract.Store_input.read_storage ~tez_client kt1 in
  Check.(
    (current_storage = initial_storage)
      string
      ~error_msg:
        "Expected Michelson storage to be rolled back to %R after enclosing \
         EVM revert, got %L") ;

  (* Check gas_used include the storage-fees *)
  let growth_bytes =
    String.length new_storage - String.length initial_storage
  in
  let gas_used_w = Int64.to_int witness_receipt.gasUsed in
  let gas_used_m = Int64.to_int measure_receipt.gasUsed in
  let diff_gas_used = gas_used_m - gas_used_w in
  Check.(
    (diff_gas_used > growth_bytes * cost_per_byte * mutez_to_evm_gas_factor)
      int
      ~error_msg:"Expected EVM gasUsed delta > %R, got %L") ;

  (* Check sender isn't paying more than the witness *)
  let balance_delta_w = Wei.(balance_before_w - balance_after_w) in
  let balance_delta_m = Wei.(balance_before_m - balance_after_m) in
  let diff_balance_delta = Wei.(balance_delta_m - balance_delta_w) in
  let gas_price = Wei.to_wei_z (Z.of_int64 measure_receipt.effectiveGasPrice) in
  Check.(
    (diff_balance_delta = Wei.(gas_price * Z.of_int diff_gas_used))
      Wei.typ
      ~error_msg:
        "Expected sender to pays nothing more than the storage cost %R, got %L") ;

  (* Check the Michelson receipt carries no storage-fees burn. *)
  let top_bus =
    Tezos_JSON.balance_updates_of_result
      (Tezos_JSON.get_operation_result content)
  in
  let internal_bus = Tezos_JSON.internal_balance_updates content in
  Check.is_false
    (BalanceUpdate.any_storage_fees_burn (top_bus @ internal_bus))
    ~error_msg:"Expected no storage-fees burn on the Michelson receipt" ;

  unit

(** Sends the first CRAC from a fresh EVM EOA to a Michelson
    contract, which triggers the materialization of the EOA's alias
    forwarder on the Michelson side, and checks that the
    materialization cost is paid in EVM gas by the EVM caller. *)
let test_evm_to_michelson_alias_origination_evm_pays () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "EVM->Michelson CRAC: alias materialization is paid by the EVM caller"
    ~tags:["cross_runtime"; "delegated_storage"; "alias"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in
  let initial_storage = "hello" in
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

  let sender = Eth_account.bootstrap_accounts.(0) in
  let* initial_storage_hex =
    encode_michelson_string ~tez_client initial_storage
  in

  (* Measure: first call from this EOA, triggers alias materialization. *)
  let*@ balance_before_m =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in
  let* measure_receipt =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender
      ~nonce:0
      ~destination:kt1
      ~parameters:initial_storage_hex
      ()
  in
  let* content = get_first_manager_operations_content ~tez_endpoint in
  let*@ balance_after_m =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in

  (* Witness: second call from the same EOA, alias already classified. *)
  let balance_before_w = balance_after_m in
  let* witness_receipt =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender
      ~nonce:1
      ~destination:kt1
      ~parameters:initial_storage_hex
      ()
  in
  let*@ balance_after_w =
    Rpc.get_balance ~address:sender.Eth_account.address evm_node
  in

  (* Check the storage stayed at initial *)
  let* current_storage = Contract.Store_input.read_storage ~tez_client kt1 in
  Check.(
    (current_storage = initial_storage)
      string
      ~error_msg:"Expected Michelson storage to stay at %R, got %L") ;

  (* Check gas_used include the alias materialization cost *)
  let gas_used_w = Int64.to_int witness_receipt.gasUsed in
  let gas_used_m = Int64.to_int measure_receipt.gasUsed in
  let diff_gas_used = gas_used_m - gas_used_w in
  Check.(
    (diff_gas_used > alias_materialization_cost * mutez_to_evm_gas_factor)
      int
      ~error_msg:"Expected EVM gasUsed delta > %R, got %L") ;

  (* Check sender isn't paying more than the witness *)
  let balance_delta_m = Wei.(balance_before_m - balance_after_m) in
  let balance_delta_w = Wei.(balance_before_w - balance_after_w) in
  let diff_balance_delta = Wei.(balance_delta_m - balance_delta_w) in
  let gas_price = Wei.to_wei_z (Z.of_int64 measure_receipt.effectiveGasPrice) in
  Check.(
    (diff_balance_delta = Wei.(gas_price * Z.of_int diff_gas_used))
      Wei.typ
      ~error_msg:
        "Expected sender to pays nothing more than the storage cost %R, got %L") ;

  (* Check the Michelson receipt carries no storage-fees burn. *)
  let top_bus =
    Tezos_JSON.balance_updates_of_result
      (Tezos_JSON.get_operation_result content)
  in
  let internal_bus = Tezos_JSON.internal_balance_updates content in
  Check.is_false
    (BalanceUpdate.any_storage_fees_burn (top_bus @ internal_bus))
    ~error_msg:"Expected no storage-fees burn on the Michelson receipt" ;

  unit

(** Sends the first CRAC from a fresh EVM EOA to a Michelson
    contract, with a gas budget too low to cover the alias
    materialization, and checks that the call OOGs, the alias
    materialization is rolled back, and the EVM caller pays nothing
    extra. *)
let test_evm_to_michelson_alias_origination_evm_oog () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "EVM->Michelson CRAC: insufficient gas for g2_alias rolls back the alias \
       materialization"
    ~tags:["cross_runtime"; "delegated_storage"; "oog"; "alias"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun evm_node ->
  let* tez_client = tezlink_client evm_node in
  let initial_storage = "hello" in
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

  let sender_1 = Eth_account.bootstrap_accounts.(0) in
  let sender_2 = Eth_account.bootstrap_accounts.(1) in
  let* initial_storage_hex =
    encode_michelson_string ~tez_client initial_storage
  in
  (* Pre-originate the alias for sender_1. *)
  let* _ =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender:sender_1
      ~nonce:0
      ~destination:kt1
      ~parameters:initial_storage_hex
      ()
  in

  (* Witness: call from sender_1, alias already classified. *)
  let*@ balance_before_w =
    Rpc.get_balance ~address:sender_1.Eth_account.address evm_node
  in
  let* witness_receipt =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender:sender_1
      ~nonce:1
      ~destination:kt1
      ~parameters:initial_storage_hex
      ()
  in
  let*@ balance_after_w =
    Rpc.get_balance ~address:sender_1.Eth_account.address evm_node
  in
  let gas_used_w = Int64.to_int witness_receipt.gasUsed in

  (* Measure: call from sender_2 (fresh, alias not classified), capped at the witness gas. *)
  let*@ balance_before_m =
    Rpc.get_balance ~address:sender_2.Eth_account.address evm_node
  in
  let* _ =
    send_crac_to_michelson
      ~sequencer:evm_node
      ~sender:sender_2
      ~nonce:0
      ~destination:kt1
      ~parameters:initial_storage_hex
      ~gas:gas_used_w
      ~expected_status:false
      ()
  in
  let*@ balance_after_m =
    Rpc.get_balance ~address:sender_2.Eth_account.address evm_node
  in

  (* Check sender isn't paying more than the witness *)
  let balance_delta_w = Wei.(balance_before_w - balance_after_w) in
  let balance_delta_m = Wei.(balance_before_m - balance_after_m) in
  Check.(
    (balance_delta_m <= balance_delta_w)
      Wei.typ
      ~error_msg:
        "Expected sender to pay no more than the witness on OOG, got %L vs %R") ;

  unit

(** Send a tz1 transfer to the Michelson-side gateway with the
    [%call] entrypoint, dispatching a CRAC to a Michelson contract.
    The URL is wired for in-runtime routing
    ([http://tezos/<kt1>/<entrypoint>]); the body is the
    Michelson-packed parameter (e.g. [encode_michelson_string]); the
    callback is [None]. Used by SCENARIO 7. *)
let tz1_to_michelson_via_gateway ~tez_client ~source ~kt1 ~entrypoint ~body_hex
    ?(gas_limit = michelson_hard_gas_limit_per_operation) ?fee () =
  let arg =
    sf
      {|Pair "http://tezos/%s/%s" (Pair {} (Pair %s (Pair 1 None)))|}
      kt1
      entrypoint
      body_hex
  in
  Client.transfer
    ?fee
    ~amount:Tez.zero
    ~giver:source
    ~burn_cap:Tez.one
    ~receiver:michelson_gateway_address
    ~entrypoint:"call"
    ~arg
    ~gas_limit
    tez_client

(** Sends a CRAC from a top-level tz1 manager-op to a Michelson
    contract that grows its storage, and checks that the storage
    cost is burned on the top-level Michelson sender's balance. *)
let test_michelson_to_michelson_storage_growth_michelson_pays () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "Michelson->Michelson CRAC: storage growth is paid by the top-level \
       Michelson manager-op"
    ~tags:["cross_runtime"; "delegated_storage"; "m_to_m"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in
  let initial_storage = "x" in
  let new_storage = "hello world this is now substantially larger" in
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

  (* Measure: tz1 → gateway → Store_input with storage growth. *)
  let* new_storage_hex = encode_michelson_string ~tez_client new_storage in
  let fee = Tez.one in
  let* balance_before =
    Client.get_balance_for ~account:Constant.bootstrap1.alias tez_client
  in
  let* () =
    tz1_to_michelson_via_gateway
      ~tez_client
      ~source:Constant.bootstrap1.alias
      ~kt1
      ~entrypoint:"default"
      ~body_hex:new_storage_hex
      ~fee
      ()
  in
  let*@ _ = Rpc.produce_block evm_node in
  let* content = get_first_manager_operations_content ~tez_endpoint in
  let* balance_after =
    Client.get_balance_for ~account:Constant.bootstrap1.alias tez_client
  in

  (* Check the storage was updated *)
  let* current_storage = Contract.Store_input.read_storage ~tez_client kt1 in
  Check.(
    (current_storage = new_storage)
      string
      ~error_msg:"Expected Michelson storage to be %R, got %L") ;

  (* Check the top-level Michelson paid for the storage growth *)
  let debited_mutez = Tez.(to_mutez (balance_before - balance_after)) in
  let storage_burn = debited_mutez - Tez.to_mutez fee in
  let growth_bytes =
    String.length new_storage - String.length initial_storage
  in
  Check.(
    (storage_burn = growth_bytes * cost_per_byte)
      int
      ~error_msg:
        "Expected top-level Michelson storage burn = %R mutez (growth_bytes × \
         cost_per_byte), got %L") ;

  (* Check the storage-fees burn pair lands on the top-level operation_result, not on any
     internal operation. *)
  let top_bus =
    Tezos_JSON.balance_updates_of_result
      (Tezos_JSON.get_operation_result content)
  in
  Check.is_true
    (BalanceUpdate.has_storage_fees_burn_pair
       ~payer:Constant.bootstrap1.public_key_hash
       ~burn:(growth_bytes * cost_per_byte)
       top_bus)
    ~error_msg:
      (sf
         "Expected a storage-fees burn pair of %d mutez on the top-level \
          manager-op for payer %s"
         (growth_bytes * cost_per_byte)
         Constant.bootstrap1.public_key_hash) ;
  let internal_bus = Tezos_JSON.internal_balance_updates content in
  Check.is_false
    (BalanceUpdate.any_storage_fees_burn internal_bus)
    ~error_msg:"Expected no storage-fees burn on the internal operations" ;

  unit

(** SCENARIO: Michelson(1) -> Michelson(2) CRAC followed by a
    FAILWITH in the same frame.  The wrapper contract (1) emits
    a CRAC to (2) (which grows its storage) and a callback whose
    [%on_result] always FAILWITHs.  The cascade then backtracks
    every internal op and the top-level — but the inner CRAC's
    state change in (2) was committed independently by the
    gateway.  Used to verify that the burn debit on the payer's
    balance and the rendered storage-fees pairs in the receipt
    reconcile under failure. *)
let test_michelson_to_michelson_storage_growth_michelson_failwith_backtracks ()
    =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "Michelson->Michelson CRAC: failing callback backtracks the outer \
       manager-op"
    ~tags:["cross_runtime"; "delegated_storage"; "m_to_m"; "failwith"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in
  let initial_storage = "x" in
  let new_storage = "hello world this is now substantially larger" in
  (* Originate (2) — the CRAC target whose storage grows. *)
  let* kt1_target =
    Contract.Store_input.originate
      ~alias:"store_input"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ~initial_storage
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in
  (* Originate (1) — the wrapper that CRACs to (2) and FAILWITHs
     in its callback. *)
  let* kt1_wrapper =
    Contract.Failing_cross_runtime_http_call_tez_callback.originate
      ~alias:"failing_crac_wrapper"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~burn_cap:Tez.one
      ~destination:kt1_target
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in

  (* Trigger: tz1 -> (1).run(new_storage).  Internally emits the
     CRAC to (2) — whose body field forwards [new_storage] so (2)
     actually grows — plus a callback that FAILWITHs; the cascade
     backtracks the whole manager-op. *)
  let* new_storage_hex = encode_michelson_string ~tez_client new_storage in
  let fee = Tez.one in
  let* balance_before =
    Client.get_balance_for ~account:Constant.bootstrap1.alias tez_client
  in
  let* () =
    Contract.Failing_cross_runtime_http_call_tez_callback.run
      ~body_hex:new_storage_hex
      ~amount:Tez.zero
      ~fee
      ~giver:Constant.bootstrap1.alias
      ~burn_cap:Tez.one
      ~storage_limit:1000
      ~force:true
      kt1_wrapper
      tez_client
  in
  let*@ _ = Rpc.produce_block evm_node in
  let* content = get_first_manager_operations_content ~tez_endpoint in
  let* balance_after =
    Client.get_balance_for ~account:Constant.bootstrap1.alias tez_client
  in

  (* Check the top-level manager-op backtracked. *)
  let operation_result = Tezos_JSON.get_operation_result content in
  let status = Tezos_JSON.status_of_result operation_result in
  Check.(
    (status = "backtracked")
      string
      ~error_msg:"Expected main op status %R, got %L") ;

  (* Check the CRAC target's storage was NOT updated. *)
  let* current_storage =
    Contract.Store_input.read_storage ~tez_client kt1_target
  in
  Check.(
    (current_storage = initial_storage)
      string
      ~error_msg:
        "Expected Michelson storage on the CRAC target to remain %R, got %L") ;

  (* Check the payer was debited only the manager-op fee (no storage burn). *)
  let debited_mutez = Tez.(to_mutez (balance_before - balance_after)) in
  Check.(
    (debited_mutez = Tez.to_mutez fee)
      int
      ~error_msg:
        "Expected the payer to be debited only the manager-op fee (%R mutez), \
         got %L") ;

  (* Check no storage-fees burn appears anywhere in the receipt. *)
  let top_bus = Tezos_JSON.balance_updates_of_result operation_result in
  let internal_bus = Tezos_JSON.internal_balance_updates content in
  Check.is_false
    (BalanceUpdate.any_storage_fees_burn (top_bus @ internal_bus))
    ~error_msg:
      "Expected no storage-fees burn anywhere in the receipt (top-level + \
       internal)" ;

  unit

(** [send_crac_to_evm ~tez_client ~source ~evm_address ~fn_sig ()] sends a CRAC from
    [source] to the EVM contract [evm_address]
    through the gateway's [call_evm] entrypoint. *)
let send_crac_to_evm ~tez_client ~source ~evm_address ~fn_sig
    ?(amount = Tez.zero) ?(calldata = "0x")
    ?(gas_limit = michelson_hard_gas_limit_per_operation) ?fee () =
  let arg =
    sf {|Pair "%s" (Pair "%s" (Pair %s None))|} evm_address fn_sig calldata
  in
  Client.transfer
    ?fee
    ~amount
    ~giver:source
    ~burn_cap:Tez.one
    ~receiver:michelson_gateway_address
    ~entrypoint:"call_evm"
    ~arg
    ~gas_limit
    tez_client

(** Sends a CRAC from a top-level tz1 manager-op through an EVM
    intermediary contract to a Michelson contract that grows its
    storage, and checks that the top-level Michelson sender pays
    nothing extra — the storage cost is absorbed in EVM gas by the
    intermediary. *)
let test_michelson_to_evm_to_michelson_storage_growth_evm_pays () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "Michelson->EVM->Michelson CRAC: storage growth is absorbed by the EVM \
       intermediary"
    ~tags:["cross_runtime"; "delegated_storage"; "m_to_evm_to_m"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun evm_node ->
  let tez_endpoint = tezlink_foreign_endpoint evm_node in
  let* tez_client = tezlink_client evm_node in

  (* Deploy and initialize the EVM intermediary with the Michelson
     destination + the Michelson-packed parameter. *)
  let deploy_and_initialize_cross_runtime_store_evm ~deployer ~kt1
      michelson_value =
    let* contract_address =
      let* raw_tx =
        let* contract =
          Solidity_contracts.cross_runtime_store_evm Evm_version.Cancun
        in
        let bytecode = Tezt.Base.read_file contract.bin in
        Cast.craft_deploy_tx
          ~source_private_key:deployer.Eth_account.private_key
          ~chain_id:1337
          ~nonce:0
          ~gas:2_000_000
          ~gas_price:1_000_000_000
          ~data:("0x" ^ bytecode)
          ()
      in
      let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx evm_node in
      let*@ _ = Rpc.produce_block evm_node in
      let*@ receipt = Rpc.get_transaction_receipt ~tx_hash evm_node in
      match receipt with
      | Some {contractAddress = Some addr; status = true; _} -> return addr
      | _ -> Test.fail "Failed to deploy CrossRuntimeStoreEvm"
    in
    let* _ =
      let* raw_tx =
        let* michelson_value_hex =
          encode_michelson_string ~tez_client michelson_value
        in
        Cast.craft_tx
          ~source_private_key:deployer.Eth_account.private_key
          ~chain_id:1337
          ~nonce:1
          ~gas:1_000_000
          ~gas_price:1_000_000_000
          ~value:Wei.zero
          ~address:contract_address
          ~signature:"initialize(string,bytes)"
          ~arguments:[kt1; michelson_value_hex]
          ()
      in
      let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx evm_node in
      let*@ _ = Rpc.produce_block evm_node in
      let*@ receipt = Rpc.get_transaction_receipt ~tx_hash evm_node in
      match receipt with
      | Some {status = true; _} -> return ()
      | _ -> Test.fail "Failed to initialize CrossRuntimeStoreEvm"
    in
    return contract_address
  in

  let consumed_milligas_of content =
    JSON.(
      content |-> "metadata" |-> "operation_result" |-> "consumed_milligas"
      |> as_string |> int_of_string)
  in

  let initial_storage = "x" in
  let new_storage = "hello world this is now substantially larger" in
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

  (* Witness: deploy contract A initialized with initial_storage (param
     matches Store_input's current state, so no growth). First call
     materializes the alias for A (warmup); second call is the
     measurement — no alias mat, no growth. *)
  let* contract_address_w =
    deploy_and_initialize_cross_runtime_store_evm
      ~deployer:Eth_account.bootstrap_accounts.(0)
      ~kt1
      initial_storage
  in
  let* () =
    send_crac_to_evm
      ~tez_client
      ~source:Constant.bootstrap1.alias
      ~evm_address:contract_address_w
      ~fn_sig:"run()"
      ()
  in
  let*@ _ = Rpc.produce_block evm_node in
  let* () =
    send_crac_to_evm
      ~tez_client
      ~source:Constant.bootstrap1.alias
      ~evm_address:contract_address_w
      ~fn_sig:"run()"
      ()
  in
  let*@ _ = Rpc.produce_block evm_node in
  let* content_w = get_first_manager_operations_content ~tez_endpoint in
  let consumed_milligas_w = consumed_milligas_of content_w in

  (* Measure: deploy contract B (different EVM address → fresh alias)
     initialized with new_storage. Single call triggers both alias
     materialization and Store_input growth — both costs are absorbed
     by the EVM intermediary in gas. *)
  let* contract_address_m =
    deploy_and_initialize_cross_runtime_store_evm
      ~deployer:Eth_account.bootstrap_accounts.(1)
      ~kt1
      new_storage
  in
  let fee = Tez.one in
  let* balance_before =
    Client.get_balance_for ~account:Constant.bootstrap1.alias tez_client
  in
  let* () =
    send_crac_to_evm
      ~tez_client
      ~source:Constant.bootstrap1.alias
      ~evm_address:contract_address_m
      ~fn_sig:"run()"
      ~fee
      ()
  in
  let*@ _ = Rpc.produce_block evm_node in
  let* balance_after =
    Client.get_balance_for ~account:Constant.bootstrap1.alias tez_client
  in
  let* content_m = get_first_manager_operations_content ~tez_endpoint in
  let consumed_milligas_m = consumed_milligas_of content_m in

  (* Check the storage was updated *)
  let* current_storage = Contract.Store_input.read_storage ~tez_client kt1 in
  Check.(
    (current_storage = new_storage)
      string
      ~error_msg:"Expected Michelson storage to be %R, got %L") ;

  (* Check the top-level Michelson paid nothing — EVM intermediary absorbed *)
  let debited_mutez = Tez.(to_mutez (balance_before - balance_after)) in
  let storage_burn = debited_mutez - Tez.to_mutez fee in
  Check.(
    (storage_burn = 0)
      int
      ~error_msg:
        "Expected top-level Michelson storage burn = 0 mutez (EVM intermediary \
         absorbs the delegated cost in gas), got %L") ;

  (* Check the EVM intermediary paid for both alias materialization and
     storage growth in its EVM gas, reflected in the top-level
     consumed_milligas via the gas conversion *)
  let growth_bytes =
    String.length new_storage - String.length initial_storage
  in
  let absorbed_mutez =
    alias_materialization_cost + (growth_bytes * cost_per_byte)
  in
  let absorbed_milligas =
    absorbed_mutez * mutez_to_evm_gas_factor * 1000
    / michelson_to_evm_gas_multiplier
  in
  Check.(
    (consumed_milligas_m - consumed_milligas_w >= absorbed_milligas)
      int
      ~error_msg:"Expected consumed_milligas delta ≥ %R milligas, got %L") ;

  (* Check the Michelson receipt carries no storage-fees burn. *)
  let top_bus =
    Tezos_JSON.balance_updates_of_result
      (Tezos_JSON.get_operation_result content_m)
  in
  let internal_bus = Tezos_JSON.internal_balance_updates content_m in
  Check.is_false
    (BalanceUpdate.any_storage_fees_burn (top_bus @ internal_bus))
    ~error_msg:"Expected no storage-fees burn on the Michelson receipt" ;

  unit

let () =
  test_origination_receipt_exposes_storage_fields () ;
  test_transfer_with_growth_exposes_delta () ;
  test_transfer_with_shrink_exposes_zero_diff () ;
  test_transfer_to_fresh_implicit_burns_slot () ;
  test_two_internal_transfers_burn_each () ;
  test_partial_internal_burn_failure_backtracks_all () ;
  test_partial_internal_storage_limit_overshoot_backtracks_all () ;
  test_big_map_update_insert_overwrite_delete () ;
  test_temp_big_map_dropped_no_charge () ;
  test_big_map_replacement_symmetric () ;
  test_big_map_nonempty_promotion_burns_content () ;
  test_big_map_replacement_nonempty_burns_content () ;
  test_evm_to_michelson_no_storage_growth_no_payment () ;
  test_evm_to_michelson_storage_growth_evm_pays () ;
  test_evm_to_michelson_storage_growth_evm_oog_at_g2 () ;
  test_evm_to_michelson_storage_growth_evm_reverts_after_g2 () ;
  test_evm_to_michelson_alias_origination_evm_pays () ;
  test_evm_to_michelson_alias_origination_evm_oog () ;
  test_michelson_to_michelson_storage_growth_michelson_pays () ;
  test_michelson_to_michelson_storage_growth_michelson_failwith_backtracks () ;
  test_michelson_to_evm_to_michelson_storage_growth_evm_pays ()
