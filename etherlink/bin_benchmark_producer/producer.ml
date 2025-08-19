(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Primitives
open Test_helpers
open Rpc.Syntax

let register ~title ~registered ~tx_per_call =
  Test.register
    ~__FILE__
    ~title
    ~tags:["produce"]
    ~uses_admin_client:false
    ~uses_client:false
    ~uses_node:false
    ~uses:
      [
        Constant.octez_evm_node;
        Constant.WASM.evm_kernel;
        Constant.smart_rollup_installer;
      ]
  @@ fun () ->
  (* max_gas_per_tx must be equivalent to bin_node/lib_dev/durable_storage.ml maximum_gas_per_transaction *)
  let max_gas_per_tx = 30_000_000 in
  (* da_fee_per_byte must be equivalent to kernel_latest/kernel/src/fees.rs DA_FEE_PER_BYTE *)
  let da_fee_per_byte = Wei.to_wei_z Z.(of_int 4 * pow (of_int 10) 12) in
  (* gas_price must be big yumyum value kekw *)
  let gas_price = 10_000_000_000 in

  let account = Eth_account.bootstrap_accounts.(0) in
  let* sequencer =
    init_sequencer_sandbox
      ~tx_queue_max_size:100_000
      ~tx_queue_max_lifespan:100_000
      ~tx_queue_tx_per_addr_limit:100_000
      ~da_fee_per_byte
      ()
  in
  let*@ v = Rpc.tez_kernelVersion sequencer in
  Log.report "Kernel version is %s" v ;

  let send_calls label address calls =
    Lwt_list.iter_s
      (fun (signature, arguments) ->
        let* tx_hashes =
          fold tx_per_call [] (fun _ acc ->
              let wait_for =
                Evm_node.wait_for_tx_queue_add_transaction sequencer
              in
              let*@ nonce =
                Rpc.get_transaction_count
                  ~block:"pending"
                  ~address:account.address
                  sequencer
              in
              let* crafted_tx =
                Cast.craft_tx
                  ~source_private_key:account.private_key
                  ~chain_id:1337
                  ~nonce:(Int64.to_int nonce)
                  ~value:Wei.zero
                  ~gas:max_gas_per_tx
                  ~gas_price
                  ~legacy:false
                  ~address
                  ~signature
                  ~arguments
                  ()
              in
              let*@ tx_hash =
                Rpc.send_raw_transaction ~raw_tx:crafted_tx sequencer
              in
              let* _ = wait_for in
              return (tx_hash :: acc))
        in

        let*@ txn = Rpc.produce_block sequencer in
        let*@ n = Rpc.block_number sequencer in

        let open Lwt_result_syntax in
        let*@ total_inclusion_gas, total_execution_gas =
          List.fold_left_es
            (fun ((total_inclusion, total_execution) as total) tx_hash ->
              let*@ gas_info =
                Rpc.get_transaction_gas_info ~tx_hash sequencer
              in
              match gas_info with
              | Some (`Inclusion_gas inclusion_gas, `Execution_gas execution_gas)
                ->
                  return
                    Int64.
                      ( add inclusion_gas total_inclusion,
                        add execution_gas total_execution )
              | None ->
                  Log.warn "Receipt is missing." ;
                  return total)
            (0L, 0L)
            tx_hashes
        in

        Log.report
          "Block %ld included %d tx for a total of %Ld inclusion gas, %Ld \
           execution gas, %Ld total gas = %s from %s"
          n
          txn
          total_inclusion_gas
          total_execution_gas
          (Int64.add total_inclusion_gas total_execution_gas)
          signature
          label ;
        unit)
      calls
  in

  Lwt_list.iter_s
    (fun ({contract; constructor_arg; calls} : contract_info) ->
      let* ({label; abi; bin; _} : Solidity_contracts.contract) = contract in
      let* () = Eth_cli.add_abi ~label ~abi () in
      let wait_for = Evm_node.wait_for_tx_queue_add_transaction sequencer in
      let tx =
        Eth_cli.deploy
          ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
          ~endpoint:(Evm_node.endpoint sequencer)
          ~abi
          ~bin
          ?args:constructor_arg
          ()
      in
      let* _ = wait_for in
      let*@ txn = produce_block sequencer in
      let*@ n = Rpc.block_number sequencer in
      Log.report "Block %ld included %d tx = deployment of %s" n txn label ;
      let* contract_address, _ = tx in
      send_calls label contract_address calls)
    registered
