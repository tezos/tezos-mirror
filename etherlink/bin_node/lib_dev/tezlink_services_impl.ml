(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let make (ctxt : Evm_ro_context.t) =
  (module struct
    type block_param =
      [ `Head of int32
      | `Level of int32
      | `Hash of Ethereum_types.block_hash * int32 ]

    let shell_block_param_to_block_number =
      let open Lwt_result_syntax in
      let compute_offset (Ethereum_types.Qty block_number) offset =
        let result = Int32.sub (Z.to_int32 block_number) offset in
        return (max 0l result)
      in
      function
      | `Head offset ->
          let* current_block_number =
            Evm_ro_context.block_param_to_block_number
              ctxt
              ~chain_family:L2_types.Michelson
              (Ethereum_types.Block_parameter.Block_parameter Latest)
          in
          compute_offset current_block_number offset
      | `Hash (hash, offset) ->
          let* current_block_number =
            Evm_ro_context.block_param_to_block_number
              ctxt
              ~chain_family:L2_types.Michelson
              (Ethereum_types.Block_parameter.Block_hash
                 {hash; require_canonical = false})
          in
          compute_offset current_block_number offset
      | `Level l -> return l

    let shell_block_param_to_eth_block_param =
      let open Lwt_result_syntax in
      function
      | `Head 0l ->
          return
          @@ Ethereum_types.Block_parameter.Block_parameter
               Ethereum_types.Block_parameter.Latest
      | `Hash (hash, 0l) ->
          return
          @@ Ethereum_types.Block_parameter.Block_hash
               {hash; require_canonical = false}
      | block ->
          let* num = shell_block_param_to_block_number block in
          return
          @@ Ethereum_types.Block_parameter.Block_parameter
               (Number (Ethereum_types.quantity_of_z (Z.of_int32 num)))

    let constants chain (_block : block_param) =
      let open Lwt_result_syntax in
      let `Main = chain in
      return
        (Tezlink_constants.all_constants
           ?hard_gas_limit_per_block:ctxt.michelson_hard_gas_limit_per_block
           ())

    let current_level chain block ~offset =
      let open Lwt_result_syntax in
      let `Main = chain in

      let* offset =
        (* Tezos l1 requires non-negative offset #7845 *)
        if offset >= 0l then return offset
        else failwith "The specified level offset should be positive."
      in

      let* block_number = shell_block_param_to_block_number block in

      let* constants = constants chain block in
      let level = Int32.add block_number offset in
      return
        Tezos_types.
          {
            level;
            cycle =
              Int32.(div (sub level 1l) constants.parametric.blocks_per_cycle);
            cycle_position =
              Int32.(rem (sub level 1l) constants.parametric.blocks_per_cycle);
          }

    let get_state ~block =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      Evm_ro_context.get_state ctxt ~block ()

    let subkeys ~block p =
      let open Lwt_result_syntax in
      let* state = get_state ~block in
      Evm_ro_context.subkeys state p

    let balance chain block c =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* state = get_state ~block in
      Tezlink_durable_storage.balance state c

    let list_contracts chain block =
      let open Lwt_result_syntax in
      let `Main = chain in
      let+ contracts_keys = subkeys ~block Michelson_runtime_contracts_index in
      List.filter_map
        (fun k ->
          let open Option_syntax in
          let* bytes = Hex.to_string (`Hex k) in
          Data_encoding.Binary.of_string_opt Tezos_types.Contract.encoding bytes)
        contracts_keys

    let get_storage chain block c =
      let open Lwt_result_syntax in
      (* TODO: #7986
         Support unparsing_mode argument. *)
      let `Main = chain in
      let* state = get_state ~block in
      Durable_storage.read_opt (Tezos_contract_storage c) state

    let get_code chain block c =
      let open Lwt_result_syntax in
      (* TODO: #7986
         Support unparsing_mode argument. *)
      let `Main = chain in
      let* state = get_state ~block in
      (* Resolves a code-less Tezos X alias to the shared implementation
         (mirrors the kernel), so [/script], [/storage] and [/entrypoints]
         return the resolved forwarder rather than an empty result. *)
      Durable_storage.read_contract_code c state

    let get_script chain block c =
      let open Lwt_result_syntax in
      let* code = get_code chain block c in
      let* storage = get_storage chain block c in
      match (code, storage) with
      | Some code, Some storage ->
          return
          @@ Some
               Tezlink_imports.Imported_context.Script.
                 {code = lazy_expr code; storage = lazy_expr storage}
      | _ -> return_none

    let manager_key chain block (c : Tezos_types.Contract.t) =
      let open Lwt_result_syntax in
      (* TODO: #7831 !17664
         Support non-default chain and block parameters. *)
      let `Main = chain in
      match c with
      | Implicit source -> (
          let* state = get_state ~block in
          let* manager_opt = Tezlink_durable_storage.manager state source in
          match manager_opt with
          | Some (Public_key k) -> return_some k
          | _ -> return_none)
      | Originated _ -> return_none

    let counter chain block (c : Tezos_types.Contract.t) =
      let open Lwt_result_syntax in
      (* TODO: #7831 !17664
         Support non-default chain and block parameters. *)
      let `Main = chain in
      match c with
      | Implicit source ->
          let* state = get_state ~block in
          Tezlink_durable_storage.counter state source
      | Originated _ -> return_none

    let used_storage_space chain block (c : Tezos_types.Contract.t) =
      let open Lwt_result_syntax in
      let `Main = chain in
      match c with
      | Implicit _ -> return_none
      | Originated _ ->
          let* state = get_state ~block in
          let* used =
            Durable_storage.michelson_contract_used_storage_space c state
          in
          return_some used

    let paid_storage_space chain block (c : Tezos_types.Contract.t) =
      let open Lwt_result_syntax in
      let `Main = chain in
      match c with
      | Implicit _ -> return_none
      | Originated _ ->
          let* state = get_state ~block in
          let* paid =
            Durable_storage.michelson_contract_paid_storage_space c state
          in
          return_some paid

    let big_map_get chain block id key_hash =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* state = get_state ~block in
      Durable_storage.read_opt (Tezos_big_map_value (id, key_hash)) state

    let big_map_raw_info chain block id =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* state = get_state ~block in
      let* key_type =
        Durable_storage.read_opt (Tezos_big_map_key_type id) state
      in
      let* value_type =
        Durable_storage.read_opt (Tezos_big_map_value_type id) state
      in
      let* total_bytes =
        Durable_storage.read_opt (Tezos_big_map_total_bytes id) state
      in
      match (key_type, value_type) with
      | Some kt, Some vt ->
          (* [total_bytes] defaults to zero when the counter path is absent,
             which only happens for a big-map predating the kernel-side counter
             and not written since (the kernel migrates it lazily on write; this
             read-only RPC cannot trigger that migration).
             contents: intentionally empty, consistent with L1 raw context
             behavior (L1 never returns big_map contents in this RPC). *)
          return_some (kt, vt, Option.value ~default:Z.zero total_bytes, [])
      | _ -> return_none

    let block chain block =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* block_number = shell_block_param_to_block_number block in
      Evm_ro_context.tezlink_nth_block ctxt (Z.of_int32 block_number)

    let monitor_heads chain query =
      (* TODO: #7831
         take chain into account
         For the moment this implementation only supports the main chain, once
         the rpc support of tezlink is more stable, we can add support for other chains *)
      ignore (chain, query) ;

      let blueprint_stream, stopper = Broadcast.create_blueprint_stream () in

      let retry_delays_ms = [0.; 50.; 100.; 500.] in

      (* Convert blueprint notifications into full blocks, giving the store a
         short grace period if the block is not yet written.
         Note that this delay does not correspond to the time between blueprint production
         and block application, but rather from the time the Database says the data has been
         written in the storage and the moment it actually becomes available to be read. *)
      let rec fetch_block level =
        let open Lwt_syntax in
        function
        | [] ->
            (* After all retries failed, emit warning event. *)
            let* () = Events.missing_block @@ Z.to_int32 level in
            return_none
        | delay_ms :: rest -> (
            let* () = Lwt_unix.sleep (delay_ms /. 1000.) in
            let* block_result = Evm_ro_context.tezlink_nth_block ctxt level in
            match block_result with
            | Ok block -> return_some block
            | Error _ -> fetch_block level rest)
      in

      let block_stream =
        Lwt_stream.filter_map_s
          (fun (bp_with_events : Blueprint_types.Legacy.with_events) ->
            (* Extract the level from the blueprint. *)
            let (Ethereum_types.Qty level) = bp_with_events.blueprint.number in
            fetch_block level retry_delays_ms)
          blueprint_stream
      in
      (block_stream, stopper)

    (* TODO: #7963 Support Observer Mode
       Here the catchup mechanism to fetch blueprints is not taken into account as
       the observer mode is not supported yet *)
    let bootstrapped () =
      let open Lwt_result_syntax in
      let* (Qty current_block_number) =
        Evm_ro_context.block_param_to_block_number
          ctxt
          ~chain_family:L2_types.Michelson
          (Ethereum_types.Block_parameter.Block_parameter Latest)
      in
      let* (block : L2_types.Tezos_block.t) =
        Evm_ro_context.tezlink_nth_block ctxt current_block_number
      in
      return (block.hash, block.timestamp)

    let block_hash chain block =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* number = shell_block_param_to_block_number block in
      Evm_ro_context.tezlink_nth_block_hash ctxt (Z.of_int32 number)

    let simulate_operation ~chain_id ~simulator_mode op hash block =
      let open Lwt_result_syntax in
      let* () =
        match block with
        | `Head 0l -> return_unit
        | _ ->
            failwith "operation simulation is only possible on the head block"
      in
      let block = Ethereum_types.Block_parameter.(Block_parameter Latest) in
      Simulator.Tezlink.simulate_operation
        ctxt
        ~chain_id
        ~simulator_mode
        op
        hash
        block

    let get_entrypoints chain block contract ~normalize_types =
      let open Lwt_result_syntax in
      let* code = get_code chain block contract in
      match code with
      | None -> return_none
      | Some code -> Tezlink_mock.list_entrypoints code normalize_types
  end : Tezlink_backend_sig.S)
