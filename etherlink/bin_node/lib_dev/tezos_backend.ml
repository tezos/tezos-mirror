(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type error += Entrypoints_decode_error of string

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezosx.entrypoints_decode_error"
    ~title:"Entrypoints result decode error"
    ~description:
      "Failed to decode the entrypoints result returned by the kernel."
    ~pp:(fun ppf msg -> Format.fprintf ppf "Entrypoints decode error: %s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Entrypoints_decode_error msg -> Some msg | _ -> None)
    (fun msg -> Entrypoints_decode_error msg)

(* Decode the entrypoints + synthetic-views result written by the
   kernel's tezosx_michelson_entrypoints entrypoint.

   Two RLP shapes are accepted, both wrapped by the canonical-option
   [Some] wrapper from [append_option_canonical] on the kernel side
   (empty list is the canonical [None]):

   - Current shape (kernels from !21936 onward):
       List []                  → None (contract not found)
       List [[entries, views]]  → Some ([], entries, views)
     where
     * [entries] is an RLP list of [name_bytes, micheline_type_bytes]
       pairs;
     * [views] is an RLP list of [name_bytes, parameter_type_bytes,
       return_type_bytes] triples (empty when the contract has no
       synthetic views).

   - Legacy shape (kernels predating !21936):
       List [entries]  → Some ([], entries, [])
     i.e. only the entries list, no views element. Required so the
     EVM node can be upgraded ahead of the rollup kernel upgrade
     that introduces the new shape — without this branch, every
     [/script] call against an enshrined contract on an old kernel
     would surface as a 500.

   The two shapes are distinguished by the structure of the first
   child of the inner list: in the new shape it is itself a list
   (the [entries_list]); in the legacy shape it is the first
   entrypoint pair, whose first element is a [Value name_bytes]. *)
let decode_entrypoints_result bytes =
  let open Lwt_result_syntax in
  let decode_type_bytes type_bytes =
    match
      Data_encoding.Binary.of_bytes_opt
        Tezlink_imports.Imported_context.Script.expr_encoding
        type_bytes
    with
    | None ->
        Result_syntax.tzfail
          (Entrypoints_decode_error "Failed to decode Micheline type bytes")
    | Some type_expr -> Result_syntax.return type_expr
  in
  let decode_entry = function
    | Rlp.List [Value name_bytes; Value type_bytes] ->
        let open Result_syntax in
        let name = Bytes.to_string name_bytes in
        let* type_expr = decode_type_bytes type_bytes in
        return (name, type_expr)
    | _ ->
        Result_syntax.tzfail
          (Entrypoints_decode_error "Invalid RLP entry format")
  in
  let decode_view = function
    | Rlp.List
        [Value name_bytes; Value param_type_bytes; Value return_type_bytes] ->
        let open Result_syntax in
        let name = Bytes.to_string name_bytes in
        let* param_expr = decode_type_bytes param_type_bytes in
        let* return_expr = decode_type_bytes return_type_bytes in
        return (name, param_expr, return_expr)
    | _ ->
        Result_syntax.tzfail
          (Entrypoints_decode_error "Invalid RLP view triple format")
  in
  let*? rlp = Rlp.decode bytes in
  match rlp with
  | Rlp.List [] -> return_none
  | Rlp.List [Rlp.List inner] -> (
      match inner with
      | [(Rlp.List (Rlp.List _ :: _) as entries_rlp); (Rlp.List _ as views_rlp)]
      | [(Rlp.List [] as entries_rlp); (Rlp.List _ as views_rlp)] ->
          (* New shape: the first child of [inner] is itself a list
             (the entries list, whose children are pairs), so we
             know the second child is the views list. *)
          let*? entries = Rlp.decode_list decode_entry entries_rlp in
          let*? views = Rlp.decode_list decode_view views_rlp in
          (* The kernel does not encode unreachable entrypoints;
             always return an empty list for the unreachable field. *)
          return_some ([], entries, views)
      | _ ->
          (* Legacy shape (`[entries]`): [inner] is the entries list
             itself. No views are reported, matching the pre-!21936
             behaviour. *)
          let*? entries = Rlp.decode_list decode_entry (Rlp.List inner) in
          return_some ([], entries, []))
  | _ ->
      tzfail
        (Entrypoints_decode_error "Invalid RLP structure for entrypoints result")

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
              ~hash_column:`Michelson
              (Ethereum_types.Block_parameter.Block_parameter Latest)
          in
          compute_offset current_block_number offset
      | `Hash (hash, offset) ->
          let* current_block_number =
            Evm_ro_context.block_param_to_block_number
              ctxt
              ~chain_family:L2_types.Michelson
              ~hash_column:`Michelson
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
      | block ->
          let* num = shell_block_param_to_block_number block in
          return
          @@ Ethereum_types.Block_parameter.Block_parameter
               (Number (Ethereum_types.quantity_of_z (Z.of_int32 num)))

    let on_implicit_account (c : Tezos_types.Contract.t) k =
      match c with
      | Implicit pkh -> k pkh
      | Originated _ -> failwith "Only implicit account are supported"

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

    let balance _chain block contract =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Tezlink_durable_storage.balance state contract

    let subkeys ~block p =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Evm_ro_context.subkeys state p

    let list_contracts chain block =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* contracts_keys = subkeys ~block Michelson_runtime_contracts_index in
      let contracts =
        List.filter_map
          (fun k ->
            let open Option_syntax in
            let* bytes = Hex.to_string (`Hex k) in
            Data_encoding.Binary.of_string_opt
              Tezos_types.Contract.encoding
              bytes)
          contracts_keys
      in
      let* accounts_keys = subkeys ~block Michelson_runtime_ledger in
      let accounts =
        List.filter_map
          (fun k -> Result.to_option @@ Tezos_types.Contract.of_b58check k)
          accounts_keys
      in
      return (accounts @ contracts)

    let get_storage chain block c =
      let open Lwt_result_syntax in
      (* TODO: #7986
         Support unparsing_mode argument. *)
      let `Main = chain in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Durable_storage.read_opt (Tezos_contract_storage c) state

    let get_code chain block c =
      let open Lwt_result_syntax in
      (* TODO: #7986
         Support unparsing_mode argument. *)
      let `Main = chain in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Durable_storage.read_opt (Tezos_contract_code c) state

    let get_script chain block c =
      let open Lwt_result_syntax in
      let* code = get_code chain block c in
      match code with
      | Some code -> (
          let* storage = get_storage chain block c in
          match storage with
          | Some storage ->
              return_some
                Tezlink_imports.Imported_context.Script.
                  {code = lazy_expr code; storage = lazy_expr storage}
          | None -> return_none)
      | None -> (
          match c with
          | Implicit _ -> return_none
          | Originated _ -> (
              (* Enshrined TezosX contract — no code in durable storage.
                 Derive the script from the entrypoints returned by the
                 kernel, using a unit storage and FAILWITH code as stubs. *)
              let* eth_block = shell_block_param_to_eth_block_param block in
              let addr_bytes =
                Data_encoding.Binary.to_bytes_exn
                  Tezos_types.Contract.encoding
                  c
              in
              let* state = Evm_ro_context.get_state ctxt ~block:eth_block () in
              let* bytes =
                Evm_ro_context.execute_entrypoint
                  ctxt
                  state
                  ~input_path:Durable_storage_path.Tezosx_entrypoints.input
                  ~input:addr_bytes
                  ~output_path:Durable_storage_path.Tezosx_entrypoints.result
                  ~entrypoint:"tezosx_michelson_entrypoints"
              in
              let* result = decode_entrypoints_result bytes in
              match result with
              | None -> return_none
              | Some (_, entries, views) ->
                  return_some (Tezlink_mock.script_of_metadata ~views entries)))

    let manager_key _chain block contract =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      on_implicit_account contract @@ fun pkh ->
      let* info_opt = Durable_storage.read_opt (Tezos_account_info pkh) state in
      match info_opt with
      | Some info -> return info.public_key
      | None ->
          (* An implicit account absent from durable storage is treated
             as unrevealed, matching L1's `manager_key` RPC which returns
             null for any unrevealed implicit account. *)
          return_none

    let counter _chain block (contract : Tezos_types.Contract.t) =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      match contract with
      | Originated _ -> return_none
      | Implicit pkh -> (
          let* info_opt =
            Durable_storage.read_opt (Tezos_account_info pkh) state
          in
          match info_opt with
          | Some info -> return_some (Z.of_int64 info.nonce)
          | None -> return_none)

    let big_map_get chain block id key_hash =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Durable_storage.read_opt (Tezos_big_map_value (id, key_hash)) state

    let big_map_key_type state id =
      Durable_storage.read_opt (Tezos_big_map_key_type id) state

    let big_map_value_type state id =
      Durable_storage.read_opt (Tezos_big_map_value_type id) state

    let big_map_raw_info chain block id =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      let* key_type = big_map_key_type state id in
      let* value_type = big_map_value_type state id in
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

    let block _chain block =
      let open Lwt_result_syntax in
      let* block_number = shell_block_param_to_block_number block in
      Evm_ro_context.tezosx_nth_block ctxt (Z.of_int32 block_number)

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
            let* block_result = Evm_ro_context.tezosx_nth_block ctxt level in
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
          ~hash_column:`Michelson
          (Ethereum_types.Block_parameter.Block_parameter Latest)
      in
      let* (block : L2_types.Tezos_block.t) =
        Evm_ro_context.tezosx_nth_block ctxt current_block_number
      in
      return (block.hash, block.timestamp)

    let block_hash chain block =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* number = shell_block_param_to_block_number block in
      Evm_ro_context.tezosx_nth_block_hash ctxt (Z.of_int32 number)

    let simulate_operation ~chain_id ~simulator_mode op hash block =
      let open Lwt_result_syntax in
      let* () =
        match block with
        | `Head 0l -> return_unit
        | _ ->
            failwith "operation simulation is only possible on the head block"
      in
      let block = Ethereum_types.Block_parameter.(Block_parameter Latest) in
      Simulator.TezosX.simulate_operation
        ctxt
        ~simulator_mode
        ~chain_id
        op
        hash
        block

    let get_entrypoints chain block contract ~normalize_types =
      let open Lwt_result_syntax in
      (* For originated contracts (code stored in durable storage), use the
         OCaml-side entrypoints extraction which follows L1 behavior exactly
         (correct handling of the implicit `default` entrypoint).
         For enshrined contracts (no code in durable storage, e.g. TezosX
         Gateway), fall through to the kernel via tezosx_michelson_entrypoints. *)
      let* code = get_code chain block contract in
      match code with
      | Some code -> Tezlink_mock.list_entrypoints code normalize_types
      | None -> (
          let* eth_block = shell_block_param_to_eth_block_param block in
          let addr_bytes =
            Bytes.to_string
              (Data_encoding.Binary.to_bytes_exn
                 Tezos_types.Contract.encoding
                 contract)
          in
          let* state = Evm_ro_context.get_state ctxt ~block:eth_block () in
          let* bytes =
            Evm_ro_context.execute_entrypoint
              ctxt
              state
              ~input_path:Durable_storage_path.Tezosx_entrypoints.input
              ~input:(Bytes.of_string addr_bytes)
              ~output_path:Durable_storage_path.Tezosx_entrypoints.result
              ~entrypoint:"tezosx_michelson_entrypoints"
          in
          let* result = decode_entrypoints_result bytes in
          match result with
          | None -> return_none
          | Some (unreachable, entries, _views) ->
              if normalize_types then
                let* normalized =
                  Tezlink_mock.normalize_entrypoint_type_exprs entries
                in
                return_some (unreachable, normalized)
              else return_some (unreachable, entries))
  end : Tezlink_backend_sig.S)
