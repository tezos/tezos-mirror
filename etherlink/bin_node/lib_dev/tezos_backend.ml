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

(* Decode the entrypoints result written by the kernel's
   tezosx_michelson_entrypoints entrypoint. RLP encoding:
     List []        → None (contract not found)
     List [entries] → Some ([], entries)
   where entries is an RLP list of [name_bytes, micheline_type_bytes] pairs. *)
let decode_entrypoints_result bytes =
  let open Lwt_result_syntax in
  let decode_entry = function
    | Rlp.List [Value name_bytes; Value type_bytes] -> (
        let open Result_syntax in
        let name = Bytes.to_string name_bytes in
        match
          Data_encoding.Binary.of_bytes_opt
            Tezlink_imports.Imported_context.Script.expr_encoding
            type_bytes
        with
        | None ->
            tzfail
              (Entrypoints_decode_error "Failed to decode Micheline type bytes")
        | Some type_expr -> return (name, type_expr))
    | _ ->
        Result_syntax.tzfail
          (Entrypoints_decode_error "Invalid RLP entry format")
  in
  let*? rlp = Rlp.decode bytes in
  match rlp with
  | Rlp.List [] -> return_none
  | Rlp.List [entries_rlp] ->
      let*? entries = Rlp.decode_list decode_entry entries_rlp in
      (* The kernel does not encode unreachable entrypoints; always return
         an empty list for the unreachable field. *)
      return_some ([], entries)
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

    let contract_path contract suffix =
      Durable_storage_path.etherlink_root ^ "/contracts/index/"
      ^ Tezlink_durable_storage.Path.to_path
          Tezos_types.Contract.encoding
          contract
      ^ suffix

    let constants chain (_block : block_param) =
      let open Lwt_result_syntax in
      let `Main = chain in
      return Tezlink_constants.all_constants

    let current_level chain block ~offset =
      let open Lwt_result_syntax in
      let `Main = chain in

      let* offset =
        (* Tezos l1 requires non-negative offset #7845 *)
        if offset >= 0l then return offset
        else failwith "The specified level offset should be positive."
      in

      let* block_number = shell_block_param_to_block_number block in

      let constants = Tezlink_constants.all_constants in
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
      match (contract : Tezos_types.Contract.t) with
      | Implicit pkh -> (
          let* read_result =
            Evm_ro_context.read_state
              state
              (Tezosx.Durable_storage_path.Accounts.Tezos.info pkh)
          in
          match read_result with
          | Some bytes ->
              let*? info = Tezosx.Tezos_runtime.decode_account_info bytes in
              return info.balance
          | None -> return Tezos_types.Tez.zero)
      | Originated _ -> (
          let path = contract_path contract "/balance" in
          let* read_result = Evm_ro_context.read_state state path in
          match read_result with
          | Some bytes -> (
              match
                Data_encoding.Binary.of_bytes Tezos_types.Tez.encoding bytes
              with
              | Ok balance -> return balance
              | Error e ->
                  failwith
                    "Cannot decode KT1 balance: %a"
                    Data_encoding.Binary.pp_read_error
                    e)
          | None -> return Tezos_types.Tez.zero)

    let subkeys ~block p =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Evm_ro_context.subkeys state p

    let list_contracts chain block =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* contracts_keys =
        subkeys ~block (Durable_storage_path.etherlink_root ^ "/contracts/index")
      in
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
      let* accounts_keys =
        subkeys ~block "/evm/world_state/eth_accounts/tezos"
      in
      let accounts =
        List.filter_map
          (fun k -> Result.to_option @@ Tezos_types.Contract.of_b58check k)
          accounts_keys
      in
      return (accounts @ contracts)

    let bootstrap_accounts () =
      let open Lwt_result_syntax in
      let block = `Level 0l in
      let chain = `Main in
      let* accounts_keys =
        subkeys ~block "/evm/world_state/eth_accounts/tezos"
      in
      let accounts =
        List.filter_map
          Signature.V2.Public_key_hash.of_b58check_opt
          accounts_keys
      in
      List.map_es
        (fun c ->
          let* balance = balance chain block (Implicit c) in
          return (c, balance))
        accounts

    let get_storage chain block c =
      let open Lwt_result_syntax in
      (* TODO: #7986
         Support unparsing_mode argument. *)
      let `Main = chain in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Lwt_result.map (Option.value ~default:None)
      @@ Durable_storage.inspect_durable_and_decode_opt
           (Evm_ro_context.read_state state)
           (contract_path c "/data/storage")
           (Data_encoding.Binary.of_bytes_opt
              Tezlink_imports.Imported_context.Script.expr_encoding)

    let get_code chain block c =
      let open Lwt_result_syntax in
      (* TODO: #7986
         Support unparsing_mode argument. *)
      let `Main = chain in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Lwt_result.map (Option.value ~default:None)
      @@ Durable_storage.inspect_durable_and_decode_opt
           (Evm_ro_context.read_state state)
           (contract_path c "/data/code")
           (Data_encoding.Binary.of_bytes_opt
              Tezlink_imports.Imported_context.Script.expr_encoding)

    let get_script chain block c =
      let open Lwt_result_syntax in
      match Tezlink_mock.mocked_script c with
      | Some c -> return_some c
      | None -> (
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
                  let* state =
                    Evm_ro_context.get_state ctxt ~block:eth_block ()
                  in
                  let* bytes =
                    Evm_ro_context.execute_entrypoint
                      ctxt
                      state
                      ~input_path:Durable_storage_path.Tezosx_entrypoints.input
                      ~input:addr_bytes
                      ~output_path:
                        Durable_storage_path.Tezosx_entrypoints.result
                      ~entrypoint:"tezosx_michelson_entrypoints"
                  in
                  let* result = decode_entrypoints_result bytes in
                  match result with
                  | None -> return_none
                  | Some (_, entries) ->
                      return_some (Tezlink_mock.script_of_entrypoints entries)))
          )

    let manager_key _chain block contract =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      on_implicit_account contract @@ fun pkh ->
      let* read_result =
        Evm_ro_context.read_state
          state
          (Tezosx.Durable_storage_path.Accounts.Tezos.info pkh)
      in
      match read_result with
      | Some bytes ->
          let*? info = Tezosx.Tezos_runtime.decode_account_info bytes in
          return info.public_key
      | None -> failwith "Account not found"

    let counter _chain block (contract : Tezos_types.Contract.t) =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      match contract with
      | Originated _ -> return_none
      | Implicit pkh -> (
          let* read_result =
            Evm_ro_context.read_state
              state
              (Tezosx.Durable_storage_path.Accounts.Tezos.info pkh)
          in
          match read_result with
          | Some bytes ->
              let*? info = Tezosx.Tezos_runtime.decode_account_info bytes in
              return_some (Z.of_int64 info.nonce)
          | None -> return_none)

    let big_map_get chain block id key_hash =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      let raw_hash =
        Tezlink_imports.Imported_protocol.Script_expr_hash.to_bytes key_hash
      in
      let (`Hex key_hex) = Hex.of_bytes raw_hash in
      let path =
        "/evm/world_state/big_map/"
        ^ Z.to_string
            (Tezlink_imports.Imported_context.Big_map.Id.unparse_to_z id)
        ^ "/" ^ key_hex
      in
      let decode =
        Data_encoding.Binary.of_bytes_opt
          Tezlink_imports.Imported_context.Script.expr_encoding
      in
      let+ result =
        Durable_storage.inspect_durable_and_decode_opt
          (Evm_ro_context.read_state state)
          path
          decode
      in
      Option.join result

    let big_map_key_type state id =
      let open Lwt_result_syntax in
      let path =
        "/evm/world_state/big_map/"
        ^ Z.to_string
            (Tezlink_imports.Imported_context.Big_map.Id.unparse_to_z id)
        ^ "/key_type"
      in
      let decode =
        Data_encoding.Binary.of_bytes_opt
          Tezlink_imports.Imported_context.Script.expr_encoding
      in
      let+ result =
        Durable_storage.inspect_durable_and_decode_opt
          (Evm_ro_context.read_state state)
          path
          decode
      in
      Option.join result

    let big_map_value_type state id =
      let open Lwt_result_syntax in
      let path =
        "/evm/world_state/big_map/"
        ^ Z.to_string
            (Tezlink_imports.Imported_context.Big_map.Id.unparse_to_z id)
        ^ "/value_type"
      in
      let decode =
        Data_encoding.Binary.of_bytes_opt
          Tezlink_imports.Imported_context.Script.expr_encoding
      in
      let+ result =
        Durable_storage.inspect_durable_and_decode_opt
          (Evm_ro_context.read_state state)
          path
          decode
      in
      Option.join result

    let big_map_raw_info chain block id =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      let* key_type = big_map_key_type state id in
      let* value_type = big_map_value_type state id in
      match (key_type, value_type) with
      | Some kt, Some vt ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/8229
             - total_bytes:
               Not yet implemented, requires kernel-side tracking
               (L1 stores this at /big_maps/index/<id>/total_bytes)
             - contents: intentionally empty, consistent with L1 raw context
               behavior (L1 never returns big_map contents in this RPC) *)
          return_some (kt, vt, Z.zero, [])
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
      let* state = Evm_ro_context.get_state ctxt ~block () in
      let read = Evm_ro_context.read_state state in
      Simulator.TezosX.simulate_operation
        ctxt
        ~simulator_mode
        ~chain_id
        ~read
        ~data_model:Tezlink_durable_storage.Rlp
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
          | Some (unreachable, entries) ->
              if normalize_types then
                let* normalized =
                  Tezlink_mock.normalize_entrypoint_type_exprs entries
                in
                return_some (unreachable, normalized)
              else return_some (unreachable, entries))
  end : Tezlink_backend_sig.S)
