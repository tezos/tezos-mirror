(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)
open Tezos_services

type error +=
  | Unsupported_chain_parameter of string
  | Unsupported_block_parameter of string
  | Failed_operation_forging

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.unsupported_chain_parameter"
    ~title:"Unsupported chain in RPC parameter"
    ~description:"In a RPC call, the chain parameter was unsupported."
    ~pp:(fun ppf chain ->
      Format.fprintf ppf "Unsupported chain parameter %s" chain)
    Data_encoding.(obj1 (req "chain" string))
    (function Unsupported_chain_parameter chain -> Some chain | _ -> None)
    (fun chain -> Unsupported_chain_parameter chain) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.unsupported_block_parameter"
    ~title:"Unsupported block in RPC parameter"
    ~description:"In a RPC call, the block parameter was unsupported."
    ~pp:(fun ppf block ->
      Format.fprintf ppf "Unsupported block parameter %s" block)
    Data_encoding.(obj1 (req "block" string))
    (function Unsupported_block_parameter block -> Some block | _ -> None)
    (fun block -> Unsupported_block_parameter block) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.failed_operation_forging"
    ~title:"Operation forging RPC failed"
    ~description:"An error occued in an operation forging RPC call."
    ~pp:(fun ppf () -> Format.fprintf ppf "Operation forging RPC failed")
    Data_encoding.empty
    (function Failed_operation_forging -> Some () | _ -> None)
    (fun () -> Failed_operation_forging)

let check_chain =
  let open Result_syntax in
  function
  | `Main -> return `Main
  | chain ->
      tzfail
        (Unsupported_chain_parameter
           (Tezos_shell_services.Chain_services.to_string chain))

let ethereum_to_tezos_block_hash hash =
  hash |> Ethereum_types.block_hash_to_bytes |> Block_hash.of_string

let tezos_to_ethereum_block_hash hash =
  hash |> Block_hash.to_bytes |> Ethereum_types.block_hash_of_bytes

let check_block =
  let open Result_syntax in
  function
  | `Genesis -> return (`Level 0l)
  | `Level l -> return (`Level l)
  | `Head offset -> return (`Head (Int32.of_int offset))
  | `Hash (hash, offset) ->
      let hash = tezos_to_ethereum_block_hash hash in
      let offset = Int32.of_int offset in
      return (`Hash (hash, offset))
  | block ->
      tzfail
        (Unsupported_block_parameter
           (Tezos_shell_services.Block_services.to_string block))

let version () =
  (* TODO: #7857 need proper implementation *)
  Lwt_result_syntax.return Tezlink_mock.version

let make_contract_info contract_balance counter_opt contract_script =
  let open Lwt_result_syntax in
  let open Imported_protocol_plugin.Contract_services in
  let*? counter =
    let open Result_syntax in
    match counter_opt with
    | None -> return_none
    | Some counter_z ->
        let* counter = Protocol_types.Counter.of_z counter_z in
        return_some counter
  in
  return
    {
      balance = contract_balance;
      delegate = None;
      counter;
      script = contract_script;
      revealed = None;
    }

let tezlink_to_tezos_chain_id ~l2_chain_id _chain =
  let (L2_types.Chain_id l2_chain_id) = l2_chain_id in
  let bytes = Bytes.make 4 '\000' in
  l2_chain_id |> Z.to_int32_unsigned |> Bytes.set_int32_be bytes 0 ;
  Chain_id.of_bytes bytes

let chain_directory_path = Tezos_shell_services.Chain_services.path

let block_directory_path =
  Tezos_rpc.Path.prefix
    chain_directory_path
    Tezos_shell_services.Block_services.path

module Make_block_header
    (Proto : Tezlink_protocol)
    (Next_proto : Tezlink_protocol) =
struct
  module Block_services = Make_block_service (Proto) (Next_proto)

  let protocols =
    Tezlink_protocols.Shell_impl.
      {current_protocol = Proto.hash; next_protocol = Next_proto.hash}

  let tezlink_block_to_shell_header (block : L2_types.Tezos_block.t) :
      Block_header.shell_header tzresult =
    let open Result_syntax in
    let open Tezlink_mock in
    let* predecessor = ethereum_to_tezos_block_hash block.parent_hash in
    return
      Block_header.
        {
          level = block.level;
          proto_level;
          predecessor;
          timestamp = block.timestamp;
          validation_passes;
          operations_hash;
          fitness;
          context;
        }

  let tezlink_block_to_block_header ~l2_chain_id
      ((block : L2_types.Tezos_block.t), chain) :
      Block_services.block_header tzresult =
    let open Result_syntax in
    let* chain_id = tezlink_to_tezos_chain_id ~l2_chain_id chain in
    let* protocol_data = Block_services.mock_block_header_data ~chain_id in
    let* hash = ethereum_to_tezos_block_hash block.hash in
    let* shell = tezlink_block_to_shell_header block in
    let block_header : Block_services.block_header =
      {chain_id; hash; shell; protocol_data}
    in
    return block_header

  let tezlink_block_to_raw_block_header ~chain_id block =
    let open Result_syntax in
    let* protocol_data = Block_services.mock_block_header_data ~chain_id in
    let* shell = tezlink_block_to_shell_header block in
    let raw_block_header : Block_services.raw_block_header =
      {shell; protocol_data}
    in
    return raw_block_header

  let make_metadata ~level_info =
    let open Result_syntax in
    let* block_header_metadata =
      Block_services.mock_block_header_metadata level_info
    in
    return
      Block_services.
        {
          protocol_data = block_header_metadata;
          test_chain_status = Test_chain_status.Not_running;
          max_operations_ttl = 0;
          max_operation_data_length = 0;
          max_block_header_length = 0;
          operation_list_quota = [];
        }

  let bootstrap_transfers_receipt chain_id backend =
    let open Lwt_result_syntax in
    let* transfers =
      Current_block_services.activate_bootstraps_with_transfers backend
    in
    let*? transfers =
      List.fold_left
        (fun acc (receipt, operation) ->
          let open Result_syntax in
          let* acc = acc in
          let* receipt =
            Tezos_types.convert_using_serialization
              ~name:"operation_receipt"
              ~src:Imported_protocol.operation_receipt_encoding
              ~dst:Block_services.Proto.operation_receipt_encoding
              receipt
          in
          let* operation =
            Tezos_types.convert_using_serialization
              ~name:"operation_data"
              ~src:Imported_protocol.operation_data_encoding
              ~dst:Block_services.Proto.operation_data_encoding
              operation
          in
          let item =
            Block_services.
              {
                chain_id;
                hash = Operation_hash.zero;
                shell = {branch = Tezos_crypto.Hashed.Block_hash.zero};
                protocol_data = operation;
                receipt = Receipt receipt;
              }
          in
          Ok (item :: acc))
        (Ok [])
        transfers
    in
    return transfers

  let tezlink_block_to_block_info ~l2_chain_id backend
      (level_info, version, chain, block) =
    let open Lwt_result_syntax in
    let*? chain_id = tezlink_to_tezos_chain_id ~l2_chain_id chain in
    let*? hash = ethereum_to_tezos_block_hash block.L2_types.Tezos_block.hash in
    let*? header = tezlink_block_to_raw_block_header ~chain_id block in
    let*? metadata = make_metadata ~level_info in
    let consensus_opperations = [] in
    let voting_operations = [] in
    let anonymous_operations = [] in

    let*? manager_operations =
      Block_services.deserialize_operations ~chain_id block.operations
    in
    let* bootstrap_transfers =
      (* To allow tzkt to index the bootstrap accounts, we add dummy transfers
       from a faucet address to all the accounts present in durable storage at
       block 0 (ie accounts added by an installer kernel). *)
      if block.level = 2l then bootstrap_transfers_receipt chain_id backend
      else return_nil
    in
    let block_info : Block_services.block_info =
      {
        chain_id;
        hash;
        header;
        metadata = Some metadata;
        operations =
          [
            consensus_opperations;
            voting_operations;
            anonymous_operations;
            bootstrap_transfers @ manager_operations;
          ];
      }
    in
    return (version, block_info)

  let tezlink_block_metadata (level_info, version) =
    let open Result_syntax in
    let+ metadata = make_metadata ~level_info in
    (version, metadata)
end

module Current_block_header =
  Make_block_header (Imported_protocol) (Imported_protocol)
module Zero_block_header = Make_block_header (Zero_protocol) (Genesis_protocol)
module Genesis_block_header =
  Make_block_header (Genesis_protocol) (Imported_protocol)

(** [wrap conversion service_implementation] changes the output type
    of [service_implementation] using [conversion]. *)
let wrap conv impl p q i =
  let open Lwt_result_syntax in
  let* res = impl p q i in
  let*? result = conv res in
  return result

let register ~service ~impl dir = Tezos_rpc.Directory.register dir service impl

let register_with_conversion ~service ~impl ~convert_output dir =
  register ~service ~impl:(wrap convert_output impl) dir

let opt_register ~service ~impl dir =
  Tezos_rpc.Directory.opt_register dir service impl

let opt_register_with_conversion ~service ~impl ~convert_output dir =
  opt_register ~service ~impl:(wrap (Option.map_e convert_output) impl) dir

(** Builds the static part of the directory registering services under `/chains/<main>/blocks/<head>/...`.
    This part is based on the current protocol supported by Tezlink, which means that if we request the counter
    of a tz1 on an old block the encoding used will be the one of the current live protocol. *)
let build_block_static_directory ~l2_chain_id
    (module Backend : Tezlink_backend_sig.S) :
    tezlink_rpc_context Imported_env.RPC_directory.t =
  let open Lwt_result_syntax in
  Tezos_rpc.Directory.empty
  |> register_with_conversion
       ~service:Tezos_services.current_level
       ~impl:(fun (((), chain), block) query () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.current_level chain block ~offset:query.offset)
       ~convert_output:Protocol_types.Level.convert
  |> register
       ~service:Tezos_services.contract_info
       ~impl:(fun ((((), chain), block), contract) _query () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         let* balance = Backend.balance chain block contract in
         let* counter = Backend.counter chain block contract in
         let* script = Backend.get_script chain block contract in
         make_contract_info balance counter script)
  |> opt_register
       ~service:Tezos_services.get_script
       ~impl:(fun ((((), chain), block), contract) _ _ ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.get_script chain block contract)
  |> opt_register
       ~service:Tezos_services.list_entrypoints
       ~impl:(fun ((((), chain), block), contract) {normalize_types} () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         let* code = Backend.get_code chain block contract in
         match code with
         | None -> return_none
         | Some code -> Tezlink_mock.list_entrypoints code normalize_types)
  |> register
       ~service:Tezos_services.balance
       ~impl:(fun ((((), chain), block), contract) _ _ ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.balance chain block contract)
  |> register
       ~service:Tezos_services.list
       ~impl:(fun (((), chain), block) _ _ ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.list_contracts chain block)
  |> register
       ~service:Tezos_services.get_storage_normalized
         (* TODO: #7995
            Take unparsing_mode argument into account *)
       ~impl:(fun ((((), chain), block), contract) () _unparsing_mode ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.get_storage chain block contract)
  |> opt_register
       ~service:Tezos_services.get_storage
       ~impl:(fun ((((), chain), block), contract) _ _ ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.get_storage chain block contract)
  |> opt_register
       ~service:Tezos_services.Big_map.get
       ~impl:(fun (((((), chain), block), id), key_hash) _ _ ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.big_map_get chain block id key_hash)
  |> register
       ~service:Tezos_services.manager_key
       ~impl:(fun ((((), chain), block), contract) _ _ ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.manager_key chain block contract)
  |> opt_register_with_conversion
       ~service:Tezos_services.counter
       ~impl:(fun ((((), chain), block), contract) () () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         (* On not-yet-allocated implicit accounts, L1 returns the
            current global counter instead of [None]. We don't have a
            global counter so we return 0 in this case instead. See
            also https://gitlab.com/tezos/tezos/-/issues/7960. *)
         let* counter_opt = Backend.counter chain block contract in
         match (counter_opt, contract) with
         | None, Implicit _ -> return_some Z.zero
         | counter_opt, _ -> return counter_opt)
       ~convert_output:Protocol_types.Counter.of_z
  |> register
       ~service:Tezos_services.constants
       ~impl:(fun (((), chain), block) () () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.constants chain block)
  |> register
       ~service:Tezos_services.expected_issuance
       ~impl:(fun (((), chain), block) () () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         let* level = Backend.current_level chain block ~offset:0l in
         Lwt.return
         @@ Tezos_services.Adaptive_issuance_services.dummy_rewards level.cycle)
  |> register
       ~service:Tezos_services.Forge.operations
       ~impl:(fun (((), chain), block) () operation ->
         let*? _chain = check_chain chain in
         let*? _block = check_block block in
         let*? bytes =
           Data_encoding.Binary.to_bytes
             Alpha_context.Operation.unsigned_encoding
             operation
           |> Result.map_error_e @@ fun _ ->
              Result_syntax.tzfail Failed_operation_forging
         in
         return bytes)
  |> register
     (* TODO: https://gitlab.com/tezos/tezos/-/issues/7965 *)
     (* We need a proper implementation *)
       ~service:Tezos_services.simulate_operation
       ~impl:(fun
           (((), chain), block)
           _param
           ( _blocks_before_activation,
             operation,
             _chain_id,
             _operation_inclusion_latency )
         ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         let hash = Alpha_context.Operation.hash_packed operation in
         let*? chain_id = tezlink_to_tezos_chain_id ~l2_chain_id chain in
         let* result =
           Backend.simulate_operation
             ~chain_id
             ~skip_signature:true
             operation
             hash
             block
         in
         return (operation.protocol_data, result))
  |> register
       ~service:Tezos_services.run_operation
       ~impl:(fun (((), chain), block) _param (operation, _chain_id) ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         let*? chain_id = tezlink_to_tezos_chain_id ~l2_chain_id chain in
         let* result =
           Backend.simulate_operation
             ~chain_id
             ~skip_signature:true
             operation
             (Alpha_context.Operation.hash_packed operation)
             block
         in
         return (operation.protocol_data, result))
  |> register
       ~service:Tezos_services.pack_data
       ~impl:(fun _ctxt () (data, ty, gas) ->
         Tezlink_mock.pack_data ~data ~ty ~gas)
  |> register
       ~service:Tezos_services.preapply_operations
       ~impl:(fun (((), chain), block) param ops ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         let*? chain_id = tezlink_to_tezos_chain_id ~l2_chain_id chain in
         let* receipts =
           List.map_es
             (fun (operation : Alpha_context.packed_operation) ->
               let hash = Alpha_context.Operation.hash_packed operation in
               let* result =
                 Backend.simulate_operation
                   ~chain_id
                   ~skip_signature:false
                   operation
                   hash
                   block
               in
               return (operation.protocol_data, result))
             ops
         in
         return (param#version, receipts))
  |> register
       ~service:Tezos_services.raw_json_cycle
       ~impl:(fun ((((), chain), block), _cycle) () () ->
         let*? _chain = check_chain chain in
         let*? _block = check_block block in
         Tezlink_mock.storage_cycle ())
  |> opt_register
       ~service:Tezos_services.Big_map.raw_info
       ~impl:(fun ((((), chain), block), id) () () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.big_map_raw_info chain block id)

let retrieve_level (module Backend : Tezlink_backend_sig.S) chain block =
  let open Lwt_result_syntax in
  let*? chain = check_chain chain in
  let*? block = check_block block in
  let* tezlink_level = Backend.current_level chain block ~offset:0l in
  return tezlink_level.level

let protocol_for_block_or_level level_result :
    (module Tezlink_protocol) * (module Tezlink_protocol) =
  let imported =
    ( (module Tezos_services.Imported_protocol : Tezlink_protocol),
      (module Tezos_services.Imported_protocol : Tezlink_protocol) )
  in
  match level_result with
  | Ok level -> (
      match level with
      | 0l ->
          ( (module Zero_protocol : Tezlink_protocol),
            (module Genesis_protocol : Tezlink_protocol) )
      | 1l ->
          ( (module Genesis_protocol : Tezlink_protocol),
            (module Tezos_services.Imported_protocol : Tezlink_protocol) )
      | _ -> imported)
  | _ ->
      (* TezosX PoC also uses the same backend as Tezlink for now. So when
        requesting the current_level, it fails as blocks are not stored at the same path.
        Until TezosX Poc has its own backend, we should return imported to prevent the failure.*)
      imported

(** We currently support a single target protocol version but we need to handle early blocks (blocks at
    levels 0 and 1) specifically because TzKT expects the `protocol` and `next_protocol` fields of the
    block headers and block metadata at these levels to indicate the hashes of the genesis protocols.
    Patching these fields is unfortunately not doable from within the implementation of the services
    because these fields are added in the output encodings of the services. For this reason, the services
    for which a special treatment of early blocks is needed are registered dynamically.

    To ensure consistency, we register all the services returning block infos and block headers the same way. *)
let register_dynamic_block_services ~l2_chain_id
    (module Backend : Tezlink_backend_sig.S) (chain : chain) (block : block) =
  let open Lwt_result_syntax in
  let*! tezlink_level = retrieve_level (module Backend) chain block in
  let (module Proto : Tezlink_protocol), (module Next_proto : Tezlink_protocol)
      =
    protocol_for_block_or_level tezlink_level
  in
  let module Block_header = Make_block_header (Proto) (Next_proto) in
  let module S = Block_header.Block_services.S in
  let static_dir = build_block_static_directory ~l2_chain_id (module Backend) in
  let dir =
    static_dir
    |> register ~service:S.info ~impl:(fun (((), chain), block) q () ->
           let*? chain = check_chain chain in
           let*? block = check_block block in
           let* tezlink_block = Backend.block chain block in
           let* level = Backend.current_level chain block ~offset:0l in
           let* block_info =
             Block_header.tezlink_block_to_block_info
               ~l2_chain_id
               (module Backend)
               (level, q#version, chain, tezlink_block)
           in
           return block_info)
    |> register_with_conversion
         ~service:(import_service S.header)
         ~impl:(fun (((), chain), block) () () ->
           let*? chain = check_chain chain in
           let*? block = check_block block in
           let* tezlink_block = Backend.block chain block in
           Lwt_result_syntax.return (tezlink_block, chain))
         ~convert_output:
           (Block_header.tezlink_block_to_block_header ~l2_chain_id)
    |> register_with_conversion
         ~service:(import_service S.Header.shell_header)
         ~impl:(fun (((), chain), block) () () ->
           let*? chain = check_chain chain in
           let*? block = check_block block in
           let* tezlink_block = Backend.block chain block in
           Lwt_result_syntax.return tezlink_block)
         ~convert_output:Block_header.tezlink_block_to_shell_header
    |> register
         ~service:S.protocols
         ~impl:(fun (((), chain), block) _query () ->
           let*? `Main = check_chain chain in
           let*? _block = check_block block in
           return Block_header.protocols)
    |> register_with_conversion
         ~service:(import_service S.metadata)
         ~impl:(fun (((), chain), block) q () ->
           let*? chain = check_chain chain in
           let*? block = check_block block in
           let* level = Backend.current_level chain block ~offset:0l in
           Lwt_result_syntax.return (level, q#version))
         ~convert_output:Block_header.tezlink_block_metadata
    |> opt_register_with_conversion
         ~service:S.hash
         ~impl:(fun (((), chain), block) () () ->
           let*? chain = check_chain chain in
           let*? block = check_block block in
           Backend.block_hash chain block)
         ~convert_output:ethereum_to_tezos_block_hash
    |> register
         ~service:S.Operations.operations
         ~impl:(fun (((), chain), block) o () ->
           let*? chain = check_chain chain in
           let*? block = check_block block in
           let*? chain_id = tezlink_to_tezos_chain_id ~l2_chain_id chain in
           let* block = Backend.block chain block in
           let consensus_operations = [] in
           let voting_operations = [] in
           let anonymous_operations = [] in
           let*? manager_operations =
             Block_header.Block_services.deserialize_operations
               ~chain_id
               block.operations
           in
           return
             ( o#version,
               [
                 consensus_operations;
                 voting_operations;
                 anonymous_operations;
                 manager_operations;
               ] ))
    |> opt_register
         ~service:S.Operations.operation
         ~impl:(fun
             (((((), chain), block), operation_pass), operation_index) o () ->
           let*? chain = check_chain chain in
           let*? block = check_block block in
           let*? chain_id = tezlink_to_tezos_chain_id ~l2_chain_id chain in
           if operation_pass <> Imported_protocol.Operation_repr.manager_pass
           then
             (* All tezlink operations are manager operations *)
             return_none
           else
             let* block = Backend.block chain block in
             let*? operations =
               Block_header.Block_services.deserialize_operations
                 ~chain_id
                 block.operations
             in
             let operation_opt = List.nth_opt operations operation_index in
             return (Option.map (fun op -> (o#version, op)) operation_opt))
    |> register
         ~service:S.Operation_hashes.operation_hashes
         ~impl:(fun (((), chain), block) () () ->
           let*? chain = check_chain chain in
           let*? block = check_block block in
           let consensus_operation_hashes = [] in
           let voting_operation_hashes = [] in
           let anonymous_operation_hashes = [] in
           let* manager_operation_hashes =
             let* block = Backend.block chain block in
             let*? operations =
               Block_header.Block_services.deserialize_operations_header
                 block.operations
             in
             return
             @@ List.map
                  (fun (hash, _header, _op_and_receipt) -> hash)
                  operations
           in
           return
             [
               consensus_operation_hashes;
               voting_operation_hashes;
               anonymous_operation_hashes;
               manager_operation_hashes;
             ])
    |> register
         ~service:S.Operation_hashes.operation_hashes_in_pass
         ~impl:(fun ((((), chain), block), list_offset) () () ->
           if list_offset <> Imported_protocol.Operation_repr.manager_pass
           (* All tezlink operations are manager operations *)
           then return_nil
           else
             let*? chain = check_chain chain in
             let*? block = check_block block in
             let* block = Backend.block chain block in
             let*? operations =
               Block_header.Block_services.deserialize_operations_header
                 block.operations
             in
             return (List.map (fun (hash, _, _) -> hash) operations))
  in
  Lwt.return dir

let register_chain_services ~l2_chain_id
    (module Backend : Tezlink_backend_sig.S) base_dir =
  let dir =
    Tezos_rpc.Directory.empty
    |> register_with_conversion
         ~service:Tezos_services.chain_id
         ~impl:(fun chain () () ->
           Lwt_result_syntax.return (l2_chain_id, chain))
         ~convert_output:(fun (l2_chain_id, chain) ->
           tezlink_to_tezos_chain_id ~l2_chain_id chain)
    |> register
         ~service:Tezos_services.is_bootstrapped
         ~impl:(fun _chain () () ->
           Lwt_result_syntax.return
             ( true,
               Tezos_shell_services.Chain_validator_worker_state.Synchronised
                 {is_chain_stuck = false} ))
    |> Tezos_rpc.Directory.map (fun ((), chain) -> Lwt.return chain)
  in
  Tezos_rpc.Directory.merge
    base_dir
    (Tezos_rpc.Directory.prefix chain_directory_path dir)

(** Builds the directory registering the service at `/monitor/heads/<chain>`. *)
let register_monitor_heads (module Backend : Tezlink_backend_sig.S) dir =
  Tezos_rpc.Directory.gen_register
    dir
    Tezos_services.monitor_heads
    (fun ((), chain) query () ->
      let stream, stopper = Backend.monitor_heads chain query in
      let shutdown () = Lwt_watcher.shutdown stopper in
      let next () =
        let open Lwt_syntax in
        let* block_opt = Lwt_stream.get stream in
        match block_opt with
        | None -> return_none
        | Some block -> (
            match
              ( Current_block_header.tezlink_block_to_shell_header block,
                ethereum_to_tezos_block_hash block.hash )
            with
            | Ok shell, Ok hash ->
                return_some
                  ( hash,
                    ({
                       shell;
                       protocol_data =
                         Data_encoding.Binary.to_bytes_exn
                           Imported_protocol.Block_header_repr
                           .protocol_data_encoding
                           Imported_protocol.mock_protocol_data;
                     }
                      : Block_header.t) )
            | Error _, _ | _, Error _ -> return_none)
      in

      Tezos_rpc.Answer.return_stream {next; shutdown})

(** Builds the root directory. *)
let build_dir ~l2_chain_id ~add_operation backend =
  let (module Backend : Tezlink_backend_sig.S) = backend in
  let base_dir =
    Tezos_rpc.Directory.register_dynamic_directory
      Tezos_rpc.Directory.empty
      block_directory_path
      (fun (((), chain), block) ->
        register_dynamic_block_services ~l2_chain_id backend chain block)
  in
  base_dir
  |> register_chain_services ~l2_chain_id backend
  |> register_with_conversion
       ~service:Tezos_services.bootstrapped
       ~impl:(fun () () () -> Backend.bootstrapped ())
       ~convert_output:(fun (input_hash, input_time) ->
         let open Result_syntax in
         let* hash = ethereum_to_tezos_block_hash input_hash in
         return (hash, input_time))
  |> register_monitor_heads backend
  |> register ~service:Tezos_services.version ~impl:(fun () () () -> version ())
  |> register_with_conversion
       ~service:Tezos_services.injection_operation
       ~impl:(fun
           ()
             (* TODO: https://gitlab.com/tezos/tezos/-/issues/8007
                When async is true, the RPC should return before
                validation. *)
           (_ : < async : bool ; chain : chain option >)
           (raw_operation : bytes)
         -> add_operation raw_operation)
       ~convert_output:(fun hash ->
         hash |> Ethereum_types.hash_to_bytes |> Operation_hash.of_string_exn
         |> Result_syntax.return)

let tezlink_root = Tezos_rpc.Path.(open_root / "tezlink")

(* module entrypoint *)
let register_tezlink_services ~l2_chain_id ~add_operation backend =
  let directory = build_dir ~l2_chain_id ~add_operation backend in
  let directory =
    Tezos_rpc.Directory.register_describe_directory_service
      directory
      Tezos_rpc.Service.description_service
  in
  let tezlink_directory = Tezos_rpc.Directory.prefix tezlink_root directory in
  tezlink_directory
