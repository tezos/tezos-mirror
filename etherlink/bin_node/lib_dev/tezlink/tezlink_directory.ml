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
    (fun block -> Unsupported_block_parameter block)

let check_chain =
  let open Result_syntax in
  function
  | `Main -> return `Main
  | chain ->
      tzfail
        (Unsupported_chain_parameter
           (Tezos_shell_services.Chain_services.to_string chain))

let check_block =
  let open Result_syntax in
  function
  | `Genesis -> return (`Level 0l)
  | `Level l -> return (`Level l)
  | `Head offset -> return (`Head (Int32.of_int offset))
  | block ->
      tzfail
        (Unsupported_block_parameter
           (Tezos_shell_services.Block_services.to_string block))

let protocols () = Lwt_result_syntax.return Tezlink_protocols.current

let version () =
  (* TODO: #7857 need proper implementation *)
  Lwt_result_syntax.return Tezlink_mock.version

let chain_directory_path = Tezos_shell_services.Chain_services.path

let block_directory_path =
  Tezos_rpc.Path.subst2
  @@ Tezos_rpc.Path.prefix
       chain_directory_path
       Tezos_shell_services.Block_services.path

module type HEADER = sig
  module Block_services : BLOCK_SERVICES

  val tezlink_block_to_shell_header :
    L2_types.Tezos_block.t -> Block_header.shell_header tzresult

  val tezlink_block_to_block_header :
    l2_chain_id:L2_types.chain_id ->
    L2_types.Tezos_block.t * 'b ->
    Block_services.block_header tzresult

  val tezlink_block_to_block_info :
    l2_chain_id:L2_types.chain_id ->
    'a * L2_types.Tezos_block.t * 'b ->
    ('a * Block_services.block_info) tzresult
end

module Make_block_header (Block_services : BLOCK_SERVICES) :
  HEADER with module Block_services = Block_services = struct
  module Block_services = Block_services

  let tezlink_block_to_shell_header (block : L2_types.Tezos_block.t) :
      Block_header.shell_header tzresult =
    let open Result_syntax in
    let open Tezlink_mock in
    let* predecessor =
      Protocol_types.ethereum_to_tezos_block_hash block.parent_hash
    in
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
    let* chain_id =
      Protocol_types.tezlink_to_tezos_chain_id ~l2_chain_id chain
    in
    let* protocol_data = Block_services.mock_block_header_data in
    let* hash = Protocol_types.ethereum_to_tezos_block_hash block.hash in
    let* shell = tezlink_block_to_shell_header block in
    let block_header : Block_services.block_header =
      {chain_id; hash; shell; protocol_data}
    in
    return block_header

  let tezlink_block_to_raw_block_header block =
    let open Result_syntax in
    let* protocol_data = Block_services.mock_block_header_data in
    let* shell = tezlink_block_to_shell_header block in
    let raw_block_header : Block_services.raw_block_header =
      {shell; protocol_data}
    in
    return raw_block_header

  let tezlink_block_to_block_info ~l2_chain_id (version, block, chain) =
    let open Result_syntax in
    let* chain_id =
      Protocol_types.tezlink_to_tezos_chain_id ~l2_chain_id chain
    in
    let* hash =
      Protocol_types.ethereum_to_tezos_block_hash
        block.L2_types.Tezos_block.hash
    in
    let* header = tezlink_block_to_raw_block_header block in
    let block_info : Block_services.block_info =
      {chain_id; hash; header; metadata = None; operations = []}
    in
    return (version, block_info)
end

module Current_block_header = Make_block_header (Block_services)
module Zero_block_header = Make_block_header (Zero_block_services)
module Genesis_block_header = Make_block_header (Genesis_block_services)

(** [wrap conversion service_implementation] changes the output type
    of [service_implementation] using [conversion]. *)
let wrap conv impl p q i =
  let open Lwt_result_syntax in
  let* res = impl p q i in
  let*? result = conv res in
  return result

let register ~service ~impl dir = Tezos_rpc.Directory.register dir service impl

let register_with_conversion ~service ~impl ~convert_output dir =
  Tezos_rpc.Directory.register dir service (wrap convert_output impl)

let opt_register_with_conversion ~service ~impl ~convert_output dir =
  Tezos_rpc.Directory.opt_register
    dir
    service
    (wrap (Option.map_e convert_output) impl)

let register_dynamic ~root_dir ~path dir_of_path =
  Tezos_rpc.Directory.register_dynamic_directory root_dir path dir_of_path

(** Builds the static part of the directory registering services under `/chains/<main>/blocks/<head>/...`. *)
let build_block_static_directory ~l2_chain_id
    (module Backend : Tezlink_backend_sig.S) =
  let open Lwt_result_syntax in
  Tezos_rpc.Directory.empty
  |> register_with_conversion
       ~service:Tezos_services.current_level
       ~impl:(fun {block; chain} query () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.current_level chain block ~offset:query.offset)
       ~convert_output:Protocol_types.Level.convert
  |> register
       ~service:Tezos_services.protocols
       ~impl:(fun {block; chain} _query () ->
         let*? `Main = check_chain chain in
         let*? _block = check_block block in
         protocols ())
  |> register_with_conversion
       ~service:Tezos_services.contract_info
       ~impl:(fun ({block; chain}, contract) _query () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         let* balance = Backend.balance chain block contract in
         let* counter = Backend.counter chain block contract in
         return (contract, balance, counter))
       ~convert_output:Protocol_types.Contract.make_info
  |> register
       ~service:Tezos_services.balance
       ~impl:(fun ({chain; block}, contract) _ _ ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.balance chain block contract)
  |> register
       ~service:Tezos_services.get_storage_normalized
         (* TODO: #7995
            Take unparsing_mode argument into account *)
       ~impl:(fun ({chain; block}, contract) () _unparsing_mode ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.get_storage chain block contract)
  |> register
       ~service:Tezos_services.manager_key
       ~impl:(fun ({chain; block}, contract) _ _ ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.manager_key chain block contract)
  |> register_with_conversion
       ~service:Tezos_services.counter
       ~impl:(fun ({block; chain}, contract) () () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.counter chain block contract)
       ~convert_output:Protocol_types.Counter.of_z
  |> register
       ~service:Tezos_services.constants
       ~impl:(fun {block; chain} () () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.constants chain block)
  |> register_with_conversion
       ~service:Tezos_services.header
       ~impl:(fun {chain; block} () () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         let* tezlink_block = Backend.block chain block in
         Lwt_result_syntax.return (tezlink_block, chain))
       ~convert_output:
         (Current_block_header.tezlink_block_to_block_header ~l2_chain_id)
  |> opt_register_with_conversion
       ~service:Tezos_services.hash
       ~impl:(fun {block; chain} () () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         Backend.block_hash chain block)
       ~convert_output:Protocol_types.ethereum_to_tezos_block_hash
  |> register
       ~service:Adaptive_issuance_services.expected_issuance
       ~impl:(fun _ () () ->
         (* The mock assumes we stay in cycle 0 for now *)
         Lwt.return @@ Adaptive_issuance_services.dummy_rewards Int32.zero)
  |> register
     (* TODO: https://gitlab.com/tezos/tezos/-/issues/7965 *)
     (* We need a proper implementation *)
       ~service:Tezos_services.simulate_operation
       ~impl:(fun
           {block; chain}
           _param
           ( _blocks_before_activation,
             operation,
             _chain_id,
             _operation_inclusion_latency )
         ->
         let*? _chain = check_chain chain in
         let*? _block = check_block block in
         let op = operation.protocol_data in
         let*? mock_result =
           Tezlink_mock.Operation_metadata.operation_metadata op
         in
         return (op, mock_result))

let register_block_info ~l2_chain_id (module Backend : Tezlink_backend_sig.S)
    (module Block_header : HEADER) base_dir =
  let open Lwt_result_syntax in
  base_dir
  |> register_with_conversion
       ~service:(import_service Block_header.Block_services.S.info)
       ~impl:(fun {block; chain} q () ->
         let*? chain = check_chain chain in
         let*? block = check_block block in
         let* tezlink_block = Backend.block chain block in
         Lwt_result_syntax.return (q#version, tezlink_block, chain))
       ~convert_output:(Block_header.tezlink_block_to_block_info ~l2_chain_id)

(** We currently support a single target protocol version but we need to handle early blocks (blocks at
    levels 0 and 1) specifically because TzKT expects the `protocol` and `next_protocol` fields of the
    block headers and block metadata at these levels to indicate the hashes of the genesis protocols.
    Patching these fields is unfortunately not doable from within the implementation of the services
    because these fields are added in the output encodings of the services. For this reason, the services
    for which a special treatment of early blocks is needed are registered dynamically. *)
let register_dynamic_block_services ~l2_chain_id
    (module Backend : Tezlink_backend_sig.S) base_dir =
  let static_dir = build_block_static_directory ~l2_chain_id (module Backend) in
  let dynamic_dir_current_proto =
    static_dir
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7993 *)
    (* RPCs at directory level doesn't appear properly in the describe RPC *)
    |> register_block_info
         ~l2_chain_id
         (module Backend)
         (module Current_block_header)
    |> Tezos_rpc.Directory.map (fun (((), chain), block) ->
           make_env chain block)
  in
  let dynamic_dir_zero_proto =
    static_dir
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7993 *)
    (* RPCs at directory level doesn't appear properly in the describe RPC *)
    |> register_block_info
         ~l2_chain_id
         (module Backend)
         (module Zero_block_header)
    |> Tezos_rpc.Directory.map (fun (((), chain), block) ->
           make_env chain block)
  in
  let dynamic_dir_genesis_proto =
    static_dir
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7993 *)
    (* RPCs at directory level doesn't appear properly in the describe RPC *)
    |> register_block_info
         ~l2_chain_id
         (module Backend)
         (module Genesis_block_header)
    |> Tezos_rpc.Directory.map (fun (((), chain), block) ->
           make_env chain block)
  in

  let dynamic_dir =
    register_dynamic
      ~root_dir:Tezos_rpc.Directory.empty
      ~path:block_directory_path
      (fun (((), _chain), block) ->
        match block with
        (* Trying to access the first blocks info using `Head offset will lead
           to incoherent results. If it becomes a pb we will need to ignore
           potential errors from Backend.current_level. *)
        | `Genesis | `Level 0l -> Lwt.return dynamic_dir_zero_proto
        | `Level 1l -> Lwt.return dynamic_dir_genesis_proto
        | _ -> Lwt.return dynamic_dir_current_proto)
  in
  Tezos_rpc.Directory.merge base_dir dynamic_dir

let register_chain_services ~l2_chain_id
    (module Backend : Tezlink_backend_sig.S) base_dir =
  let dir =
    Tezos_rpc.Directory.empty
    |> register_with_conversion
         ~service:Tezos_services.chain_id
         ~impl:(fun chain () () ->
           Lwt_result_syntax.return (l2_chain_id, chain))
         ~convert_output:(fun (l2_chain_id, chain) ->
           Protocol_types.tezlink_to_tezos_chain_id ~l2_chain_id chain)
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
                Protocol_types.ethereum_to_tezos_block_hash block.hash )
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
let build_dir ~l2_chain_id backend =
  let (module Backend : Tezlink_backend_sig.S) = backend in
  Tezos_rpc.Directory.empty
  |> register_dynamic_block_services ~l2_chain_id backend
  |> register_chain_services ~l2_chain_id backend
  |> register_with_conversion
       ~service:Tezos_services.bootstrapped
       ~impl:(fun () () () -> Backend.bootstrapped ())
       ~convert_output:(fun (input_hash, input_time) ->
         let open Result_syntax in
         let* hash = Protocol_types.ethereum_to_tezos_block_hash input_hash in
         return (hash, input_time))
  |> register_monitor_heads backend
  |> register ~service:Tezos_services.version ~impl:(fun () () () -> version ())

let tezlink_root = Tezos_rpc.Path.(open_root / "tezlink")

(* module entrypoint *)
let register_tezlink_services ~l2_chain_id backend =
  let directory = build_dir ~l2_chain_id backend in
  let directory =
    Tezos_rpc.Directory.register_describe_directory_service
      directory
      Tezos_rpc.Service.description_service
  in
  let tezlink_directory = Tezos_rpc.Directory.prefix tezlink_root directory in
  tezlink_directory
