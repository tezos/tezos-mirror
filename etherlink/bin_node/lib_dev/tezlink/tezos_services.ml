(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Tezlink_imports

type level = Tezos_types.level

(* Provides mock values necessary for constructing L1 types that contain fields
    that are either irrelevant to the L2 or are not yet supported. *)
module Mock = struct
  let proto_level = 1

  let validation_passes = 4

  (* TODO #7866
     When blocks are populated, this mock value will be unnecessary,
     being replaced by actual data. *)
  let operations_hash =
    Operation_list_list_hash.of_bytes_exn (Bytes.make 32 '\000')

  let fitness = [Bytes.make 32 '\255']

  (* TODO #7866
     When blocks are populated, this mock value will be unnecessary,
     being replaced by actual data. *)
  let context = Context_hash.of_bytes_exn (Bytes.make 32 '\255')

  let contents : Imported_protocol.Block_header_repr.contents =
    {
      payload_hash = Imported_protocol.Block_payload_hash.zero;
      payload_round = Imported_protocol.Round_repr.zero;
      seed_nonce_hash = None;
      proof_of_work_nonce =
        Bytes.make
          Imported_protocol.Constants_repr.proof_of_work_nonce_size
          '\000';
      per_block_votes =
        {
          liquidity_baking_vote = Per_block_vote_pass;
          adaptive_issuance_vote = Per_block_vote_pass;
        };
    }

  let signature : Imported_protocol.Alpha_context.signature =
    Unknown (Bytes.make Tezos_crypto.Signature.Ed25519.size '\000')

  let protocol_data : Imported_protocol.Block_header_repr.protocol_data =
    {contents; signature}
end

(* Module importing, amending, and converting, protocol types. Those types
   might be difficult to actually build, so we define conversion function from
   local types to protocol types. *)
module Protocol_types = struct
  module Alpha_context = Imported_protocol.Alpha_context
  module Raw_level = Alpha_context.Raw_level

  module Cycle = struct
    include Alpha_context.Cycle

    (* This function is copied from [cycle_repr.ml] because it is not exposed
       in [alpha_context.mli]. *)
    let of_int32_exn i =
      if Compare.Int32.(i >= 0l) then add root (Int32.to_int i)
      else invalid_arg "Cycle_repr.of_int32_exn"
  end

  let tezlink_to_tezos_chain_id_exn ~l2_chain_id _chain =
    let (L2_types.Chain_id l2_chain_id) = l2_chain_id in
    let bytes = Bytes.make 4 '\000' in
    l2_chain_id |> Z.to_int32 |> Bytes.set_int32_be bytes 0 ;
    Chain_id.of_bytes_exn bytes

  module Protocol_data = struct
    let get_mock_protocol_data =
      Tezos_types.convert_using_serialization
        ~name:"protocol_data"
        ~dst:Tezlink_imports.Imported_protocol.block_header_data_encoding
        ~src:Imported_protocol.Block_header_repr.protocol_data_encoding
        Mock.protocol_data
  end

  let ethereum_to_tezos_block_hash hash =
    hash |> Ethereum_types.block_hash_to_bytes |> Block_hash.of_string_exn

  module Block_header = struct
    let tezlink_block_to_shell_header (block : L2_types.Tezos_block.t) :
        Block_header.shell_header =
      let open Mock in
      let (Ethereum_types.Qty number) = block.number in
      let (Ethereum_types.Qty timestamp) = block.timestamp in
      let predecessor = ethereum_to_tezos_block_hash block.parent_hash in
      {
        level = Z.to_int32 number;
        proto_level;
        predecessor;
        timestamp = Time.Protocol.of_seconds @@ Z.to_int64 timestamp;
        validation_passes;
        operations_hash;
        fitness;
        context;
      }

    let _tezlink_block_to_block_header ~l2_chain_id
        ((block : L2_types.Tezos_block.t), chain) :
        Block_services.block_header tzresult =
      let open Result_syntax in
      let chain_id = tezlink_to_tezos_chain_id_exn ~l2_chain_id chain in
      let* protocol_data = Protocol_data.get_mock_protocol_data in
      let hash = ethereum_to_tezos_block_hash block.hash in
      let shell = tezlink_block_to_shell_header block in
      let block_header : Block_services.block_header =
        {chain_id; hash; shell; protocol_data}
      in
      return block_header
  end

  module Level = struct
    type t = Alpha_context.Level.t

    let encoding = Alpha_context.Level.encoding

    (** The sole purpose of this encoding is to reflect as closely as possible
        the encoding of Alpha_context.Level.t, so it can be used to convert to
        that type, with a serialization pass. This is necessary only because
        there isn't a simple way to build that type. *)
    let conversion_encoding =
      let open Data_encoding in
      conv
        (fun ({level; cycle; cycle_position} : level) ->
          ( Raw_level.of_int32_exn level,
            Int32.pred level,
            Cycle.of_int32_exn cycle,
            cycle_position,
            false ))
        (fun ( level,
               _level_position,
               cycle,
               cycle_position,
               _expected_commitment ) ->
          let level = Raw_level.to_int32 level in
          let cycle = Cycle.to_int32 cycle in
          {level; cycle; cycle_position})
        (obj5
           (req "level" Raw_level.encoding)
           (req "level_position" int32)
           (req "cycle" Cycle.encoding)
           (req "cycle_position" int32)
           (req "expected_commitment" bool))

    let convert : level -> t tzresult =
      Tezos_types.convert_using_serialization
        ~name:"level"
        ~dst:encoding
        ~src:conversion_encoding
  end

  module Counter = struct
    type t = Alpha_context.Manager_counter.t

    let encoding = Alpha_context.Manager_counter.encoding_for_RPCs

    let of_z : Z.t -> t tzresult =
      Tezos_types.convert_using_serialization
        ~name:"counter"
        ~dst:encoding
        ~src:Data_encoding.z
  end
end

(** [wrap conversion service_implementation] changes the output type
    of [service_implementation] using [conversion]. *)
let wrap conv impl p q i =
  let open Lwt_result_syntax in
  let* res = impl p q i in
  let*? result = conv res in
  return result

(** [import_service s] makes it possible to substitute new [prefix] and [param]
    types in the signature of the service [s]. *)
let import_service s = Tezos_rpc.Service.subst0 s

let register_with_conversion ~service ~impl ~convert_output dir =
  Tezos_rpc.Directory.register dir service (wrap convert_output impl)

let register ~service ~impl dir = Tezos_rpc.Directory.register dir service impl

type block = Tezos_shell_services.Block_services.block

type chain = Tezos_shell_services.Chain_services.chain

type tezlink_rpc_context = {block : block; chain : chain}

(** Builds a [tezlink_rpc_context] from paths parameters. *)
let make_env (chain : chain) (block : block) : tezlink_rpc_context Lwt.t =
  Lwt.return {block; chain}

module Tezlink_version = struct
  type version = Tezos_version.Octez_node_version.t = {
    version : Tezos_version.Version.t;
    network_version : Tezos_version.Network_version.t;
    commit_info : commit_info option;
  }

  and commit_info = Tezos_version.Octez_node_version.commit_info = {
    commit_hash : string;
    commit_date : string;
  }

  let mock =
    Tezos_version.
      {
        version = Tezos_version_parser.default;
        network_version = Network_version.Internal_for_tests.mock ();
        commit_info = Some {commit_hash = ""; commit_date = ""};
      }
end

module Tezlink_protocols = struct
  module Shell_impl = Tezos_shell_services.Block_services

  type protocols = Shell_impl.protocols

  let quebec = Tezos_protocol_021_PsQuebec.Protocol.hash

  let current = Shell_impl.{current_protocol = quebec; next_protocol = quebec}
end

(* Copied from src/proto_alpha/lib_protocol/constants_services.ml. *)
(* TODO: #7875
   Import from the protocol once it is exposed instead of copying it here. *)
module Constants_services = struct
  module RPC_path = Tezos_rpc.Path
  module RPC_service = Tezos_rpc.Service
  module RPC_query = Tezos_rpc.Query

  let custom_root =
    (RPC_path.(open_root / "context" / "constants")
      : tezlink_rpc_context RPC_path.context)

  let all =
    RPC_service.get_service
      ~description:"All constants"
      ~query:RPC_query.empty
      ~output:Alpha_context.Constants.encoding
      custom_root
end

(* This is where we import service declarations from the protocol. *)
module Imported_services = struct
  module Protocol_plugin_services = Imported_protocol_plugin.RPC.S

  type level_query = Protocol_plugin_services.level_query = {offset : int32}

  let current_level :
      ( [`GET],
        tezlink_rpc_context,
        tezlink_rpc_context,
        level_query,
        unit,
        Protocol_types.Level.t )
      Tezos_rpc.Service.t =
    import_service Protocol_plugin_services.current_level

  let version :
      ( [`GET],
        unit,
        unit,
        unit,
        unit,
        Tezlink_version.version )
      Tezos_rpc.Service.t =
    Tezos_shell_services.Version_services.S.version

  let protocols :
      ( [`GET],
        tezlink_rpc_context,
        tezlink_rpc_context,
        unit,
        unit,
        Tezlink_protocols.protocols )
      Tezos_rpc.Service.t =
    import_service Tezos_shell_services.Shell_services.Blocks.S.protocols

  let balance :
      ( [`GET],
        tezlink_rpc_context,
        tezlink_rpc_context * Tezos_types.Contract.t,
        unit,
        unit,
        Tezos_types.Tez.t )
      Tezos_rpc.Service.t =
    Tezos_rpc.Service.subst1
      Imported_protocol_plugin.Contract_services.S.balance

  let manager_key :
      ( [`GET],
        tezlink_rpc_context,
        tezlink_rpc_context * Tezos_types.Contract.t,
        unit,
        unit,
        Protocol_types.Alpha_context.public_key option )
      Tezos_rpc.Service.t =
    let open Tezos_rpc in
    Service.subst1 Imported_protocol_plugin.Contract_services.S.manager_key

  let counter :
      ( [`GET],
        tezlink_rpc_context,
        tezlink_rpc_context * Tezos_types.Contract.t,
        unit,
        unit,
        Protocol_types.Counter.t )
      Tezos_rpc.Service.t =
    let open Tezos_rpc in
    Service.subst1 Imported_protocol_plugin.Contract_services.S.counter

  let constants :
      ( [`GET],
        tezlink_rpc_context,
        tezlink_rpc_context,
        unit,
        unit,
        Protocol_types.Alpha_context.Constants.t )
      Tezos_rpc.Service.t =
    import_service Constants_services.all

  let chain_id :
      ([`GET], chain, chain, unit, unit, Chain_id.t) Tezos_rpc.Service.t =
    import_service Tezos_shell_services.Chain_services.S.chain_id
  let _header :
      ( [`GET],
        tezlink_rpc_context,
        tezlink_rpc_context,
        unit,
        unit,
        Block_services.block_header )
      Tezos_rpc.Service.t =
    import_service Block_services.S.header
end

let chain_directory_path = Tezos_shell_services.Chain_services.path

let block_directory_path =
  Tezos_rpc.Path.subst2
  @@ Tezos_rpc.Path.prefix
       chain_directory_path
       Tezos_shell_services.Block_services.path

let protocols () = Lwt_result_syntax.return Tezlink_protocols.current

let version () =
  (* TODO: #7857 need proper implementation *)
  Lwt_result_syntax.return Tezlink_version.mock

(** Builds the directory registering services under `/chains/<main>/blocks/<head>/...`. *)
let register_block_services (module Backend : Tezlink_backend_sig.S) base_dir =
  let dir =
    Tezos_rpc.Directory.empty
    |> register_with_conversion
         ~service:Imported_services.current_level
         ~impl:(fun {block; chain} query () ->
           Backend.current_level chain block ~offset:query.offset)
         ~convert_output:Protocol_types.Level.convert
    |> register ~service:Imported_services.protocols ~impl:(fun _ _ () ->
           protocols ())
    |> register
         ~service:Imported_services.balance
         ~impl:(fun ({chain; block}, contract) _ _ ->
           Backend.balance chain block contract)
    |> register
         ~service:Imported_services.manager_key
         ~impl:(fun ({chain; block}, contract) _ _ ->
           Backend.manager_key chain block contract)
    |> register_with_conversion
         ~service:Imported_services.counter
         ~impl:(fun ({block; chain}, contract) () () ->
           Backend.counter chain block contract)
         ~convert_output:Protocol_types.Counter.of_z
    |> register_with_conversion
         ~service:Imported_services.constants
         ~impl:(fun {block; chain} () () -> Backend.constants chain block)
         ~convert_output:Tezlink_constants.convert
  in
  Tezos_rpc.Directory.prefix
    block_directory_path
    (Tezos_rpc.Directory.map
       (fun (((), chain), block) -> make_env chain block)
       dir)
  |> Tezos_rpc.Directory.merge base_dir

let register_chain_services ~l2_chain_id
    (module Backend : Tezlink_backend_sig.S) base_dir =
  let dir =
    Tezos_rpc.Directory.empty
    |> register_with_conversion
         ~service:Imported_services.chain_id
         ~impl:(fun chain () () ->
           Lwt_result_syntax.return (l2_chain_id, chain))
         ~convert_output:(fun (l2_chain_id, chain) ->
           Result_syntax.return
           @@ Protocol_types.tezlink_to_tezos_chain_id_exn ~l2_chain_id chain)
    |> Tezos_rpc.Directory.map (fun ((), chain) -> Lwt.return chain)
  in
  Tezos_rpc.Directory.merge
    base_dir
    (Tezos_rpc.Directory.prefix chain_directory_path dir)

(** Builds the root directory. *)
let build_dir ~l2_chain_id backend =
  Tezos_rpc.Directory.empty
  |> register_block_services backend
  |> register_chain_services ~l2_chain_id backend
  |> register ~service:Imported_services.version ~impl:(fun () () () ->
         version ())

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
