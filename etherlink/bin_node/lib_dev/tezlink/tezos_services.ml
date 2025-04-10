(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Tezlink_imports

type level = Tezos_types.level

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

(* TODO *)
type tezlink_rpc_context = {
  block : Tezos_shell_services.Block_services.block;
  chain : Tezos_shell_services.Chain_services.chain;
}

(** Builds a [tezlink_rpc_context] from paths parameters. *)
let make_env (chain : Tezos_shell_services.Chain_services.chain)
    (block : Tezos_shell_services.Block_services.block) :
    tezlink_rpc_context Lwt.t =
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

  let contract_arg_path s =
    let contract_path = Imported_protocol_plugin.RPC.Contract.S.path in
    let contract_args = Imported_protocol.Alpha_context.Contract.rpc_arg in
    Imported_env.RPC_path.(contract_path /: contract_args / s)

  (* TODO: #7876 Expose and import the service definition from the plugin. *)
  let balance :
      ( [`GET],
        tezlink_rpc_context,
        tezlink_rpc_context * Tezos_types.Contract.t,
        unit,
        unit,
        Tezos_types.Tez.t )
      Tezos_rpc.Service.t =
    Tezos_rpc.Service.subst1
    (* TODO: #7876 should be imported *)
    @@ Tezos_rpc.Service.get_service
         ~description:
           "The spendable balance of a contract (in mutez), also known as \
            liquid balance. Corresponds to tez owned by the contract that are \
            neither staked, nor in unstaked requests, nor in frozen bonds. \
            Identical to the 'spendable' RPC."
         ~query:Tezos_rpc.Query.empty
         ~output:Tezos_types.Tez.encoding
         (contract_arg_path "balance")

  let constants :
      ( [`GET],
        tezlink_rpc_context,
        tezlink_rpc_context,
        unit,
        unit,
        Protocol_types.Alpha_context.Constants.t )
      Tezos_rpc.Service.t =
    import_service Constants_services.all
end

let block_directory_path =
  Tezos_rpc.Path.subst2
  @@ Tezos_rpc.Path.prefix
       Tezos_shell_services.Chain_services.path
       Tezos_shell_services.Block_services.path

let protocols () = Lwt_result_syntax.return Tezlink_protocols.current

let version () =
  (* TODO: #7857 need proper implementation *)
  Lwt_result_syntax.return Tezlink_version.mock

(** Builds the directory registering services under `/chains/<main>/blocks/<head>/...`. *)
let build_block_dir (module Backend : Tezlink_backend_sig.S) =
  let dir : tezlink_rpc_context Tezos_rpc.Directory.t =
    Tezos_rpc.Directory.empty
  in
  dir
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
  |> register_with_conversion
       ~service:Imported_services.constants
       ~impl:(fun {block; chain} () () -> Backend.constants chain block)
       ~convert_output:Tezlink_constants.convert

(** Builds the root director. *)
let build_dir backend =
  let helper_dir = build_block_dir backend in
  let root_directory =
    Tezos_rpc.Directory.prefix
      block_directory_path
      (Tezos_rpc.Directory.map
         (fun (((), chain), block) -> make_env chain block)
         helper_dir)
  in
  Tezos_rpc.Directory.register
    root_directory
    Imported_services.version
    (fun () () () -> version ())

let tezlink_root = Tezos_rpc.Path.(open_root / "tezlink")

(* module entrypoint *)
let register_tezlink_services backend =
  let directory = build_dir backend in
  let directory =
    Tezos_rpc.Directory.register_describe_directory_service
      directory
      Tezos_rpc.Service.description_service
  in
  let tezlink_directory = Tezos_rpc.Directory.prefix tezlink_root directory in
  tezlink_directory
