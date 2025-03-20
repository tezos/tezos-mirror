(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
module Imported_protocol = Tezos_protocol_021_PsQuebec
module Imported_protocol_plugin = Tezos_protocol_plugin_021_PsQuebec
module Imported_protocol_parameters = Tezos_protocol_021_PsQuebec_parameters

(* The output type of the current_level service but with less duplicated
   information. Can be changed, as long as the [conversion_encoding] is also
   changed. *)
type level = {level : int32; cycle : int32; cycle_position : int32}

type error += Serialization_for_conversion of string * string

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.conversion_failure"
    ~title:"Failed to convert a value through serialization"
    ~description:
      "Failed to convert a value to a protocol private datatype through \
       serialization."
    ~pp:(fun ppf (name, error) ->
      Format.fprintf
        ppf
        "Failed to convert a value of type %s to a protocol private datatype \
         through serialization: %s"
        name
        error)
    Data_encoding.(obj2 (req "type_name" string) (req "error_msg" string))
    (function
      | Serialization_for_conversion (name, error) -> Some (name, error)
      | _ -> None)
    (fun (name, error) -> Serialization_for_conversion (name, error))

(** [convert_using_serialization ~dst ~src value] Conversion from one type with
    encoding [src] to another with encoding [dst], through serialization.
    Costly, but useful to build instances of a type when no builder is
    accessible. *)
let convert_using_serialization ~name ~dst ~src value =
  let open Result_syntax in
  let* bytes =
    Data_encoding.Binary.to_bytes src value
    |> Result.map_error_e (fun e ->
           tzfail
           @@ Serialization_for_conversion
                ( name,
                  Format.asprintf "%a" Data_encoding.Binary.pp_write_error e ))
  in
  Data_encoding.Binary.of_bytes dst bytes
  |> Result.map_error_e (fun e ->
         tzfail
         @@ Serialization_for_conversion
              (name, Format.asprintf "%a" Data_encoding.Binary.pp_read_error e))

(* Module importing, amending, and converting, protocol types. Those types
   might be difficult to actually build, so we define conversion function from
   local types to protocol types. *)
module Protocol_types = struct
  module Alpha_context = Imported_protocol.Protocol.Alpha_context
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
      convert_using_serialization
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

let register_protocol_service ~dir ~service ~impl ~convert_output =
  Tezos_rpc.Directory.register dir service (wrap convert_output impl)

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
end

type block = Tezos_shell_services.Block_services.block

type chain = Tezos_shell_services.Chain_services.chain

let block_directory_path =
  Tezos_rpc.Path.subst2
  @@ Tezos_rpc.Path.prefix
       Tezos_shell_services.Chain_services.path
       Tezos_shell_services.Block_services.path

type level_query = Imported_services.level_query = {offset : int32}

type tezos_services_implementation = {
  current_level :
    chain -> block -> Imported_services.level_query -> level tzresult Lwt.t;
}

(** Builds the directory registering services under `.../helpers`. *)
let build_helper_dir impl =
  let dir : tezlink_rpc_context Tezos_rpc.Directory.t =
    Tezos_rpc.Directory.empty
  in
  register_protocol_service
    ~dir
    ~service:Imported_services.current_level
    ~impl:(fun {block; chain} query () -> impl.current_level chain block query)
    ~convert_output:Protocol_types.Level.convert

(** Builds the root director. *)
let build_dir impl =
  let helper_dir = build_helper_dir impl in
  Tezos_rpc.Directory.prefix
    block_directory_path
    (Tezos_rpc.Directory.map
       (fun (((), chain), block) -> make_env chain block)
       helper_dir)

let tezlink_root = Tezos_rpc.Path.(open_root / "tezlink")

(* module entrypoint *)
let register_tezlink_services impl =
  let directory = build_dir impl in
  let tezlink_directory = Tezos_rpc.Directory.prefix tezlink_root directory in
  Evm_directory.init_from_resto_directory tezlink_directory
