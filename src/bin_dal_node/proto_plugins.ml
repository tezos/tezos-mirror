(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Plugins = struct
  module LevelMap = Map.Make (struct
    type t = Int32.t

    (* keys are ordered descendingly *)
    let compare a b = compare b a
  end)

  type proto_plugin = {proto_level : int; plugin : (module Dal_plugin.T)}

  type t = proto_plugin LevelMap.t

  let empty = LevelMap.empty

  let add t ~first_level ~proto_level plugin =
    LevelMap.add first_level {proto_level; plugin} t

  let to_list t =
    LevelMap.bindings t
    |> List.map (fun (_block_level, {proto_level = _; plugin}) -> plugin)
end

type error += No_plugin_for_proto of {proto_hash : Protocol_hash.t}

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.no_plugin_for_proto"
    ~title:"DAL node: no plugin for protocol"
    ~description:"DAL node: no plugin for the protocol %a"
    ~pp:(fun ppf proto_hash ->
      Format.fprintf
        ppf
        "No plugin for the protocol %a."
        Protocol_hash.pp
        proto_hash)
    Data_encoding.(obj1 (req "proto_hash" Protocol_hash.encoding))
    (function No_plugin_for_proto {proto_hash} -> Some proto_hash | _ -> None)
    (fun proto_hash -> No_plugin_for_proto {proto_hash})

let resolve_plugin_by_hash proto_hash =
  let open Lwt_result_syntax in
  let plugin_opt = Dal_plugin.get proto_hash in
  match plugin_opt with
  | None ->
      let*! () = Event.(emit no_protocol_plugin proto_hash) in
      tzfail (No_plugin_for_proto {proto_hash})
  | Some plugin ->
      let*! () = Event.(emit protocol_plugin_resolved proto_hash) in
      return plugin

let resolve_plugin_for_level cctxt ~level =
  let open Lwt_result_syntax in
  let* protocols =
    Tezos_shell_services.Chain_services.Blocks.protocols
      cctxt
      ~block:(`Level level)
      ()
  in
  let proto_hash = protocols.next_protocol in
  resolve_plugin_by_hash proto_hash

let initial_plugins cctxt ~current_level ~attestation_lag =
  let open Lwt_result_syntax in
  let last_level = current_level in
  let first_level =
    Int32.max 1l (Int32.sub current_level (Int32.of_int (attestation_lag + 2)))
  in
  let block = `Level first_level in
  let* protocols =
    Tezos_shell_services.Chain_services.Blocks.protocols cctxt ~block ()
  in
  let first_proto = protocols.next_protocol in
  let* plugin = resolve_plugin_by_hash first_proto in
  let* header = Shell_services.Blocks.Header.shell_header cctxt ~block () in
  let proto_level = header.proto_level in
  let proto_plugins =
    Plugins.add Plugins.empty ~first_level ~proto_level plugin
  in
  let* last_protocols =
    Chain_services.Blocks.protocols cctxt ~block:(`Level last_level) ()
  in
  let last_proto = last_protocols.Chain_services.Blocks.next_protocol in
    if Protocol_hash.equal first_proto last_proto then
      (* There's no migration in between, we're done. *)
      return proto_plugins
    else
      (* There was a migration in between; we search the migration level and then
         we add the plugin *)
      let rec find_migration_level level protocols =
        if
          Protocol_hash.equal
            first_proto
            protocols.Chain_services.Blocks.current_protocol
        then return level
        else
          let block = `Level (Int32.pred level) in
          let* protocols = Chain_services.Blocks.protocols cctxt ~block () in
          find_migration_level (Int32.pred level) protocols
      in
      let* migration_level = find_migration_level last_level last_protocols in
      let* plugin = resolve_plugin_by_hash last_proto in
      let* header =
        Shell_services.Blocks.Header.shell_header
          cctxt
          ~block:(`Level migration_level)
          ()
      in
      let proto_level = header.proto_level in
      Plugins.add proto_plugins ~first_level:migration_level ~proto_level plugin
      |> return

let may_add cctxt plugins ~first_level ~proto_level =
  let open Lwt_result_syntax in
  let plugin_opt = Plugins.LevelMap.min_binding_opt plugins in
  match plugin_opt with
  | None ->
      let* plugin = resolve_plugin_for_level cctxt ~level:first_level in
      Plugins.add plugins ~proto_level ~first_level plugin |> return
  | Some (_, Plugins.{proto_level = prev_proto_level; _})
    when prev_proto_level < proto_level ->
      let* plugin = resolve_plugin_for_level cctxt ~level:first_level in
      Plugins.add plugins ~proto_level ~first_level plugin |> return
  | _ -> return plugins

type error += No_plugin_for_level of {level : int32}

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.no_plugin_for_given_level"
    ~title:"DAL node: no plugin for given level"
    ~description:"DAL node: no plugin for the given level"
    ~pp:(fun ppf level ->
      Format.fprintf ppf "No plugin for the level %ld." level)
    Data_encoding.(obj1 (req "level" int32))
    (function No_plugin_for_level {level} -> Some level | _ -> None)
    (fun level -> No_plugin_for_level {level})

let get_plugin_for_level plugins ~level =
  let open Result_syntax in
  let plugins = Plugins.LevelMap.bindings plugins in
  (* Say that [plugins = [(level_1, plugin_1); ... ; (level_n, plugin_n)]]. We
     have [level_1 > ... > level_n]. We return the plugin [plugin_i] with the
     smallest [i] such that [level_i <= level]. *)
  let rec find = function
    | [] -> tzfail @@ No_plugin_for_level {level}
    | (plugin_first_level, Plugins.{plugin; proto_level = _}) :: rest ->
        if level >= plugin_first_level then return plugin else find rest
  in
  find plugins

include Plugins
