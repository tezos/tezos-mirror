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

  type proto_plugin = {
    proto_level : int;
    plugin : (module Dal_plugin.T);
    proto_parameters : Types.proto_parameters;
  }

  type t = proto_plugin LevelMap.t

  let empty = LevelMap.empty

  let add t ~first_level ~proto_level plugin proto_parameters =
    LevelMap.add first_level {proto_level; plugin; proto_parameters} t

  let to_list t =
    LevelMap.bindings t
    |> List.map
         (fun (_block_level, {proto_level = _; plugin; proto_parameters = _}) ->
           plugin)
end

let singleton = Plugins.add Plugins.empty

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

let last_failed_protocol = ref None

let resolve_plugin_by_hash ?(emit_failure_event = true) ~start_level proto_hash
    =
  let open Lwt_result_syntax in
  let plugin_opt = Dal_plugin.get proto_hash in
  match plugin_opt with
  | None ->
      let*! () =
        match !last_failed_protocol with
        | Some hash when Protocol_hash.equal hash proto_hash -> Lwt.return_unit
        | _ ->
            last_failed_protocol := Some proto_hash ;
            if emit_failure_event then Event.emit_no_protocol_plugin ~proto_hash
            else Lwt.return_unit
      in
      tzfail (No_plugin_for_proto {proto_hash})
  | Some plugin ->
      let*! () = Event.emit_protocol_plugin_resolved ~proto_hash ~start_level in
      return plugin

let may_add cctxt plugins ~first_level ~proto_level =
  let open Lwt_result_syntax in
  let add first_level =
    let* protocols =
      Chain_services.Blocks.protocols cctxt ~block:(`Level first_level) ()
    in
    let proto_hash = protocols.next_protocol in
    let* ((module Plugin) as plugin) =
      resolve_plugin_by_hash proto_hash ~start_level:first_level
    in
    let+ proto_parameters =
      Plugin.get_constants `Main (`Level first_level) cctxt
    in
    Plugins.add plugins ~proto_level ~first_level plugin proto_parameters
  in
  let plugin_opt = Plugins.LevelMap.min_binding_opt plugins in
  match plugin_opt with
  | None -> add first_level
  | Some (_, Plugins.{proto_level = prev_proto_level; _})
    when prev_proto_level < proto_level ->
      add first_level
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

(* Say that [plugins = [(level_1, plugin_1); ... ; (level_n, plugin_n)]]. We
   have [level_1 > ... > level_n]. We return the plugin [plugin_i] with the
   smallest [i] such that [level_i <= level]. *)
let get_plugin_and_parameters_for_level plugins ~level =
  let open Result_syntax in
  let plugin_opt =
    Plugins.LevelMap.to_seq plugins
    |> Seq.find (fun (plugin_first_level, _) -> level >= plugin_first_level)
  in
  match plugin_opt with
  | None -> tzfail @@ No_plugin_for_level {level}
  | Some (_first_level, Plugins.{plugin; proto_level = _; proto_parameters}) ->
      return (plugin, proto_parameters)

include Plugins

(* [highest_level] is the highest known level of the protocol for which we
   register the plugin. We use this level when getting the protocol parameters,
   because the first level of the protocol (the activation level) might be too
   old, in that the L1 node might not have the context for that protocol. *)
let add_plugin_for_proto cctxt plugins
    Chain_services.
      {protocol; proto_level; activation_block = _, activation_level}
    highest_level =
  let open Lwt_result_syntax in
  let* plugin =
    resolve_plugin_by_hash
      ~emit_failure_event:false
      protocol
      ~start_level:activation_level
  in
  let block = `Level highest_level in
  let (module Plugin) = plugin in
  let+ proto_parameters = Plugin.get_constants `Main block cctxt in
  Plugins.add
    plugins
    ~first_level:activation_level
    ~proto_level
    plugin
    proto_parameters

let get_supported_proto_plugins cctxt ~head_level =
  let open Lwt_result_syntax in
  let* protocols = Chain_services.Protocols.list cctxt () in
  (* [protocols] are ordered increasingly wrt their protocol level; we treat
     them from the last one backwards, because we stop at the most recent one
     which cannot be registered *)
  let protocols = List.rev protocols in
  let*! res =
    List.fold_left_es
      (fun (plugins, highest_level) protocol_info ->
        let*! res =
          add_plugin_for_proto cctxt plugins protocol_info highest_level
        in
        let _hash, level = protocol_info.activation_block in
        let highest_level = Int32.pred level in
        match res with
        | Ok plugins -> return (plugins, highest_level)
        | Error [No_plugin_for_proto {proto_hash}]
          when Protocol_hash.equal proto_hash protocol_info.protocol ->
            fail (`End_loop_ok plugins)
        | Error err -> fail (`End_loop_nok err))
      (Plugins.empty, head_level)
      protocols
  in
  match res with
  | Ok (plugins, _) | Error (`End_loop_ok plugins) -> return plugins
  | Error (`End_loop_nok err) -> fail err

let has_plugins plugins = not (LevelMap.is_empty plugins)
