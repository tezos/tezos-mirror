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

let resolve_plugin_by_hash proto_hash =
  let open Lwt_result_syntax in
  let plugin_opt = Dal_plugin.get proto_hash in
  match plugin_opt with
  | None ->
      let*! () =
        match !last_failed_protocol with
        | Some hash when Protocol_hash.equal hash proto_hash -> Lwt.return_unit
        | _ ->
            last_failed_protocol := Some proto_hash ;
            Event.emit_no_protocol_plugin ~proto_hash
      in
      tzfail (No_plugin_for_proto {proto_hash})
  | Some plugin ->
      let*! () = Event.emit_protocol_plugin_resolved ~proto_hash in
      return plugin

let resolve_plugin_for_level cctxt ~level =
  let open Lwt_result_syntax in
  let* protocols =
    Chain_services.Blocks.protocols cctxt ~block:(`Level level) ()
  in
  let proto_hash = protocols.next_protocol in
  resolve_plugin_by_hash proto_hash

let add_plugin_for_level cctxt plugins
    (protocols : Chain_services.Blocks.protocols) ~level =
  let open Lwt_result_syntax in
  let* plugin = resolve_plugin_by_hash protocols.next_protocol in
  let block = `Level level in
  let* header = Shell_services.Blocks.Header.shell_header cctxt ~block () in
  let proto_level = header.proto_level in
  let (module Plugin) = plugin in
  let+ proto_parameters = Plugin.get_constants `Main block cctxt in
  Plugins.add plugins ~first_level:level ~proto_level plugin proto_parameters

(* This function performs a (kind of) binary search to search for all values
   that satisfy the given condition [cond] on values. There is bijection between
   values and levels (which are here just positive int32 integers). The
   functions [to_level] and [from_level] retrieve the associates levels/values
   from given values/levels (respectively). The function [no_satisfying_value l1
   l2] returns true iff no value satisfying [cond] is present in the interval
   [l1, l2] (both inclusive). The search is performed between (the levels
   associated to) the values [first] and [last] (both inclusive). *)
let binary_search (cond : 'a -> bool) (no_satisfying_value : 'a -> 'a -> bool)
    (to_level : 'a -> int32) (from_level : int32 -> 'a tzresult Lwt.t)
    ~(first : 'a) ~(last : 'a) =
  let open Lwt_result_syntax in
  let rec search ~first ~last acc =
    if no_satisfying_value first last then return acc
    else
      let first_level = to_level first in
      let last_level = to_level last in
      if first_level >= last_level then
        (* search ended *)
        if cond last then return (last :: acc) else return acc
      else if Int32.succ first_level = last_level then
        (* search ended as well *)
        let acc = if cond last then last :: acc else acc in
        return @@ if cond first then first :: acc else acc
      else
        let mid_level =
          Int32.(add first_level (div (sub last_level first_level) 2l))
        in
        let* mid = from_level mid_level in
        let* acc = search ~first ~last:mid acc in
        search ~first:mid ~last acc
  in
  search ~first ~last []

let migration protocols =
  not
  @@ Protocol_hash.equal
       protocols.Chain_services.Blocks.current_protocol
       protocols.Chain_services.Blocks.next_protocol

type level_with_protos = {
  level : int32;
  protocols : Chain_services.Blocks.protocols;
}

(* Return the smallest levels between [first_level] and [last_level] for which a
   different plugin should be added. *)
let find_first_levels cctxt ~first_level ~last_level =
  let open Lwt_result_syntax in
  let to_level {level; _} = level in
  let from_level level =
    let* protocols =
      Chain_services.Blocks.protocols cctxt ~block:(`Level level) ()
    in
    return {level; protocols}
  in
  let cond {protocols; _} = migration protocols in
  let no_satisfying_value first last =
    let first_proto = first.protocols.Chain_services.Blocks.next_protocol in
    let last_proto = last.protocols.Chain_services.Blocks.next_protocol in
    Protocol_hash.equal first_proto last_proto
  in
  let* first = from_level first_level in
  let* last = from_level last_level in
  (* Performs a binary search between [first_working_level] and [last_level]
     to search for migration levels. *)
  let* migration_levels =
    binary_search cond no_satisfying_value to_level from_level ~first ~last
  in
  let sorted_levels =
    List.sort
      (fun {level = level1; _} {level = level2; _} ->
        Int32.compare level1 level2)
      migration_levels
  in
  (* We need to add the plugin for [first_level] even if it's not a migration
     level. *)
  match sorted_levels with
  | [] -> return [first]
  | {level; _} :: _ when first.level <> level -> return (first :: sorted_levels)
  | _ -> return sorted_levels

let initial_plugins cctxt ~first_level ~last_level =
  let open Lwt_result_syntax in
  let* first_levels = find_first_levels cctxt ~first_level ~last_level in
  List.fold_left_es
    (fun plugins {level; protocols} ->
      add_plugin_for_level cctxt plugins protocols ~level)
    Plugins.empty
    first_levels

let may_add cctxt plugins ~first_level ~proto_level =
  let open Lwt_result_syntax in
  let add first_level =
    let* plugin = resolve_plugin_for_level cctxt ~level:first_level in
    let (module Plugin) = plugin in
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

(* This function fetches the protocol plugins for levels in the past for which
   the node may need a plugin, namely for adding skip list cells, or for
   obtaining the protocol parameters.

   Concerning the skip list, getting the plugin is (almost) necessary as skip
   list cells are stored in the storage for a certain period and
   [store_skip_list_cells] needs the L1 context for levels in this period. (It
   would actually not be necessary to go as far in the past, because the
   protocol parameters and the relevant encodings do not change for now, so the
   head plugin could be used). *)
let get_proto_plugins cctxt profile_ctxt ~last_processed_level ~first_seen_level
    ~head_level proto_parameters =
  let storage_period =
    Profile_manager.get_storage_period
      profile_ctxt
      proto_parameters
      ~head_level
      ~first_seen_level
  in
  let first_level =
    Int32.max
      (match last_processed_level with None -> 1l | Some level -> level)
      Int32.(sub head_level (of_int storage_period))
  in
  let first_level =
    if Profile_manager.supports_refutations profile_ctxt then
      Int32.sub
        first_level
        (Int32.of_int (History_check.skip_list_offset proto_parameters))
    else
      (* The DAL node may need the protocol parameters [attestation_lag] in the
         past wrt to the head level. *)
      Int32.sub first_level (Int32.of_int proto_parameters.attestation_lag)
  in
  let first_level = Int32.(max 1l first_level) in
  initial_plugins cctxt ~first_level ~last_level:head_level
