(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Tezt_core
open Tezt_core.Base
open Tezt_tezos

let checks (state : Tezt_migration_registry.Register.state) current_level =
  let open Lwt_syntax in
  let* () =
    (* Store the content of the 10 blocks before and after the migration block *)
    if
      current_level >= state.migration_level - 10
      && current_level <= state.migration_level + 10
    then (
      Log.info "Retrieving block %d data." current_level ;
      RPC.get_chain_block ()
      |> RPC_core.call_json (Node.as_rpc_endpoint state.node)
      |> Lwt.map (fun {RPC_core.body; _} ->
             JSON.encode_to_file
               (state.artifacts_dir_path // sf "%d.json" current_level)
               body))
    else Lwt.return_unit
  in
  let* () =
    if current_level = state.migration_level - 10 then (
      Log.info "Retrieving current protocol constants." ;
      let* parameters =
        RPC.get_chain_block_context_constants_parametric ()
        |> RPC_core.call_json (Node.as_rpc_endpoint state.node)
        |> Lwt.map (fun {RPC_core.body; _} -> body)
      in
      state.protocol_parameters <- Some parameters ;
      JSON.encode_to_file
        (state.artifacts_dir_path // "protocol_constants_before_migration.json")
        parameters ;
      Lwt.return_unit)
    else Lwt.return_unit
  in
  let* () =
    if current_level = state.migration_level + 10 then (
      Log.info "Retrieving next protocol constants." ;
      let* parameters =
        RPC.get_chain_block_context_constants_parametric ()
        |> RPC_core.call_json (Node.as_rpc_endpoint state.node)
        |> Lwt.map (fun {RPC_core.body; _} -> body)
      in
      state.next_protocol_parameters <- Some parameters ;
      JSON.encode_to_file
        (state.artifacts_dir_path // "protocol_constants_after_migration.json")
        parameters ;

      (* Perform specific Alpha checks *)
      return_unit)
    else Lwt.return_unit
  in
  Lwt.return state

let () = Tezt_migration_registry.Register.register ~title:"Alpha: checks" checks
