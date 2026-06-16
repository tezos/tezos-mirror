(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Tezt_core
open Tezt_core.Base
open Tezt_tezos

let parameter_check ~next json name expected =
  let value = JSON.(json |-> name |> as_int) in
  if value <> expected then
    Test.fail
      "%s: unexpected %s value, got %d, expected %d"
      (if next then "Next proto" else "Current proto")
      name
      value
      expected

let parameters_checks (state : Tezt_migration_registry.Register.state) =
  match state.network with
  | `Mainnet -> (
      match (state.protocol_parameters, state.next_protocol_parameters) with
      | Some parameters, Some next_parameters ->
          parameter_check ~next:false parameters "minimal_block_delay" 8 ;
          parameter_check ~next:true next_parameters "minimal_block_delay" 6 ;
          parameter_check ~next:false parameters "delay_increment_per_round" 4 ;
          parameter_check
            ~next:true
            next_parameters
            "delay_increment_per_round"
            3
      | None, Some _ ->
          Test.fail
            "Current protocol parameter not retrieved, impossible to do the \
             checks"
      | Some _, None ->
          Test.fail
            "Next protocol parameter not retrieved, impossible to do the checks"
      | None, None ->
          Test.fail
            "Current and Next Protocol parameter not retrieved, impossible to \
             do the checks")
  | _ -> ()

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
      parameters_checks state ;

      (* Perform specific T024 checks *)
      Log.info
        "Retrieving address registry amount and ensure that its greater than 1." ;
      let* registry =
        RPC.get_chain_block_context_address_registry ()
        |> RPC_core.call_json (Node.as_rpc_endpoint state.node)
        |> Lwt.map (fun {RPC_core.body; _} ->
               body |> JSON.as_string |> int_of_string)
      in
      if registry <> 1 then
        Test.fail "Address registry number should be 1, got %d" registry ;

      Log.info
        "Retrieving abaab activation level and ensure that it is null since \
         the threshold is not yet reached" ;
      let* abaab_activation_level =
        RPC.get_abaab_activation_level ()
        |> RPC_core.call_json (Node.as_rpc_endpoint state.node)
        |> Lwt.map (fun {RPC_core.body; _} -> body |> JSON.is_null)
      in
      if not abaab_activation_level then
        Test.fail "Abaab activation level not null" ;
      Log.info
        "Retrieving the tz4 baker number ratio and ensure that it is greater \
         than 0 since we already have tz4 baker on the network." ;
      let* tz4_baker_number_ratio =
        RPC.get_tz4_baker_number_ratio ()
        |> RPC_core.call_json (Node.as_rpc_endpoint state.node)
        |> Lwt.map (fun {RPC_core.body; _} ->
               body |> JSON.as_string |> String.split_on_char '%'
               |> Stdlib.List.hd |> float_of_string)
      in
      if not (tz4_baker_number_ratio > 0.) then
        Test.fail
          "tz4 Baker ratio should be higher than 0, got %f"
          tz4_baker_number_ratio ;
      return_unit)
    else Lwt.return_unit
  in
  Lwt.return state

let () = Tezt_migration_registry.Register.register ~title:"T024: checks" checks
