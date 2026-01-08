(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Errors

let may_lock_pidfile pidfile_opt f =
  match pidfile_opt with
  | None -> f ()
  | Some pidfile ->
      Lwt_lock_file.with_lock
        ~when_locked:
          (`Fail (Exn (Failure ("Failed to create the pidfile: " ^ pidfile))))
        ~filename:pidfile
        f

let check_node_version cctxt bypass allowed =
  let open Lwt_result_syntax in
  (* Parse and check allowed versions *)
  let*? allowed =
    let open Result_syntax in
    Option.map_e
      (fun allowed ->
        match
          Tezos_version_parser.version_commit (Lexing.from_string allowed)
        with
        | None -> tzfail (Node_version_malformatted allowed)
        | Some x -> return x)
      allowed
  in
  let is_allowed node_version
      (node_commit_info : Tezos_version.Octez_node_version.commit_info option) =
    match allowed with
    | None -> false
    | Some (v, c) -> (
        let c =
          Option.map
            (fun commit_hash ->
              Tezos_version.Octez_node_version.{commit_hash; commit_date = ""})
            c
        in
        match
          Tezos_version.Octez_node_version.partially_compare
            v
            c
            node_version
            node_commit_info
        with
        | None -> false
        | Some x -> x = 0)
  in
  if bypass then
    let*! () = Events.(emit node_version_check_bypass ()) in
    return_unit
  else
    let baker_version = Tezos_version_value.Current_git_info.octez_version in
    let (baker_commit_info
          : Tezos_version.Octez_node_version.commit_info option) =
      Some
        {
          commit_hash = Tezos_version_value.Current_git_info.commit_hash;
          commit_date = Tezos_version_value.Current_git_info.committer_date;
        }
    in
    let* node_version = Version_services.version cctxt in
    let*! () =
      Events.(
        emit
          node_version_check
          ( node_version.version,
            node_version.commit_info,
            baker_version,
            baker_commit_info ))
    in
    if is_allowed node_version.version node_version.commit_info then return_unit
    else
      match
        Tezos_version.Octez_node_version.partially_compare
          baker_version
          baker_commit_info
          node_version.version
          node_version.commit_info
      with
      | Some r when r <= 0 -> return_unit
      | _ ->
          tzfail
            (Node_version_incompatible
               {
                 node_version = node_version.version;
                 node_commit_info = node_version.commit_info;
                 baker_version;
                 baker_commit_info;
               })

(* This function checks that a DAL node endpoint was given,
   and that the specified DAL node is "healthy",
   (the DAL's nodes 'health' RPC is used for that). *)
let check_dal_node =
  let last_check_successful = ref false in
  fun without_dal dal_node_rpc_ctxt ->
    let open Lwt_result_syntax in
    let result_emit f x =
      let*! () = Events.emit f x in
      return_unit
    in
    match (dal_node_rpc_ctxt, without_dal) with
    | None, true ->
        (* The user is aware that no DAL node is running, since they explicitly
           used the [--without-dal] option. However, we do not want to reduce the
           exposition of bakers to warnings about DAL, so we keep it. *)
        result_emit Events.Commands.no_dal_node_provided ()
    | None, false -> tzfail No_dal_node_endpoint
    | Some _, true -> tzfail Incompatible_dal_options
    | Some ctxt, false -> (
        let*! health = Rpc_services.get_dal_health ctxt in
        match health with
        | Ok health -> (
            match health.status with
            | Tezos_dal_node_services.Types.Health.Up ->
                if !last_check_successful then return_unit
                else (
                  last_check_successful := true ;
                  result_emit Events.Commands.healthy_dal_node ())
            | _ ->
                last_check_successful := false ;
                result_emit
                  Events.Commands.unhealthy_dal_node
                  (ctxt#base, health))
        | Error _ ->
            last_check_successful := false ;
            result_emit Events.Commands.unreachable_dal_node ctxt#base)

let create_dal_node_rpc_ctxt endpoint =
  let open Tezos_rpc_http_client_unix in
  let rpc_config =
    {Tezos_rpc_http_client_unix.RPC_client_unix.default_config with endpoint}
  in
  let media_types =
    Tezos_rpc_http.Media_type.Command_line.of_command_line rpc_config.media_type
  in
  new RPC_client_unix.http_ctxt rpc_config media_types

let run_baker (module Plugin : Protocol_plugin_sig.S)
    Configuration.
      {
        pidfile;
        node_version_check_bypass;
        node_version_allowed;
        minimal_fees;
        minimal_nanotez_per_gas_unit;
        minimal_nanotez_per_byte;
        force_apply_from_round;
        keep_alive;
        liquidity_baking_vote;
        adaptive_issuance_vote;
        per_block_vote_file;
        extra_operations;
        dal_node_endpoint;
        without_dal;
        state_recorder;
        pre_emptive_forge_time;
        remote_calls_timeout;
        allow_signing_delay;
      } baking_mode sources cctxt =
  let open Lwt_result_syntax in
  Octez_baking_common.Signing_delay.enforce_signing_delay_gating
    ~allow:allow_signing_delay ;
  may_lock_pidfile pidfile @@ fun () ->
  let* () =
    check_node_version cctxt node_version_check_bypass node_version_allowed
  in
  let*! per_block_vote_file =
    if per_block_vote_file = None then
      (* If the votes file was not explicitly given, we
         look into default locations. *)
      Per_block_vote_file.lookup_default_vote_file_path cctxt
    else Lwt.return per_block_vote_file
  in
  let*! () =
    Events.warn_if_adaptive_issuance_vote_present ~adaptive_issuance_vote
  in
  (* We don't let the user run the baker without providing some
     option (CLI, file path, or file in default location) for
     the per-block votes. *)
  let* votes =
    Per_block_vote_file.load_per_block_votes_config
      ~default_liquidity_baking_vote:liquidity_baking_vote
      ~per_block_vote_file
  in
  let dal_node_rpc_ctxt =
    Option.map create_dal_node_rpc_ctxt dal_node_endpoint
  in
  let* () = check_dal_node without_dal dal_node_rpc_ctxt in
  Plugin.Baker_commands_helpers.run_baker
    cctxt
    ?dal_node_rpc_ctxt
    ~minimal_fees
    ~minimal_nanotez_per_gas_unit
    ~minimal_nanotez_per_byte
    ~votes
    ?extra_operations
    ?pre_emptive_forge_time
    ?force_apply_from_round
    ?remote_calls_timeout
    ~chain:cctxt#chain
    ?data_dir:baking_mode
    ~keep_alive
    ~state_recorder
    sources
