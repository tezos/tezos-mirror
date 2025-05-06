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
