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
