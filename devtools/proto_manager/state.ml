(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type snapshot_label = {name : string; version : int}

type label = Snapshot of snapshot_label | Stabilise of string

type t = {
  commits : int;
  protocol_source : string;
  protocol_target : string;
  target_label : label;
  capitalize_label : string;
  git_dir : string;
}

let pp_label fmt = function
  | Stabilise label -> Format.pp_print_string fmt label
  | Snapshot label -> Format.fprintf fmt "%03d_%s" label.version label.name

let pp_label_name fmt = function
  | Stabilise label -> Format.pp_print_string fmt label
  | Snapshot label -> Format.fprintf fmt "%s" label.name

let exit ~__LOC__ (state : t) =
  let open Log in
  error "Exiting because of an error at %s" __LOC__ ;
  if state.commits > 0 then
    printfln
      "@[<v 2>@{<green>To clean up git commits run:@}@,\
       git reset -- hard  HEAD~%i"
      state.commits ;
  printfln
    "@[<v 0>@[<v 2>@{<green>To clean up other created files:@}@,\
     rm -rf docs/%a@,\
     rm -rf src/proto_%a@]@,\
     exiting ...@]"
    pp_label_name
    state.target_label
    pp_label
    state.target_label ;
  exit 1

let create ~git_dir ~protocol_source ~protocol_target ~target_pattern =
  let target_label =
    match String.split_on_char '_' protocol_target with
    | [name; version] -> (
        match int_of_string_opt version with
        | Some version -> Snapshot {name; version}
        | None ->
            Log.error "Cannot parse protocol target version." ;
            Log.error "Protocol target should be of form '%s'." target_pattern ;
            Stdlib.exit 1)
    | [name] -> Stabilise name
    | _ ->
        Log.error "Cannot parse protocol target." ;
        Log.error "Protocol target should be of form '%s'." target_pattern ;
        Stdlib.exit 1
  in
  {
    commits = 0;
    protocol_source;
    protocol_target;
    target_label;
    capitalize_label = String.capitalize_ascii protocol_source;
    git_dir;
  }
