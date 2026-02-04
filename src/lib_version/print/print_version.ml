(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Version
open Tezos_version_value.Current_git_info

let print_version version =
  let help_string =
    Format.sprintf
      "This script prints out the current version of \n\
       %s as it is deduced from the git tag of the current branch.\n\
       print_version \
       [--product|--major|--minor|--build|--additional-info|--full|--full-with-commit|--commit|--json]"
    @@ string_of_product version.product
  in
  match Sys.argv with
  | [|_; "--product"|] -> print_endline (string_of_product version.product)
  | [|_; "--major"|] -> print_endline (string_of_int version.major)
  | [|_; "--minor"|] -> print_endline (string_of_int version.minor)
  | [|_; "--build"|] -> print_endline (string_of_int version.build)
  | [|_; "--additional-info"|] ->
      print_endline (string_of_additional_info version.additional_info)
  | [|_; "--full"|] | [|_|] -> print_endline (to_string version)
  | [|_; "--full-with-commit"|] ->
      print_endline
      @@ Tezos_version_value.Bin_version.simple_version_string version
  | [|_; "--commit"|] -> print_endline abbreviated_commit_hash
  | [|_; "--json"|] -> print_endline (to_json version abbreviated_commit_hash)
  | [|_; "--help"|] -> print_endline help_string
  | _ ->
      print_endline help_string ;
      prerr_endline
        ("invalid argument: " ^ String.concat " " (Array.to_list Sys.argv)) ;
      exit 1
