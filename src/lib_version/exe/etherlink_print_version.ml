(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* this is a script run at build time to print out the current version of the
   node *)

open Version
open Current_git_info

let help_string =
  "This script prints out the current version of \n\
   Etherlink as it is deduced from the git tag of the current branch.\n\
   print_version \
   [--product|--major|--minor|--additional-info|--full|--full-with-commit|--commit]"

let () =
  match Sys.argv with
  | [|_; "--product"|] ->
      print_endline (string_of_product etherlink_version.product)
  | [|_; "--major"|] -> print_endline (string_of_int etherlink_version.major)
  | [|_; "--minor"|] -> print_endline (string_of_int etherlink_version.minor)
  | [|_; "--additional-info"|] ->
      print_endline
        (string_of_additional_info etherlink_version.additional_info)
  | [|_; "--full"|] | [|_|] -> print_endline (to_string etherlink_version)
  | [|_; "--full-with-commit"|] ->
      print_endline
        Tezos_version_value.Bin_version.etherlink_simple_version_string
  | [|_; "--commit"|] ->
      print_endline Tezos_version_value.Current_git_info.abbreviated_commit_hash
  | [|_; "--json"|] ->
      print_endline
        (to_json
           etherlink_version
           Tezos_version_value.Current_git_info.abbreviated_commit_hash)
  | [|_; "--help"|] -> print_endline help_string
  | _ ->
      print_endline help_string ;
      prerr_endline
        ("invalid argument: " ^ String.concat " " (Array.to_list Sys.argv)) ;
      exit 1
