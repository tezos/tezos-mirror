(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let binaries_directory_arg = "--binaries-directory"

let endpoint_arg = "--endpoint"

let endpoint_short_arg = "-E"

let base_dir_arg = "--base-dir"

let base_dir_short_arg = "--b"

let help_arg = "--help"

let print_help () =
  Format.printf
    "Usage:\n\
    \  octez-agnostic-baker [OCTEZ-AGNOSTIC-BAKER-COMMANDS] -- \
     [OCTEZ-BAKER-COMMANDS]@.@." ;
  Format.printf
    "OCTEZ-AGNOSTIC-BAKER-COMMANDS:\n\
    \  %s: display help\n\
    \  %s: path to the octez-baker binaries@.@."
    help_arg
    binaries_directory_arg ;
  Format.printf
    "OCTEZ-BAKER-COMMANDS:\n Run ./octez-baker-<PROTOCOL_HASH> --help@."

let help_cmd args =
  if List.mem ~equal:String.equal help_arg args then (
    print_help () ;
    exit 0)
  else ()

let version_cmd args =
  if List.mem ~equal:String.equal "--version" args then (
    Format.printf "%s@." Tezos_version_value.Bin_version.octez_version_string ;
    exit 0)
  else ()

let split_args ?(on = "--") =
  let rec loop acc = function
    | [] -> (List.rev acc, [])
    | hd :: tl when hd = on -> (List.rev acc, tl)
    | hd :: tl -> loop (hd :: acc) tl
  in
  loop []

let get_arg_value ~arg ?(short_arg = "") =
  let rec loop = function
    | [] -> None
    | x :: y :: _ when x = arg || x = short_arg -> Some y
    | _ :: l -> loop l
  in
  loop

let get_endpoint = get_arg_value ~arg:endpoint_arg ~short_arg:endpoint_short_arg

let get_binaries_directory = get_arg_value ~arg:binaries_directory_arg

let fail_on_empty_baker_args baker_args =
  if List.is_empty baker_args then (
    Format.eprintf
      "Cannot run agnostic baker without any baker arguments. Please refer to \
       the following help:@." ;
    print_help () ;
    exit 1)

let get_base_dir = get_arg_value ~arg:base_dir_arg ~short_arg:base_dir_short_arg

let parse_args all_args =
  let all_args = Array.to_list all_args in
  (* Specific vesrion case *)
  let () = version_cmd all_args in
  (* Remove the binary path *)
  let all_args = Option.value ~default:[] (List.tl all_args) in
  (* Split agnostic baker and baker arguments, that aims to be delimited by -- *)
  let agnostic_baker_args, baker_args = split_args all_args in
  let () = fail_on_empty_baker_args baker_args in
  let () = help_cmd agnostic_baker_args in
  let endpoint =
    Option.value
      ~default:Parameters.default_node_endpoint
      (get_endpoint baker_args)
  in
  let binaries_directory = get_binaries_directory agnostic_baker_args in
  let base_dir = get_base_dir baker_args in
  (endpoint, base_dir, binaries_directory, baker_args)
