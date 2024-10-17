(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Simple wrapper to handle the arguments of the agnostic baker. *)
type args = {
  node_endpoint : string;
  base_dir : string option;
  binaries_directory : string option;
  baker_args : string list;
}

(** [parse_args args] is a raw utility that aims to parse the give
    arguments from the command line and to return, respectively, the
    endpoint, base_dir, binaries_directory and baker_args. *)
val parse_args : string array -> args
