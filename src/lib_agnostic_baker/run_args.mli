(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Simple wrapper to handle the arguments of the agnostic baker. *)
type args = {
  node_endpoint : string;
  base_dir : string option;
  baker_args : string list;
}

(** [parse_args args] is a raw utility that aims to parse the given
    arguments from the command line and to return, respectively, the
    [endpoint], [base_dir] and [baker_args]. *)
val parse_args : string array -> args
