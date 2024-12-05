(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [write_file ?runner ~contents filename]
    Write [contents] into [filename], using [ssh] if need (i.e. if [runner]
    is defined).
*)
val write_file : ?runner:Runner.t -> contents:string -> string -> unit Lwt.t
