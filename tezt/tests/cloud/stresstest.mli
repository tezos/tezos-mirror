(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Stresstest parameters
    - [tps]: targeted number of transactions per second
    - [seed]: seed used for stresstest traffic generation
 *)
type t = {tps : int; seed : int}

val encoding : t Data_encoding.t

val typ : t Clap.typ
