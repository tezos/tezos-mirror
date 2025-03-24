(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Run a round of matching for levels that are both available in L1 and the EVM
   node. *)
val run : Sqlite.t -> unit tzresult Lwt.t
