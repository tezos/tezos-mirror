(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups
   Invocation:   dune exec tezt/tests/main.exe -- --file sc_rollup_migration.ml
*)

val register : migrate_from:Protocol.t -> migrate_to:Protocol.t -> unit
