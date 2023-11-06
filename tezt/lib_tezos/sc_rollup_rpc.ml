(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*****************************************************************************)

open RPC_core

let get_global_block_ticks ?(block = "head") () =
  make GET ["global"; "block"; block; "ticks"] JSON.as_int

let get_global_block_state_hash ?(block = "head") () =
  make GET ["global"; "block"; block; "state_hash"] JSON.as_string
