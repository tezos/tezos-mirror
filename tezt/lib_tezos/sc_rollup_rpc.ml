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

let get_global_block_total_ticks ?(block = "head") () =
  make GET ["global"; "block"; block; "total_ticks"] JSON.as_int

let get_global_block_state_current_level ?(block = "head") () =
  make GET ["global"; "block"; block; "state_current_level"] JSON.as_int

let get_global_block_status ?(block = "head") () =
  make GET ["global"; "block"; block; "status"] JSON.as_string

let get_global_block_outbox ?(block = "cemented") ~outbox_level () =
  make
    GET
    ["global"; "block"; block; "outbox"; string_of_int outbox_level; "messages"]
    Fun.id

let get_global_smart_rollup_address () =
  make GET ["global"; "smart_rollup_address"] JSON.as_string

let get_global_block_aux ?(block = "head") ?(path = []) () =
  make GET (["global"; "block"; block] @ path) Fun.id

let get_global_block = get_global_block_aux ~path:[]

let get_global_block_inbox = get_global_block_aux ~path:["inbox"]

let get_global_block_hash = get_global_block_aux ~path:["hash"]

let get_global_block_level = get_global_block_aux ~path:["level"]

let get_global_block_num_messages = get_global_block_aux ~path:["num_messages"]

let get_global_tezos_head () = make GET ["global"; "tezos_head"] Fun.id

let get_global_tezos_level () = make GET ["global"; "tezos_level"] Fun.id

type slot_header = {level : int; commitment : string; index : int}

let get_global_block_dal_slot_headers ?(block = "head") () =
  make GET ["global"; "block"; block; "dal"; "slot_headers"] (fun json ->
      JSON.(
        as_list json
        |> List.map (fun obj ->
               {
                 level = obj |-> "level" |> as_int;
                 commitment = obj |-> "commitment" |> as_string;
                 index = obj |-> "index" |> as_int;
               })))
