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

let get_local_batcher_queue () =
  make GET ["local"; "batcher"; "queue"] (fun json ->
      JSON.as_list json
      |> List.map @@ fun o ->
         let hash = JSON.(o |-> "hash" |> as_string) in
         let hex_msg = JSON.(o |-> "message" |-> "content" |> as_string) in
         (hash, Hex.to_string (`Hex hex_msg)))

let get_local_batcher_queue_msg_hash ~msg_hash =
  make GET ["local"; "batcher"; "queue"; msg_hash] (fun json ->
      if JSON.is_null json then failwith "Message is not in the queue"
      else
        let hex_msg = JSON.(json |-> "content" |> as_string) in
        (Hex.to_string (`Hex hex_msg), JSON.(json |-> "status" |> as_string)))

type simulation_result = {
  state_hash : string;
  status : string;
  output : JSON.t;
  inbox_level : int;
  num_ticks : int;
  insights : string option list;
}

let post_global_block_simulate ?(block = "head") ?(reveal_pages = [])
    ?(insight_requests = []) messages =
  let messages_json =
    `A (List.map (fun s -> `String Hex.(of_string s |> show)) messages)
  in
  let reveal_json =
    match reveal_pages with
    | [] -> []
    | pages ->
        [
          ( "reveal_pages",
            `A (List.map (fun s -> `String Hex.(of_string s |> show)) pages) );
        ]
  in
  let insight_requests_json =
    let insight_request_json insight_request =
      let insight_request_kind, key =
        match insight_request with
        | `Pvm_state_key key -> ("pvm_state", key)
        | `Durable_storage_key key -> ("durable_storage", key)
      in
      let x = `A (List.map (fun s -> `String s) key) in
      `O [("kind", `String insight_request_kind); ("key", x)]
    in
    [("insight_requests", `A (List.map insight_request_json insight_requests))]
  in
  let data =
    Data
      (`O
        ((("messages", messages_json) :: reveal_json) @ insight_requests_json))
  in
  make POST ["global"; "block"; block; "simulate"] ~data (fun obj ->
      JSON.
        {
          state_hash = obj |-> "state_hash" |> as_string;
          status = obj |-> "status" |> as_string;
          output = obj |-> "output";
          inbox_level = obj |-> "inbox_level" |> as_int;
          num_ticks = obj |-> "num_ticks" |> as_string |> int_of_string;
          insights = obj |-> "insights" |> as_list |> List.map as_string_opt;
        })

let get_global_block_dal_processed_slots ?(block = "head") () =
  make GET ["global"; "block"; block; "dal"; "processed_slots"] (fun json ->
      JSON.as_list json
      |> List.map (fun obj ->
             let index = JSON.(obj |-> "index" |> as_int) in
             let status = JSON.(obj |-> "status" |> as_string) in
             (index, status)))
