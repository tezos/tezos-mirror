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

type commitment_and_hash = {
  commitment : RPC.smart_rollup_commitment;
  hash : string;
}

type commitment_info = {
  commitment_and_hash : commitment_and_hash;
  first_published_at_level : int option;
  published_at_level : int option;
}

let commitment_with_hash_from_json json =
  let hash, commitment_json =
    (JSON.get "hash" json, JSON.get "commitment" json)
  in
  Option.map
    (fun commitment -> {hash = JSON.as_string hash; commitment})
    (RPC.smart_rollup_commitment_from_json commitment_json)

let commitment_info_from_json json =
  let hash, commitment_json, first_published_at_level, published_at_level =
    ( JSON.get "hash" json,
      JSON.get "commitment" json,
      JSON.get "first_published_at_level" json,
      JSON.get "published_at_level" json )
  in
  Option.map
    (fun commitment ->
      {
        commitment_and_hash = {hash = JSON.as_string hash; commitment};
        first_published_at_level =
          first_published_at_level |> JSON.as_opt |> Option.map JSON.as_int;
        published_at_level =
          published_at_level |> JSON.as_opt |> Option.map JSON.as_int;
      })
    (RPC.smart_rollup_commitment_from_json commitment_json)

let get_global_last_stored_commitment () =
  make GET ["global"; "last_stored_commitment"] commitment_with_hash_from_json

let get_local_last_published_commitment () =
  make GET ["local"; "last_published_commitment"] commitment_info_from_json

let get_local_commitments ~commitment_hash () =
  make GET ["local"; "commitments"; commitment_hash] commitment_info_from_json

type gc_info = {last_gc_level : int; first_available_level : int}

let get_local_gc_info () =
  make GET ["local"; "gc_info"] (fun obj ->
      {
        last_gc_level = JSON.(obj |-> "last_gc_level" |> as_int);
        first_available_level = JSON.(obj |-> "first_available_level" |> as_int);
      })

let get_global_block_state ?(block = "head") ~key () =
  make
    ~query_string:[("key", key)]
    GET
    ["global"; "block"; block; "state"]
    (fun json ->
      let out = JSON.as_string json in
      let bytes = `Hex out |> Hex.to_bytes in
      bytes)

type 'output_type durable_state_operation =
  | Value : string option durable_state_operation
  | Length : int64 option durable_state_operation
  | Subkeys : string list durable_state_operation

let string_of_durable_state_operation (type a) (x : a durable_state_operation) =
  match x with Value -> "value" | Length -> "length" | Subkeys -> "subkeys"

let get_global_block_durable_state_value ?(block = "head") ~pvm_kind ~operation
    ~key () =
  let op = string_of_durable_state_operation operation in
  let f : type k. k durable_state_operation -> JSON.t -> k =
   fun operation ->
    match operation with
    | Value -> JSON.as_string_opt
    | Length -> JSON.as_int64_opt
    | Subkeys -> fun json -> List.map JSON.as_string (JSON.as_list json)
  in
  make
    ~query_string:[("key", String.trim key)]
    GET
    ["global"; "block"; block; "durable"; pvm_kind; op]
    (f operation)

let post_local_batcher_injection ~messages =
  let data =
    Data (`A (List.map (fun s -> `String Hex.(of_string s |> show)) messages))
  in
  make
    POST
    ["local"; "batcher"; "injection"]
    ~data
    JSON.(fun json -> as_list json |> List.map as_string)

type outbox_proof = {commitment_hash : string; proof : string}

let outbox_proof_simple ?(block = "head") ~outbox_level ~message_index () =
  let open RPC_core in
  let query_string = [("index", string_of_int message_index)] in
  make
    ~query_string
    GET
    [
      "global";
      "block";
      block;
      "helpers";
      "proofs";
      "outbox";
      string_of_int outbox_level;
      "messages";
    ]
    (fun json ->
      let open JSON in
      let commitment_hash = json |-> "commitment" |> as_string in
      let proof_string = json |-> "proof" |> as_string in
      (* There is 0x return in the proof hash as it is a hex *)
      let proof_hex = sf "0x%s" proof_string in
      Some {commitment_hash; proof = proof_hex})

let outbox_proof_single ?(block = "head") ~message_index ~outbox_level ~message
    () =
  let open RPC_core in
  let query_string =
    [
      ("message_index", string_of_int message_index);
      ("outbox_level", string_of_int outbox_level);
      ("serialized_outbox_message", Hex.to_string (`Hex message));
    ]
  in
  make
    ~query_string
    GET
    ["global"; "block"; block; "helpers"; "proofs"; "outbox"]
    (fun json ->
      let open JSON in
      let commitment_hash = json |-> "commitment" |> as_string in
      let proof_string = json |-> "proof" |> as_string in
      (* There is 0x return in the proof hash as it is a hex *)
      let proof_hex = sf "0x%s" proof_string in
      Some {commitment_hash; proof = proof_hex})
