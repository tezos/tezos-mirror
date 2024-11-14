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

let get_global_block_aux ?(block = "head") ?(path = []) ?query_string () =
  make ?query_string GET (["global"; "block"; block] @ path) Fun.id

let get_global_block ?block ?(outbox = false) () =
  let query_string = if outbox then Some [("outbox", "true")] else None in
  get_global_block_aux ?block ~path:[] ?query_string ()

let get_global_block_inbox ?(block = "head") () =
  make GET ["global"; "block"; block; "inbox"] RPC.smart_rollup_inbox_from_json

let get_global_block_hash ?block () =
  get_global_block_aux ~path:["hash"] ?block ()

let get_global_block_level ?block () =
  get_global_block_aux ~path:["level"] ?block ()

let get_global_block_num_messages ?block () =
  get_global_block_aux ~path:["num_messages"] ?block ()

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
         let id = JSON.(o |-> "id" |> as_string) in
         let hex_msg = JSON.(o |-> "message" |-> "content" |> as_string) in
         (id, Hex.to_string (`Hex hex_msg)))

let get_local_batcher_queue_msg_id ~msg_id =
  make GET ["local"; "batcher"; "queue"; msg_id] (fun json ->
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
  {
    hash = JSON.as_string hash;
    commitment = RPC.smart_rollup_commitment_from_json commitment_json;
  }

let commitment_info_from_json json =
  let hash, commitment_json, first_published_at_level, published_at_level =
    ( JSON.get "hash" json,
      JSON.get "commitment" json,
      JSON.get "first_published_at_level" json,
      JSON.get "published_at_level" json )
  in
  {
    commitment_and_hash =
      {
        hash = JSON.as_string hash;
        commitment = RPC.smart_rollup_commitment_from_json commitment_json;
      };
    first_published_at_level =
      first_published_at_level |> JSON.as_opt |> Option.map JSON.as_int;
    published_at_level =
      published_at_level |> JSON.as_opt |> Option.map JSON.as_int;
  }

let get_global_last_stored_commitment () =
  make GET ["global"; "last_stored_commitment"] commitment_with_hash_from_json

let get_local_last_published_commitment () =
  make GET ["local"; "last_published_commitment"] commitment_info_from_json

let get_local_commitments ~commitment_hash () =
  make GET ["local"; "commitments"; commitment_hash] commitment_info_from_json

type gc_info = {
  first_available_level : int;
  last_gc_started_at : int option;
  last_context_split_level : int option;
  last_successful_gc_target : int option;
}

let get_local_gc_info () =
  make GET ["local"; "gc_info"] (fun obj ->
      {
        first_available_level = JSON.(obj |-> "first_available_level" |> as_int);
        last_gc_started_at = JSON.(obj |-> "last_gc_level" |> as_int_opt);
        last_context_split_level =
          JSON.(obj |-> "last_context_split_level" |> as_int_opt);
        last_successful_gc_target =
          JSON.(obj |-> "last_successful_gc_target" |> as_int_opt);
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

let post_local_batcher_injection ?drop_duplicate ~messages () =
  let data =
    Data (`A (List.map (fun s -> `String Hex.(of_string s |> show)) messages))
  in
  let query_string =
    Option.map (fun b -> [("drop_duplicate", string_of_bool b)]) drop_duplicate
  in
  make
    POST
    ["local"; "batcher"; "injection"]
    ?query_string
    ~data
    JSON.(fun json -> as_list json |> List.map as_string)

let as_empty_object_or_fail t =
  match JSON.as_object t with
  | [] -> ()
  | _ -> JSON.error t "Not an empty object"

let post_local_dal_batcher_injection ~messages =
  let json = `A (List.map Dal_common.RPC.unistring_to_json messages) in
  let data = Data json in
  make
    POST
    ["local"; "dal"; "batcher"; "injection"]
    ~data
    as_empty_object_or_fail

let post_dal_slot_indices ~slot_indices =
  let data =
    `O
      [
        ( "indices",
          `A (List.map (fun i -> `Float (float_of_int i)) slot_indices) );
      ]
  in
  make
    POST
    ["local"; "dal"; "slot"; "indices"]
    ~data:(Data data)
    as_empty_object_or_fail

let get_dal_injected_operations_statuses () =
  make GET ["local"; "dal"; "injected"; "operations"; "statuses"] JSON.as_list

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

type outbox_msg = {message_index : int; message : JSON.t}

let outbox_list_of_json json =
  let open JSON in
  as_list json
  |> List.map @@ fun json ->
     let outbox_level = json |-> "outbox_level" |> as_int in
     let msgs =
       json |-> "messages" |> as_list
       |> List.map @@ fun json ->
          let message_index = json |-> "message_index" |> as_int in
          let message = json |-> "message" in
          {message_index; message}
     in
     (outbox_level, msgs)

let get_local_outbox_pending_executable () =
  make GET ["local"; "outbox"; "pending"; "executable"] outbox_list_of_json

let get_local_outbox_pending_unexecutable () =
  make GET ["local"; "outbox"; "pending"; "unexecutable"] outbox_list_of_json
