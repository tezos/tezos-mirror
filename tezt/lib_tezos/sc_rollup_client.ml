(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Runnable.Syntax

type t = {
  name : string;
  path : string;
  sc_node : Sc_rollup_node.t;
  base_dir : string;
  color : Log.Color.t;
  runner : Runner.t option;
}

type commitment = {
  compressed_state : string;
  inbox_level : int;
  predecessor : string;
  number_of_ticks : int;
}

type commitment_and_hash = {commitment : commitment; hash : string}

type commitment_info = {
  commitment_and_hash : commitment_and_hash;
  first_published_at_level : int option;
  included_at_level : int option;
}

type slot_header = {level : int; commitment : string; index : int}

type simulation_result = {
  state_hash : string;
  status : string;
  output : JSON.t;
  inbox_level : int;
  num_ticks : int;
  insights : string option list;
}

let commitment_from_json json =
  if JSON.is_null json then None
  else
    let compressed_state = JSON.as_string @@ JSON.get "compressed_state" json in
    let inbox_level = JSON.as_int @@ JSON.get "inbox_level" json in
    let predecessor = JSON.as_string @@ JSON.get "predecessor" json in
    let number_of_ticks = JSON.as_int @@ JSON.get "number_of_ticks" json in
    Some {compressed_state; inbox_level; predecessor; number_of_ticks}

let commitment_with_hash_from_json json =
  let hash, commitment_json =
    (JSON.get "hash" json, JSON.get "commitment" json)
  in
  Option.map
    (fun commitment -> {hash = JSON.as_string hash; commitment})
    (commitment_from_json commitment_json)

let commitment_info_from_json json =
  let hash, commitment_json, first_published_at_level, included_at_level =
    ( JSON.get "hash" json,
      JSON.get "commitment" json,
      JSON.get "first_published_at_level" json,
      JSON.get "included_at_level" json )
  in
  Option.map
    (fun commitment ->
      {
        commitment_and_hash = {hash = JSON.as_string hash; commitment};
        first_published_at_level =
          first_published_at_level |> JSON.as_opt |> Option.map JSON.as_int;
        included_at_level =
          included_at_level |> JSON.as_opt |> Option.map JSON.as_int;
      })
    (commitment_from_json commitment_json)

let next_name = ref 1

let fresh_name () =
  let index = !next_name in
  incr next_name ;
  "client" ^ string_of_int index

let () = Test.declare_reset_function @@ fun () -> next_name := 1

let create ~protocol ?runner ?name ?base_dir ?(color = Log.Color.FG.green)
    sc_node =
  let name = match name with None -> fresh_name () | Some name -> name in
  let path = Protocol.sc_rollup_client protocol in
  let base_dir =
    match base_dir with None -> Temp.dir ?runner name | Some dir -> dir
  in
  {name; path; sc_node; base_dir; color; runner}

let base_dir_arg sc_client = ["--base-dir"; sc_client.base_dir]

let endpoint_arg sc_client =
  ["--endpoint"; Sc_rollup_node.endpoint sc_client.sc_node]

let spawn_command ?hooks sc_client command =
  let process =
    Process.spawn
      ?runner:sc_client.runner
      ~name:sc_client.name
      ~color:sc_client.color
      ?hooks
      sc_client.path
      (base_dir_arg sc_client @ endpoint_arg sc_client @ command)
  in
  Runnable.{value = process; run = Process.check_and_read_stdout}

let sc_rollup_address ?hooks sc_client =
  spawn_command ?hooks sc_client ["get"; "smart"; "rollup"; "address"]
  |> Runnable.map String.trim

let state_value ?hooks ?(block = "head") sc_client ~key =
  spawn_command
    ?hooks
    sc_client
    ["get"; "state"; "value"; "for"; key; "--block"; block]
  |> Runnable.map @@ fun out ->
     Scanf.sscanf (String.trim out) "%S" (fun s -> s) |> String.to_bytes

type transaction = {
  destination : string;
  entrypoint : string option;
  parameters : string;
  parameters_ty : string option;
}

let json_of_batch ts =
  let json_of_transaction {destination; entrypoint; parameters; parameters_ty} =
    `O
      (List.filter_map
         Fun.id
         [
           Some ("destination", `String destination);
           Some ("parameters", `String parameters);
           Option.map (fun v -> ("entrypoint", `String v)) entrypoint;
           Option.map (fun v -> ("parameters_ty", `String v)) parameters_ty;
         ])
  in
  `A (List.map json_of_transaction ts)

type outbox_proof = {commitment_hash : string; proof : string}

let outbox_proof_batch ?hooks ?expected_error sc_client ~message_index
    ~outbox_level batch =
  let*? process =
    spawn_command
      ?hooks
      sc_client
      [
        "get";
        "proof";
        "for";
        "message";
        string_of_int message_index;
        "of";
        "outbox";
        "at";
        "level";
        string_of_int outbox_level;
        "transferring";
        JSON.encode_u (json_of_batch batch);
      ]
  in
  match expected_error with
  | None ->
      let* answer = Process.check_and_read_stdout process in
      let open JSON in
      let json = parse ~origin:"outbox_proof" answer in
      let commitment_hash = json |-> "commitment_hash" |> as_string in
      let proof = json |-> "proof" |> as_string in
      return (Some {commitment_hash; proof})
  | Some msg ->
      let* () = Process.check_error ~msg process in
      return None

let outbox_proof_single ?hooks ?expected_error ?entrypoint ?parameters_ty
    sc_client ~message_index ~outbox_level ~destination ~parameters =
  outbox_proof_batch
    ?hooks
    ?expected_error
    sc_client
    ~message_index
    ~outbox_level
    [{destination; entrypoint; parameters; parameters_ty}]

let encode_batch ?hooks ?expected_error sc_client batch =
  let runnable =
    spawn_command
      ?hooks
      sc_client
      ["encode"; "outbox"; "message"; JSON.encode_u (json_of_batch batch)]
  in
  match expected_error with
  | None ->
      let* answer = Process.check_and_read_stdout runnable.value in
      return (Some (String.trim answer))
  | Some msg ->
      let* () = Process.check_error ~msg runnable.value in
      return None

let rpc_get ?hooks sc_client path =
  spawn_command ?hooks sc_client ["rpc"; "get"; Client.string_of_path path]
  |> Runnable.map @@ fun output ->
     JSON.parse ~origin:(Client.string_of_path path ^ " response") output

let rpc_post ?hooks sc_client path data =
  spawn_command
    ?hooks
    sc_client
    ["rpc"; "post"; Client.string_of_path path; "with"; JSON.encode data]
  |> Runnable.map @@ fun output ->
     JSON.parse ~origin:(Client.string_of_path path ^ " response") output

let rpc_get_rich ?hooks sc_client path parameters =
  let parameters =
    if parameters = [] then ""
    else
      "?" ^ String.concat "&"
      @@ List.map (fun (k, v) -> Format.asprintf "%s=%s" k v) parameters
  in
  let uri = Client.string_of_path path ^ parameters in
  spawn_command ?hooks sc_client ["rpc"; "get"; uri]
  |> Runnable.map @@ fun output ->
     JSON.parse ~origin:(Client.string_of_path path ^ " response") output

type 'output_type durable_state_operation =
  | Value : string option durable_state_operation
  | Length : int64 option durable_state_operation
  | Subkeys : string list durable_state_operation

let string_of_durable_state_operation (type a) (x : a durable_state_operation) =
  match x with Value -> "value" | Length -> "length" | Subkeys -> "subkeys"

let inspect_durable_state_value :
    type a.
    ?hooks:Process.hooks ->
    ?block:string ->
    t ->
    pvm_kind:string ->
    operation:a durable_state_operation ->
    key:string ->
    a Runnable.process =
 fun ?hooks ?(block = "head") sc_client ~pvm_kind ~operation ~key ->
  let op = string_of_durable_state_operation operation in
  let rpc_req () =
    rpc_get_rich
      ?hooks
      sc_client
      ["global"; "block"; block; "durable"; pvm_kind; op]
      [("key", key)]
  in
  match operation with
  | Value -> rpc_req () |> Runnable.map (fun json -> JSON.as_string_opt json)
  | Length -> rpc_req () |> Runnable.map (fun json -> JSON.as_int64_opt json)
  | Subkeys ->
      rpc_req ()
      |> Runnable.map (fun json -> List.map JSON.as_string (JSON.as_list json))

(* match expected with
   | Expected_success -> (
       let*! json = rpc_req () in
       match operation with
       | Value -> return (JSON.as_string json : a)
       | Length -> return (JSON.as_int64 json : a)
       | Subkeys -> return (List.map JSON.as_string (JSON.as_list json)))
   | Expected_error msg ->
       let*? process = rpc_req () in
       Process.check_error ~msg process *)

let ticks ?hooks ?(block = "head") sc_client =
  let res = rpc_get ?hooks sc_client ["global"; "block"; block; "ticks"] in
  Runnable.map JSON.as_int res

let total_ticks ?hooks ?(block = "head") sc_client =
  rpc_get ?hooks sc_client ["global"; "block"; block; "total_ticks"]
  |> Runnable.map JSON.as_int

let state_hash ?hooks ?(block = "head") sc_client =
  rpc_get ?hooks sc_client ["global"; "block"; block; "state_hash"]
  |> Runnable.map JSON.as_string

let state_current_level ?hooks ?(block = "head") sc_client =
  rpc_get ?hooks sc_client ["global"; "block"; block; "state_current_level"]
  |> Runnable.map JSON.as_int

let status ?hooks ?(block = "head") sc_client =
  rpc_get ?hooks sc_client ["global"; "block"; block; "status"]
  |> Runnable.map JSON.as_string

let outbox ?hooks ?(block = "cemented") ~outbox_level sc_client =
  rpc_get
    ?hooks
    sc_client
    ["global"; "block"; block; "outbox"; string_of_int outbox_level; "messages"]

let last_stored_commitment ?hooks sc_client =
  rpc_get ?hooks sc_client ["global"; "last_stored_commitment"]
  |> Runnable.map commitment_with_hash_from_json

let last_published_commitment ?hooks sc_client =
  rpc_get ?hooks sc_client ["local"; "last_published_commitment"]
  |> Runnable.map commitment_info_from_json

let dal_slot_headers ?hooks ?(block = "head") sc_client =
  rpc_get ?hooks sc_client ["global"; "block"; block; "dal"; "slot_headers"]
  |> Runnable.map (fun json ->
         JSON.(
           as_list json
           |> List.map (fun obj ->
                  {
                    level = obj |> get "level" |> as_int;
                    commitment = obj |> get "commitment" |> as_string;
                    index = obj |> get "index" |> as_int;
                  })))

let get_dal_processed_slots ?hooks ?(block = "head") sc_client =
  rpc_get ?hooks sc_client ["global"; "block"; block; "dal"; "processed_slots"]
  |> Runnable.map (fun json ->
         JSON.as_list json
         |> List.map (fun obj ->
                let index = obj |> JSON.get "index" |> JSON.as_int in
                let status = obj |> JSON.get "status" |> JSON.as_string in
                (index, status)))

let simulate ?hooks ?(block = "head") sc_client ?(reveal_pages = [])
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
    `O ((("messages", messages_json) :: reveal_json) @ insight_requests_json)
    |> JSON.annotate ~origin:"simulation data"
  in
  rpc_post ?hooks sc_client ["global"; "block"; block; "simulate"] data
  |> Runnable.map (fun obj ->
         JSON.
           {
             state_hash = obj |> get "state_hash" |> as_string;
             status = obj |> get "status" |> as_string;
             output = obj |> get "output";
             inbox_level = obj |> get "inbox_level" |> as_int;
             num_ticks = obj |> get "num_ticks" |> as_string |> int_of_string;
             insights =
               obj |> get "insights" |> as_list |> List.map as_string_opt;
           })

let inject ?hooks sc_client messages =
  let messages_json =
    `A (List.map (fun s -> `String Hex.(of_string s |> show)) messages)
    |> JSON.annotate ~origin:"injection messages"
  in
  rpc_post ?hooks sc_client ["local"; "batcher"; "injection"] messages_json
  |> Runnable.map @@ fun obj -> JSON.as_list obj |> List.map JSON.as_string

let batcher_queue ?hooks sc_client =
  rpc_get ?hooks sc_client ["local"; "batcher"; "queue"]
  |> Runnable.map @@ fun obj ->
     JSON.as_list obj
     |> List.map @@ fun o ->
        let hash = JSON.(o |> get "hash" |> as_string) in
        let hex_msg = JSON.(o |> get "message" |> get "content" |> as_string) in
        (hash, Hex.to_string (`Hex hex_msg))

let get_batcher_msg ?hooks sc_client msg_hash =
  rpc_get ?hooks sc_client ["local"; "batcher"; "queue"; msg_hash]
  |> Runnable.map @@ fun obj ->
     if JSON.is_null obj then failwith "Message is not in the queue" ;
     let hex_msg = JSON.(obj |> get "content" |> as_string) in
     (Hex.to_string (`Hex hex_msg), obj)

let spawn_generate_keys ?hooks ?(force = false) ~alias sc_client =
  spawn_command
    ?hooks
    sc_client
    (["gen"; "unencrypted"; "keys"; alias] @ if force then ["--force"] else [])

let generate_keys ?hooks ?force ~alias sc_client =
  let*? process = spawn_generate_keys ?hooks ?force ~alias sc_client in
  Process.check process

let spawn_list_keys ?hooks sc_client =
  spawn_command ?hooks sc_client ["list"; "keys"]

let parse_list_keys output =
  output |> String.trim |> String.split_on_char '\n'
  |> List.fold_left (fun acc s -> (s =~** rex "^(\\w+): (\\w{36})") :: acc) []
  |> List.fold_left
       (fun acc k ->
         match (k, acc) with
         | None, _ | _, None -> None
         | Some k, Some acc -> Some (k :: acc))
       (Some [])
  |> function
  | None ->
      Test.fail
        ~__LOC__
        "Cannot extract `list keys` format from client_output: %s"
        output
  | Some l -> l

let list_keys ?hooks sc_client =
  let*! out = spawn_list_keys ?hooks sc_client in
  return (parse_list_keys out)

let spawn_show_address ?hooks ~alias sc_client =
  spawn_command ?hooks sc_client ["show"; "address"; alias]

let show_address ?hooks ~alias sc_client =
  let*! out = spawn_show_address ?hooks ~alias sc_client in
  return (Account.parse_client_output_aggregate ~alias ~client_output:out)

let spawn_import_secret_key ?hooks ?(force = false)
    (key : Account.aggregate_key) sc_client =
  let sk_uri =
    "aggregate_unencrypted:"
    ^ Account.require_unencrypted_secret_key ~__LOC__ key.aggregate_secret_key
  in
  spawn_command
    ?hooks
    sc_client
    (["import"; "secret"; "key"; key.aggregate_alias; sk_uri]
    @ if force then ["--force"] else [])

let import_secret_key ?hooks ?force key sc_client =
  let*? process = spawn_import_secret_key ?hooks ?force key sc_client in
  Process.check process
