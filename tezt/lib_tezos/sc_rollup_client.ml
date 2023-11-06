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
  published_at_level : int option;
}

type simulation_result = {
  state_hash : string;
  status : string;
  output : JSON.t;
  inbox_level : int;
  num_ticks : int;
  insights : string option list;
}

type gc_info = {last_gc_level : int; first_available_level : int}

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
    (commitment_from_json commitment_json)

let next_name = ref 1

let () = Test.declare_reset_function @@ fun () -> next_name := 1

let create ~protocol ?runner ?name ?base_dir ?color sc_node =
  let name =
    match name with
    | Some name -> name
    | None -> Sc_rollup_node.name sc_node ^ "_client"
  in
  let path = Protocol.sc_rollup_client protocol in
  let base_dir =
    match base_dir with None -> Temp.dir ?runner name | Some dir -> dir
  in
  let color =
    match color with
    | Some c -> c
    | None -> Log.Color.(bold ++ Sc_rollup_node.color sc_node)
  in
  {name; path; sc_node; base_dir; color; runner}

let base_dir_arg sc_client = ["--base-dir"; sc_client.base_dir]

let endpoint_arg sc_client =
  ["--endpoint"; Sc_rollup_node.endpoint sc_client.sc_node]

let spawn_command ?hooks ?log_output sc_client command =
  let process =
    Process.spawn
      ?runner:sc_client.runner
      ~name:sc_client.name
      ~color:sc_client.color
      ?hooks
      ?log_output
      sc_client.path
      (base_dir_arg sc_client @ endpoint_arg sc_client @ command)
  in
  Runnable.{value = process; run = Process.check_and_read_stdout}

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

let outbox_proof ?hooks ?expected_error sc_client ~message_index ~outbox_level
    transactions =
  let*? process =
    spawn_command
      ?hooks
      sc_client
      (List.concat
         [
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
           ];
           (match transactions with
           | Some batch -> ["transferring"; JSON.encode_u (json_of_batch batch)]
           | None -> []);
         ])
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

let outbox_proof_batch ?hooks ?expected_error sc_client ~message_index
    ~outbox_level batch =
  outbox_proof
    ?hooks
    ?expected_error
    sc_client
    (Some batch)
    ~message_index
    ~outbox_level

let outbox_proof_single ?hooks ?expected_error ?entrypoint ?parameters_ty
    sc_client ~message_index ~outbox_level ~destination ~parameters =
  outbox_proof_batch
    ?hooks
    ?expected_error
    sc_client
    ~message_index
    ~outbox_level
    [{destination; entrypoint; parameters; parameters_ty}]

let outbox_proof ?hooks ?expected_error sc_client ~message_index ~outbox_level =
  outbox_proof
    ?hooks
    ?expected_error
    sc_client
    None
    ~message_index
    ~outbox_level

let encode_json_outbox_msg ?hooks sc_client outbox_msg =
  spawn_command
    ?hooks
    sc_client
    ["encode"; "outbox"; "message"; JSON.encode_u outbox_msg]
  |> Runnable.map String.trim

let encode_batch ?hooks ?expected_error sc_client batch =
  let runnable =
    encode_json_outbox_msg ?hooks sc_client (json_of_batch batch)
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

let rpc_get_rich ?hooks ?log_output sc_client path parameters =
  let parameters =
    if parameters = [] then ""
    else
      "?" ^ String.concat "&"
      @@ List.map (fun (k, v) -> Format.asprintf "%s=%s" k v) parameters
  in
  let uri = Client.string_of_path path ^ parameters in
  spawn_command ?hooks ?log_output sc_client ["rpc"; "get"; uri]
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
    ?log_output:bool ->
    ?block:string ->
    t ->
    pvm_kind:string ->
    operation:a durable_state_operation ->
    key:string ->
    a Runnable.process =
 fun ?hooks ?log_output ?(block = "head") sc_client ~pvm_kind ~operation ~key ->
  let op = string_of_durable_state_operation operation in
  let rpc_req () =
    rpc_get_rich
      ?hooks
      ?log_output
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

let last_stored_commitment ?hooks sc_client =
  rpc_get ?hooks sc_client ["global"; "last_stored_commitment"]
  |> Runnable.map commitment_with_hash_from_json

let last_published_commitment ?hooks sc_client =
  rpc_get ?hooks sc_client ["local"; "last_published_commitment"]
  |> Runnable.map commitment_info_from_json

let commitment ?hooks sc_client commitment_hash =
  rpc_get ?hooks sc_client ["local"; "commitments"; commitment_hash]
  |> Runnable.map commitment_info_from_json

let gc_info ?hooks sc_client =
  rpc_get ?hooks sc_client ["local"; "gc_info"]
  |> Runnable.map @@ fun obj ->
     {
       last_gc_level = JSON.(obj |-> "last_gc_level" |> as_int);
       first_available_level = JSON.(obj |-> "first_available_level" |> as_int);
     }

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
