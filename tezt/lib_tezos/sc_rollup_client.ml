(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
}

type commitment = {
  compressed_state : string;
  inbox_level : int;
  predecessor : string;
  number_of_ticks : int;
}

type slot_header = {level : int; commitment : string; index : int}

type simulation_result = {
  state_hash : string;
  status : string;
  output : JSON.t;
  inbox_level : int;
  num_ticks : int;
}

let commitment_from_json json =
  if JSON.is_null json then None
  else
    let compressed_state = JSON.as_string @@ JSON.get "compressed_state" json in
    let inbox_level = JSON.as_int @@ JSON.get "inbox_level" json in
    let predecessor = JSON.as_string @@ JSON.get "predecessor" json in
    let number_of_ticks = JSON.as_int @@ JSON.get "number_of_ticks" json in
    Some {compressed_state; inbox_level; predecessor; number_of_ticks}

let commitment_with_hash_and_level_from_json json =
  let hash, commitment_json, published_at_level =
    ( JSON.get "hash" json,
      JSON.get "commitment" json,
      JSON.get "published_at_level" json )
  in
  Option.map
    (fun commitment ->
      ( JSON.as_string hash,
        commitment,
        published_at_level |> JSON.as_opt |> Option.map JSON.as_int ))
    (commitment_from_json commitment_json)

let next_name = ref 1

let fresh_name () =
  let index = !next_name in
  incr next_name ;
  "client" ^ string_of_int index

let () = Test.declare_reset_function @@ fun () -> next_name := 1

let create ?name ?path ?base_dir ?(color = Log.Color.FG.green) sc_node =
  let name = match name with None -> fresh_name () | Some name -> name in
  let path =
    match path with None -> Constant.sc_rollup_client | Some p -> p
  in
  let base_dir =
    match base_dir with None -> Temp.dir name | Some dir -> dir
  in
  {name; path; sc_node; base_dir; color}

let base_dir_arg sc_client = ["--base-dir"; sc_client.base_dir]

let endpoint_arg sc_client =
  ["--endpoint"; Sc_rollup_node.endpoint sc_client.sc_node]

let spawn_command ?hooks sc_client command =
  let process =
    Process.spawn
      ~name:sc_client.name
      ~color:sc_client.color
      ?hooks
      sc_client.path
      (base_dir_arg sc_client @ endpoint_arg sc_client @ command)
  in
  Runnable.{value = process; run = Process.check_and_read_stdout}

let sc_rollup_address ?hooks sc_client =
  spawn_command ?hooks sc_client ["get"; "sc"; "rollup"; "address"]
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
}

let string_of_transaction {destination; entrypoint; parameters} =
  Format.asprintf
    {| { "destination" : "%s", %s"parameters" : "%s" } |}
    destination
    (match entrypoint with
    | None -> ""
    | Some entrypoint -> Format.asprintf {| "entrypoint" : "%s", |} entrypoint)
    parameters

let string_of_batch ts =
  "[ " ^ String.concat "," (List.map string_of_transaction ts) ^ " ]"

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
        string_of_batch batch;
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

let outbox_proof_single ?hooks ?expected_error ?entrypoint sc_client
    ~message_index ~outbox_level ~destination ~parameters =
  outbox_proof_batch
    ?hooks
    ?expected_error
    sc_client
    ~message_index
    ~outbox_level
    [{destination; entrypoint; parameters}]

let encode_batch ?hooks ?expected_error sc_client batch =
  let process =
    spawn_command
      ?hooks
      sc_client
      ["encode"; "outbox"; "message"; string_of_batch batch]
  in
  match expected_error with
  | None ->
      let* answer = Process.check_and_read_stdout process in
      return (Some (String.trim answer))
  | Some msg ->
      let* () = Process.check_error ~msg process in
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
  let process = spawn_command ?hooks sc_client ["rpc"; "get"; uri] in
  let* output = Process.check_and_read_stdout process in
  return (JSON.parse ~origin:(Client.string_of_path path ^ " response") output)

let ticks ?hooks ?(block = "head") sc_client =
  let res = rpc_get ?hooks sc_client ["global"; "block"; block; "ticks"] in
  Runnable.map JSON.as_int res

let total_ticks ?hooks ?(block = "head") sc_client =
  rpc_get ?hooks sc_client ["global"; "block"; block; "total_ticks"]
  |> Runnable.map JSON.as_int

let state_hash ?hooks ?(block = "head") sc_client =
  rpc_get ?hooks sc_client ["global"; "block"; block; "state_hash"]
  |> Runnable.map JSON.as_string

let status ?hooks ?(block = "head") sc_client =
  rpc_get ?hooks sc_client ["global"; "block"; block; "status"]
  |> Runnable.map JSON.as_string

let outbox ?hooks ?(block = "cemented") ~outbox_level sc_client =
  let open Lwt.Syntax in
  rpc_get_rich
    ?hooks
    sc_client
    ["global"; "block"; block; "outbox"]
    [("outbox_level", string_of_int outbox_level)]

let last_stored_commitment ?hooks sc_client =
  rpc_get ?hooks sc_client ["global"; "last_stored_commitment"]
  |> Runnable.map commitment_with_hash_and_level_from_json

let last_published_commitment ?hooks sc_client =
  rpc_get ?hooks sc_client ["local"; "last_published_commitment"]
  |> Runnable.map commitment_with_hash_and_level_from_json

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

let dal_downloaded_confirmed_slot_pages ?hooks ?(block = "head") sc_client =
  rpc_get
    ?hooks
    sc_client
    ["global"; "block"; block; "dal"; "confirmed_slot_pages"]
  |> Runnable.map (fun json ->
         JSON.as_list json
         |> List.map (fun obj ->
                let index = obj |> JSON.get "index" |> JSON.as_int in
                let contents =
                  obj |> JSON.get "contents" |> JSON.as_list
                  |> List.map (fun page ->
                         page |> JSON.as_string |> fun s ->
                         Hex.to_string (`Hex s))
                in
                (index, contents)))

let simulate ?hooks ?(block = "head") sc_client ?(reveal_pages = []) messages =
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
  let data =
    `O (("messages", messages_json) :: reveal_json)
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
    let (Unencrypted sk) = key.aggregate_secret_key in
    "aggregate_unencrypted:" ^ sk
  in
  spawn_command
    ?hooks
    sc_client
    (["import"; "secret"; "key"; key.aggregate_alias; sk_uri]
    @ if force then ["--force"] else [])

let import_secret_key ?hooks ?force key sc_client =
  let*? process = spawn_import_secret_key ?hooks ?force key sc_client in
  Process.check process
