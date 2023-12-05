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

let next_name = ref 1

let () = Test.declare_reset_function @@ fun () -> next_name := 1

let create ~protocol ?runner ?name ?base_dir ?color sc_node =
  let name =
    match name with
    | Some name -> name
    | None -> Sc_rollup_node.name sc_node ^ "_client"
  in
  let path = Uses.path (Protocol.sc_rollup_client protocol) in
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

let inject ?hooks sc_client messages =
  let messages_json =
    `A (List.map (fun s -> `String Hex.(of_string s |> show)) messages)
    |> JSON.annotate ~origin:"injection messages"
  in
  rpc_post ?hooks sc_client ["local"; "batcher"; "injection"] messages_json
  |> Runnable.map @@ fun obj -> JSON.as_list obj |> List.map JSON.as_string
