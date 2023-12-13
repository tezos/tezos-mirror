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
