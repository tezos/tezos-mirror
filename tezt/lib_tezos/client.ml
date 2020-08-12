(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type mode = Client of Node.t option | Mockup

let mode_needs_node = function Client _ -> true | Mockup -> false

type t = {
  path : string;
  admin_path : string;
  name : string;
  color : Log.Color.t;
  base_dir : string;
  mode : mode;
}

let next_name = ref 1

let fresh_name () =
  let index = !next_name in
  incr next_name ;
  "client" ^ string_of_int index

let () = Test.declare_reset_function @@ fun () -> next_name := 1

let create_with_mode ?(path = Constant.tezos_client)
    ?(admin_path = Constant.tezos_admin_client) ?name
    ?(color = Log.Color.FG.blue) ?base_dir mode =
  let name = match name with None -> fresh_name () | Some name -> name in
  let base_dir =
    match base_dir with None -> Temp.dir name | Some dir -> dir
  in
  {path; admin_path; name; color; base_dir; mode}

let create ?path ?admin_path ?name ?color ?base_dir ?node () =
  create_with_mode ?path ?admin_path ?name ?color ?base_dir @@ Client node

let check_node_is_specified ?node command client =
  match client.mode with
  | Mockup ->
      ()
  | Client node_opt -> (
    match (node, node_opt) with
    | (None, None) ->
        Test.fail
          "%s has no node, cannot run command: %s"
          client.name
          (String.concat " " command)
    | (Some _, _) | (_, Some _) ->
        () )

let base_args ?node client =
  let node =
    match client.mode with
    | Mockup ->
        None
    | Client node_opt -> (
      match (node, node_opt) with
      | (Some node, _) | (None, Some node) ->
          Some node
      | (None, None) ->
          None )
  in
  ( match node with
  | None ->
      []
  | Some node ->
      ["--endpoint"; "http://localhost:" ^ string_of_int (Node.rpc_port node)]
  )
  @ ["--base-dir"; client.base_dir]

let mode_args = function
  | Client _ ->
      [] (* It's the default mode *)
  | Mockup ->
      ["--mode"; "mockup"]

let common_args ?node client = base_args ?node client @ mode_args client.mode

let spawn_command ?(needs_node = true) ?(admin = false) ?node client command =
  if needs_node then check_node_is_specified ?node command client ;
  Process.spawn
    ~name:client.name
    ~color:client.color
    (if admin then client.admin_path else client.path)
  @@ common_args ?node client @ command

let url_encode str =
  let buffer = Buffer.create (String.length str * 3) in
  for i = 0 to String.length str - 1 do
    match str.[i] with
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-' | '/') as c ->
        Buffer.add_char buffer c
    | c ->
        Buffer.add_char buffer '%' ;
        let (c1, c2) = Hex.of_char c in
        Buffer.add_char buffer c1 ; Buffer.add_char buffer c2
  done ;
  let result = Buffer.contents buffer in
  Buffer.reset buffer ; result

type meth = GET | PUT | POST | PATCH

let string_of_meth = function
  | GET ->
      "get"
  | PUT ->
      "put"
  | POST ->
      "post"
  | PATCH ->
      "patch"

type path = string list

let string_of_path path = "/" ^ String.concat "/" (List.map url_encode path)

type query_string = (string * string) list

let string_of_query_string = function
  | [] ->
      ""
  | qs ->
      let qs' = List.map (fun (k, v) -> (url_encode k, url_encode v)) qs in
      "?" ^ String.concat "&" @@ List.map (fun (k, v) -> k ^ "=" ^ v) qs'

let rpc ?node ?data ?query_string meth path client : JSON.t Lwt.t =
  check_node_is_specified ?node ["rpc"] client ;
  let data =
    Option.fold ~none:[] ~some:(fun x -> ["with"; JSON.encode_u x]) data
  in
  let query_string =
    Option.fold ~none:"" ~some:string_of_query_string query_string
  in
  let path = string_of_path path in
  let full_path = path ^ query_string in
  let* output =
    Process.run_and_read_stdout
      ~name:client.name
      ~color:client.color
      client.path
      ( common_args ?node client
      @ ["rpc"; string_of_meth meth; full_path]
      @ data )
  in
  return (JSON.parse ~origin:(path ^ " response") output)

let rpc_list client : string Lwt.t =
  Process.run_and_read_stdout
    ~name:client.name
    ~color:client.color
    client.path
    (common_args ?node:None client @ ["rpc"; "list"])

module Admin = struct
  let spawn_command = spawn_command ~admin:true

  let spawn_connect_address ?node ~peer client =
    spawn_command
      client
      ?node
      ["connect"; "address"; "127.0.0.1:" ^ string_of_int (Node.net_port peer)]

  let connect_address ?node ~peer client =
    spawn_connect_address ?node ~peer client |> Process.check

  let spawn_kick_peer ?node ~peer client =
    spawn_command client ?node ["kick"; "peer"; peer]

  let kick_peer ?node ~peer client =
    spawn_kick_peer ?node ~peer client |> Process.check
end

let spawn_import_secret_key ?node client (key : Constant.key) =
  spawn_command
    ~needs_node:false
    ?node
    client
    ["import"; "secret"; "key"; key.alias; key.secret]

let import_secret_key ?node client key =
  spawn_import_secret_key ?node client key |> Process.check

let spawn_activate_protocol ?node ?(protocol = Constant.alpha) ?(fitness = 1)
    ?(key = Constant.activator.alias) ?timestamp
    ?(timestamp_delay = 3600. *. 24. *. 365.) client =
  let timestamp =
    match timestamp with
    | Some timestamp ->
        timestamp
    | None ->
        let tm = Unix.gmtime (Unix.time () -. timestamp_delay) in
        Printf.sprintf
          "%04d-%02d-%02dT%02d:%02d:%02dZ"
          (tm.tm_year + 1900)
          (tm.tm_mon + 1)
          tm.tm_mday
          tm.tm_hour
          tm.tm_min
          tm.tm_sec
  in
  spawn_command
    client
    ?node
    [ "activate";
      "protocol";
      protocol.hash;
      "with";
      "fitness";
      string_of_int fitness;
      "and";
      "key";
      key;
      "and";
      "parameters";
      protocol.parameter_file;
      "--timestamp";
      timestamp ]

let activate_protocol ?node ?protocol ?fitness ?key ?timestamp ?timestamp_delay
    client =
  spawn_activate_protocol
    ?node
    ?protocol
    ?fitness
    ?key
    ?timestamp
    ?timestamp_delay
    client
  |> Process.check

let spawn_bake_for ?node ?(key = Constant.bootstrap1.alias)
    ?(minimal_timestamp = true) client =
  let needs_node = mode_needs_node client.mode in
  spawn_command
    ~needs_node
    client
    ?node
    ( "bake" :: "for" :: key
    :: (if minimal_timestamp then ["--minimal-timestamp"] else []) )

let bake_for ?node ?key ?minimal_timestamp client =
  spawn_bake_for ?node ?key ?minimal_timestamp client |> Process.check

let create_mockup ?(protocol = Constant.alpha) client : unit Lwt.t =
  let cmd = ["--protocol"; protocol.hash; "create"; "mockup"] in
  spawn_command ~needs_node:false ?node:None client cmd |> Process.check

let init_mockup ~(protocol : Constant.protocol) =
  (* To create the mockup, we don't want --mode mockup to be specified,
     as per the public tutorial:
     https://tezos.gitlab.io/user/mockup.html *)
  let mockup_creator = create () in
  let* () = create_mockup ~protocol mockup_creator in
  let base_dir = mockup_creator.base_dir in
  (* but then we want it in the client we use, hence: *)
  return @@ create_with_mode ~base_dir Mockup

let spawn_submit_proposals ?node ?(key = Constant.bootstrap1.alias) ~proto_hash
    client =
  spawn_command client ?node ["submit"; "proposals"; "for"; key; proto_hash]

let submit_proposals ?node ?key ~proto_hash client =
  spawn_submit_proposals ?node ?key ~proto_hash client |> Process.check

type ballot = Nay | Pass | Yay

let spawn_submit_ballot ?node ?(key = Constant.bootstrap1.alias) ~proto_hash
    vote client =
  let string_of_vote = function
    | Yay ->
        "yay"
    | Nay ->
        "nay"
    | Pass ->
        "pass"
  in
  spawn_command
    client
    ?node
    ["submit"; "ballot"; "for"; key; proto_hash; string_of_vote vote]

let submit_ballot ?node ?key ~proto_hash vote client =
  spawn_submit_ballot ?node ?key ~proto_hash vote client |> Process.check

let init ?path ?admin_path ?name ?color ?base_dir ?node () =
  let client = create ?path ?admin_path ?name ?color ?base_dir ?node () in
  let* () =
    Lwt_list.iter_s (import_secret_key client) Constant.all_secret_keys
  in
  return client
