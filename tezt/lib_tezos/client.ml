(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
open Cli_arg

type endpoint =
  | Node of Node.t
  | Proxy_server of Proxy_server.t
  | Foreign_endpoint of Endpoint.t

type media_type = Json | Binary | Any

let rpc_port = function
  | Node n -> Node.rpc_port n
  | Proxy_server ps -> Proxy_server.rpc_port ps
  | Foreign_endpoint fe -> Endpoint.rpc_port fe

type mode =
  | Client of endpoint option * media_type option
  | Mockup
  | Light of float * endpoint list
  | Proxy of endpoint

type mockup_sync_mode = Asynchronous | Synchronous

type normalize_mode = Readable | Optimized | Optimized_legacy

let normalize_mode_to_string = function
  | Readable -> "Readable"
  | Optimized -> "Optimized"
  | Optimized_legacy -> "Optimized_legacy"

type t = {
  path : string;
  admin_path : string;
  name : string;
  color : Log.Color.t;
  base_dir : string;
  mutable additional_bootstraps : Account.key list;
  mutable mode : mode;
  runner : Runner.t option;
}

type stresstest_gas_estimation = {
  regular : int;
  smart_contracts : (string * int) list;
}

type stresstest_contract_parameters = {
  probability : float;
  invocation_fee : Tez.t;
  invocation_gas_limit : int;
}

let name t = t.name

let base_dir t = t.base_dir

let additional_bootstraps t = t.additional_bootstraps

let get_mode t = t.mode

let set_mode mode t = t.mode <- mode

let endpoint_wait_for ?where endpoint event filter =
  match endpoint with
  | Node node -> Node.wait_for ?where node event filter
  | Proxy_server ps -> Proxy_server.wait_for ?where ps event filter
  | Foreign_endpoint _ -> Test.fail "Cannot wait_for with a Foreign_endpoint"

let next_name = ref 1

let fresh_name () =
  let index = !next_name in
  incr next_name ;
  "client" ^ string_of_int index

let () = Test.declare_reset_function @@ fun () -> next_name := 1

let runner endpoint =
  match endpoint with
  | Node node -> Node.runner node
  | Proxy_server ps -> Proxy_server.runner ps
  | Foreign_endpoint _ -> None

let scheme = function
  | Node n -> Node.rpc_scheme n
  | Proxy_server _ -> Proxy_server.rpc_scheme
  | Foreign_endpoint fe -> Endpoint.rpc_scheme fe

let address ?(hostname = false) ?from peer =
  (* We locally overload the runner function to fake a runner for foreign
     endpoints. Because the API contract of the [Runner] module is to
     provide a way to connect to a remote host using SSH, we cannot return a
     runner for a foreign endpoint, because the module does not provide such
     invariants (for instance, the public RPC endpoint of Mondaynet is a good
     candidate for a foreign endpoint to which we likely cannot connect to
     using SSH).

     We do provide this function because the runner is only used in the context
     of the {!Runner.address} function, which only compares the addresses of
     two runners to decide whether or not to use the localhost address
     in the case where [from] has the same address as [peer]. *)
  let runner = function
    | Foreign_endpoint endpoint ->
        Some (Runner.create ~address:(Endpoint.rpc_host endpoint) ())
    | peer -> runner peer
  in
  match from with
  | None -> Runner.address ~hostname (runner peer)
  | Some endpoint ->
      Runner.address ~hostname ?from:(runner endpoint) (runner peer)

let create_with_mode ?runner ?(path = Uses.path Constant.octez_client)
    ?(admin_path = Uses.path Constant.octez_admin_client) ?name
    ?(color = Log.Color.FG.blue) ?base_dir mode =
  let name = match name with None -> fresh_name () | Some name -> name in
  let base_dir =
    match base_dir with None -> Temp.dir ?runner name | Some dir -> dir
  in
  let additional_bootstraps = [] in
  {path; admin_path; name; color; base_dir; additional_bootstraps; mode; runner}

let create ?runner ?path ?admin_path ?name ?color ?base_dir ?endpoint
    ?media_type () =
  create_with_mode
    ?runner
    ?path
    ?admin_path
    ?name
    ?color
    ?base_dir
    (Client (endpoint, media_type))

let base_dir_arg client = ["--base-dir"; client.base_dir]

(* To avoid repeating unduly the sources file name, we create a function here
   to get said file name as string.
   Do not call it from a client in Mockup or Client (nominal) mode. *)
let sources_file client =
  match client.mode with
  | Mockup | Client _ | Proxy _ -> assert false
  | Light _ -> client.base_dir // "sources.json"

let mode_to_endpoint = function
  | Client (None, _) | Mockup | Light (_, []) -> None
  | Client (Some endpoint, _) | Light (_, endpoint :: _) | Proxy endpoint ->
      Some endpoint

let string_of_endpoint ?hostname e =
  sf "%s://%s:%d" (scheme e) (address ?hostname e) (rpc_port e)

(* [?endpoint] can be used to override the default node stored in the client.
   Mockup nodes do not use [--endpoint] at all: RPCs are mocked up.
   Light mode needs a file (specified with [--sources] on the CLI)
   that contains a list of endpoints.
*)
let endpoint_arg ?(endpoint : endpoint option) client =
  let either o1 o2 = match (o1, o2) with Some _, _ -> o1 | _ -> o2 in
  (* pass [?endpoint] first: it has precedence over client.mode *)
  match either endpoint (mode_to_endpoint client.mode) with
  | None -> []
  | Some e -> ["--endpoint"; string_of_endpoint ~hostname:true e]

let media_type_arg client =
  match client with
  | Client (_, Some media_type) -> (
      match media_type with
      | Json -> ["--media-type"; "json"]
      | Binary -> ["--media-type"; "binary"]
      | Any -> ["--media-type"; "any"])
  | _ -> []

let mode_arg client =
  match client.mode with
  | Client _ -> []
  | Mockup -> ["--mode"; "mockup"]
  | Light _ -> ["--mode"; "light"; "--sources"; sources_file client]
  | Proxy _ -> ["--mode"; "proxy"]

let spawn_command ?log_command ?log_status_on_exit ?log_output
    ?(env = String_map.empty) ?endpoint ?hooks ?(admin = false) ?protocol_hash
    ?config_file ?(no_base_dir_warnings = false) ?block client command =
  let env =
    (* Set disclaimer to "Y" if unspecified, otherwise use given value *)
    String_map.update
      "TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER"
      (fun o -> Option.value ~default:"Y" o |> Option.some)
      env
  in
  let protocol_arg = optional_arg "protocol" Fun.id protocol_hash in
  let config_file_arg = optional_arg "config-file" Fun.id config_file in
  let no_base_dir_warnings_arg =
    optional_switch "no-base-dir-warnings" no_base_dir_warnings
  in
  let block_arg = Cli_arg.optional_arg "block" Fun.id block in
  Process.spawn
    ?runner:client.runner
    ~name:client.name
    ~color:client.color
    ~env
    ?log_command
    ?log_status_on_exit
    ?log_output
    ?hooks
    (if admin then client.admin_path else client.path)
  @@ endpoint_arg ?endpoint client
  @ protocol_arg @ media_type_arg client.mode @ mode_arg client
  @ base_dir_arg client @ config_file_arg @ no_base_dir_warnings_arg @ block_arg
  @ command

let spawn_command_with_stdin ?log_command ?log_status_on_exit ?log_output
    ?(env = String_map.empty) ?endpoint ?hooks ?(admin = false) ?protocol_hash
    client command =
  let env =
    (* Set disclaimer to "Y" if unspecified, otherwise use given value *)
    String_map.update
      "TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER"
      (fun o -> Option.value ~default:"Y" o |> Option.some)
      env
  in
  let protocol_arg = Cli_arg.optional_arg "protocol" Fun.id protocol_hash in
  Process.spawn_with_stdin
    ~name:client.name
    ~color:client.color
    ~env
    ?log_command
    ?log_status_on_exit
    ?log_output
    ?hooks
    (if admin then client.admin_path else client.path)
  @@ endpoint_arg ?endpoint client
  @ protocol_arg @ media_type_arg client.mode @ mode_arg client
  @ base_dir_arg client @ command

let url_encode str =
  let buffer = Buffer.create (String.length str * 3) in
  for i = 0 to String.length str - 1 do
    match str.[i] with
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-' | '/') as c ->
        Buffer.add_char buffer c
    | c ->
        Buffer.add_char buffer '%' ;
        let c1, c2 = Hex.of_char c in
        Buffer.add_char buffer c1 ;
        Buffer.add_char buffer c2
  done ;
  let result = Buffer.contents buffer in
  Buffer.reset buffer ;
  result

type meth = RPC_core.verb = GET | PUT | POST | PATCH | DELETE

type data = RPC_core.data

let string_of_meth = function
  | GET -> "get"
  | PUT -> "put"
  | POST -> "post"
  | PATCH -> "patch"
  | DELETE -> "delete"

type path = string list

let string_of_path path = "/" ^ String.concat "/" (List.map url_encode path)

type query_string = (string * string) list

let string_of_query_string = function
  | [] -> ""
  | qs ->
      let qs' = List.map (fun (k, v) -> (url_encode k, url_encode v)) qs in
      "?" ^ String.concat "&" @@ List.map (fun (k, v) -> k ^ "=" ^ v) qs'

let rpc_path_query_to_string ?(query_string = []) path =
  string_of_path path ^ string_of_query_string query_string

module Spawn = struct
  let rpc ?log_command ?log_status_on_exit ?log_output ?(better_errors = false)
      ?endpoint ?hooks ?env ?data ?query_string ?protocol_hash meth path client
      : JSON.t Runnable.process =
    let process =
      let data =
        Option.fold
          ~none:[]
          ~some:(function
            | RPC_core.Data data -> ["with"; JSON.encode_u data]
            | File file -> ["with"; "file:" ^ file])
          data
      in
      let query_string =
        Option.fold ~none:"" ~some:string_of_query_string query_string
      in
      let path = string_of_path path in
      let full_path = path ^ query_string in
      let better_error = if better_errors then ["--better-errors"] else [] in
      spawn_command
        ?log_command
        ?log_status_on_exit
        ?log_output
        ?endpoint
        ?hooks
        ?env
        ?protocol_hash
        client
        (better_error @ ["rpc"; string_of_meth meth; full_path] @ data)
    in
    let parse process =
      let* output = Process.check_and_read_stdout process in
      return (JSON.parse ~origin:(string_of_path path ^ " response") output)
    in
    {value = process; run = parse}
end

let spawn_rpc ?log_command ?log_status_on_exit ?log_output ?better_errors
    ?endpoint ?hooks ?env ?data ?query_string ?protocol_hash meth path client =
  let*? res =
    Spawn.rpc
      ?log_command
      ?log_status_on_exit
      ?log_output
      ?better_errors
      ?endpoint
      ?hooks
      ?env
      ?data
      ?query_string
      ?protocol_hash
      meth
      path
      client
  in
  res

let rpc ?log_command ?log_status_on_exit ?log_output ?better_errors ?endpoint
    ?hooks ?env ?data ?query_string ?protocol_hash meth path client =
  let*! res =
    Spawn.rpc
      ?log_command
      ?log_status_on_exit
      ?log_output
      ?better_errors
      ?endpoint
      ?hooks
      ?env
      ?data
      ?query_string
      ?protocol_hash
      meth
      path
      client
  in
  return res

let spawn_rpc_list ?endpoint client =
  spawn_command ?endpoint client ["rpc"; "list"]

let rpc_list ?endpoint client =
  spawn_rpc_list ?endpoint client |> Process.check_and_read_stdout

let spawn_rpc_schema ?log_command ?log_status_on_exit ?log_output
    ?(better_errors = false) ?endpoint ?hooks ?env ?protocol_hash meth path
    client =
  spawn_command
    ?log_command
    ?log_status_on_exit
    ?log_output
    ?endpoint
    ?hooks
    ?env
    ?protocol_hash
    client
    (optional_switch "better-errors" better_errors
    @ ["rpc"; "schema"; string_of_meth meth; string_of_path path])

let rpc_schema ?log_command ?log_status_on_exit ?log_output ?better_errors
    ?endpoint ?hooks ?env ?protocol_hash meth path client =
  let* json_schema =
    spawn_rpc_schema
      ?log_command
      ?log_status_on_exit
      ?log_output
      ?better_errors
      ?endpoint
      ?hooks
      ?env
      ?protocol_hash
      meth
      path
      client
    |> Process.check_and_read_stdout
  in
  return (JSON.parse ~origin:"rpc_schema" json_schema)

let spawn_shell_header ?endpoint ?(chain = "main") ?(block = "head") client =
  let path = ["chains"; chain; "blocks"; block; "header"; "shell"] in
  spawn_rpc ?endpoint GET path client

let shell_header ?endpoint ?chain ?block client =
  spawn_shell_header ?endpoint ?chain ?block client
  |> Process.check_and_read_stdout

let level ?endpoint ?chain ?block client =
  let* shell = shell_header ?endpoint ?chain ?block client in
  let json = JSON.parse ~origin:"level" shell in
  JSON.get "level" json |> JSON.as_int |> return

let parse_list_protocols_output output =
  String.split_on_char '\n' output |> List.filter (fun s -> s <> "")

module Admin = struct
  let spawn_command = spawn_command ~admin:true

  let spawn_trust_address ?endpoint ~peer client =
    spawn_command
      ?endpoint
      client
      [
        "trust";
        "address";
        Printf.sprintf
          "%s:%d"
          (address ?from:endpoint (Node peer))
          (Node.net_port peer);
      ]

  let spawn_untrust_address ?endpoint ~peer client =
    spawn_command
      ?endpoint
      client
      [
        "untrust";
        "address";
        Printf.sprintf
          "%s:%d"
          (address ?from:endpoint (Node peer))
          (Node.net_port peer);
      ]

  let trust_address ?endpoint ~peer client =
    spawn_trust_address ?endpoint ~peer client |> Process.check

  let untrust_address ?endpoint ~peer client =
    spawn_untrust_address ?endpoint ~peer client |> Process.check

  let spawn_connect_address ?endpoint ~peer client =
    spawn_command
      ?endpoint
      client
      [
        "connect";
        "address";
        Printf.sprintf
          "%s:%d"
          (address ?from:endpoint (Node peer))
          (Node.net_port peer);
      ]

  let connect_address ?endpoint ~peer client =
    spawn_connect_address ?endpoint ~peer client |> Process.check

  let spawn_kick_peer ?endpoint ~peer client =
    spawn_command ?endpoint client ["kick"; "peer"; peer]

  let kick_peer ?endpoint ~peer client =
    spawn_kick_peer ?endpoint ~peer client |> Process.check

  let spawn_ban_peer ?endpoint ~peer client =
    spawn_command ?endpoint client ["ban"; "peer"; peer]

  let ban_peer ?endpoint ~peer client =
    spawn_ban_peer ?endpoint ~peer client |> Process.check

  let spawn_p2p_stat ?endpoint client =
    spawn_command ?endpoint client ["p2p"; "stat"]

  let p2p_stat ?endpoint client =
    spawn_p2p_stat ?endpoint client |> Process.check_and_read_stdout

  let spawn_inject_protocol ?endpoint ~protocol_path client =
    spawn_command ?endpoint client ["inject"; "protocol"; protocol_path]

  let inject_protocol ?endpoint ~protocol_path client =
    let* output =
      spawn_inject_protocol ?endpoint ~protocol_path client
      |> Process.check_and_read_stdout
    in
    match output =~* rex "Injected protocol ([^ ]+) successfully" with
    | None ->
        Test.fail
          "octez-admin-client inject protocol did not answer \"Injected \
           protocol ... successfully\""
    | Some hash -> return hash

  let spawn_list_protocols ?endpoint client =
    spawn_command ?endpoint client ["list"; "protocols"]

  let list_protocols ?endpoint client =
    let* output =
      spawn_list_protocols ?endpoint client |> Process.check_and_read_stdout
    in
    return (parse_list_protocols_output output)

  let spawn_protocol_environment ?endpoint client protocol =
    spawn_command ?endpoint client ["protocol"; "environment"; protocol]

  let protocol_environment ?endpoint client protocol =
    let* output =
      spawn_protocol_environment ?endpoint client protocol
      |> Process.check_and_read_stdout
    in
    match output =~* rex "Protocol [^ ]+ uses environment (V\\d+)" with
    | None ->
        Test.fail
          "octez-admin-client protocol environment did not answer \"Protocol \
           ... uses environment V...\""
    | Some version -> return version
end

let spawn_version client = spawn_command client ["--version"]

let version client = spawn_version client |> Process.check

let spawn_import_encrypted_secret_key ?hooks ?(force = false) ?endpoint client
    (secret_key : Account.secret_key) ~alias =
  let sk_uri =
    match secret_key with
    | Encrypted _ -> Account.uri_of_secret_key secret_key
    | Unencrypted _ -> Test.fail "Unencrypted key"
  in
  spawn_command_with_stdin
    ?hooks
    ?endpoint
    client
    (["import"; "secret"; "key"; alias; sk_uri]
    @ if force then ["--force"] else [])

let import_encrypted_secret_key ?hooks ?force ?endpoint client
    (secret_key : Account.secret_key) ~alias ~password =
  let process, output_channel =
    spawn_import_encrypted_secret_key
      ?hooks
      ?force
      ?endpoint
      client
      secret_key
      ~alias
  in
  let* () = Lwt_io.write_line output_channel password in
  let* () = Lwt_io.close output_channel in
  Process.check process

let spawn_import_secret_key ?(force = false) ?endpoint client
    (secret_key : Account.secret_key) ~alias =
  let sk_uri =
    "unencrypted:" ^ Account.require_unencrypted_secret_key ~__LOC__ secret_key
  in
  let force = if force then ["--force"] else [] in
  spawn_command
    ?endpoint
    client
    (["import"; "secret"; "key"; alias; sk_uri] @ force)

let spawn_import_signer_key ?endpoint ?(force = false) client ~public_key_hash
    ~alias signer_uri =
  let uri = Uri.with_path signer_uri public_key_hash in
  spawn_command
    ?endpoint
    client
    (["import"; "secret"; "key"; alias; Uri.to_string uri]
    @ if force then ["--force"] else [])

let import_signer_key ?endpoint ?force client ~public_key_hash ~alias signer_uri
    =
  spawn_import_signer_key
    ?endpoint
    ?force
    client
    ~public_key_hash
    ~alias
    signer_uri
  |> Process.check

let import_secret_key ?force ?endpoint client secret_key ~alias =
  spawn_import_secret_key ?force ?endpoint client secret_key ~alias
  |> Process.check

let spawn_import_keys_from_mnemonic ?endpoint ?(force = false)
    ?(encrypt = false) client ~alias =
  spawn_command_with_stdin
    ?endpoint
    client
    (["import"; "keys"; "from"; "mnemonic"; alias]
    @ optional_switch "force" force
    @ optional_switch "encrypt" encrypt)

let import_keys_from_mnemonic ?endpoint ?force ?passphrase ?encryption_password
    client ~alias ~mnemonic =
  let stdin =
    (* The client first asks for the mnemonic, then its passphrase.
       If the [encryption_password] is set, then the [--encrypt] flag
       will be passed to [import keys from mnemonic]. In this case, the
       client will also ask for the encryption password twice.
    *)
    let lines =
      [String.concat " " mnemonic; Option.value ~default:"" passphrase]
      @
      match encryption_password with
      | Some password ->
          [password; (* a second time for confirmation *) password]
      | None -> []
    in
    String.concat "\n" lines ^ "\n"
  in
  let process, output_channel =
    spawn_import_keys_from_mnemonic
      ?endpoint
      ?force
      ~encrypt:(Option.is_some encryption_password)
      client
      ~alias
  in
  let* () = Lwt_io.write_line output_channel stdin in
  let* () = Lwt_io.close output_channel in
  Process.check process

module Time = Tezos_base.Time.System

let default_delay = Time.Span.of_seconds_exn (3600. *. 24. *. 365.)

type timestamp = Now | Ago of Time.Span.t | At of Time.t

let time_of_timestamp timestamp =
  match timestamp with
  | Now -> Time.now ()
  | Ago delay -> (
      match Ptime.sub_span (Time.now ()) delay with
      | None -> Ptime.epoch
      | Some tm -> tm)
  | At tm -> tm

let spawn_activate_protocol ?endpoint ?block ?protocol ?protocol_hash
    ?(fitness = 1) ?(key = Constant.activator.alias)
    ?(timestamp = Ago default_delay) ?parameter_file client =
  let timestamp = time_of_timestamp timestamp in
  let protocol_hash, parameter_file =
    match (protocol, protocol_hash, parameter_file) with
    | Some protocol, None, _ ->
        ( Protocol.hash protocol,
          Option.value
            parameter_file
            ~default:(Protocol.parameter_file protocol) )
    | None, Some protocol_hash, Some parameter_file ->
        (protocol_hash, parameter_file)
    | _, _, _ ->
        Test.fail
          ~__LOC__
          "Must pass either protocol or both protocol_file and parameter_file \
           to spawn_activate_protocol"
  in
  spawn_command
    ?endpoint
    ?block
    client
    [
      "activate";
      "protocol";
      protocol_hash;
      "with";
      "fitness";
      string_of_int fitness;
      "and";
      "key";
      key;
      "and";
      "parameters";
      parameter_file;
      "--timestamp";
      Time.to_notation timestamp;
    ]

let activate_protocol ?endpoint ?block ?protocol ?protocol_hash ?fitness ?key
    ?timestamp ?parameter_file client =
  spawn_activate_protocol
    ?endpoint
    ?block
    ?protocol
    ?protocol_hash
    ?fitness
    ?key
    ?timestamp
    ?parameter_file
    client
  |> Process.check

let node_of_endpoint = function
  | Node n -> Some n
  | Proxy_server _ | Foreign_endpoint _ -> None

let node_of_client_mode = function
  | Client (Some endpoint, _) -> node_of_endpoint endpoint
  | Proxy endpoint -> node_of_endpoint endpoint
  | Light (_, endpoints) -> List.find_map node_of_endpoint endpoints
  | Client (None, _) -> None
  | Mockup -> None

let activate_protocol_and_wait ?endpoint ?protocol ?protocol_hash ?fitness ?key
    ?timestamp ?parameter_file ?node client =
  let node =
    match node with
    | Some n -> n
    | None -> (
        match node_of_client_mode client.mode with
        | Some n -> n
        | None -> Test.fail "No node found for activate_protocol_and_wait")
  in
  let level_before = Node.get_last_seen_level node in
  let* () =
    activate_protocol
      ?endpoint
      ?protocol
      ?protocol_hash
      ?fitness
      ?key
      ?timestamp
      ?parameter_file
      client
  in
  let* _lvl = Node.wait_for_level node (level_before + 1) in
  unit

let empty_mempool_file ?(filename = "mempool.json") () =
  let mempool_str = "[]" in
  let mempool = Temp.file filename in
  write_file mempool ~contents:mempool_str ;
  mempool

let spawn_bake_for ?endpoint ?protocol ?(keys = [Constant.bootstrap1.alias])
    ?minimal_fees ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte
    ?(minimal_timestamp = true) ?mempool ?(ignore_node_mempool = false) ?count
    ?force ?context_path ?dal_node_endpoint ?(state_recorder = false) client =
  spawn_command
    ?endpoint
    client
    (optional_arg "protocol" Protocol.hash protocol
    @ ["bake"; "for"] @ keys
    @ optional_arg "minimal-fees" string_of_int minimal_fees
    @ optional_arg
        "minimal-nanotez-per-gas-unit"
        string_of_int
        minimal_nanotez_per_gas_unit
    @ optional_arg
        "minimal-nanotez-per-byte"
        string_of_int
        minimal_nanotez_per_byte
    @ optional_arg "operations-pool" Fun.id mempool
    @ (if ignore_node_mempool then ["--ignore-node-mempool"] else [])
    @ (if minimal_timestamp then ["--minimal-timestamp"] else [])
    @ optional_arg "count" string_of_int count
    @ (match force with None | Some false -> [] | Some true -> ["--force"])
    @ optional_arg "context" Fun.id context_path
    @ optional_arg "dal-node" Fun.id dal_node_endpoint
    @ optional_switch "record_state" state_recorder)

let bake_for ?endpoint ?protocol ?keys ?minimal_fees
    ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte ?minimal_timestamp
    ?mempool ?ignore_node_mempool ?count ?force ?context_path ?dal_node_endpoint
    ?state_recorder ?expect_failure client =
  spawn_bake_for
    ?endpoint
    ?keys
    ?minimal_fees
    ?minimal_nanotez_per_gas_unit
    ?minimal_nanotez_per_byte
    ?minimal_timestamp
    ?mempool
    ?ignore_node_mempool
    ?count
    ?force
    ?context_path
    ?protocol
    ?dal_node_endpoint
    ?state_recorder
    client
  |> Process.check ?expect_failure

let bake_for_and_wait_level ?endpoint ?protocol ?keys ?minimal_fees
    ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte ?minimal_timestamp
    ?mempool ?ignore_node_mempool ?count ?force ?context_path ?level_before
    ?node ?dal_node_endpoint ?state_recorder client =
  let node =
    match node with
    | Some n -> n
    | None -> (
        match node_of_client_mode client.mode with
        | Some n -> n
        | None -> Test.fail "No node found for bake_for_and_wait")
  in
  let actual_level_before = Node.get_last_seen_level node in
  Option.iter
    (fun level_before ->
      Check.(actual_level_before = level_before)
        Check.int
        ~error_msg:"bake_for_and_wait: level before is %L, expected %R")
    level_before ;
  let* () =
    bake_for
      ?endpoint
      ?protocol
      ?keys
      ?minimal_fees
      ?minimal_nanotez_per_gas_unit
      ?minimal_nanotez_per_byte
      ?minimal_timestamp
      ?mempool
      ?ignore_node_mempool
      ?count
      ?force
      ?context_path
      ?dal_node_endpoint
      ?state_recorder
      client
  in
  Node.wait_for_level node (actual_level_before + 1)

let bake_for_and_wait ?endpoint ?protocol ?keys ?minimal_fees
    ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte ?minimal_timestamp
    ?mempool ?ignore_node_mempool ?count ?force ?context_path ?level_before
    ?node ?dal_node_endpoint client =
  let* (_level : int) =
    bake_for_and_wait_level
      ?endpoint
      ?protocol
      ?keys
      ?minimal_fees
      ?minimal_nanotez_per_gas_unit
      ?minimal_nanotez_per_byte
      ?minimal_timestamp
      ?mempool
      ?ignore_node_mempool
      ?count
      ?force
      ?context_path
      ?level_before
      ?node
      ?dal_node_endpoint
      client
  in
  unit

(* Handle attesting and preattesting similarly *)
type tenderbake_action = Preattest | Attest | Propose

let tenderbake_action_to_string ~use_legacy_attestation_name = function
  | Preattest ->
      if use_legacy_attestation_name then "preendorse" else "preattest"
  | Attest -> if use_legacy_attestation_name then "endorse" else "attest"
  | Propose -> "propose"

let spawn_tenderbake_action_for ~tenderbake_action ?endpoint ?protocol
    ?(key = [Constant.bootstrap1.alias]) ?(minimal_timestamp = false)
    ?(force = false) client =
  let use_legacy_attestation_name =
    match protocol with
    | None -> false
    | Some protocol -> Protocol.(number protocol < 018)
  in
  spawn_command
    ?endpoint
    client
    (optional_arg "protocol" Protocol.hash protocol
    @ [
        tenderbake_action_to_string
          ~use_legacy_attestation_name
          tenderbake_action;
        "for";
      ]
    @ key
    @ (if minimal_timestamp then ["--minimal-timestamp"] else [])
    @ if force then ["--force"] else [])

let spawn_attest_for ?endpoint ?protocol ?key ?force client =
  spawn_tenderbake_action_for
    ~tenderbake_action:Attest
    ~minimal_timestamp:false
    ?endpoint
    ?protocol
    ?key
    ?force
    client

let spawn_preattest_for ?endpoint ?protocol ?key ?force client =
  spawn_tenderbake_action_for
    ~tenderbake_action:Preattest
    ~minimal_timestamp:false
    ?endpoint
    ?protocol
    ?key
    ?force
    client

let spawn_propose_for ?endpoint ?minimal_timestamp ?protocol ?key ?force client
    =
  spawn_tenderbake_action_for
    ~tenderbake_action:Propose
    ?minimal_timestamp
    ?endpoint
    ?protocol
    ?key
    ?force
    client

let attest_for ?endpoint ?protocol ?key ?force client =
  spawn_attest_for ?endpoint ?protocol ?key ?force client |> Process.check

let preattest_for ?endpoint ?protocol ?key ?force client =
  spawn_preattest_for ?endpoint ?protocol ?key ?force client |> Process.check

let propose_for ?endpoint ?(minimal_timestamp = true) ?protocol ?key ?force
    client =
  spawn_propose_for ?endpoint ?protocol ?key ?force ~minimal_timestamp client
  |> Process.check

let id = ref 0

let spawn_gen_keys ?(force = false) ?alias ?sig_alg client =
  let alias =
    match alias with
    | None ->
        incr id ;
        sf "tezt_%d" !id
    | Some alias -> alias
  in
  let force = if force then ["--force"] else [] in
  ( spawn_command client @@ ["gen"; "keys"; alias] @ force
    @ optional_arg "sig" Fun.id sig_alg,
    alias )

let gen_keys ?force ?alias ?sig_alg client =
  let p, alias = spawn_gen_keys ?force ?alias ?sig_alg client in
  let* () = Process.check p in
  return alias

let spawn_activate_account ?(wait = "none") client ~alias ~activation_key =
  spawn_command client
  @@ ["--wait"; wait; "activate"; "account"; alias; "with"; activation_key]

let activate_account ?wait client ~alias ~activation_key =
  spawn_activate_account ?wait client ~alias ~activation_key |> Process.check

let spawn_show_address ~alias client =
  spawn_command client ["show"; "address"; alias; "--show-secret"]

let show_address ~alias client =
  let* client_output =
    spawn_show_address ~alias client |> Process.check_and_read_stdout
  in
  return @@ Account.parse_client_output ~alias ~client_output

let spawn_list_known_addresses client =
  spawn_command client ["list"; "known"; "addresses"]

let list_known_addresses client =
  let* client_output =
    spawn_list_known_addresses client |> Process.check_and_read_stdout
  in
  let addresses =
    client_output |> String.trim |> String.split_on_char '\n'
    |> List.map @@ fun line ->
       match line =~** rex "(.*): ([^ ]*)" with
       | Some (alias, pkh) -> (alias, pkh)
       | None ->
           Test.fail
             "Cannot parse line %S from list of known addresses from \
              client_output: %s"
             line
             client_output
  in
  return addresses

let gen_and_show_keys ?alias ?sig_alg client =
  let* alias = gen_keys ?alias ?sig_alg client in
  show_address ~alias client

let spawn_add_address ?(force = false) client ~alias ~src =
  spawn_command client
  @@ ["add"; "address"; alias; src]
  @ optional_switch "force" force

let add_address ?force client ~alias ~src =
  spawn_add_address ?force client ~alias ~src |> Process.check

let spawn_bls_gen_keys ?hooks ?(force = false) ?alias client =
  let alias =
    match alias with
    | None ->
        incr id ;
        sf "tezt_%d" !id
    | Some alias -> alias
  in
  ( spawn_command
      ?hooks
      client
      (["bls"; "gen"; "keys"; alias] @ optional_switch "force" force),
    alias )

let bls_gen_keys ?hooks ?force ?alias client =
  let p, alias = spawn_bls_gen_keys ?hooks ?force ?alias client in
  let* () = Process.check p in
  return alias

let spawn_bls_list_keys ?hooks client =
  spawn_command ?hooks client ["bls"; "list"; "keys"]

let parse_list_keys output =
  output |> String.trim |> String.split_on_char '\n'
  |> List.map (fun s ->
         match s =~** rex "^(\\w+): (\\w{36})" with
         | Some s -> s
         | None ->
             Test.fail
               ~__LOC__
               "Cannot extract `list keys` format from client_output: %s"
               output)

let bls_list_keys ?hooks client =
  let* out =
    spawn_bls_list_keys ?hooks client |> Process.check_and_read_stdout
  in
  return (parse_list_keys out)

let spawn_bls_show_address ?hooks ~alias client =
  spawn_command ?hooks client ["bls"; "show"; "address"; alias; "--show-secret"]

let bls_show_address ?hooks ~alias client =
  let* out =
    spawn_bls_show_address ?hooks ~alias client |> Process.check_and_read_stdout
  in
  return (Account.parse_client_output_aggregate ~alias ~client_output:out)

let bls_gen_and_show_keys ?alias client =
  let* alias = bls_gen_keys ?alias client in
  bls_show_address ~alias client

let spawn_bls_import_secret_key ?hooks ?(force = false)
    (key : Account.aggregate_key) client =
  let sk_uri =
    "aggregate_unencrypted:"
    ^ Account.require_unencrypted_secret_key ~__LOC__ key.aggregate_secret_key
  in
  spawn_command
    ?hooks
    client
    (["bls"; "import"; "secret"; "key"; key.aggregate_alias; sk_uri]
    @ if force then ["--force"] else [])

let bls_import_secret_key ?hooks ?force key sc_client =
  spawn_bls_import_secret_key ?hooks ?force key sc_client |> Process.check

let spawn_transfer ?hooks ?log_output ?endpoint ?(wait = "none") ?burn_cap ?fee
    ?gas_limit ?storage_limit ?counter ?entrypoint ?arg ?(simulation = false)
    ?(force = false) ~amount ~giver ~receiver client =
  spawn_command
    ?log_output
    ?endpoint
    ?hooks
    client
    (["--wait"; wait]
    @ ["transfer"; Tez.to_string amount; "from"; giver; "to"; receiver]
    @ Option.fold
        ~none:[]
        ~some:(fun f -> ["--fee"; Tez.to_string f; "--force-low-fee"])
        fee
    @ optional_arg "burn-cap" Tez.to_string burn_cap
    @ optional_arg "gas-limit" string_of_int gas_limit
    @ optional_arg "storage-limit" string_of_int storage_limit
    @ optional_arg "counter" string_of_int counter
    @ optional_arg "entrypoint" Fun.id entrypoint
    @ optional_arg "arg" Fun.id arg
    @ (if simulation then ["--simulation"] else [])
    @ if force then ["--force"] else [])

let transfer ?hooks ?log_output ?endpoint ?wait ?burn_cap ?fee ?gas_limit
    ?storage_limit ?counter ?entrypoint ?arg ?simulation ?force ?expect_failure
    ~amount ~giver ~receiver client =
  spawn_transfer
    ?log_output
    ?endpoint
    ?hooks
    ?wait
    ?burn_cap
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    ?entrypoint
    ?arg
    ?simulation
    ?force
    ~amount
    ~giver
    ~receiver
    client
  |> Process.check ?expect_failure

let spawn_call ?hooks ?log_output ?endpoint ?(wait = "none") ?burn_cap
    ?entrypoint ?arg ~destination ~source client =
  spawn_command ?log_output ?endpoint ?hooks client
  @@ ["--wait"; wait]
  @ ["call"; destination; "from"; source]
  @ optional_arg "burn-cap" Tez.to_string burn_cap
  @ optional_arg "entrypoint" Fun.id entrypoint
  @ optional_arg "arg" Fun.id arg

let call ?hooks ?log_output ?endpoint ?wait ?burn_cap ?entrypoint ?arg
    ~destination ~source client =
  spawn_call
    ?hooks
    ?log_output
    ?endpoint
    ?wait
    ?burn_cap
    ?entrypoint
    ?arg
    ~destination
    ~source
    client
  |> Process.check

let multiple_transfers ?log_output ?endpoint ?(wait = "none") ?burn_cap ?fee_cap
    ?gas_limit ?storage_limit ?counter ?(simulation = false) ?(force = false)
    ~giver ~json_batch client =
  let value =
    spawn_command
      ?log_output
      ?endpoint
      client
      (["--wait"; wait]
      @ ["multiple"; "transfers"; "from"; giver; "using"; json_batch]
      @ Option.fold
          ~none:[]
          ~some:(fun f -> ["--fee-cap"; Tez.to_string f; "--force-low-fee"])
          fee_cap
      @ optional_arg "burn-cap" Tez.to_string burn_cap
      @ optional_arg "gas-limit" string_of_int gas_limit
      @ optional_arg "storage-limit" string_of_int storage_limit
      @ optional_arg "counter" string_of_int counter
      @ (if simulation then ["--simulation"] else [])
      @ if force then ["--force"] else [])
  in
  {value; run = Process.check}

let spawn_register_delegate ?endpoint ?(wait = "none") ~delegate client =
  spawn_command
    ?endpoint
    client
    (["--wait"; wait] @ ["register"; "key"; delegate; "as"; "delegate"])

let register_delegate ?endpoint ?wait ~delegate client =
  spawn_register_delegate ?endpoint ?wait ~delegate client
  |> Process.check_and_read_stdout

let spawn_get_delegate ?endpoint ~src client =
  spawn_command ?endpoint client ["get"; "delegate"; "for"; src]

let get_delegate ?endpoint ~src client =
  let* output =
    spawn_get_delegate ?endpoint ~src client |> Process.check_and_read_stdout
  in
  Lwt.return (output =~* rex "(tz[a-zA-Z0-9]+) \\(.*\\)")

let set_delegate ?endpoint ?(wait = "none") ?fee ?fee_cap
    ?(force_low_fee = false) ?expect_failure ?(simulation = false) ~src
    ~delegate client =
  let value =
    spawn_command
      ?endpoint
      client
      (["--wait"; wait]
      @ ["set"; "delegate"; "for"; src; "to"; delegate]
      @ optional_arg "fee" Tez.to_string fee
      @ optional_arg "fee-cap" Tez.to_string fee_cap
      @ (if simulation then ["--simulation"] else [])
      @ if force_low_fee then ["--force-low-fee"] else [])
  in
  {value; run = Process.check ?expect_failure}

let spawn_call_contract ?hooks ?endpoint ?burn_cap ~src ~destination ?entrypoint
    ?arg client =
  spawn_command
    ?hooks
    ?endpoint
    client
    (["call"; destination; "from"; src]
    @ optional_arg "entrypoint" Fun.id entrypoint
    @ optional_arg "arg" Fun.id arg
    @ optional_arg "burn-cap" Tez.to_string burn_cap)

let call_contract ?hooks ?endpoint ?burn_cap ~src ~destination ?entrypoint ?arg
    client =
  spawn_call_contract
    ?hooks
    ?endpoint
    ?burn_cap
    ~src
    ~destination
    ?entrypoint
    ?arg
    client
  |> Process.check

let reveal ?endpoint ?(wait = "none") ?fee ?fee_cap ?(force_low_fee = false)
    ~src client =
  let value =
    spawn_command
      ?endpoint
      client
      (["--wait"; wait]
      @ ["reveal"; "key"; "for"; src]
      @ optional_arg "fee" Tez.to_string fee
      @ optional_arg "fee-cap" Tez.to_string fee_cap
      @ if force_low_fee then ["--force-low-fee"] else [])
  in
  {value; run = Process.check}

let spawn_withdraw_delegate ?endpoint ?(wait = "none") ~src client =
  spawn_command
    ?endpoint
    client
    (["--wait"; wait] @ ["withdraw"; "delegate"; "from"; src])

let withdraw_delegate ?endpoint ?wait ?expect_failure ~src client =
  spawn_withdraw_delegate ?endpoint ?wait ~src client
  |> Process.check ?expect_failure

let spawn_get_balance_for ?endpoint ~account client =
  spawn_command ?endpoint client ["get"; "balance"; "for"; account]

let get_balance_for ?endpoint ~account client =
  let* output =
    spawn_get_balance_for ?endpoint ~account client
    |> Process.check_and_read_stdout
  in
  return @@ Tez.parse_floating output

let spawn_ticket_balance ?hooks ~contract ~ticketer ~content_type ~content
    client =
  spawn_command
    ?hooks
    client
    [
      "get";
      "ticket";
      "balance";
      "for";
      contract;
      "with";
      "ticketer";
      ticketer;
      "and";
      "type";
      content_type;
      "and";
      "content";
      content;
    ]

let ticket_balance ?hooks ~contract ~ticketer ~content_type ~content client =
  spawn_ticket_balance ?hooks ~contract ~ticketer ~content_type ~content client
  |> Process.check_and_read_stdout

let all_ticket_balances ?hooks ~contract client =
  let value =
    spawn_command
      ?hooks
      client
      ["get"; "all"; "ticket"; "balances"; "for"; contract]
  in
  let run = Process.check_and_read_stdout in
  {value; run}

let spawn_create_mockup ?(sync_mode = Synchronous) ?parameter_file
    ?bootstrap_accounts_file ~protocol client =
  let cmd =
    let common = ["--protocol"; Protocol.hash protocol; "create"; "mockup"] in
    (match sync_mode with
    | Synchronous -> common
    | Asynchronous -> common @ ["--asynchronous"])
    @ optional_arg "protocol-constants" Fun.id parameter_file
    @ optional_arg "bootstrap-accounts" Fun.id bootstrap_accounts_file
  in
  spawn_command client cmd

let create_mockup ?sync_mode ?parameter_file ?bootstrap_accounts_file ~protocol
    client =
  spawn_create_mockup
    ?sync_mode
    ?parameter_file
    ?bootstrap_accounts_file
    ~protocol
    client
  |> Process.check

let spawn_submit_proposals ?(key = Constant.bootstrap1.alias) ?(wait = "none")
    ?proto_hash ?(proto_hashes = []) ?(force = false) client =
  let proto_hashes =
    match proto_hash with None -> proto_hashes | Some h -> h :: proto_hashes
  in
  spawn_command
    client
    ("--wait" :: wait :: "submit" :: "proposals" :: "for" :: key :: proto_hashes
    @ optional_switch "force" force)

let submit_proposals ?key ?wait ?proto_hash ?proto_hashes ?force ?expect_failure
    client =
  spawn_submit_proposals ?key ?wait ?proto_hash ?proto_hashes ?force client
  |> Process.check ?expect_failure

type ballot = Nay | Pass | Yay

let spawn_submit_ballot ?(key = Constant.bootstrap1.alias) ?(wait = "none")
    ~proto_hash vote client =
  let string_of_vote = function
    | Yay -> "yay"
    | Nay -> "nay"
    | Pass -> "pass"
  in
  spawn_command
    client
    (["--wait"; wait]
    @ ["submit"; "ballot"; "for"; key; proto_hash; string_of_vote vote])

let submit_ballot ?key ?wait ~proto_hash vote client =
  spawn_submit_ballot ?key ?wait ~proto_hash vote client |> Process.check

let set_deposits_limit ?hooks ?endpoint ?(wait = "none") ~src ~limit client =
  spawn_command
    ?hooks
    ?endpoint
    client
    (["--wait"; wait] @ ["set"; "deposits"; "limit"; "for"; src; "to"; limit])
  |> Process.check_and_read_stdout

let unset_deposits_limit ?hooks ?endpoint ?(wait = "none") ~src client =
  spawn_command
    ?hooks
    ?endpoint
    client
    (["--wait"; wait] @ ["unset"; "deposits"; "limit"; "for"; src])
  |> Process.check_and_read_stdout

let increase_paid_storage ?hooks ?endpoint ?(wait = "none") ~contract ~amount
    ~payer client =
  spawn_command
    ?hooks
    ?endpoint
    client
    (["--wait"; wait]
    @ [
        "increase";
        "the";
        "paid";
        "storage";
        "of";
        contract;
        "by";
        string_of_int amount;
        "bytes";
        "from";
        payer;
      ])
  |> Process.check_and_read_stdout

let used_storage_space ?hooks ?endpoint ?(wait = "none") ~contract client =
  spawn_command
    ?hooks
    ?endpoint
    client
    (["--wait"; wait]
    @ ["get"; "contract"; "used"; "storage"; "space"; "for"; contract])
  |> Process.check_and_read_stdout

let paid_storage_space ?hooks ?endpoint ?(wait = "none") ~contract client =
  spawn_command
    ?hooks
    ?endpoint
    client
    (["--wait"; wait]
    @ ["get"; "contract"; "paid"; "storage"; "space"; "for"; contract])
  |> Process.check_and_read_stdout

let update_consensus_key ?hooks ?endpoint ?(wait = "none") ?burn_cap
    ?expect_failure ~src ~pk client =
  spawn_command
    ?hooks
    ?endpoint
    client
    (["--wait"; wait]
    @ ["set"; "consensus"; "key"; "for"; src; "to"; pk]
    @ optional_arg "burn-cap" Tez.to_string burn_cap)
  |> Process.check ?expect_failure

let drain_delegate ?hooks ?endpoint ?(wait = "none") ?expect_failure ~delegate
    ~consensus_key ?destination client =
  let destination =
    match destination with
    | None -> []
    | Some destination -> [destination; "with"]
  in
  spawn_command
    ?hooks
    ?endpoint
    client
    (["--wait"; wait]
    @ ["drain"; "delegate"; delegate; "to"]
    @ destination @ [consensus_key])
  |> Process.check ?expect_failure

let spawn_originate_contract ?hooks ?log_output ?endpoint ?(wait = "none") ?init
    ?burn_cap ?gas_limit ?(dry_run = false) ?(force = false) ~alias ~amount ~src
    ~prg client =
  spawn_command
    ?hooks
    ?log_output
    ?endpoint
    client
    (["--wait"; wait]
    @ [
        "originate";
        "contract";
        alias;
        "transferring";
        Tez.to_string amount;
        "from";
        src;
        "running";
        prg;
      ]
    @ optional_arg "init" Fun.id init
    @ optional_arg "burn-cap" Tez.to_string burn_cap
    @ optional_arg "gas-limit" string_of_int gas_limit
    @ optional_switch "dry-run" dry_run
    @ optional_switch "force" force)

type conversion_kind = Script | Data

let conversion_format_to_string = function
  | `Michelson -> "michelson"
  | `Binary -> "binary"
  | `Json -> "json"
  | `OCaml -> "ocaml"

let convert ?endpoint ~kind ~input ~src_format ~dst_format ?typecheck client =
  spawn_command
    ?endpoint
    client
    ([
       "convert";
       (match kind with Script -> "script" | Data -> "data");
       input;
       "from";
       conversion_format_to_string src_format;
       "to";
       conversion_format_to_string dst_format;
     ]
    @ optional_arg "type" Fun.id typecheck)
  |> Process.check_and_read_stdout

let convert_script ~script ~src_format ~dst_format ?typecheck client =
  convert ~kind:Script ~input:script ~src_format ~dst_format ?typecheck client

let convert_data ~data ~src_format ~dst_format ?typecheck client =
  convert ~kind:Data ~input:data ~src_format ~dst_format ?typecheck client

let convert_michelson_to_json ~kind ?endpoint ~input ?typecheck client =
  let* client_output =
    convert
      ?endpoint
      ~kind
      ~input
      ~src_format:`Michelson
      ~dst_format:`Json
      ?typecheck
      client
  in
  Lwt.return (Ezjsonm.from_string client_output)

let convert_script_to_json ?endpoint ~script client =
  convert_michelson_to_json ~kind:Script ?endpoint ~input:script client

let convert_data_to_json ?endpoint ~data ?typecheck client =
  convert_michelson_to_json ~kind:Data ?endpoint ~input:data ?typecheck client

let originate_contract ?hooks ?log_output ?endpoint ?wait ?init ?burn_cap
    ?gas_limit ?dry_run ?force ~alias ~amount ~src ~prg client =
  let* client_output =
    spawn_originate_contract
      ?endpoint
      ?log_output
      ?hooks
      ?wait
      ?init
      ?burn_cap
      ?gas_limit
      ?dry_run
      ?force
      ~alias
      ~amount
      ~src
      ~prg
      client
    |> Process.check_and_read_stdout
  in
  match client_output =~* rex "New contract ?(KT1\\w{33})" with
  | None ->
      Test.fail
        "Cannot extract contract hash from client_output: %s"
        client_output
  | Some hash -> return hash

let spawn_originate_contract_at ?hooks ?log_output ?endpoint ?wait ?init
    ?burn_cap ?gas_limit ?dry_run ?force ?prefix ?alias ~amount ~src client name
    protocol =
  let alias =
    match alias with
    | Some alias -> alias
    | None -> (
        match List.rev name with
        | [] -> Test.fail "name must not be an empty list"
        | last :: _ -> last)
  in
  let prg =
    Michelson_script.find ?prefix name protocol |> Michelson_script.path
  in
  let process =
    spawn_originate_contract
      ?hooks
      ?log_output
      ?endpoint
      ?wait
      ?init
      ?burn_cap
      ?gas_limit
      ?dry_run
      ?force
      ~alias
      ~amount
      ~src
      ~prg
      client
  in
  (alias, process)

let originate_contract_at ?hooks ?log_output ?endpoint ?wait ?init ?burn_cap
    ?gas_limit ?dry_run ?force ?prefix ?alias ~amount ~src client name protocol
    =
  let alias =
    match alias with
    | Some alias -> alias
    | None -> (
        match List.rev name with
        | [] -> Test.fail "name must not be an empty list"
        | last :: _ -> last)
  in
  let prg =
    Michelson_script.find ?prefix name protocol |> Michelson_script.path
  in
  let* res =
    originate_contract
      ?hooks
      ?log_output
      ?endpoint
      ?wait
      ?init
      ?burn_cap
      ?gas_limit
      ?dry_run
      ?force
      ~alias
      ~amount
      ~src
      ~prg
      client
  in
  Lwt.return (alias, res)

let spawn_remember_contract ?(force = false) ~alias ~address client =
  spawn_command client
  @@ ["remember"; "contract"; alias; address]
  @ optional_switch "force" force

let remember_contract ?force ~alias ~address client =
  spawn_remember_contract ?force ~alias ~address client |> Process.check

let spawn_remember_script ~alias ~src client =
  spawn_command client ["remember"; "script"; alias; src]

let remember_script ~alias ~src client =
  spawn_remember_script ~alias ~src client |> Process.check

let spawn_stresstest ?endpoint ?(source_aliases = []) ?(source_pkhs = [])
    ?(source_accounts = []) ?seed ?fee ?gas_limit ?transfers ?tps
    ?fresh_probability ?smart_contract_parameters client =
  let sources =
    (* [sources] is a string containing all the [source_aliases],
       [source_pkhs], and [source_accounts] in JSON format, as
       expected by the [stresstest] client command. If all three lists
       [source_aliases], [source_pkhs], and [source_accounts] are
       empty (typically, when none of these optional arguments is
       provided to {!spawn_stresstest}), then [sources] instead
       contains the [Constant.bootstrap_keys] i.e. [bootstrap1], ...,
       [bootstrap5]. *)
    (* Note: We provide the sources JSON directly as a string, rather
       than writing it to a file, to avoid concurrency issues (we
       would need to ensure that each call writes to a different file:
       this would be doable, but providing a string containing the
       JSON is simpler). *)
    let open Account in
    let account_to_obj account =
      let sk = require_unencrypted_secret_key ~__LOC__ account.secret_key in
      `O
        [
          ("pkh", `String account.public_key_hash);
          ("pk", `String account.public_key);
          ("sk", `String sk);
        ]
    in
    let source_objs =
      List.map (fun alias -> `O [("alias", `String alias)]) source_aliases
      @ List.map (fun pkh -> `O [("pkh", `String pkh)]) source_pkhs
      @ List.map account_to_obj source_accounts
    in
    let source_objs =
      match source_objs with
      | [] -> Array.map account_to_obj Account.Bootstrap.keys |> Array.to_list
      | _ :: _ -> source_objs
    in
    `A source_objs
  in
  (* It is important to write the sources to a file because if we use a few
     thousands of sources the command line becomes too long. *)
  let sources_filename =
    Temp.file (Format.sprintf "sources-%s.json" client.name)
  in
  with_open_out sources_filename (fun ch ->
      output_string ch (JSON.encode_u sources)) ;
  let seed =
    (* Note: Tezt does not call [Random.self_init] so this is not
       randomized from one run to the other (if the exact same tests
       are run).

       The goal here is to use different seeds for instances of the
       [stresstest] command called in the same test, so that they
       don't all inject the same operations. *)
    (match seed with Some seed -> seed | None -> Random.int 0x3FFFFFFF)
    |> Int.to_string
  in
  let make_int_opt_arg (name : string) = function
    | Some (arg : int) -> [name; Int.to_string arg]
    | None -> []
  in
  let make_float_opt_arg (name : string) = function
    | Some (arg : float) -> [name; Float.to_string arg]
    | None -> []
  in
  let fee_arg =
    match fee with None -> [] | Some x -> ["--fee"; Tez.to_string x]
  in
  let smart_contract_parameters_arg =
    match smart_contract_parameters with
    | None -> []
    | Some items ->
        [
          "--smart-contract-parameters";
          Ezjsonm.value_to_string
            (`O
              (List.map
                 (fun ( alias,
                        {probability; invocation_fee; invocation_gas_limit} ) ->
                   ( alias,
                     `O
                       [
                         ("probability", Ezjsonm.float probability);
                         ( "invocation_fee",
                           Ezjsonm.string
                             (Int.to_string (Tez.to_mutez invocation_fee)) );
                         ( "invocation_gas_limit",
                           Ezjsonm.string (Int.to_string invocation_gas_limit)
                         );
                       ] ))
                 items));
        ]
  in
  spawn_command ?endpoint client
  @@ [
       "stresstest";
       "transfer";
       "using";
       "file:" ^ sources_filename;
       "--seed";
       seed;
     ]
  @ fee_arg
  @ make_int_opt_arg "--gas-limit" gas_limit
  @ make_int_opt_arg "--transfers" transfers
  @ make_int_opt_arg "--tps" tps
  @ make_float_opt_arg "--fresh-probability" fresh_probability
  @ smart_contract_parameters_arg

let stresstest ?endpoint ?source_aliases ?source_pkhs ?source_accounts ?seed
    ?fee ?gas_limit ?transfers ?tps ?fresh_probability
    ?smart_contract_parameters client =
  spawn_stresstest
    ?endpoint
    ?source_aliases
    ?source_pkhs
    ?source_accounts
    ?seed
    ?fee
    ?gas_limit
    ?transfers
    ?tps
    ?fresh_probability
    ?smart_contract_parameters
    client
  |> Process.check

let spawn_run_script ?hooks ?protocol_hash ?no_base_dir_warnings ?balance
    ?self_address ?source ?payer ?gas ?(trace_stack = false) ?level ?now
    ?other_contracts ?extra_big_maps ~prg ~storage ~input client =
  spawn_command ?hooks ?protocol_hash ?no_base_dir_warnings client
  @@ ["run"; "script"; prg; "on"; "storage"; storage; "and"; "input"; input]
  @ optional_arg "payer" Fun.id payer
  @ optional_arg "source" Fun.id source
  @ optional_arg "balance" Tez.to_string balance
  @ optional_arg "self-address" Fun.id self_address
  @ optional_arg "gas" (fun gas -> string_of_int gas) gas
  @ optional_arg "level" string_of_int level
  @ optional_switch "trace-stack" trace_stack
  @ optional_arg "now" Fun.id now
  @ optional_arg "other-contracts" Fun.id other_contracts
  @ optional_arg "extra-big-maps" Fun.id extra_big_maps

let spawn_run_script_at ?hooks ?protocol_hash ?balance ?self_address ?source
    ?payer ?prefix ?now ?trace_stack ?level ?other_contracts ?extra_big_maps
    ~storage ~input client script_name protocol =
  let prg =
    Michelson_script.find ?prefix script_name protocol |> Michelson_script.path
  in
  spawn_run_script
    ?hooks
    ?protocol_hash
    ?balance
    ?self_address
    ?source
    ?payer
    ?now
    ?trace_stack
    ?level
    ?other_contracts
    ?extra_big_maps
    ~prg
    ~storage
    ~input
    client

let spawn_run_code ?hooks ?protocol_hash ?no_base_dir_warnings ?amount ?balance
    ?source ?payer ?self_address ?gas ?mode ?level ?now ?other_contracts
    ?extra_big_maps ~src ~stack client =
  spawn_command ?hooks ?protocol_hash ?no_base_dir_warnings client
  @@ ["run"; "michelson"; "code"; src; "on"; "stack"; stack]
  @ optional_arg "amount" Tez.to_string amount
  @ optional_arg "balance" Tez.to_string balance
  @ optional_arg "source" Fun.id source
  @ optional_arg "payer" Fun.id payer
  @ optional_arg "self-address" Fun.id self_address
  @ optional_arg "gas" string_of_int gas
  @ optional_arg "unparsing-mode" normalize_mode_to_string mode
  @ optional_arg "now" Fun.id now
  @ optional_arg "level" string_of_int level
  @ optional_arg "other-contracts" Fun.id other_contracts
  @ optional_arg "extra-big-maps" Fun.id extra_big_maps

let stresstest_estimate_gas ?endpoint client =
  let* output =
    spawn_command ?endpoint client ["stresstest"; "estimate"; "gas"]
    |> Process.check_and_read_stdout
  in
  let json = JSON.parse ~origin:"transaction_costs" output in
  let regular = JSON.get "regular" json |> JSON.as_int in
  let prepare_pair (contract_name, json) = (contract_name, JSON.as_int json) in
  let smart_contracts =
    List.map prepare_pair (JSON.get "smart_contracts" json |> JSON.as_object)
  in
  Lwt.return {regular; smart_contracts}

let stresstest_originate_smart_contracts ?endpoint (source : Account.key) client
    =
  spawn_command
    ?endpoint
    client
    ["stresstest"; "originate"; "smart"; "contracts"; "from"; source.alias]
  |> Process.check

let stresstest_fund_accounts_from_source ?endpoint ~source_key_pkh ?batch_size
    ?batches_per_block ?initial_amount client =
  spawn_command
    ?endpoint
    client
    (["stresstest"; "fund"; "accounts"; "from"; source_key_pkh]
    @ optional_arg "batch-size" string_of_int batch_size
    @ optional_arg "batches-per-block" string_of_int batches_per_block
    @ optional_arg
        "initial-amount"
        (fun v -> string_of_int (Tez.to_mutez v))
        initial_amount)
  |> Process.check

type run_script_result = {storage : string; big_map_diff : string list}

let run_script ?hooks ?protocol_hash ?no_base_dir_warnings ?balance
    ?self_address ?source ?payer ?gas ?trace_stack ?level ?now ?other_contracts
    ?extra_big_maps ~prg ~storage ~input client =
  let* client_output =
    spawn_run_script
      ?hooks
      ?protocol_hash
      ?no_base_dir_warnings
      ?balance
      ?source
      ?payer
      ?self_address
      ?gas
      ?trace_stack
      ?level
      ?now
      ?other_contracts
      ?extra_big_maps
      ~prg
      ~storage
      ~input
      client
    |> Process.check_and_read_stdout
  in
  (* Extract the final storage and the big_map diff
     from [client_output].

     The [client_ouput] has the following format:
     storage
       <final storage>
     emitted operations
       <operations>
     big_map diff
       <big_map diff>
     trace
       <trace>

     But the trace is only present if `--trace-stack`
     was given. *)
  let client_output_lines = String.split_on_char '\n' client_output in
  let _header, tail =
    span (fun line -> not (String.equal "storage" line)) client_output_lines
  in
  let storage, tail =
    span (fun line -> not (String.equal "emitted operations" line)) tail
  in
  let storage =
    match storage with
    | "storage" :: storage -> String.trim (String.concat "\n" storage)
    | _ ->
        Test.fail
          "Cannot extract new storage from client_output: %s"
          client_output
  in
  let _emitted_operations, tail =
    span (fun line -> not (String.equal "big_map diff" line)) tail
  in
  let big_map_diff =
    let diff =
      match trace_stack with
      | Some true ->
          let big_map_diff, _tail =
            span (fun line -> not (String.equal "trace" line)) tail
          in
          big_map_diff
      | _ -> tail
    in
    match diff with
    | ["big_map diff"; s] when String.(equal (trim s) "") -> []
    | "big_map diff" :: diff -> List.map String.trim diff
    | _ ->
        Test.fail
          "Cannot extract big_map diff from client_output: %s"
          client_output
  in
  return {storage; big_map_diff}

let run_script_at ?hooks ?protocol_hash ?balance ?self_address ?source ?payer
    ?prefix ?now ?trace_stack ?level ?other_contracts ?extra_big_maps ~storage
    ~input client name protocol =
  let prg =
    Michelson_script.find name ?prefix protocol |> Michelson_script.path
  in
  run_script
    ?hooks
    ?protocol_hash
    ?balance
    ?self_address
    ?source
    ?payer
    ?now
    ?trace_stack
    ?level
    ?other_contracts
    ?extra_big_maps
    ~storage
    ~input
    ~prg
    client

let run_code ?hooks ?protocol_hash ?no_base_dir_warnings ?amount ?balance
    ?source ?payer ?self_address ?gas ?mode ?level ?now ?other_contracts
    ?extra_big_maps ~src ~stack client =
  let* client_output =
    spawn_run_code
      ?hooks
      ?protocol_hash
      ?no_base_dir_warnings
      ?amount
      ?balance
      ?source
      ?payer
      ?self_address
      ?gas
      ?mode
      ?level
      ?now
      ?other_contracts
      ?extra_big_maps
      ~src
      ~stack
      client
    |> Process.check_and_read_stdout
  in
  (* Extract the final stack from [client_output].

     The [client_ouput] has the following format:
     Result
       <stack>
     Gas_remaining: <gas> units remaining *)
  let client_output_lines = String.split_on_char '\n' client_output in
  let stack, _tail =
    span
      (fun line -> not (String.starts_with ~prefix:"Gas remaining" line))
      client_output_lines
  in
  match stack with
  | "Result" :: stack -> return (String.trim (String.concat "\n" stack))
  | _ ->
      Test.fail
        "Cannot extract resulting stack from client_output: %s,"
        client_output

let spawn_register_global_constant ?(wait = "none") ?burn_cap ~value ~src client
    =
  spawn_command
    client
    (["--wait"; wait]
    @ ["register"; "global"; "constant"; value; "from"; src]
    @ optional_arg "burn-cap" Tez.to_string burn_cap)

let register_global_constant ?wait ?burn_cap ~src ~value client =
  let* client_output =
    spawn_register_global_constant ?wait ?burn_cap ~src ~value client
    |> Process.check_and_read_stdout
  in
  match client_output =~* rex "Global address: (expr\\w{50})" with
  | None ->
      Test.fail
        "Cannot extract constant hash from client_output: %s"
        client_output
  | Some hash -> return hash

type hash_script_format = TSV | CSV

let show_hash_script_format = function TSV -> "tsv" | CSV -> "csv"

let spawn_hash_script ?hooks ?for_script ~script client =
  spawn_command ?hooks client
  @@ ["hash"; "script"; script]
  @ optional_arg "for-script" show_hash_script_format for_script

let hash_script ?hooks ?for_script ~script client =
  let* client_output =
    spawn_hash_script ?hooks ?for_script ~script client
    |> Process.check_and_read_stdout
  in
  return (String.trim client_output)

let spawn_get_contract_hash ?hooks ~contract client =
  spawn_command
    ?hooks
    client
    ["get"; "contract"; "script"; "hash"; "for"; contract]

let get_contract_hash ?hooks ~contract client =
  let* client_output =
    spawn_get_contract_hash ?hooks ~contract client
    |> Process.check_and_read_stdout
  in
  return (String.trim client_output)

let spawn_hash_scripts ?hooks ?(display_names = false) ?for_script scripts
    client =
  spawn_command ?hooks client
  @@ ["hash"; "script"] @ scripts
  @ optional_switch "display-names" display_names
  @ optional_arg "for-script" show_hash_script_format for_script

let hash_scripts ?hooks ?display_names ?for_script scripts client =
  let* output =
    spawn_hash_scripts ?hooks ?display_names ?for_script scripts client
    |> Process.check_and_read_stdout
  in
  return
    (match String.trim output with
    | "" -> []
    | output -> String.split_on_char '\n' output)

type hash_data_result = {
  packed : string;
  script_expr_hash : string;
  raw_script_expr_hash : string;
  ledger_blake2b_hash : string;
  raw_sha256_hash : string;
  raw_sha512_hash : string;
}

let spawn_hash_data ?hooks ~data ~typ client =
  let cmd = ["hash"; "data"; data; "of"; "type"; typ] in
  spawn_command ?hooks client cmd

let hash_data ?hooks ~data ~typ client =
  let* output =
    spawn_hash_data ?hooks ~data ~typ client |> Process.check_and_read_stdout
  in
  let parse_line line =
    match line =~** rex "(.*): (.*)" with
    | None ->
        Log.warn
          "Unparsable output line of `hash data %s of type %s`: %s"
          data
          typ
          line ;
        None
    | Some _ as x -> x
  in
  (* Filtering avoids the last line (after the trailing \n).
     We don't want to produce a warning about an empty line. *)
  let lines = String.split_on_char '\n' output |> List.filter (( <> ) "") in
  let key_value_list = List.map parse_line lines |> List.filter_map Fun.id in
  let get hash_typ =
    match List.assoc_opt hash_typ key_value_list with
    | None ->
        Test.fail
          "Unparsable output line of `hash data %s of type %s`. Could not find \
           hash type %s in output: %s"
          data
          typ
          hash_typ
          output
    | Some hash -> hash
  in
  Lwt.return
    {
      packed = get "Raw packed data";
      script_expr_hash = get "Script-expression-ID-Hash";
      raw_script_expr_hash = get "Raw Script-expression-ID-Hash";
      ledger_blake2b_hash = get "Ledger Blake2b hash";
      raw_sha256_hash = get "Raw Sha256 hash";
      raw_sha512_hash = get "Raw Sha512 hash";
    }

let spawn_normalize_data ?hooks ?mode ?(legacy = false) ~data ~typ client =
  let mode_cmd =
    Option.map normalize_mode_to_string mode
    |> Option.map (fun s -> ["--unparsing-mode"; s])
  in
  let cmd =
    ["normalize"; "data"; data; "of"; "type"; typ]
    @ Option.value ~default:[] mode_cmd
    @ if legacy then ["--legacy"] else []
  in
  spawn_command ?hooks client cmd

let normalize_data ?hooks ?mode ?legacy ~data ~typ client =
  spawn_normalize_data ?hooks ?mode ?legacy ~data ~typ client
  |> Process.check_and_read_stdout

let spawn_normalize_stack ?hooks ?mode ?(legacy = false) ~stack client =
  let mode_cmd =
    Option.map normalize_mode_to_string mode
    |> Option.map (fun s -> ["--unparsing-mode"; s])
  in
  let cmd =
    ["normalize"; "stack"; stack]
    @ Option.value ~default:[] mode_cmd
    @ if legacy then ["--legacy"] else []
  in
  spawn_command ?hooks client cmd

let normalize_stack ?hooks ?mode ?legacy ~stack client =
  spawn_normalize_stack ?hooks ?mode ?legacy ~stack client
  |> Process.check_and_read_stdout

let spawn_normalize_script ?hooks ?mode ~script client =
  let mode_cmd =
    Option.map normalize_mode_to_string mode
    |> Option.map (fun s -> ["--unparsing-mode"; s])
  in
  let cmd =
    ["normalize"; "script"; script] @ Option.value ~default:[] mode_cmd
  in
  spawn_command ?hooks client cmd

let normalize_script ?hooks ?mode ~script client =
  spawn_normalize_script ?hooks ?mode ~script client
  |> Process.check_and_read_stdout

let spawn_typecheck_data ~data ~typ ?gas ?(legacy = false) client =
  let gas_cmd =
    Option.map Int.to_string gas |> Option.map (fun g -> ["--gas"; g])
  in
  let cmd =
    ["typecheck"; "data"; data; "against"; "type"; typ]
    @ Option.value ~default:[] gas_cmd
    @ if legacy then ["--legacy"] else []
  in
  spawn_command client cmd

let spawn_normalize_type ?hooks ~typ client =
  spawn_command ?hooks client ["normalize"; "type"; typ]

let normalize_type ?hooks ~typ client =
  spawn_normalize_type ?hooks ~typ client |> Process.check_and_read_stdout

let typecheck_data ~data ~typ ?gas ?(legacy = false) client =
  spawn_typecheck_data ~data ~typ ?gas ~legacy client |> Process.check

let spawn_typecheck_script ?hooks ?protocol_hash ~scripts ?no_base_dir_warnings
    ?(details = false) ?(emacs = false) ?(no_print_source = false) ?gas
    ?(legacy = false) ?(display_names = false) client =
  let gas_cmd =
    Option.map Int.to_string gas |> Option.map (fun g -> ["--gas"; g])
  in
  spawn_command ?hooks ?protocol_hash ?no_base_dir_warnings client
  @@ ["typecheck"; "script"] @ scripts
  @ Option.value ~default:[] gas_cmd
  @ (if details then ["--details"] else [])
  @ (if emacs then ["--emacs"] else [])
  @ (if no_print_source then ["--no-print-source"] else [])
  @ (if legacy then ["--legacy"] else [])
  @ if display_names then ["--display-names"] else []

let typecheck_script ?hooks ?protocol_hash ~scripts ?no_base_dir_warnings
    ?(details = false) ?(emacs = false) ?(no_print_source = false) ?gas
    ?(legacy = false) ?display_names client =
  spawn_typecheck_script
    ?hooks
    ?protocol_hash
    ~scripts
    ?no_base_dir_warnings
    ~details
    ~emacs
    ~no_print_source
    ?gas
    ~legacy
    ?display_names
    client
  |> Process.check

let spawn_run_tzt_unit_tests ?hooks ?protocol_hash ~tests ?no_base_dir_warnings
    client =
  spawn_command ?hooks ?protocol_hash ?no_base_dir_warnings client
  @@ ["run"; "unit"; "tests"; "from"]
  @ tests

let run_tzt_unit_tests ?hooks ?protocol_hash ~tests ?no_base_dir_warnings client
    =
  spawn_run_tzt_unit_tests
    ?hooks
    ?protocol_hash
    ~tests
    ?no_base_dir_warnings
    client
  |> Process.check

let spawn_run_tzip4_view ?hooks ?source ?payer ?gas ?unparsing_mode
    ?other_contracts ?extra_big_maps ~entrypoint ~contract ?input
    ?(unlimited_gas = false) client =
  let input_params =
    match input with None -> [] | Some input -> ["with"; "input"; input]
  in
  spawn_command
    ?hooks
    client
    (["run"; "tzip4"; "view"; entrypoint; "on"; "contract"; contract]
    @ input_params
    @ optional_arg "payer" Fun.id payer
    @ optional_arg "source" Fun.id source
    @ optional_arg "unparsing-mode" normalize_mode_to_string unparsing_mode
    @ optional_arg "gas" Int.to_string gas
    @ optional_switch "unlimited-gas" unlimited_gas
    @ optional_arg "other-contracts" Fun.id other_contracts
    @ optional_arg "extra-big-maps" Fun.id extra_big_maps)

let run_tzip4_view ?hooks ?source ?payer ?gas ?unparsing_mode ?other_contracts
    ?extra_big_maps ~entrypoint ~contract ?input ?unlimited_gas client =
  spawn_run_tzip4_view
    ?hooks
    ?source
    ?payer
    ?gas
    ?unparsing_mode
    ?other_contracts
    ?extra_big_maps
    ~entrypoint
    ~contract
    ?input
    ?unlimited_gas
    client
  |> Process.check_and_read_stdout

let spawn_run_view ?hooks ?source ?payer ?gas ?unparsing_mode ?other_contracts
    ?extra_big_maps ~view ~contract ?input ?(unlimited_gas = false) client =
  let input_params =
    match input with None -> [] | Some input -> ["with"; "input"; input]
  in
  spawn_command
    ?hooks
    client
    (["run"; "view"; view; "on"; "contract"; contract]
    @ input_params
    @ optional_arg "payer" Fun.id payer
    @ optional_arg "source" Fun.id source
    @ optional_arg "unparsing-mode" normalize_mode_to_string unparsing_mode
    @ optional_arg "gas" Int.to_string gas
    @ optional_arg "other-contracts" Fun.id other_contracts
    @ optional_arg "extra-big-maps" Fun.id extra_big_maps
    @ if unlimited_gas then ["--unlimited-gas"] else [])

let run_view ?hooks ?source ?payer ?gas ?unparsing_mode ?other_contracts
    ?extra_big_maps ~view ~contract ?input ?unlimited_gas client =
  spawn_run_view
    ?hooks
    ?source
    ?payer
    ?gas
    ?unparsing_mode
    ?other_contracts
    ?extra_big_maps
    ~view
    ~contract
    ?input
    ?unlimited_gas
    client
  |> Process.check_and_read_stdout

let spawn_list_protocols mode client =
  let mode_str =
    match mode with
    | `Mockup -> "mockup"
    | `Light -> "light"
    | `Proxy -> "proxy"
  in
  spawn_command client (mode_arg client @ ["list"; mode_str; "protocols"])

let list_protocols mode client =
  let* output =
    spawn_list_protocols mode client |> Process.check_and_read_stdout
  in
  return (parse_list_protocols_output output)

let spawn_list_understood_protocols ?config_file client =
  spawn_command
    ?config_file
    client
    (mode_arg client @ ["list"; "understood"; "protocols"])

let list_understood_protocols ?config_file client =
  let* output =
    spawn_list_understood_protocols ?config_file client
    |> Process.check_and_read_stdout
  in
  return (parse_list_protocols_output output)

let spawn_migrate_mockup ~next_protocol client =
  spawn_command
    client
    (mode_arg client @ ["migrate"; "mockup"; "to"; Protocol.hash next_protocol])

let migrate_mockup ~next_protocol client =
  spawn_migrate_mockup ~next_protocol client |> Process.check

let spawn_sign_block client block_hex ~delegate =
  spawn_command client ["sign"; "block"; block_hex; "for"; delegate]

let sign_block client block_hex ~delegate =
  spawn_sign_block client block_hex ~delegate |> Process.check_and_read_stdout

let spawn_sign_message ?branch client message ~src =
  spawn_command
    client
    (["sign"; "message"; message; "for"; src]
    @ optional_arg "branch" Fun.id branch)

let sign_message ?branch client message ~src =
  let* output =
    spawn_sign_message ?branch client message ~src
    |> Process.check_and_read_stdout
  in
  match output =~* rex "Signature: ([a-zA-Z0-9]+)" with
  | Some signature -> Lwt.return signature
  | None -> Test.fail "Couldn't sign message '%s' for %s." message src

let spawn_check_message ?branch client ~src ~signature message =
  spawn_command
    client
    ([
       "check";
       "that";
       "message";
       message;
       "was";
       "signed";
       "by";
       src;
       "to";
       "produce";
       signature;
     ]
    @ optional_arg "branch" Fun.id branch)

let check_message ?branch client ~src ~signature message =
  spawn_check_message ?branch client ~src ~signature message |> Process.check

let transfer_tickets ?(wait = "none") ?burn_cap ?hooks ?expect_failure ~qty ~src
    ~destination ~entrypoint ~contents ~ty ~ticketer client =
  let process =
    spawn_command
      ?hooks
      client
      (["--wait"; wait]
      @ [
          "transfer";
          Int64.to_string qty;
          "tickets";
          "from";
          src;
          "to";
          destination;
          "with";
          "entrypoint";
          entrypoint;
          "and";
          "contents";
          contents;
          "and";
          "type";
          ty;
          "and";
          "ticketer";
          ticketer;
        ]
      @ optional_arg "burn-cap" Tez.to_string burn_cap)
  in
  let parse process = Process.check ?expect_failure process in
  {value = process; run = parse}

let spawn_show_voting_period ?endpoint client =
  spawn_command ?endpoint client (mode_arg client @ ["show"; "voting"; "period"])

let show_voting_period ?endpoint client =
  let* output =
    spawn_show_voting_period ?endpoint client |> Process.check_and_read_stdout
  in
  match output =~* rex "Current period: \"([a-z]+)\"" with
  | None ->
      Test.fail
        "octez-client show voting period did not print the current period"
  | Some period -> return period

module Sc_rollup = struct
  let spawn_originate ?hooks ?(wait = "none") ?(force = false) ?burn_cap
      ?whitelist ~alias ~src ~kind ~parameters_ty ~boot_sector client =
    spawn_command
      ?hooks
      client
      (["--wait"; wait]
      @ [
          "originate";
          "smart";
          "rollup";
          alias;
          "from";
          src;
          "of";
          "kind";
          kind;
          "of";
          "type";
          parameters_ty;
          "with";
          "kernel";
          boot_sector;
        ]
      @ optional_arg
          "whitelist"
          (fun v -> JSON.encode_u (`A (List.map (fun s -> `String s) v)))
          whitelist
      @ optional_arg "burn-cap" Tez.to_string burn_cap
      @ optional_switch "force" force)

  let parse_rollup_address_in_receipt output =
    match output =~* rex "Address: (.*)" with
    | None -> Test.fail "Cannot extract rollup address from receipt."
    | Some x -> return x

  let originate ?hooks ?wait ?force ?burn_cap ?whitelist ~alias ~src ~kind
      ~parameters_ty ~boot_sector client =
    let process =
      spawn_originate
        ?hooks
        ?wait
        ?force
        ?burn_cap
        ?whitelist
        ~alias
        ~src
        ~kind
        ~boot_sector
        ~parameters_ty
        client
    in
    let* output = Process.check_and_read_stdout process in
    parse_rollup_address_in_receipt output

  let remember_smart_rollup ?hooks ?(force = false) client ~alias ~address =
    let process =
      spawn_command
        ?hooks
        client
        (["remember"; "smart"; "rollup"; alias; address]
        @ optional_switch "force" force)
    in
    let parse process = Process.check process in
    {value = process; run = parse}

  let list_known_smart_rollups ?hooks client =
    let process =
      spawn_command ?hooks client ["list"; "known"; "smart"; "rollups"]
    in
    let parse process =
      let* output = Process.check_and_read_stdout process in
      let output = String.trim output in
      if output = "" then return []
      else
        let alias_and_address =
          String.split_on_char '\n' output
          |> List.map @@ fun line ->
             match line =~** rex "(.*): ([^ ]*)" with
             | Some (alias, pkh) -> (alias, pkh)
             | None ->
                 Test.fail
                   "Cannot parse line %S from list of known smart rollups from \
                    client_output: %s"
                   line
                   output
        in
        return alias_and_address
    in
    {value = process; run = parse}

  let forget_all_smart_rollups ?hooks ?(force = false) client =
    let process =
      spawn_command
        ?hooks
        client
        (["forget"; "all"; "smart"; "rollups"] @ optional_switch "force" force)
    in
    let parse process = Process.check process in
    {value = process; run = parse}

  let show_known_smart_rollup ?hooks client ~alias =
    let process =
      spawn_command ?hooks client ["show"; "known"; "smart"; "rollup"; alias]
    in
    let parse process = Process.check_and_read_stdout process in
    {value = process; run = parse}

  let spawn_send_message ?hooks ?(wait = "none") ?burn_cap ?fee ?fee_cap ~msg
      ~src client =
    spawn_command
      ?hooks
      client
      (["--wait"; wait]
      @ ["send"; "smart"; "rollup"; "message"; msg; "from"; src]
      @ optional_arg "fee" Tez.to_string fee
      @ optional_arg "fee-cap" Tez.to_string fee_cap
      @ optional_arg "burn-cap" Tez.to_string burn_cap)

  let send_message ?hooks ?wait ?burn_cap ?fee ?fee_cap ~msg ~src client =
    let process =
      spawn_send_message ?hooks ?wait ?burn_cap ?fee ?fee_cap ~msg ~src client
    in
    Process.check process

  let publish_commitment ?hooks ?(wait = "none") ?burn_cap ~src ~sc_rollup
      ~compressed_state ~inbox_level ~predecessor ~number_of_ticks client =
    let process =
      spawn_command
        ?hooks
        client
        (["--wait"; wait]
        @ [
            "publish";
            "commitment";
            "from";
            src;
            "for";
            "smart";
            "rollup";
            sc_rollup;
            "with";
            "compressed";
            "state";
            compressed_state;
            "at";
            "inbox";
            "level";
            string_of_int inbox_level;
            "and";
            "predecessor";
            predecessor;
            "and";
            "number";
            "of";
            "ticks";
            string_of_int number_of_ticks;
          ]
        @ optional_arg "burn-cap" Tez.to_string burn_cap)
    in
    let parse process = Process.check process in
    {value = process; run = parse}

  let cement_commitment protocol ?hooks ?(wait = "none") ?burn_cap ~hash ~src
      ~dst client =
    let commitment_arg =
      if Protocol.(number protocol >= 018) then
        (* Version after protocol 017 do not specify the commitment. *) []
      else [hash]
    in
    let process =
      spawn_command
        ?hooks
        client
        (["--wait"; wait] @ ["cement"; "commitment"] @ commitment_arg
        @ ["from"; src; "for"; "smart"; "rollup"; dst]
        @ optional_arg "burn-cap" Tez.to_string burn_cap)
    in
    let parse process = Process.check process in
    {value = process; run = parse}

  let timeout ?expect_failure ?hooks ?(wait = "none") ?burn_cap ~staker1
      ~staker2 ~src ~dst client =
    let process =
      spawn_command
        ?hooks
        client
        (["--wait"; wait]
        @ [
            "timeout";
            "dispute";
            "on";
            "smart";
            "rollup";
            dst;
            "with";
            staker1;
            "against";
            staker2;
            "from";
            src;
          ]
        @ optional_arg "burn-cap" Tez.to_string burn_cap)
    in
    let parse process = Process.check ?expect_failure process in
    {value = process; run = parse}

  let submit_recover_bond ?(wait = "none") ?burn_cap ?storage_limit ?fee ?hooks
      ~rollup ~src ~staker client =
    let process =
      spawn_command
        ?hooks
        client
        (["--wait"; wait]
        @ [
            "recover";
            "bond";
            "of";
            staker;
            "for";
            "smart";
            "rollup";
            rollup;
            "from";
            src;
          ]
        @ Option.fold
            ~none:[]
            ~some:(fun burn_cap -> ["--burn-cap"; Tez.to_string burn_cap])
            burn_cap
        @ Option.fold
            ~none:[]
            ~some:(fun fee -> ["--fee"; Tez.to_string fee])
            fee
        @ Option.fold
            ~none:[]
            ~some:(fun s -> ["--storage-limit"; string_of_int s])
            storage_limit)
    in
    let parse process = Process.check process in
    {value = process; run = parse}

  (** Run [octez-client execute outbox message of sc rollup <rollup> from <src>
      for commitment hash <hash> and output proof <proof>]. *)
  let execute_outbox_message ?(wait = "none") ?burn_cap ?storage_limit ?fee
      ?hooks ~rollup ~src ~commitment_hash ~proof client =
    let process =
      spawn_command
        ?hooks
        client
        (["--wait"; wait]
        @ ["execute"; "outbox"; "message"; "of"; "smart"; "rollup"; rollup]
        @ ["from"; src]
        @ ["for"; "commitment"; "hash"; commitment_hash]
        @ ["and"; "output"; "proof"; proof]
        @ Option.fold
            ~none:[]
            ~some:(fun burn_cap -> ["--burn-cap"; Tez.to_string burn_cap])
            burn_cap
        @ Option.fold
            ~none:[]
            ~some:(fun fee -> ["--fee"; Tez.to_string fee])
            fee
        @ Option.fold
            ~none:[]
            ~some:(fun s -> ["--storage-limit"; string_of_int s])
            storage_limit)
    in
    let parse process = Process.check process in
    {value = process; run = parse}
end

module Zk_rollup = struct
  let extract_zku_address client_output =
    client_output =~* rex ".*?(epx1\\w{33}.*)"

  let originate ?(expect_failure = false) client ~src ~alias
      ~public_parameters_file ~init_state_file ~circuits_info_file ~nb_ops
      ~gas_cap ~burn_cap ~storage_limit =
    let* client_output =
      spawn_command
        client
        (["--wait"; "none"]
        @ [
            "originate";
            "epoxy";
            alias;
            "from";
            src;
            "public_parameters";
            "file:" ^ public_parameters_file;
            "init_state";
            "file:" ^ init_state_file;
            "circuits_info";
            "file:" ^ circuits_info_file;
            "nb_ops";
            string_of_int nb_ops;
            "-G";
            string_of_int gas_cap;
            "--burn-cap";
            string_of_int burn_cap;
            "-S";
            string_of_int storage_limit;
          ])
      |> Process.check_and_read_stdout ~expect_failure
    in
    let address = extract_zku_address client_output in
    Lwt.return address

  let publish ?(expect_failure = false) client ~src ~zk_rollup ~ops_file
      ~gas_cap ~burn_cap =
    let* () =
      spawn_command
        client
        (["--wait"; "none"]
        @ [
            "epoxy";
            "publish";
            "from";
            src;
            "rollup";
            zk_rollup;
            "ops";
            "file:" ^ ops_file;
            "-G";
            string_of_int gas_cap;
            "--burn-cap";
            string_of_int burn_cap;
          ])
      |> Process.check ~expect_failure
    in
    Lwt.return_unit

  let update ?(expect_failure = false) client ~src ~zk_rollup ~update_file
      ~gas_cap ~burn_cap =
    let* () =
      spawn_command
        client
        (["--wait"; "none"]
        @ [
            "epoxy";
            "update";
            "from";
            src;
            "rollup";
            zk_rollup;
            "update";
            "file:" ^ update_file;
            "-G";
            string_of_int gas_cap;
            "--burn-cap";
            string_of_int burn_cap;
          ])
      |> Process.check ~expect_failure
    in
    Lwt.return_unit
end

let init ?path ?admin_path ?name ?color ?base_dir ?endpoint ?media_type () =
  let client =
    create ?path ?admin_path ?name ?color ?base_dir ?endpoint ?media_type ()
  in
  Account.write Constant.all_secret_keys ~base_dir:client.base_dir ;
  return client

let init_mockup ?path ?admin_path ?name ?color ?base_dir ?sync_mode
    ?parameter_file ?constants ~protocol () =
  (* The mockup's public documentation doesn't use `--mode mockup`
     for `create mockup` (as it is not required). We wanna do the same here.
     Hence `Client None` here: *)
  let client =
    create_with_mode
      ?path
      ?admin_path
      ?name
      ?color
      ?base_dir
      (Client (None, None))
  in
  let parameter_file =
    Option.value
      ~default:(Protocol.parameter_file ?constants protocol)
      parameter_file
  in
  let* () = create_mockup ?sync_mode ~parameter_file ~protocol client in
  (* We want, however, to return a mockup client; hence the following: *)
  set_mode Mockup client ;
  return client

let write_sources_file ~min_agreement ~uris client =
  (* Create a services.json file in the base directory with correctly
     JSONified data *)
  Lwt_io.with_file ~mode:Lwt_io.Output (sources_file client) (fun oc ->
      let obj =
        `O
          [
            ("min_agreement", `Float min_agreement);
            ("uris", `A (List.map (fun s -> `String s) uris));
          ]
      in
      Lwt_io.fprintf oc "%s" @@ Ezjsonm.value_to_string obj)

let init_light ?path ?admin_path ?name ?color ?base_dir ?(min_agreement = 0.66)
    ?event_level ?event_sections_levels ?(nodes_args = []) () =
  let filter_node_arg = function
    | Node.Connections _ | Synchronisation_threshold _ -> None
    | x -> Some x
  in
  let nodes_args =
    List.filter_map filter_node_arg nodes_args
    @ Node.[Connections 1; Synchronisation_threshold 0]
  in
  let* node1 =
    Node.init ?event_level ?event_sections_levels ~name:"node1" nodes_args
  and* node2 =
    Node.init ?event_level ?event_sections_levels ~name:"node2" nodes_args
  in
  let nodes = [node1; node2] in
  let client =
    create_with_mode
      ?path
      ?admin_path
      ?name
      ?color
      ?base_dir
      (Light (min_agreement, List.map (fun n -> Node n) nodes))
  in
  let* () =
    write_sources_file
      ~min_agreement
      ~uris:
        (List.map
           (fun node ->
             sf "http://%s:%d" (Node.rpc_host node) (Node.rpc_port node))
           nodes)
      client
  in
  let json = JSON.parse_file (sources_file client) in
  Log.info "%s" @@ JSON.encode json ;
  Log.info "Importing keys" ;
  Account.write Constant.all_secret_keys ~base_dir:client.base_dir ;
  Log.info "Syncing peers" ;
  let* () =
    assert (nodes <> []) ;
    (* endpoint_arg is the first element of the list by default so we sync it
       with all other nodes. *)
    Lwt_list.iter_s
      (fun peer -> Admin.connect_address ~peer client)
      (List.tl nodes)
  in
  return (client, node1, node2)

let stresstest_gen_keys ?endpoint ?alias_prefix n client =
  let* output =
    spawn_command
      ?endpoint
      client
      (["stresstest"; "gen"; "keys"; Int.to_string n]
      @ optional_arg "alias-prefix" Fun.id alias_prefix)
    |> Process.check_and_read_stdout
  in
  let json = JSON.parse ~origin:"stresstest_gen_keys" output in
  let read_one i json : Account.key =
    let bootstrap_accounts = Account.Bootstrap.keys |> Array.length in
    let alias = Account.Bootstrap.alias (i + bootstrap_accounts + 1) in
    let public_key_hash = JSON.(json |-> "pkh" |> as_string) in
    let public_key = JSON.(json |-> "pk" |> as_string) in
    let secret_key = Account.Unencrypted JSON.(json |-> "sk" |> as_string) in
    {alias; public_key_hash; public_key; secret_key}
  in
  let additional_bootstraps = List.mapi read_one (JSON.as_list json) in
  client.additional_bootstraps <- additional_bootstraps ;
  Lwt.return additional_bootstraps

let get_parameter_file ?additional_bootstrap_accounts ?default_accounts_balance
    ?additional_revealed_bootstrap_accounts ?parameter_file protocol =
  if
    Option.is_none additional_bootstrap_accounts
    && Option.is_none additional_revealed_bootstrap_accounts
  then return parameter_file
  else
    let additional_accounts acc ~revealed = function
      | None -> acc
      | Some accounts ->
          List.fold_left
            (fun acc x -> (x, default_accounts_balance, revealed) :: acc)
            acc
            accounts
    in
    let additional_unreveal_bootstrap_accounts =
      additional_accounts [] ~revealed:false additional_bootstrap_accounts
    in
    let additional_bootstrap_accounts =
      additional_accounts
        additional_unreveal_bootstrap_accounts
        ~revealed:true
        additional_revealed_bootstrap_accounts
    in
    let* parameter_file =
      Protocol.write_parameter_file
        ~additional_bootstrap_accounts
        ~base:
          (Option.fold
             ~none:(Either.right protocol)
             ~some:Either.left
             parameter_file)
        []
    in
    return (Some parameter_file)

let init_with_node ?path ?admin_path ?name ?color ?base_dir ?event_level
    ?event_sections_levels
    ?(nodes_args = Node.[Connections 0; Synchronisation_threshold 0])
    ?(keys = Constant.all_secret_keys) ?rpc_local tag () =
  match tag with
  | (`Client | `Proxy) as mode ->
      let* node =
        Node.init ?event_level ?event_sections_levels ?rpc_local nodes_args
      in
      let endpoint = Node node in
      let mode =
        match mode with
        | `Client -> Client (Some endpoint, None)
        | `Proxy -> Proxy endpoint
      in
      let client =
        create_with_mode ?path ?admin_path ?name ?color ?base_dir mode
      in
      Account.write keys ~base_dir:client.base_dir ;
      return (node, client)
  | `Light ->
      let* client, node1, _ =
        init_light ?path ?admin_path ?name ?color ?base_dir ~nodes_args ()
      in
      return (node1, client)

let init_with_protocol ?path ?admin_path ?name ?color ?base_dir ?event_level
    ?event_sections_levels ?nodes_args ?additional_bootstrap_account_count
    ?additional_revealed_bootstrap_account_count ?default_accounts_balance
    ?parameter_file ?timestamp ?keys ?rpc_local tag ~protocol () =
  let* node, client =
    init_with_node
      ?path
      ?admin_path
      ?name
      ?color
      ?base_dir
      ?event_level
      ?event_sections_levels
      ?nodes_args
      ?keys
      ?rpc_local
      tag
      ()
  in
  let* additional_bootstrap_accounts =
    match additional_bootstrap_account_count with
    | None -> return None
    | Some n ->
        let* r = stresstest_gen_keys n client in
        return (Some r)
  in
  let* additional_revealed_bootstrap_accounts =
    match additional_revealed_bootstrap_account_count with
    | None -> return None
    | Some n ->
        let* r = stresstest_gen_keys n client in
        return (Some r)
  in
  let* parameter_file =
    get_parameter_file
      ?additional_bootstrap_accounts
      ?default_accounts_balance
      ?additional_revealed_bootstrap_accounts
      ?parameter_file
      (protocol, None)
  in
  let* () = activate_protocol ?parameter_file ~protocol ?timestamp client in
  let* _ = Node.wait_for_level node 1 in
  return (node, client)

let spawn_register_key ?hooks ?consensus owner client =
  spawn_command
    ?hooks
    client
    (["--wait"; "none"; "register"; "key"; owner; "as"; "delegate"]
    @
    match consensus with
    | None -> []
    | Some pk -> ["with"; "consensus"; "key"; pk])

let register_key ?hooks ?expect_failure ?consensus owner client =
  spawn_register_key ?hooks ?consensus owner client
  |> Process.check ?expect_failure

let contract_storage ?hooks ?unparsing_mode address client =
  spawn_command
    client
    ?hooks
    (["get"; "contract"; "storage"; "for"; address]
    @ optional_arg "unparsing-mode" normalize_mode_to_string unparsing_mode)
  |> Process.check_and_read_stdout

let contract_code ?unparsing_mode address client =
  spawn_command
    client
    (["get"; "contract"; "code"; "for"; address]
    @ optional_arg "unparsing-mode" normalize_mode_to_string unparsing_mode)
  |> Process.check_and_read_stdout

let spawn_contract_entrypoint_type ~entrypoint ~contract client =
  spawn_command
    client
    ["get"; "contract"; "entrypoint"; "type"; "of"; entrypoint; "for"; contract]

let contract_entrypoint_type ~entrypoint ~contract client =
  spawn_contract_entrypoint_type ~entrypoint ~contract client
  |> Process.check_and_read_stdout

let sign_bytes ~signer ~data client =
  let* output =
    spawn_command client ["sign"; "bytes"; data; "for"; signer]
    |> Process.check_and_read_stdout
  in
  match output =~* rex "Signature: ([a-zA-Z0-9]+)" with
  | Some signature -> Lwt.return signature
  | None -> Test.fail "Couldn't sign message '%s' for %s." data signer

let bootstrapped client = spawn_command client ["bootstrapped"] |> Process.check

let spawn_config_show ?config_file ?protocol client =
  spawn_command ?config_file client
  @@ Cli_arg.optional_arg "protocol" Protocol.hash protocol
  @ ["config"; "show"]

let config_show ?config_file ?protocol client =
  spawn_config_show ?config_file ?protocol client
  |> Process.check_and_read_stdout

let spawn_config_init ?config_file ?protocol ?bootstrap_accounts
    ?protocol_constants ?output client =
  spawn_command ?config_file client
  @@ Cli_arg.optional_arg "protocol" Protocol.hash protocol
  @ ["config"; "init"]
  @ Cli_arg.optional_arg "bootstrap-accounts" Fun.id bootstrap_accounts
  @ Cli_arg.optional_arg "protocol-constants" Fun.id protocol_constants
  @ Cli_arg.optional_arg "output" Fun.id output

let config_init ?config_file ?protocol ?bootstrap_accounts ?protocol_constants
    ?output client =
  spawn_config_init
    ?config_file
    ?protocol
    ?bootstrap_accounts
    ?protocol_constants
    ?output
    client
  |> Process.check

let spawn_check_contract_implements_fa1_2 ~contract client =
  spawn_command client @@ ["check"; "contract"; contract; "implements"; "fa1.2"]

let check_contract_implements_fa1_2 ~contract client =
  let* output =
    spawn_check_contract_implements_fa1_2 ~contract client
    |> Process.check_and_read_stdout
  in
  if output =~ rex "has an FA1\\.2 interface" then return ()
  else
    Test.fail
      "fa12_check_contract_implements_fa1_2: could not parse client output: %s"
      output

let spawn_from_fa1_2_contract_get_balance ~contract ~from client =
  spawn_command client
  @@ ["from"; "fa1.2"; "contract"; contract; "get"; "balance"; "for"; from]

let from_fa1_2_contract_get_balance ~contract ~from client =
  let* output =
    spawn_from_fa1_2_contract_get_balance ~contract ~from client
    |> Process.check_and_read_stdout
  in
  return @@ int_of_string (String.trim output)

let spawn_from_fa1_2_contract_get_allowance ~contract ~owner ~operator client =
  spawn_command client
  @@ [
       "from";
       "fa1.2";
       "contract";
       contract;
       "get";
       "allowance";
       "on";
       owner;
       "as";
       operator;
     ]

let from_fa1_2_contract_get_allowance ~contract ~owner ~operator client =
  let* output =
    spawn_from_fa1_2_contract_get_allowance ~contract ~owner ~operator client
    |> Process.check_and_read_stdout
  in
  return (int_of_string (String.trim output))

let spawn_from_fa1_2_contract_get_total_supply ~contract client =
  spawn_command client
  @@ ["from"; "fa1.2"; "contract"; contract; "get"; "total"; "supply"]

let from_fa1_2_contract_get_total_supply ~contract client =
  let* output =
    spawn_from_fa1_2_contract_get_total_supply ~contract client
    |> Process.check_and_read_stdout
  in
  return (int_of_string (String.trim output))

let spawn_from_fa1_2_contract_get_balance_callback ?burn_cap ~contract ~from
    ~callback client =
  spawn_command client
  @@ [
       "from";
       "fa1.2";
       "contract";
       contract;
       "get";
       "balance";
       "for";
       from;
       "callback";
       "on";
       callback;
     ]
  @ optional_arg "burn-cap" Tez.to_string burn_cap

let from_fa1_2_contract_get_balance_callback ?burn_cap ~contract ~from ~callback
    client =
  spawn_from_fa1_2_contract_get_balance_callback
    ?burn_cap
    ~contract
    ~from
    ~callback
    client
  |> Process.check

let spawn_from_fa1_2_contract_get_allowance_callback ?burn_cap ~contract ~from
    ~to_ ~callback client =
  spawn_command client
  @@ [
       "from";
       "fa1.2";
       "contract";
       contract;
       "get";
       "allowance";
       "on";
       from;
       "as";
       to_;
       "callback";
       "on";
       callback;
     ]
  @ optional_arg "burn-cap" Tez.to_string burn_cap

let from_fa1_2_contract_get_allowance_callback ?burn_cap ~contract ~from ~to_
    ~callback client =
  spawn_from_fa1_2_contract_get_allowance_callback
    ?burn_cap
    ~contract
    ~from
    ~to_
    ~callback
    client
  |> Process.check

let spawn_from_fa1_2_contract_get_total_supply_callback ?burn_cap ~contract
    ~from ~callback client =
  spawn_command client
  @@ [
       "from";
       "fa1.2";
       "contract";
       contract;
       "get";
       "total";
       "supply";
       "as";
       from;
       "callback";
       "on";
       callback;
     ]
  @ optional_arg "burn-cap" Tez.to_string burn_cap

let from_fa1_2_contract_get_total_supply_callback ?burn_cap ~contract ~from
    ~callback client =
  spawn_from_fa1_2_contract_get_total_supply_callback
    ?burn_cap
    ~contract
    ~from
    ~callback
    client
  |> Process.check

let spawn_from_fa1_2_contract_transfer ?(wait = "none") ?burn_cap ~contract
    ~amount ~from ~to_ ?as_ client =
  spawn_command client
  @@ [
       "--wait";
       wait;
       "from";
       "fa1.2";
       "contract";
       contract;
       "transfer";
       string_of_int amount;
       "from";
       from;
       "to";
       to_;
     ]
  @ optional_arg "as" Fun.id as_
  @ optional_arg "burn-cap" Tez.to_string burn_cap

let from_fa1_2_contract_transfer ?wait ?burn_cap ~contract ~amount ~from ~to_
    ?as_ client =
  spawn_from_fa1_2_contract_transfer
    ?wait
    ?burn_cap
    ~contract
    ~amount
    ~from
    ~to_
    ?as_
    client
  |> Process.check

let spawn_from_fa1_2_contract_approve ?(wait = "none") ?burn_cap ~contract ~as_
    ~amount ~from client =
  spawn_command client
  @@ [
       "--wait";
       wait;
       "from";
       "fa1.2";
       "contract";
       contract;
       "as";
       as_;
       "approve";
       string_of_int amount;
       "from";
       from;
     ]
  @ optional_arg "burn-cap" Tez.to_string burn_cap

let from_fa1_2_contract_approve ?wait ?burn_cap ~contract ~as_ ~amount ~from
    client =
  spawn_from_fa1_2_contract_approve
    ?wait
    ?burn_cap
    ~contract
    ~as_
    ~amount
    ~from
    client
  |> Process.check

let spawn_multiple_fa1_2_transfers ?burn_cap ~src ~transfers_json ?as_ client =
  spawn_command client
  @@ ["multiple"; "fa1.2"; "transfers"; "from"; src; "using"; transfers_json]
  @ optional_arg "burn-cap" Tez.to_string burn_cap
  @ optional_arg "as" Fun.id as_

let multiple_fa1_2_transfers ?burn_cap ~src ~transfers_json ?as_ client =
  spawn_multiple_fa1_2_transfers ?burn_cap ~src ~transfers_json ?as_ client
  |> Process.check

let spawn_sapling_gen_key ~name ?(force = false) ?(unencrypted = false) client =
  spawn_command client
  @@ ["sapling"; "gen"; "key"; name]
  @ optional_switch "force" force
  @ optional_switch "unencrypted" unencrypted

let sapling_gen_key ~name ?force ?unencrypted client =
  let* client_output =
    spawn_sapling_gen_key ~name ?force ?unencrypted client
    |> Process.check_and_read_stdout
  in
  let pattern =
    rex
      "It is important to save this mnemonic in a secure place:\n\n\
       ([\\w\\s]+)\n\n\
       The mnemonic"
  in
  match client_output =~* pattern with
  | Some mnemonic ->
      return
        (mnemonic |> String.split_on_char '\n'
        |> List.concat_map (String.split_on_char ' ')
        |> List.map String.trim)
  | None ->
      Test.fail "[sapling_gen_key] Cannot parse client output: %S" client_output

let spawn_sapling_use_key ~sapling_key ~contract ?memo_size client =
  spawn_command client
  @@ ["sapling"; "use"; "key"; sapling_key; "for"; "contract"; contract]
  @ optional_arg "memo-size" string_of_int memo_size

let sapling_use_key ~sapling_key ~contract ?memo_size client =
  spawn_sapling_use_key ~sapling_key ~contract ?memo_size client
  |> Process.check

let spawn_sapling_import_key ~new_ ?(force = false) ?(unencrypted = false)
    ?mnemonic client =
  spawn_command client
  @@ ["sapling"; "import"; "key"; new_]
  @ optional_switch "force" force
  @ optional_switch "unencrypted" unencrypted
  @ optional_arg "mnemonic" (String.concat " ") mnemonic

let sapling_import_key ~new_ ?force ?unencrypted ?mnemonic client =
  spawn_sapling_import_key ~new_ ?force ?unencrypted ?mnemonic client
  |> Process.check

let spawn_sapling_derive_key ~new_ ~name ~child_index ?(force = false)
    ?for_contract ?(unencrypted = false) ?memo_size client =
  spawn_command client
  @@ [
       "sapling";
       "derive";
       "key";
       new_;
       "from";
       name;
       "at";
       "index";
       string_of_int child_index;
     ]
  @ optional_switch "force" force
  @ optional_arg "for-contract" Fun.id for_contract
  @ optional_switch "unencrypted" unencrypted
  @ optional_arg "memo-size" string_of_int memo_size

let sapling_derive_key ~new_ ~name ~child_index ?force ?for_contract
    ?unencrypted ?memo_size client =
  let* client_output =
    spawn_sapling_derive_key
      ~new_
      ~name
      ~child_index
      ?force
      ?for_contract
      ?unencrypted
      ?memo_size
      client
    |> Process.check_and_read_stdout
  in
  match client_output =~* rex "with path (\\S+)" with
  | Some path -> return path
  | None ->
      Test.fail
        "[sapling_derive_key] Cannot parse client output: %S"
        client_output

let spawn_sapling_gen_address ~name ?address_index client =
  spawn_command client
  @@ ["sapling"; "gen"; "address"; name]
  @ optional_arg "address-index" string_of_int address_index

let sapling_gen_address ~name ?address_index client =
  let* client_output =
    spawn_sapling_gen_address ~name ?address_index client
    |> Process.check_and_read_stdout
  in
  let address, index =
    match
      ( client_output =~* rex ".*?(zet1\\w{65}.*)",
        client_output =~* rex "at index (\\d+)" )
    with
    | Some address, Some index -> (address, int_of_string index)
    | _ ->
        Test.fail
          "[sapling_gen_address] Cannot parse client output: %S"
          client_output
  in
  return (address, index)

let spawn_sapling_export_key ~name ~file client =
  spawn_command client @@ ["sapling"; "export"; "key"; name; "in"; file]

let sapling_export_key ~name ~file client =
  spawn_sapling_export_key ~name ~file client |> Process.check

let spawn_sapling_get_balance ~sapling_key ~contract ?(verbose = false) client =
  spawn_command client
  @@ [
       "sapling";
       "get";
       "balance";
       "for";
       sapling_key;
       "in";
       "contract";
       contract;
     ]
  @ optional_switch "verbose" verbose

let sapling_get_balance ~sapling_key ~contract ?verbose client =
  let* client_output =
    spawn_sapling_get_balance ~sapling_key ~contract ?verbose client
    |> Process.check_and_read_stdout
  in
  match client_output =~* rex "Total Sapling funds ?(\\d*)ꜩ" with
  | Some balance -> return (Tez.parse_floating balance)
  | None ->
      Test.fail
        "[sapling_get_balance] Cannot parse client output: %S"
        client_output

let spawn_sapling_list_keys client =
  spawn_command client @@ ["sapling"; "list"; "keys"]

let sapling_list_keys client =
  let* client_output =
    spawn_sapling_list_keys client |> Process.check_and_read_stdout
  in
  return (client_output |> String.trim |> String.split_on_char '\n')

(* These regexs only work from protocol J, before there is no field
   `storage fees`. *)
let sapling_extract_balance_diff_and_fees ~sapling_contract client_output =
  let fees =
    let re = ".*fees.* \\.* \\+ꜩ?(\\d*[.\\d*]*)" in
    let res = matches client_output (rex re) in
    List.map Tez.parse_floating res |> List.fold_left Tez.( + ) Tez.zero
  in
  let amount_pos =
    let re = sapling_contract ^ ".*\\+ꜩ?(\\d*[.\\d*]*)" in
    match matches client_output (rex re) with
    | [s] -> Tez.parse_floating s
    | _ -> Tez.zero
  in
  let amount_neg =
    let re = sapling_contract ^ ".*\\-ꜩ?(\\d*[.\\d*]*)" in
    match matches client_output (rex re) with
    | [s] -> Tez.parse_floating s
    | _ -> Tez.zero
  in
  (Tez.(amount_pos - amount_neg), fees)

let spawn_sapling_shield ?(wait = "none") ?burn_cap ~qty ~src_tz ~dst_sap
    ~sapling_contract ?message client =
  spawn_command client
  @@ [
       "--wait";
       wait;
       "sapling";
       "shield";
       Tez.to_string qty;
       "from";
       src_tz;
       "to";
       dst_sap;
       "using";
       sapling_contract;
     ]
  @ optional_arg "burn-cap" Tez.to_string burn_cap
  @ optional_arg "message" Fun.id message

let sapling_shield ?wait ?burn_cap ~qty ~src_tz ~dst_sap ~sapling_contract
    ?message client =
  let* client_output =
    spawn_sapling_shield
      ?wait
      ?burn_cap
      ~qty
      ~src_tz
      ~dst_sap
      ~sapling_contract
      ?message
      client
    |> Process.check_and_read_stdout
  in
  return (sapling_extract_balance_diff_and_fees ~sapling_contract client_output)

let spawn_sapling_unshield ?(wait = "none") ?burn_cap ~qty ~src_sap ~dst_tz
    ~sapling_contract client =
  spawn_command client
  @@ [
       "--wait";
       wait;
       "sapling";
       "unshield";
       Tez.to_string qty;
       "from";
       src_sap;
       "to";
       dst_tz;
       "using";
       sapling_contract;
     ]
  @ optional_arg "burn-cap" Tez.to_string burn_cap

let sapling_unshield ?wait ?burn_cap ~qty ~src_sap ~dst_tz ~sapling_contract
    client =
  let* client_output =
    spawn_sapling_unshield
      ?wait
      ?burn_cap
      ~qty
      ~src_sap
      ~dst_tz
      ~sapling_contract
      client
    |> Process.check_and_read_stdout
  in
  return (sapling_extract_balance_diff_and_fees ~sapling_contract client_output)

let spawn_sapling_forge_transaction ?(wait = "none") ?burn_cap ~qty ~src_sap
    ~dst_sap ~sapling_contract ?file ?(json = false) client =
  spawn_command client
  @@ [
       "--wait";
       wait;
       "sapling";
       "forge";
       "transaction";
       Tez.to_string qty;
       "from";
       src_sap;
       "to";
       dst_sap;
       "using";
       sapling_contract;
     ]
  @ optional_arg "burn-cap" Tez.to_string burn_cap
  @ optional_arg "file" Fun.id file
  @ optional_switch "json" json

let sapling_forge_transaction ?wait ?burn_cap ~qty ~src_sap ~dst_sap
    ~sapling_contract ?file ?json client =
  spawn_sapling_forge_transaction
    ?wait
    ?burn_cap
    ~qty
    ~src_sap
    ~dst_sap
    ~sapling_contract
    ?file
    ?json
    client
  |> Process.check

let spawn_sapling_submit ?(wait = "none") ?burn_cap ~file ~alias_tz
    ~sapling_contract ?(json = false) client =
  spawn_command client
  @@ [
       "--wait";
       wait;
       "sapling";
       "submit";
       file;
       "from";
       alias_tz;
       "using";
       sapling_contract;
     ]
  @ optional_arg "burn-cap" Tez.to_string burn_cap
  @ optional_switch "json" json

let sapling_submit ?wait ?burn_cap ~file ~alias_tz ~sapling_contract ?json
    client =
  spawn_sapling_submit
    ?wait
    ?burn_cap
    ~file
    ~alias_tz
    ~sapling_contract
    ?json
    client
  |> Process.check

let spawn_compute_chain_id_from_block_hash ?endpoint client block_hash =
  spawn_command ?endpoint client
  @@ ["compute"; "chain"; "id"; "from"; "block"; "hash"; block_hash]

(** Run [tezos-client compute chain id from block hash]. *)
let compute_chain_id_from_block_hash ?endpoint client block_hash =
  let* output =
    spawn_compute_chain_id_from_block_hash ?endpoint client block_hash
    |> Process.check_and_read_stdout
  in
  return (String.trim output)

let spawn_compute_chain_id_from_seed ?endpoint client seed =
  spawn_command ?endpoint client
  @@ ["compute"; "chain"; "id"; "from"; "seed"; seed]

(** Run [tezos-client compute chain id from seed]. *)
let compute_chain_id_from_seed ?endpoint client seed =
  let* output =
    spawn_compute_chain_id_from_seed ?endpoint client seed
    |> Process.check_and_read_stdout
  in
  return (String.trim output)

let spawn_show_supported_multisig_hashes client =
  spawn_command client @@ ["show"; "supported"; "multisig"; "hashes"]

let show_supported_multisig_hashes client =
  spawn_show_supported_multisig_hashes client |> Process.check

let spawn_show_multisig_script client =
  spawn_command client @@ ["show"; "multisig"; "script"]

let show_multisig_script client =
  spawn_show_multisig_script client |> Process.check

let spawn_deploy_multisig ~new_multisig ~qty ~src ~threshold ~keys ?delegate
    ?(force = false) ?burn_cap client =
  spawn_command client
  @@ [
       "deploy";
       "multisig";
       new_multisig;
       "transferring";
       Tez.to_string qty;
       "from";
       src;
       "with";
       "threshold";
       string_of_int threshold;
       "on";
       "public";
       "keys";
     ]
  @ keys
  @ optional_arg "delegate" Fun.id delegate
  @ optional_switch "force" force
  @ optional_arg "burn-cap" Tez.to_string burn_cap

let deploy_multisig ~new_multisig ~qty ~src ~threshold ~keys ?delegate ?force
    ?burn_cap client =
  spawn_deploy_multisig
    ~new_multisig
    ~qty
    ~src
    ~threshold
    ~keys
    ?delegate
    ?force
    ?burn_cap
    client
  |> Process.check

let spawn_sign_multisig_transaction_transfer ~multisig ~qty ~dst ~key ?arg
    ?entrypoint client =
  spawn_command client
  @@ [
       "sign";
       "multisig";
       "transaction";
       "on";
       multisig;
       "transferring";
       Tez.to_string qty;
       "to";
       dst;
       "using";
       "secret";
       "key";
       key;
     ]
  @ optional_arg "arg" Fun.id arg
  @ optional_arg "entrypoint" Fun.id entrypoint

let sign_multisig_transaction_transfer ~multisig ~qty ~dst ~key ?arg ?entrypoint
    client =
  let* client_output =
    spawn_sign_multisig_transaction_transfer
      ~multisig
      ~qty
      ~dst
      ~key
      ?arg
      ?entrypoint
      client
    |> Process.check_and_read_stdout
  in
  return (String.trim client_output)

let spawn_sign_multisig_transaction_run_lambda ~multisig ~lambda ~key client =
  spawn_command client
  @@ [
       "sign";
       "multisig";
       "transaction";
       "on";
       multisig;
       "running";
       "lambda";
       lambda;
       "using";
       "secret";
       "key";
       key;
     ]

let sign_multisig_transaction_run_lambda ~multisig ~lambda ~key client =
  let* client_output =
    spawn_sign_multisig_transaction_run_lambda ~multisig ~lambda ~key client
    |> Process.check_and_read_stdout
  in
  return (String.trim client_output)

let spawn_sign_multisig_transaction_set_delegate ~multisig ~dlgt ~key client =
  spawn_command client
  @@ [
       "sign";
       "multisig";
       "transaction";
       "on";
       multisig;
       "setting";
       "delegate";
       "to";
       dlgt;
       "using";
       "secret";
       "key";
       key;
     ]

let sign_multisig_transaction_set_delegate ~multisig ~dlgt ~key client =
  let* client_output =
    spawn_sign_multisig_transaction_set_delegate ~multisig ~dlgt ~key client
    |> Process.check_and_read_stdout
  in
  return (String.trim client_output)

let spawn_sign_multisig_transaction_withdraw_delegate ~multisig ~key client =
  spawn_command client
  @@ [
       "sign";
       "multisig";
       "transaction";
       "on";
       multisig;
       "withdrawing";
       "delegate";
       "using";
       "secret";
       "key";
       key;
     ]

let sign_multisig_transaction_withdraw_delegate ~multisig ~key client =
  let* client_output =
    spawn_sign_multisig_transaction_withdraw_delegate ~multisig ~key client
    |> Process.check_and_read_stdout
  in
  return (String.trim client_output)

let spawn_sign_multisig_transaction_set_threshold_and_public_keys ~multisig
    ~signing_key ~threshold ~public_keys client =
  spawn_command client
  @@ [
       "sign";
       "multisig";
       "transaction";
       "on";
       multisig;
       "using";
       "secret";
       "key";
       signing_key;
       "setting";
       "threshold";
       "to";
       string_of_int threshold;
       "and";
       "public";
       "keys";
       "to";
     ]
  @ public_keys

let sign_multisig_transaction_set_threshold_and_public_keys ~multisig
    ~signing_key ~threshold ~public_keys client =
  let* client_output =
    spawn_sign_multisig_transaction_set_threshold_and_public_keys
      ~multisig
      ~signing_key
      ~threshold
      ~public_keys
      client
    |> Process.check_and_read_stdout
  in
  return (String.trim client_output)

let spawn_from_multisig_transfer ~multisig ~qty ~dst ~src ~signatures ?arg
    ?burn_cap ?entrypoint client =
  spawn_command client
  @@ [
       "from";
       "multisig";
       "contract";
       multisig;
       "transfer";
       Tez.to_string qty;
       "to";
       dst;
       "on";
       "behalf";
       "of";
       src;
       "with";
       "signatures";
     ]
  @ signatures
  @ optional_arg "arg" Fun.id arg
  @ optional_arg "burn-cap" Tez.to_string burn_cap
  @ optional_arg "entrypoint" Fun.id entrypoint

let from_multisig_transfer ~multisig ~qty ~dst ~src ~signatures ?arg ?burn_cap
    ?entrypoint client =
  spawn_from_multisig_transfer
    ~multisig
    ~qty
    ~dst
    ~src
    ~signatures
    ?arg
    ?burn_cap
    ?entrypoint
    client
  |> Process.check

let spawn_from_multisig_run_lambda ~multisig ~lambda ~src ~signatures ?burn_cap
    client =
  spawn_command client
  @@ [
       "from";
       "multisig";
       "contract";
       multisig;
       "run";
       "lambda";
       lambda;
       "on";
       "behalf";
       "of";
       src;
       "with";
       "signatures";
     ]
  @ signatures
  @ optional_arg "burn-cap" Tez.to_string burn_cap

let from_multisig_run_lambda ~multisig ~lambda ~src ~signatures ?burn_cap client
    =
  spawn_from_multisig_run_lambda
    ~multisig
    ~lambda
    ~src
    ~signatures
    ?burn_cap
    client
  |> Process.check

let spawn_set_delegate_of_multisig ~multisig ~dlgt ~src ~signatures ?burn_cap
    client =
  spawn_command client
  @@ [
       "set";
       "delegate";
       "of";
       "multisig";
       "contract";
       multisig;
       "to";
       dlgt;
       "on";
       "behalf";
       "of";
       src;
       "with";
       "signatures";
     ]
  @ signatures
  @ optional_arg "burn-cap" Tez.to_string burn_cap

let set_delegate_of_multisig ~multisig ~dlgt ~src ~signatures ?burn_cap client =
  spawn_set_delegate_of_multisig
    ~multisig
    ~dlgt
    ~src
    ~signatures
    ?burn_cap
    client
  |> Process.check

let spawn_withdraw_delegate_of_multisig ~multisig ~src ~signatures ?burn_cap
    client =
  spawn_command client
  @@ [
       "withdraw";
       "delegate";
       "of";
       "multisig";
       "contract";
       multisig;
       "on";
       "behalf";
       "of";
       src;
       "with";
       "signatures";
     ]
  @ signatures
  @ optional_arg "burn-cap" Tez.to_string burn_cap

let withdraw_delegate_of_multisig ~multisig ~src ~signatures ?burn_cap client =
  spawn_withdraw_delegate_of_multisig
    ~multisig
    ~src
    ~signatures
    ?burn_cap
    client
  |> Process.check

let spawn_set_threshold_of_multisig ~multisig ~threshold ~public_keys ~src
    ~signatures ?burn_cap client =
  spawn_command client
  @@ [
       "set";
       "threshold";
       "of";
       "multisig";
       "contract";
       multisig;
       "to";
       string_of_int threshold;
       "and";
       "public";
       "keys";
       "to";
     ]
  @ public_keys
  @ ["on"; "behalf"; "of"; src; "with"; "signatures"]
  @ signatures
  @ optional_arg "burn-cap" Tez.to_string burn_cap

let set_threshold_of_multisig ~multisig ~threshold ~public_keys ~src ~signatures
    ?burn_cap client =
  spawn_set_threshold_of_multisig
    ~multisig
    ~threshold
    ~public_keys
    ~src
    ~signatures
    ?burn_cap
    client
  |> Process.check

let spawn_run_transaction_on_multisig ~bytes ~multisig ~src ~signatures
    ?burn_cap client =
  spawn_command client
  @@ [
       "run";
       "transaction";
       bytes;
       "on";
       "multisig";
       "contract";
       multisig;
       "on";
       "behalf";
       "of";
       src;
       "with";
       "signatures";
     ]
  @ signatures
  @ optional_arg "burn-cap" Tez.to_string burn_cap

let run_transaction_on_multisig ~bytes ~multisig ~src ~signatures ?burn_cap
    client =
  spawn_run_transaction_on_multisig
    ~bytes
    ~multisig
    ~src
    ~signatures
    ?burn_cap
    client
  |> Process.check

let spawn_prepare_multisig_transaction ~multisig ~qty ~dst ?arg ?entrypoint
    ?(bytes_only = false) client =
  spawn_command client
  @@ [
       "prepare";
       "multisig";
       "transaction";
       "on";
       multisig;
       "transferring";
       Tez.to_string qty;
       "to";
       dst;
     ]
  @ optional_arg "arg" Fun.id arg
  @ optional_arg "entrypoint" Fun.id entrypoint
  @ optional_switch "bytes-only" bytes_only

let prepare_multisig_transaction ~multisig ~qty ~dst ?arg ?entrypoint
    ?bytes_only client =
  let* client_output =
    spawn_prepare_multisig_transaction
      ~multisig
      ~qty
      ~dst
      ?arg
      ?entrypoint
      ?bytes_only
      client
    |> Process.check_and_read_stdout
  in
  return (String.trim client_output)

let spawn_prepare_multisig_transaction_run_lambda ~multisig ~lambda
    ?(bytes_only = false) client =
  spawn_command client
  @@ [
       "prepare";
       "multisig";
       "transaction";
       "on";
       multisig;
       "running";
       "lambda";
       lambda;
     ]
  @ optional_switch "bytes-only" bytes_only

let prepare_multisig_transaction_run_lambda ~multisig ~lambda ?bytes_only client
    =
  let* client_output =
    spawn_prepare_multisig_transaction_run_lambda
      ~multisig
      ~lambda
      ?bytes_only
      client
    |> Process.check_and_read_stdout
  in
  return (String.trim client_output)

let spawn_prepare_multisig_transaction_set_delegate ~multisig ~dlgt
    ?(bytes_only = false) client =
  spawn_command client
  @@ [
       "prepare";
       "multisig";
       "transaction";
       "on";
       multisig;
       "setting";
       "delegate";
       "to";
       dlgt;
     ]
  @ optional_switch "bytes-only" bytes_only

let prepare_multisig_transaction_set_delegate ~multisig ~dlgt ?bytes_only client
    =
  let* client_output =
    spawn_prepare_multisig_transaction_set_delegate
      ~multisig
      ~dlgt
      ?bytes_only
      client
    |> Process.check_and_read_stdout
  in
  return (String.trim client_output)

let spawn_prepare_multisig_transaction_withdraw_delegate ~multisig
    ?(bytes_only = false) client =
  spawn_command client
  @@ [
       "prepare";
       "multisig";
       "transaction";
       "on";
       multisig;
       "withdrawing";
       "delegate";
     ]
  @ optional_switch "bytes-only" bytes_only

let prepare_multisig_transaction_withdraw_delegate ~multisig ?bytes_only client
    =
  let* client_output =
    spawn_prepare_multisig_transaction_withdraw_delegate
      ~multisig
      ?bytes_only
      client
    |> Process.check_and_read_stdout
  in
  return (String.trim client_output)

let spawn_prepare_multisig_transaction_set_threshold_and_public_keys ~multisig
    ~threshold ~public_keys ?(bytes_only = false) client =
  spawn_command client
  @@ [
       "prepare";
       "multisig";
       "transaction";
       "on";
       multisig;
       "setting";
       "threshold";
       "to";
       string_of_int threshold;
       "and";
       "public";
       "keys";
       "to";
     ]
  @ public_keys
  @ optional_switch "bytes-only" bytes_only

let prepare_multisig_transaction_set_threshold_and_public_keys ~multisig
    ~threshold ~public_keys ?bytes_only client =
  let* client_output =
    spawn_prepare_multisig_transaction_set_threshold_and_public_keys
      ~multisig
      ~threshold
      ~public_keys
      ?bytes_only
      client
    |> Process.check_and_read_stdout
  in
  return (String.trim client_output)

let spawn_expand_macros ?endpoint ?hooks ?protocol_hash ?no_base_dir_warnings
    client path =
  spawn_command ?endpoint ?hooks ?protocol_hash ?no_base_dir_warnings client
  @@ ["expand"; "macros"; "in"; path]

let expand_macros ?endpoint ?hooks ?protocol_hash ?no_base_dir_warnings client
    path =
  spawn_expand_macros
    ?endpoint
    ?hooks
    ?protocol_hash
    ?no_base_dir_warnings
    client
    path
  |> Process.check_and_read_stdout

let spawn_get_timestamp ?endpoint ?block ?(seconds = false) client =
  spawn_command ?endpoint ?block client
  @@ ["get"; "timestamp"]
  @ optional_switch "seconds" seconds

let get_timestamp ?endpoint ?block ?seconds client =
  let* output =
    spawn_get_timestamp ?endpoint ?block ?seconds client
    |> Process.check_and_read_stdout
  in
  return (String.trim output)

let publish_dal_commitment ?hooks ?(wait = "none") ?burn_cap ~src ~commitment
    ~slot_index ~proof client =
  let process =
    spawn_command
      ?hooks
      client
      (["--wait"; wait]
      @ [
          "publish";
          "dal";
          "commitment";
          commitment;
          "from";
          src;
          "for";
          "slot";
          string_of_int slot_index;
          "with";
          "proof";
          proof;
        ]
      @ optional_arg "burn-cap" Tez.to_string burn_cap)
  in
  let parse process = Process.check process in
  {value = process; run = parse}

let as_foreign_endpoint = function
  | Node node -> Node.as_rpc_endpoint node
  | Foreign_endpoint fe -> fe
  | Proxy_server ps ->
      Endpoint.
        {
          scheme = Proxy_server.rpc_scheme;
          host = Proxy_server.rpc_host;
          port = Proxy_server.rpc_port ps;
        }

module RPC = struct
  let call_raw ?log_command ?log_status_on_exit ?log_output ?better_errors
      ?endpoint ?hooks ?env ?protocol_hash client
      RPC_core.{verb; path; query_string; data; decode = _; _} =
    (* No need to log here, the [Process] module already logs. *)
    spawn_rpc
      ?log_command
      ?log_status_on_exit
      ?log_output
      ?better_errors
      ?endpoint
      ?hooks
      ?env
      ?data
      ?protocol_hash
      ~query_string
      verb
      path
      client
    |> Process.check_and_read_stdout

  let call_json ?log_command ?log_status_on_exit ?log_output ?better_errors
      ?endpoint ?hooks ?env ?protocol_hash client rpc =
    let* raw =
      call_raw
        ?log_command
        ?log_status_on_exit
        ?log_output
        ?better_errors
        ?endpoint
        ?hooks
        ?env
        ?protocol_hash
        client
        rpc
    in
    return (JSON.parse ~origin:"RPC response" raw)

  let call ?log_command ?log_status_on_exit ?log_output ?better_errors ?endpoint
      ?hooks ?env ?protocol_hash client rpc =
    let* json =
      call_json
        ?log_command
        ?log_status_on_exit
        ?log_output
        ?better_errors
        ?endpoint
        ?hooks
        ?env
        ?protocol_hash
        client
        rpc
    in
    return (rpc.decode json)

  let schema ?log_command ?log_status_on_exit ?log_output ?better_errors
      ?endpoint ?hooks ?env ?protocol_hash client RPC_core.{verb; path; _} =
    rpc_schema
      ?log_command
      ?log_status_on_exit
      ?log_output
      ?better_errors
      ?endpoint
      ?hooks
      ?env
      ?protocol_hash
      verb
      path
      client

  let spawn ?log_command ?log_status_on_exit ?log_output ?better_errors
      ?endpoint ?hooks ?env ?protocol_hash client
      RPC_core.{verb; path; query_string; data; decode = _; _} =
    Spawn.rpc
      ?log_command
      ?log_status_on_exit
      ?log_output
      ?better_errors
      ?endpoint
      ?hooks
      ?env
      ?data
      ?protocol_hash
      ~query_string
      verb
      path
      client
end
