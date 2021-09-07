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

type endpoint = Node of Node.t | Proxy_server of Proxy_server.t

let rpc_port = function
  | Node n -> Node.rpc_port n
  | Proxy_server ps -> Proxy_server.rpc_port ps

type mode =
  | Client of endpoint option
  | Mockup
  | Light of float * endpoint list
  | Proxy of endpoint

type mockup_sync_mode = Asynchronous | Synchronous

type normalize_mode = Readable | Optimized | Optimized_legacy

type t = {
  path : string;
  admin_path : string;
  name : string;
  color : Log.Color.t;
  base_dir : string;
  mutable mode : mode;
}

let base_dir t = t.base_dir

let get_mode t = t.mode

let set_mode mode t = t.mode <- mode

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

let address ?(hostname = false) ?from peer =
  match from with
  | None -> Runner.address ~hostname (runner peer)
  | Some endpoint ->
      Runner.address ~hostname ?from:(runner endpoint) (runner peer)

let create_with_mode ?(path = Constant.tezos_client)
    ?(admin_path = Constant.tezos_admin_client) ?name
    ?(color = Log.Color.FG.blue) ?base_dir mode =
  let name = match name with None -> fresh_name () | Some name -> name in
  let base_dir =
    match base_dir with None -> Temp.dir name | Some dir -> dir
  in
  {path; admin_path; name; color; base_dir; mode}

let create ?path ?admin_path ?name ?color ?base_dir ?endpoint () =
  create_with_mode ?path ?admin_path ?name ?color ?base_dir (Client endpoint)

let base_dir_arg client = ["--base-dir"; client.base_dir]

(* To avoid repeating unduly the sources file name, we create a function here
   to get said file name as string.
   Do not call it from a client in Mockup or Client (nominal) mode. *)
let sources_file client =
  match client.mode with
  | Mockup | Client _ | Proxy _ -> assert false
  | Light _ -> client.base_dir // "sources.json"

let mode_to_endpoint = function
  | Client None | Mockup | Light (_, []) -> None
  | Client (Some endpoint) | Light (_, endpoint :: _) | Proxy endpoint ->
      Some endpoint

(* [?endpoint] can be used to override the default node stored in the client.
   Mockup nodes do not use [--endpoint] at all: RPCs are mocked up.
   Light mode needs a file (specified with [--sources] on the CLI)
   that contains a list of endpoints.
*)
let endpoint_arg ?(endpoint : endpoint option) client =
  let either o1 o2 = match (o1, o2) with (Some _, _) -> o1 | _ -> o2 in
  (* pass [?endpoint] first: it has precedence over client.mode *)
  match either endpoint (mode_to_endpoint client.mode) with
  | None -> []
  | Some e ->
      ["--endpoint"; sf "http://%s:%d" (address ~hostname:true e) (rpc_port e)]

let mode_arg client =
  match client.mode with
  | Client _ -> []
  | Mockup -> ["--mode"; "mockup"]
  | Light _ -> ["--mode"; "light"; "--sources"; sources_file client]
  | Proxy _ -> ["--mode"; "proxy"]

let spawn_command ?(env = String_map.empty) ?endpoint ?hooks ?(admin = false)
    client command =
  let env =
    (* Set disclaimer to "Y" if unspecified, otherwise use given value *)
    String_map.update
      "TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER"
      (fun o -> Option.value ~default:"Y" o |> Option.some)
      env
  in
  Process.spawn
    ~name:client.name
    ~color:client.color
    ~env
    ?hooks
    (if admin then client.admin_path else client.path)
  @@ endpoint_arg ?endpoint client
  @ mode_arg client @ base_dir_arg client @ command

let url_encode str =
  let buffer = Buffer.create (String.length str * 3) in
  for i = 0 to String.length str - 1 do
    match str.[i] with
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-' | '/') as c ->
        Buffer.add_char buffer c
    | c ->
        Buffer.add_char buffer '%' ;
        let (c1, c2) = Hex.of_char c in
        Buffer.add_char buffer c1 ;
        Buffer.add_char buffer c2
  done ;
  let result = Buffer.contents buffer in
  Buffer.reset buffer ;
  result

type meth = GET | PUT | POST | PATCH | DELETE

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

let spawn_rpc ?endpoint ?hooks ?env ?data ?query_string meth path client =
  let data =
    Option.fold ~none:[] ~some:(fun x -> ["with"; JSON.encode_u x]) data
  in
  let query_string =
    Option.fold ~none:"" ~some:string_of_query_string query_string
  in
  let path = string_of_path path in
  let full_path = path ^ query_string in
  spawn_command
    ?endpoint
    ?hooks
    ?env
    client
    (["rpc"; string_of_meth meth; full_path] @ data)

let rpc ?endpoint ?hooks ?env ?data ?query_string meth path client =
  let* output =
    spawn_rpc ?endpoint ?hooks ?env ?data ?query_string meth path client
    |> Process.check_and_read_stdout
  in
  return (JSON.parse ~origin:(string_of_path path ^ " response") output)

let spawn_rpc_list ?endpoint client =
  spawn_command ?endpoint client ["rpc"; "list"]

let rpc_list ?endpoint client =
  spawn_rpc_list ?endpoint client |> Process.check_and_read_stdout

let spawn_shell_header ?endpoint ?(chain = "main") ?(block = "head") client =
  let path = ["chains"; chain; "blocks"; block; "header"; "shell"] in
  spawn_rpc ?endpoint GET path client

let shell_header ?endpoint ?chain ?block client =
  spawn_shell_header ?endpoint ?chain ?block client
  |> Process.check_and_read_stdout

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

  let trust_address ?endpoint ~peer client =
    spawn_trust_address ?endpoint ~peer client |> Process.check

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
end

let spawn_version client = spawn_command client ["--version"]

let version client = spawn_version client |> Process.check

let spawn_import_secret_key ?endpoint client (key : Constant.key) =
  spawn_command
    ?endpoint
    client
    ["import"; "secret"; "key"; key.alias; key.secret]

let import_secret_key ?endpoint client key =
  spawn_import_secret_key ?endpoint client key |> Process.check

let spawn_activate_protocol ?endpoint ~protocol ?(fitness = 1)
    ?(key = Constant.activator.alias) ?timestamp
    ?(timestamp_delay = 3600. *. 24. *. 365.) ?parameter_file client =
  let timestamp =
    match timestamp with
    | Some timestamp -> timestamp
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
    ?endpoint
    client
    [
      "activate";
      "protocol";
      Protocol.hash protocol;
      "with";
      "fitness";
      string_of_int fitness;
      "and";
      "key";
      key;
      "and";
      "parameters";
      Option.value parameter_file ~default:(Protocol.parameter_file protocol);
      "--timestamp";
      timestamp;
    ]

let activate_protocol ?endpoint ~protocol ?fitness ?key ?timestamp
    ?timestamp_delay ?parameter_file client =
  spawn_activate_protocol
    ?endpoint
    ~protocol
    ?fitness
    ?key
    ?timestamp
    ?timestamp_delay
    ?parameter_file
    client
  |> Process.check

let spawn_endorse_for ?endpoint ?(key = Constant.bootstrap2.alias) client =
  spawn_command ?endpoint client ["endorse"; "for"; key]

let endorse_for ?endpoint ?key client =
  spawn_endorse_for ?endpoint ?key client |> Process.check

let spawn_bake_for ?endpoint ?protocol ?(key = Constant.bootstrap1.alias)
    ?(minimal_timestamp = true) ?mempool ?force ?context_path client =
  spawn_command
    ?endpoint
    client
    (Option.fold
       ~none:[]
       ~some:(fun p -> ["--protocol"; Protocol.hash p])
       protocol
    @ ["bake"; "for"; key]
    @ (if minimal_timestamp then ["--minimal-timestamp"] else [])
    @ Option.fold
        ~none:[]
        ~some:(fun mempool_json -> ["--mempool"; mempool_json])
        mempool
    @ (match force with None | Some false -> [] | Some true -> ["--force"])
    @ Option.fold ~none:[] ~some:(fun path -> ["--context"; path]) context_path
    )

let bake_for ?endpoint ?protocol ?key ?minimal_timestamp ?mempool ?force
    ?context_path client =
  spawn_bake_for
    ?endpoint
    ?key
    ?minimal_timestamp
    ?mempool
    ?force
    ?context_path
    ?protocol
    client
  |> Process.check

let spawn_gen_keys ~alias client = spawn_command client ["gen"; "keys"; alias]

let gen_keys ~alias client = spawn_gen_keys ~alias client |> Process.check

let spawn_show_address ?(show_secret = true) ~alias client =
  spawn_command client
  @@
  if show_secret then ["show"; "address"; alias; "--show-secret"]
  else ["show"; "address"; alias]

let show_address ?show_secret ~alias client =
  let extract_key (client_output : string) : Account.key =
    let public_key_hash =
      match client_output =~* rex "Hash: ?(\\w*)" with
      | None ->
          Test.fail "Cannot extract key from client_output: %s" client_output
      | Some hash -> hash
    in
    let public_key = client_output =~* rex "Public Key: ?(\\w*)" in
    let secret_key =
      client_output =~* rex "Secret Key: ?(?:unencrypted:)?(\\w*)"
    in
    {alias; public_key_hash; public_key; secret_key}
  in
  let* output =
    spawn_show_address ?show_secret ~alias client
    |> Process.check_and_read_stdout
  in
  return @@ extract_key output

let gen_and_show_keys ~alias client =
  let* () = gen_keys ~alias client in
  show_address ~show_secret:true ~alias client

let spawn_transfer ?endpoint ?(wait = "none") ?burn_cap ?fee ?gas_limit
    ?storage_limit ?counter ?arg ~amount ~giver ~receiver client =
  spawn_command
    ?endpoint
    client
    (["--wait"; wait]
    @ ["transfer"; Tez.to_string amount; "from"; giver; "to"; receiver]
    @ Option.fold
        ~none:[]
        ~some:(fun f -> ["--fee"; Tez.to_string f; "--force-low-fee"])
        fee
    @ Option.fold
        ~none:[]
        ~some:(fun b -> ["--burn-cap"; Tez.to_string b])
        burn_cap
    @ Option.fold
        ~none:[]
        ~some:(fun g -> ["--gas-limit"; string_of_int g])
        gas_limit
    @ Option.fold
        ~none:[]
        ~some:(fun s -> ["--storage-limit"; string_of_int s])
        storage_limit
    @ Option.fold
        ~none:[]
        ~some:(fun s -> ["--counter"; string_of_int s])
        counter
    @ Option.fold ~none:[] ~some:(fun p -> ["--arg"; p]) arg)

let transfer ?endpoint ?wait ?burn_cap ?fee ?gas_limit ?storage_limit ?counter
    ?arg ~amount ~giver ~receiver client =
  spawn_transfer
    ?endpoint
    ?wait
    ?burn_cap
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    ?arg
    ~amount
    ~giver
    ~receiver
    client
  |> Process.check

let spawn_set_delegate ?endpoint ?(wait = "none") ~src ~delegate client =
  spawn_command
    ?endpoint
    client
    (["--wait"; wait] @ ["set"; "delegate"; "for"; src; "to"; delegate])

let set_delegate ?endpoint ?wait ~src ~delegate client =
  spawn_set_delegate ?endpoint ?wait ~src ~delegate client |> Process.check

let spawn_withdraw_delegate ?endpoint ?(wait = "none") ~src client =
  spawn_command
    ?endpoint
    client
    (["--wait"; wait] @ ["withdraw"; "delegate"; "for"; src])

let withdraw_delegate ?endpoint ?wait ~src client =
  spawn_withdraw_delegate ?endpoint ?wait ~src client |> Process.check

let spawn_get_balance_for ?endpoint ~account client =
  spawn_command ?endpoint client ["get"; "balance"; "for"; account]

let get_balance_for =
  let re = rex "(\\d+(?:\\.\\d+)?) \u{A729}" in
  fun ?endpoint ~account client ->
    let extract_balance (client_output : string) : float =
      match client_output =~* re with
      | None ->
          Test.fail
            "Cannot extract balance from client_output: %s"
            client_output
      | Some balance -> float_of_string balance
    in
    let process = spawn_get_balance_for ?endpoint ~account client in
    let* () = Process.check process
    and* output = Lwt_io.read (Process.stdout process) in
    return @@ extract_balance output

let spawn_create_mockup ?(sync_mode = Synchronous) ?constants ~protocol client =
  let cmd =
    let common = ["--protocol"; Protocol.hash protocol; "create"; "mockup"] in
    (match sync_mode with
    | Synchronous -> common
    | Asynchronous -> common @ ["--asynchronous"])
    @ Option.fold
        ~none:[]
        ~some:(fun constants ->
          ["--protocol-constants"; Protocol.parameter_file ~constants protocol])
        constants
  in
  spawn_command client cmd

let create_mockup ?sync_mode ?constants ~protocol client =
  spawn_create_mockup ?sync_mode ?constants ~protocol client |> Process.check

let spawn_submit_proposals ?(key = Constant.bootstrap1.alias) ?(wait = "none")
    ~proto_hash client =
  spawn_command
    client
    (["--wait"; wait] @ ["submit"; "proposals"; "for"; key; proto_hash])

let submit_proposals ?key ?wait ~proto_hash client =
  spawn_submit_proposals ?key ?wait ~proto_hash client |> Process.check

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

let spawn_originate_contract ?endpoint ?(wait = "none") ?init ?burn_cap ~alias
    ~amount ~src ~prg client =
  spawn_command
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
    @ Option.fold ~none:[] ~some:(fun init -> ["--init"; init]) init
    @ Option.fold
        ~none:[]
        ~some:(fun burn_cap -> ["--burn-cap"; Tez.to_string burn_cap])
        burn_cap)

let originate_contract ?endpoint ?wait ?init ?burn_cap ~alias ~amount ~src ~prg
    client =
  let* client_output =
    spawn_originate_contract
      ?endpoint
      ?wait
      ?init
      ?burn_cap
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

let spawn_stresstest ?endpoint ?tps ~sources ~transfers client =
  let tps_arg =
    Option.map (fun (tps : int) -> ["--tps"; Int.to_string tps]) tps
    |> Option.value ~default:[]
  in
  spawn_command ?endpoint client
  @@ [
       "stresstest";
       "transfer";
       "using";
       sources;
       "--transfers";
       Int.to_string transfers;
     ]
  @ tps_arg

let stresstest ?endpoint ?tps ~sources ~transfers client =
  spawn_stresstest ?endpoint ?tps ~sources ~transfers client |> Process.check

let spawn_run_script ~src ~storage ~input client =
  spawn_command
    client
    ["run"; "script"; src; "on"; "storage"; storage; "and"; "input"; input]

let run_script ~src ~storage ~input client =
  let* client_output =
    spawn_run_script ~src ~storage ~input client
    |> Process.check_and_read_stdout
  in
  match client_output =~* rex "storage\n(.*)" with
  | None ->
      Test.fail
        "Cannot extract new storage from client_output: %s"
        client_output
  | Some storage -> return @@ String.trim storage

let spawn_register_global_constant ?(wait = "none") ?burn_cap ~value ~src client
    =
  spawn_command
    client
    (["--wait"; wait]
    @ ["register"; "global"; "constant"; value; "from"; src]
    @ Option.fold
        ~none:[]
        ~some:(fun burn_cap -> ["--burn-cap"; Tez.to_string burn_cap])
        burn_cap)

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

let spawn_hash_data ?hooks ~data ~typ client =
  let cmd = ["hash"; "data"; data; "of"; "type"; typ] in
  spawn_command ?hooks client cmd

let hash_data ?expect_failure ?hooks ~data ~typ client =
  let* output =
    spawn_hash_data ?hooks ~data ~typ client
    |> Process.check_and_read_stdout ?expect_failure
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
  Lwt.return key_value_list

let spawn_normalize_data ?mode ?(legacy = false) ~data ~typ client =
  let mode_to_string = function
    | Readable -> "Readable"
    | Optimized -> "Optimized"
    | Optimized_legacy -> "Optimized_legacy"
  in
  let mode_cmd =
    Option.map mode_to_string mode
    |> Option.map (fun s -> ["--unparsing-mode"; s])
  in
  let cmd =
    ["normalize"; "data"; data; "of"; "type"; typ]
    @ Option.value ~default:[] mode_cmd
    @ if legacy then ["--legacy"] else []
  in
  spawn_command client cmd

let normalize_data ?mode ?legacy ~data ~typ client =
  spawn_normalize_data ?mode ?legacy ~data ~typ client
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
  let process = spawn_list_protocols mode client in
  let* () = Process.check process
  and* output = Lwt_io.read (Process.stdout process) in
  return (String.split_on_char '\n' output |> List.filter (fun s -> s <> ""))

let spawn_migrate_mockup ~next_protocol client =
  spawn_command
    client
    (mode_arg client @ ["migrate"; "mockup"; "to"; Protocol.hash next_protocol])

let migrate_mockup ~next_protocol client =
  spawn_migrate_mockup ~next_protocol client |> Process.check

let init ?path ?admin_path ?name ?color ?base_dir ?endpoint () =
  let client = create ?path ?admin_path ?name ?color ?base_dir ?endpoint () in
  let* () =
    Lwt_list.iter_s (import_secret_key client) Constant.all_secret_keys
  in
  return client

let init_mockup ?path ?admin_path ?name ?color ?base_dir ?sync_mode ?constants
    ~protocol () =
  (* The mockup's public documentation doesn't use `--mode mockup`
     for `create mockup` (as it is not required). We wanna do the same here.
     Hence `Client None` here: *)
  let client =
    create_with_mode ?path ?admin_path ?name ?color ?base_dir (Client None)
  in
  let* () = create_mockup ?sync_mode ?constants ~protocol client in
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
    ?(nodes_args = []) () =
  let filter_node_arg = function
    | Node.Connections _ | Synchronisation_threshold _ -> None
    | x -> Some x
  in
  let nodes_args =
    List.filter_map filter_node_arg nodes_args
    @ Node.[Connections 1; Synchronisation_threshold 0]
  in
  let* node1 = Node.init ~name:"node1" nodes_args
  and* node2 = Node.init ~name:"node2" nodes_args in
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
  let* () =
    Lwt_list.iter_s (import_secret_key client) Constant.all_secret_keys
  in
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

let init_activate_bake ?path ?admin_path ?name ?color ?base_dir
    ?(nodes_args = Node.[Connections 0; Synchronisation_threshold 0])
    ?parameter_file ?(bake = true) tag ~protocol () =
  match tag with
  | (`Client | `Proxy) as mode ->
      let* node = Node.init nodes_args in
      let endpoint = Node node in
      let mode =
        match mode with
        | `Client -> Client (Some endpoint)
        | `Proxy -> Proxy endpoint
      in
      let client =
        create_with_mode ?path ?admin_path ?name ?color ?base_dir mode
      in
      let* () =
        Lwt_list.iter_s (import_secret_key client) Constant.all_secret_keys
      in
      let* () = activate_protocol ?parameter_file ~protocol client in
      let* () = if bake then bake_for client else Lwt.return_unit in
      return (node, client)
  | `Light ->
      let* (client, node1, _) =
        init_light ?path ?admin_path ?name ?color ?base_dir ~nodes_args ()
      in
      let* () = activate_protocol ?parameter_file ~protocol client in
      let* () = if bake then bake_for client else Lwt.return_unit in
      return (node1, client)
