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

type mode =
  | Client of Node.t option
  | Mockup
  | Light of float * Node.t list
  | Proxy of Node.t

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

let base_dir_arg client = ["--base-dir"; client.base_dir]

(* To avoid repeating unduly the sources file name, we create a function here
   to get said file name as string.
   Do not call it from a client in Mockup or Client (nominal) mode. *)
let sources_file client =
  match client.mode with
  | Mockup | Client _ | Proxy _ ->
      assert false
  | Light _ ->
      client.base_dir // "sources.json"

(* [?node] can be used to override the default node stored in the client.
   Mockup nodes do not use [--endpoint] at all: RPCs are mocked up.
   Light mode needs a sources file which contains a list of endpoints.
*)
let endpoint_arg ?node client =
  match (client.mode, node) with
  | (Mockup, _) | (Client None, None) | (Light (_, []), _) ->
      []
  | (Client _, Some node)
  | (Client (Some node), None)
  | (Light (_, node :: _), None)
  | (Light _, Some node)
  | (Proxy _, Some node)
  | (Proxy node, None) ->
      ["--endpoint"; "http://localhost:" ^ string_of_int (Node.rpc_port node)]

let mode_arg client =
  match client.mode with
  | Client _ ->
      []
  | Mockup ->
      ["--mode"; "mockup"]
  | Light _ ->
      ["--mode"; "light"; "--sources"; sources_file client]
  | Proxy _ ->
      ["--mode"; "proxy"]

let spawn_command ?(env = String_map.empty) ?node ?hooks ?(admin = false)
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
  @@ endpoint_arg ?node client @ mode_arg client @ base_dir_arg client
  @ command

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

let rpc_path_query_to_string ?(query_string = []) path =
  string_of_path path ^ string_of_query_string query_string

let spawn_rpc ?node ?hooks ?env ?data ?query_string meth path client =
  let data =
    Option.fold ~none:[] ~some:(fun x -> ["with"; JSON.encode_u x]) data
  in
  let query_string =
    Option.fold ~none:"" ~some:string_of_query_string query_string
  in
  let path = string_of_path path in
  let full_path = path ^ query_string in
  spawn_command
    ?node
    ?hooks
    ?env
    client
    (["rpc"; string_of_meth meth; full_path] @ data)

let rpc ?node ?hooks ?env ?data ?query_string meth path client =
  let* output =
    spawn_rpc ?node ?hooks ?env ?data ?query_string meth path client
    |> Process.check_and_read_stdout
  in
  return (JSON.parse ~origin:(string_of_path path ^ " response") output)

let spawn_rpc_list ?node client =
  spawn_command
    ?node
    client
    (endpoint_arg ?node client @ mode_arg client @ ["rpc"; "list"])

let rpc_list ?node client =
  spawn_rpc_list ?node client |> Process.check_and_read_stdout

let spawn_shell_header ?node ?(chain = "main") ?(block = "head") client =
  let path = ["chains"; chain; "blocks"; block; "header"; "shell"] in
  spawn_rpc ?node GET path client

let shell_header ?node ?chain ?block client =
  spawn_shell_header ?node ?chain ?block client
  |> Process.check_and_read_stdout

module Admin = struct
  let spawn_command = spawn_command ~admin:true

  let spawn_trust_address ?node ~peer client =
    spawn_command
      ?node
      client
      ( endpoint_arg ?node client @ mode_arg client
      @ ["trust"; "address"; "127.0.0.1:" ^ string_of_int (Node.net_port peer)]
      )

  let trust_address ?node ~peer client =
    spawn_trust_address ?node ~peer client |> Process.check

  let spawn_connect_address ?node ~peer client =
    spawn_command
      ?node
      client
      ["connect"; "address"; "127.0.0.1:" ^ string_of_int (Node.net_port peer)]

  let connect_address ?node ~peer client =
    spawn_connect_address ?node ~peer client |> Process.check

  let spawn_kick_peer ?node ~peer client =
    spawn_command ?node client ["kick"; "peer"; peer]

  let kick_peer ?node ~peer client =
    spawn_kick_peer ?node ~peer client |> Process.check
end

let spawn_import_secret_key ?node client (key : Constant.key) =
  spawn_command ?node client ["import"; "secret"; "key"; key.alias; key.secret]

let import_secret_key ?node client key =
  spawn_import_secret_key ?node client key |> Process.check

let spawn_activate_protocol ?node ~protocol ?(fitness = 1)
    ?(key = Constant.activator.alias) ?timestamp
    ?(timestamp_delay = 3600. *. 24. *. 365.) ?parameter_file client =
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
    ?node
    client
    [ "activate";
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
      timestamp ]

let activate_protocol ?node ~protocol ?fitness ?key ?timestamp ?timestamp_delay
    ?parameter_file client =
  spawn_activate_protocol
    ?node
    ~protocol
    ?fitness
    ?key
    ?timestamp
    ?timestamp_delay
    ?parameter_file
    client
  |> Process.check

let spawn_endorse_for ?node ?(key = Constant.bootstrap2.alias) client =
  spawn_command
    client
    (endpoint_arg ?node client @ mode_arg client @ ["endorse"; "for"; key])

let endorse_for ?node ?key client =
  spawn_endorse_for ?node ?key client |> Process.check

let spawn_bake_for ?node ?protocol ?(key = Constant.bootstrap1.alias)
    ?(minimal_timestamp = true) ?mempool ?force ?context_path client =
  spawn_command
    ?node
    client
    ( Option.fold
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

let bake_for ?node ?protocol ?key ?minimal_timestamp ?mempool ?force
    ?context_path client =
  spawn_bake_for
    ?node
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
      | Some hash ->
          hash
    in
    let public_key = client_output =~* rex "Public key: ?(\\w*)" in
    let secret_key = client_output =~* rex "Secret key: ?(\\w*)" in
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

let spawn_transfer ?node ?(wait = "none") ?burn_cap ?fee ?gas_limit
    ?storage_limit ?arg ~amount ~giver ~receiver client =
  spawn_command
    ?node
    client
    ( ["--wait"; wait]
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
    @ Option.fold ~none:[] ~some:(fun p -> ["--arg"; p]) arg )

let transfer ?node ?wait ?burn_cap ?fee ?gas_limit ?storage_limit ?arg ~amount
    ~giver ~receiver client =
  spawn_transfer
    ?node
    ?wait
    ?burn_cap
    ?fee
    ?gas_limit
    ?storage_limit
    ?arg
    ~amount
    ~giver
    ~receiver
    client
  |> Process.check

let spawn_set_delegate ?node ?(wait = "none") ~src ~delegate client =
  spawn_command
    ?node
    client
    (["--wait"; wait] @ ["set"; "delegate"; "for"; src; "to"; delegate])

let set_delegate ?node ?wait ~src ~delegate client =
  spawn_set_delegate ?node ?wait ~src ~delegate client |> Process.check

let spawn_withdraw_delegate ?node ?(wait = "none") ~src client =
  spawn_command
    ?node
    client
    (["--wait"; wait] @ ["withdraw"; "delegate"; "for"; src])

let withdraw_delegate ?node ?wait ~src client =
  spawn_withdraw_delegate ?node ?wait ~src client |> Process.check

let spawn_get_balance_for ?node ~account client =
  spawn_command ?node client ["get"; "balance"; "for"; account]

let get_balance_for ?node ~account client =
  let extract_balance (client_output : string) : float =
    match client_output =~* rex "(\\d+(?:\\.\\d+)?) \u{A729}" with
    | None ->
        Test.fail "Cannot extract balance from client_output: %s" client_output
    | Some balance ->
        float_of_string balance
  in
  let process = spawn_get_balance_for ?node ~account client in
  let* () = Process.check process
  and* output = Lwt_io.read (Process.stdout process) in
  return @@ extract_balance output

let spawn_create_mockup ?(sync_mode = Synchronous) ?constants ~protocol client
    =
  let cmd =
    let common = ["--protocol"; Protocol.hash protocol; "create"; "mockup"] in
    ( match sync_mode with
    | Synchronous ->
        common
    | Asynchronous ->
        common @ ["--asynchronous"] )
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
    | Yay ->
        "yay"
    | Nay ->
        "nay"
    | Pass ->
        "pass"
  in
  spawn_command
    client
    ( ["--wait"; wait]
    @ ["submit"; "ballot"; "for"; key; proto_hash; string_of_vote vote] )

let submit_ballot ?key ?wait ~proto_hash vote client =
  spawn_submit_ballot ?key ?wait ~proto_hash vote client |> Process.check

let spawn_originate_contract ?node ?(wait = "none") ?init ?burn_cap ~alias
    ~amount ~src ~prg client =
  spawn_command
    ?node
    client
    ( ["--wait"; wait]
    @ [ "originate";
        "contract";
        alias;
        "transferring";
        Tez.to_string amount;
        "from";
        src;
        "running";
        prg ]
    @ Option.fold ~none:[] ~some:(fun init -> ["--init"; init]) init
    @ Option.fold
        ~none:[]
        ~some:(fun burn_cap -> ["--burn-cap"; Tez.to_string burn_cap])
        burn_cap )

let originate_contract ?node ?wait ?init ?burn_cap ~alias ~amount ~src ~prg
    client =
  let* client_output =
    spawn_originate_contract
      ?node
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
  | Some hash ->
      return hash

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
    | Some _ as x ->
        x
  in
  (* Filtering avoids the last line (after the trailing \n).
     We don't want to produce a warning about an empty line. *)
  let lines = String.split_on_char '\n' output |> List.filter (( <> ) "") in
  let key_value_list = List.map parse_line lines |> List.filter_map Fun.id in
  Lwt.return key_value_list

let spawn_normalize_data ?mode ?(legacy = false) ~data ~typ client =
  let mode_to_string = function
    | Readable ->
        "Readable"
    | Optimized ->
        "Optimized"
    | Optimized_legacy ->
        "Optimized_legacy"
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

let spawn_list_mockup_protocols client =
  spawn_command client (mode_arg client @ ["list"; "mockup"; "protocols"])

let list_mockup_protocols client =
  let process = spawn_list_mockup_protocols client in
  let* () = Process.check process
  and* output = Lwt_io.read (Process.stdout process) in
  return (String.split_on_char '\n' output)

let spawn_migrate_mockup ~next_protocol client =
  spawn_command
    client
    (mode_arg client @ ["migrate"; "mockup"; "to"; Protocol.hash next_protocol])

let migrate_mockup ~next_protocol client =
  spawn_migrate_mockup ~next_protocol client |> Process.check

let init ?path ?admin_path ?name ?color ?base_dir ?node () =
  let client = create ?path ?admin_path ?name ?color ?base_dir ?node () in
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
  set_mode Mockup client ; return client

let init_light ?path ?admin_path ?name ?color ?base_dir ?nodes
    ?(min_agreement = 0.66) () =
  let* nodes =
    match nodes with
    | None ->
        let* node1 =
          Node.init ~name:"node1" [Connections 1; Synchronisation_threshold 0]
        and* node2 = Node.init ~name:"node2" [Connections 1] in
        return [node1; node2]
    | Some [] | Some [_] ->
        Test.fail
          "When initializing a light client and specifying the nodes, there \
           should be 2 or more nodes but found %d nodes"
          (Option.value nodes ~default:[] |> List.length)
    | Some nodes ->
        return nodes
  in
  let client =
    create_with_mode
      ?path
      ?admin_path
      ?name
      ?color
      ?base_dir
      (Light (min_agreement, nodes))
  in
  (* Create a services.json file in the base directory with correctly
    JSONified data *)
  let* () =
    Lwt_io.with_file ~mode:Lwt_io.Output (sources_file client) (fun oc ->
        let obj =
          `O
            [ ("min_agreement", `Float min_agreement);
              ( "uris",
                `A
                  (List.map
                     (fun node ->
                       `String
                         (Printf.sprintf
                            "http://localhost:%d"
                            (Node.rpc_port node)))
                     nodes) ) ]
        in
        Lwt_io.fprintf oc "%s" @@ Ezjsonm.value_to_string obj)
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
  return (client, nodes)
