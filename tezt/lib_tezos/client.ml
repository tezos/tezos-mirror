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

type mode = Client of Node.t option | Mockup | Proxy of Node.t

type mockup_sync_mode = Asynchronous | Synchronous

type t = {
  path : string;
  admin_path : string;
  name : string;
  color : Log.Color.t;
  base_dir : string;
  mutable mode : mode;
}

let base_dir t = t.base_dir

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

(* [?node] can be used to override the default node stored in the client.
   Mockup nodes do not use [--endpoint] at all: RPCs are mocked up. *)
let endpoint_arg ?node client =
  match (client.mode, node) with
  | (Mockup, _) | (Client None, None) ->
      []
  | (Client _, Some node)
  | (Client (Some node), None)
  | (Proxy _, Some node)
  | (Proxy node, None) ->
      ["--endpoint"; "http://localhost:" ^ string_of_int (Node.rpc_port node)]

let mode_arg client =
  match client.mode with
  | Client _ ->
      []
  | Mockup ->
      ["--mode"; "mockup"]
  | Proxy _ ->
      ["--mode"; "proxy"]

let spawn_command ?node ?(admin = false) client command =
  Process.spawn
    ~name:client.name
    ~color:client.color
    ~env:[("TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER", "Y")]
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

let spawn_rpc ?node ?data ?query_string meth path client =
  let data =
    Option.fold ~none:[] ~some:(fun x -> ["with"; JSON.encode_u x]) data
  in
  let query_string =
    Option.fold ~none:"" ~some:string_of_query_string query_string
  in
  let path = string_of_path path in
  let full_path = path ^ query_string in
  spawn_command ?node client (["rpc"; string_of_meth meth; full_path] @ data)

let rpc ?node ?data ?query_string meth path client =
  let* output =
    spawn_rpc ?node ?data ?query_string meth path client
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
      Protocol.parameter_file protocol;
      "--timestamp";
      timestamp ]

let activate_protocol ?node ~protocol ?fitness ?key ?timestamp ?timestamp_delay
    client =
  spawn_activate_protocol
    ?node
    ~protocol
    ?fitness
    ?key
    ?timestamp
    ?timestamp_delay
    client
  |> Process.check

let spawn_bake_for ?node ?(key = Constant.bootstrap1.alias)
    ?(minimal_timestamp = true) client =
  spawn_command
    ?node
    client
    ( "bake" :: "for" :: key
    :: (if minimal_timestamp then ["--minimal-timestamp"] else []) )

let bake_for ?node ?key ?minimal_timestamp client =
  spawn_bake_for ?node ?key ?minimal_timestamp client |> Process.check

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

let spawn_transfer ?node ?(wait = "none") ?(args = []) ~amount ~giver ~receiver
    client =
  spawn_command
    ?node
    client
    ( ["--wait"; wait]
    @ ["transfer"; string_of_int amount; "from"; giver; "to"; receiver]
    @ args )

let transfer ?node ?wait ?args ~amount ~giver ~receiver client =
  spawn_transfer ?node ?wait ?args ~amount ~giver ~receiver client
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

let spawn_create_mockup ?(sync_mode = Synchronous) ~protocol client =
  let cmd =
    let common = ["--protocol"; Protocol.hash protocol; "create"; "mockup"] in
    match sync_mode with
    | Synchronous ->
        common
    | Asynchronous ->
        common @ ["--asynchronous"]
  in
  spawn_command client cmd

let create_mockup ?sync_mode ~protocol client =
  spawn_create_mockup ?sync_mode ~protocol client |> Process.check

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
        Int.to_string amount;
        "from";
        src;
        "running";
        prg ]
    @ Option.fold ~none:[] ~some:(fun init -> ["--init"; init]) init
    @ Option.fold
        ~none:[]
        ~some:(fun burn_cap -> ["--burn-cap"; string_of_int burn_cap])
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

let init ?path ?admin_path ?name ?color ?base_dir ?node () =
  let client = create ?path ?admin_path ?name ?color ?base_dir ?node () in
  let* () =
    Lwt_list.iter_s (import_secret_key client) Constant.all_secret_keys
  in
  return client

let init_mockup ?path ?admin_path ?name ?color ?base_dir ?sync_mode ~protocol
    () =
  (* The mockup's public documentation doesn't use `--mode mockup`
     for `create mockup` (as it is not required). We wanna do the same here.
     Hence `Client None` here: *)
  let client =
    create_with_mode ?path ?admin_path ?name ?color ?base_dir (Client None)
  in
  let* () = create_mockup ?sync_mode ~protocol client in
  (* We want, however, to return a mockup client; hence the following: *)
  set_mode Mockup client ; return client
