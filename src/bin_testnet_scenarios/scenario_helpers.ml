(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let fetch ?runner src filename =
  let path = Tezt.Temp.file filename in
  if Sys.file_exists src then (
    Log.info "Using %s" src ;
    Unix.symlink src path ;
    Lwt.return path)
  else (
    Log.info "Download %s" src ;
    let*! _ = Curl.get_raw ?runner ~args:["--output"; path] src in
    Log.info "%s downloaded" src ;
    Lwt.return path)

let rec wait_for_funded_key node client expected_amount key =
  let* balance = Client.get_balance_for ~account:key.Account.alias client in
  if balance < expected_amount then (
    Log.info
      "Key %s is underfunded (got %d, expected at least %d)"
      key.public_key_hash
      Tez.(to_mutez balance)
      Tez.(to_mutez expected_amount) ;
    let* current_level = Node.get_level node in
    let* _ = Node.wait_for_level node (current_level + 1) in
    wait_for_funded_key node client expected_amount key)
  else unit

let setup_octez_node ~(testnet : Testnet.t) ?runner ?metrics_port () =
  let l1_node_args =
    Node.[Expected_pow 26; Synchronisation_threshold 1; Network testnet.network]
  in
  (* By default, Tezt sets the difficulty to generate the identity
     file of the Octez node to 0 (`--expected-pow 0`). The default
     value used in networks like mainnet, Weeklynet etc. is 26 (see
     `lib_node_config/config_file.ml`). *)
  let node =
    Node.create ?runner ?data_dir:testnet.data_dir ?metrics_port l1_node_args
  in
  let* () =
    (* init config or update existing one *)
    let* cmd =
      match testnet.data_dir with
      | Some data_dir ->
          (* Runs a node using the existing data-dir. *)
          let* file_exists = Lwt_unix.file_exists (data_dir // "config.json") in
          return
          @@
          if file_exists then
            (* update the config to ensure it's using [l1_node_args],
               is a noop when config was generated in a previous
               run. *)
            Node.config_update
          else Node.config_init
      | None -> return Node.config_init
    in
    cmd node []
  in
  let* () =
    match testnet.snapshot with
    | Some snapshot ->
        Log.info "Import snapshot" ;
        let* snapshot = fetch ?runner snapshot "snapshot" in
        let* () = Node.snapshot_import node snapshot in
        Log.info "Snapshot imported" ;
        unit
    | None -> unit
  in
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let client =
    Client.create ?base_dir:testnet.client_dir ~endpoint:(Node node) ()
  in
  Log.info "Wait for node to be bootstrapped" ;
  let* () = Client.bootstrapped client in
  Log.info "Node bootstrapped" ;
  return (client, node)

type network = Ghostnet | Weeklynet | Other

let string_contains haystack needle =
  let len_h = String.length haystack in
  let len_n = String.length needle in
  if len_n = 0 then true
  else
    let rec aux i =
      if i > len_h - len_n then false
      else if String.sub haystack i len_n = needle then true
      else aux (i + 1)
    in
    aux 0

let string_to_network network =
  if string_contains network "weeklynet" || string_contains network "Weeklynet"
  then Weeklynet
  else if
    string_contains network "ghostnet" || string_contains network "Ghostnet"
  then Ghostnet
  else Other

let faucet ?(amount = 11_000) ~network_string address =
  let network = string_to_network network_string in
  let amount = string_of_int amount in
  let npx_fun network =
    Printf.printf "Faucet %s found" network ;
    Process.run
      "npx"
      ["@tacoinfra/get-tez"; address; "--amount"; amount; "--network"; network]
  in
  let* () =
    match network with
    | Weeklynet -> npx_fun "weeklynet"
    | Ghostnet -> npx_fun "ghostnet"
    | Other ->
        Printf.printf "No faucet found" ;
        unit
  in
  unit
