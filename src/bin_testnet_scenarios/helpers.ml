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

(** [download ?runner url filename] downloads the file at [url],
    stores it in a temporary file named [filename], and returns the
    complete path to the downloaded file. *)
let download ?runner url filename =
  Log.info "Download %s" url ;
  let path = Tezt.Temp.file filename in
  let*! _ = Curl.get_raw ?runner ~args:["--output"; path] url in
  Log.info "%s downloaded" url ;
  Lwt.return path

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
     value used in networks like mainnet, Mondaynet etc. is 26 (see
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
        let* snapshot = download ?runner snapshot "snapshot" in
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
