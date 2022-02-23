(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

let update_config_with_user_activated config_file level protocol =
  let user_activated =
    Ezjsonm.(
      dict
        [
          ( "genesis",
            dict
              [
                ("timestamp", string "2018-06-30T16:07:32Z");
                ( "block",
                  string "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2"
                );
                ( "protocol",
                  string "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P"
                );
              ] );
          ("chain_name", string "TEZOS_MAINNET");
          ("old_chain_name", string "TEZOS_BETANET_2018-06-30T16:07:32Z");
          ("incompatible_chain_name", string "INCOMPATIBLE");
          ("sandboxed_chain_name", string "SANDBOXED_TEZOS_MAINNET");
          ( "user_activated_upgrades",
            list
              dict
              [
                [("level", int level); ("replacement_protocol", string protocol)];
              ] );
        ])
  in
  let config_json = JSON.parse_file config_file in
  let config_json =
    Ezjsonm.update
      (JSON.unannotate config_json)
      ["network"]
      (Some user_activated)
  in
  with_open_out config_file (fun chan ->
      Ezjsonm.value_to_channel ~minify:false chan config_json)

let rec bake_with_foundation ?(foundation_index = [1; 2; 3; 4; 5; 6; 7; 8])
    client =
  let len = List.length foundation_index in
  if len = 0 then Test.fail "No foundation contract could bake"
  else
    let index = Random.int len in
    let foundation = List.nth foundation_index index in
    let proc =
      Client.spawn_bake_for
        ~keys:["foundation" ^ string_of_int foundation]
        client
    in
    let* res = Process.wait proc in
    if res != Unix.WEXITED 0 then
      let* has_correct_error =
        Lwt_stream.find
          (fun stderr_bake ->
            stderr_bake =~ rex "No\\sslot\\sfound\\sat\\slevel\\s[0-9]+")
          (Lwt_io.read_lines (Process.stderr proc))
      in
      if Option.is_some has_correct_error then
        let foundation_index =
          List.filter (( <> ) foundation) foundation_index
        in
        bake_with_foundation ~foundation_index client
      else unit
    else unit

let create_yes_wallet () =
  let yes_wallet = Temp.dir "yes-wallet" in
  let open Yes_wallet_lib in
  json_to_file
    (pkh_list_json alias_pkh_pk_list)
    (yes_wallet // "public_key_hashs") ;
  json_to_file (pk_list_json alias_pkh_pk_list) (yes_wallet // "public_keys") ;
  json_to_file (sk_list_json alias_pkh_pk_list) (yes_wallet // "secret_keys") ;
  yes_wallet

(* This test checks the migration of a protocol with a context imported from a
   snapshot. See the tezos online documentation to have an in-depth explanation
   of that test and how to use it. The documentation can be found at this
   address: https://tezos.gitlab.io/developer/proposal_testing.html *)
let migration ?yes_node_path ?yes_wallet context protocol =
  Test.register
    ~__FILE__
    ~title:"migration test"
    ~tags:["node"; "activate"; "user_activated"; "protocol"; "migration"]
  @@ fun () ->
  Log.info
    "Starting migration test of protocol %s in context %s"
    protocol
    context ;
  Log.info "Copying context into a temporary directory" ;
  let data_dir = Temp.dir "tezos-node-test" in
  let* () = Process.run "cp" ["-R"; context ^ "/."; data_dir] in
  let* node =
    Node.init ~rpc_port:19731 ~net_port:18731 ~data_dir [Connections 0]
  in
  let endpoint = Client.(Node node) in
  let* client = Client.init ~endpoint () in
  let* json = RPC.get_current_level ~endpoint client in
  let level = JSON.(json |-> "level" |> as_int) in
  let* () = Node.terminate node in
  Log.info "Updating node config with user_activated_upgrade" ;
  let migration_level = level + 1 in
  update_config_with_user_activated
    (data_dir ^ "/config.json")
    migration_level
    protocol ;
  let node =
    Node.create ?path:yes_node_path ~rpc_port:19731 ~net_port:18731 ~data_dir []
  in
  let endpoint = Client.(Node node) in
  let* () = Node.run node [Connections 0] in
  let* () = Node.wait_for_ready node in
  Log.info "Creating yes-wallet dir" ;
  let* base_dir =
    match yes_wallet with
    | Some yes_wallet ->
        let base_dir = Temp.dir "client" in
        let* () = Process.run "cp" ["-R"; yes_wallet ^ "/."; base_dir] in
        Lwt.return base_dir
    | None -> Lwt.return @@ create_yes_wallet ()
  in
  let client = Client.create ~base_dir ~endpoint () in
  Log.info "Bake and wait until migration is finished" ;
  let* () = bake_with_foundation client in
  let* _until_mig = Node.wait_for_level node migration_level in
  let* levels_in_current_cycle = RPC.get_levels_in_current_cycle client in
  let last_block_of_cycle =
    JSON.(levels_in_current_cycle |-> "last" |> as_int)
  in
  let* prev_level = RPC.get_current_level client in
  let prev_cycle = JSON.(prev_level |-> "cycle" |> as_int) in
  Log.info "Bake until new cycle" ;
  let* () =
    repeat
      (last_block_of_cycle - migration_level + 1)
      (fun () -> bake_with_foundation client)
  in
  let* _until_end_of_cycle = Node.wait_for_level node last_block_of_cycle in
  let* after_level = RPC.get_current_level client in
  let after_cycle = JSON.(after_level |-> "cycle" |> as_int) in
  if prev_cycle + 1 <> after_cycle then
    Test.fail
      "The cycle of current level is %d where it was expected to be %d "
      prev_cycle
      after_cycle
  else unit

let protocol =
  let default = Protocol.(hash Alpha) in
  Option.value ~default (Sys.getenv_opt "TEZT_MIGRATION_TEST_PROTOCOL")

let context =
  let default =
    let home = Sys.getenv "HOME" in
    home ^ "/tezos-node-test"
  in
  Option.value ~default (Sys.getenv_opt "TEZT_MIGRATION_TEST_CONTEXT")

let register () = migration context protocol
