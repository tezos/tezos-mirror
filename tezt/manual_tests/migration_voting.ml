(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Example of the relevant fields from metadata:
   "level_info": {
     "level": 1404929,
     "level_position": 1404928,
     "cycle": 343,
     "cycle_position": 0,
     "expected_commitment": false },
   "voting_period_info": {
     "voting_period": {
       "index": 44,
       "kind": "cooldown",
       "start_position": 1404927 },
     "position": 1,
     "remaining": 40958 }
*)

let check_voting_period_invariants ~blocks_per_voting_period ~blocks_per_cycle
    ~level ~cycle_position voting_period_info =
  let voting_period = JSON.(voting_period_info |-> "voting_period") in
  let start_position = JSON.(voting_period |-> "start_position" |> as_int) in
  let position_in_period = JSON.(voting_period_info |-> "position" |> as_int) in
  let remaining = JSON.(voting_period_info |-> "remaining" |> as_int) in
  let* () =
    if blocks_per_voting_period <> 1 + position_in_period + remaining then
      Test.fail
        "Voting period invariant failed for position %d and remaining %d \
         (blocks_per_voting_period %d)"
        position_in_period
        remaining
        blocks_per_voting_period
    else unit
  in
  let* () =
    if level <> 1 + start_position + position_in_period then
      Test.fail
        "Voting period invariant failed for start_position %d and position %d"
        start_position
        position_in_period
    else unit
  in
  let* () =
    if
      cycle_position mod blocks_per_cycle
      <> position_in_period mod blocks_per_cycle
    then
      Test.fail
        "Invariant failed for position %d and cycle_position %d"
        position_in_period
        cycle_position
    else unit
  in
  unit

let check_level_invariants ~blocks_per_commitment level_info =
  let level = JSON.(level_info |-> "level" |> as_int) in
  let level_position = JSON.(level_info |-> "level_position" |> as_int) in
  let cycle_position = JSON.(level_info |-> "cycle_position" |> as_int) in
  let expected_commitment =
    JSON.(level_info |-> "expected_commitment" |> as_bool)
  in
  let* () =
    if level <> level_position + 1 then
      Test.fail
        "Level info invariant failed for level %d and level_position %d"
        level
        level_position
    else unit
  in
  let* () =
    if
      expected_commitment
      <> (1 + (cycle_position mod blocks_per_commitment)
         == blocks_per_commitment)
    then
      Test.fail
        "Level info invariant failed for expected_commitment %b \
         (cycle_position %d, blocks_per commitment %d)"
        expected_commitment
        cycle_position
        blocks_per_commitment
    else unit
  in
  unit

let check_rpcs client ~blocks_per_voting_period ~blocks_per_cycle
    ~blocks_per_commitment ~expected_period_kind ~expected_period_index block =
  let prefix = ["chains"; "main"; "blocks"; block] in
  let* metadata = Client.rpc GET (prefix @ ["metadata"]) client in
  let metadata_level_info = JSON.(metadata |-> "level_info") in
  let level = JSON.(metadata_level_info |-> "level" |> as_int) in
  let cycle_position =
    JSON.(metadata_level_info |-> "cycle_position" |> as_int)
  in
  let metadata_voting_period_info = JSON.(metadata |-> "voting_period_info") in
  let* () = check_level_invariants ~blocks_per_commitment metadata_level_info in
  let* () =
    check_voting_period_invariants
      ~blocks_per_cycle
      ~blocks_per_voting_period
      ~level
      ~cycle_position
      metadata_voting_period_info
  in
  let* current_period_info =
    Client.rpc GET (prefix @ ["votes"; "current_period"]) client
  in
  let* () =
    if
      not
        (String.equal
           (JSON.encode metadata_voting_period_info)
           (JSON.encode current_period_info))
    then
      Test.fail
        "Different values for voting_period with the RPCs 'metadata' and \
         'votes/current_period'"
    else unit
  in
  let voting_period = JSON.(current_period_info |-> "voting_period") in
  let voting_period_kind = JSON.(voting_period |-> "kind" |> encode) in
  let* () =
    if not (String.equal voting_period_kind expected_period_kind) then
      Test.fail
        "Unexpected voting period kind %s, expected %s"
        voting_period_kind
        expected_period_kind
    else unit
  in
  let voting_period_index = JSON.(voting_period |-> "index" |> as_int) in
  if voting_period_index <> expected_period_index then
    Test.fail
      "Unexpected voting period index %d, expected %d"
      voting_period_index
      expected_period_index
  else unit

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

let prepare_migration ?yes_node_path ?yes_wallet context protocol
    levels_till_migration =
  Log.info "Copying the context into a temporary directory" ;
  let data_dir = Temp.dir "tezos-node-test" in
  let* () = Process.run "cp" ["-R"; context ^ "/."; data_dir] in
  let* () = Process.run "rm" [data_dir ^ "/config.json"] in
  let* node =
    Node.init ~rpc_port:19731 ~net_port:18731 ~data_dir [Connections 0]
  in
  let endpoint = Client.(Node node) in
  let* client = Client.init ~endpoint () in
  let* json = RPC.get_current_level ~endpoint client in
  let level = JSON.(json |-> "level" |> as_int) in
  Log.info "The node is at level %d" level ;
  let* () = Node.terminate node in
  let migration_level = level + levels_till_migration in
  Log.info
    "Updating node config with user_activated_upgrade at level %d"
    migration_level ;
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
  Lwt.return (node, client, level)

(* This test is a variant of the test in `migration.ml`.  It is
   specifically designed to test the MR!2531
   (https://gitlab.com/tezos/tezos/-/merge_requests/2531). *)
let migration ?yes_node_path ?yes_wallet context protocol levels_till_migration
    expected_period_kind =
  Test.register
    ~__FILE__
    ~title:"migration voting test"
    ~tags:
      ["node"; "activate"; "user_activated"; "protocol"; "migration"; "voting"]
  @@ fun () ->
  let* (node, client, level) =
    prepare_migration
      ?yes_node_path
      ?yes_wallet
      context
      protocol
      levels_till_migration
  in
  let parameters_alpha =
    JSON.parse_file
    @@ sf
         "src/proto_010_%s/parameters/mainnet-parameters.json"
         (String.sub protocol 0 8)
  in
  let blocks_per_voting_period_alpha =
    JSON.(parameters_alpha |-> "blocks_per_voting_period" |> as_int)
  in
  let blocks_per_cycle_alpha =
    JSON.(parameters_alpha |-> "blocks_per_cycle" |> as_int)
  in
  let blocks_per_commitment_alpha =
    JSON.(parameters_alpha |-> "blocks_per_commitment" |> as_int)
  in
  let parameters_hangzhou =
    JSON.parse_file
    @@ Protocol.parameter_file ~constants:Constants_mainnet Protocol.Hangzhou
  in
  let blocks_per_cycle_hangzhou =
    JSON.(parameters_hangzhou |-> "blocks_per_cycle" |> as_int)
  in
  let blocks_per_voting_period_hangzhou =
    JSON.(parameters_hangzhou |-> "blocks_per_voting_period" |> as_int)
  in
  let* voting_period_info =
    Client.rpc
      GET
      ["chains"; "main"; "blocks"; "head"; "votes"; "current_period"]
      client
  in
  let voting_period = JSON.(voting_period_info |-> "voting_period") in
  let period_index = JSON.(voting_period |-> "index" |> as_int) in
  let blocks_to_bake =
    levels_till_migration + blocks_per_voting_period_alpha + 2
  in
  Log.info
    "Bake %d blocks, until the next voting period of the new protocol..."
    blocks_to_bake ;
  let* () =
    let rec iter i =
      if i <= blocks_to_bake then (
        let* () = bake_with_foundation client in
        let edo = i <= levels_till_migration in
        let edo_cycle = i <= levels_till_migration + 1 in
        (* We do not check the RPCs for the first two blocks of the
           protocol, because we know the RPCs are buggy then; nor for
           the first block of the next voting period in Alpha, because
           we don't know its kind. *)
        let check_rpcs_flag =
          i > levels_till_migration + 2 && i < blocks_to_bake
        in
        Log.info
          "Baked %d-th block (level %d, protocol %s, cycle_position %d, \
           voting_position %d) %s"
          i
          (level + i)
          (if edo then "Edo" else "Alpha")
          (if edo_cycle then
           blocks_per_cycle_hangzhou - levels_till_migration + i - 2
          else (i - 2 - levels_till_migration) mod blocks_per_cycle_alpha)
          (if edo then
           blocks_per_voting_period_hangzhou - levels_till_migration + i - 1
          else
            (i - 2 - levels_till_migration) mod blocks_per_voting_period_alpha)
          (if check_rpcs_flag then "*" else "") ;
        let expected_period_index =
          if i <= levels_till_migration then period_index else period_index + 1
        in
        if i == levels_till_migration then
          Log.info
            "The baked block is the last of a voting period, and is the \
             migration block" ;
        let* () =
          if check_rpcs_flag then
            check_rpcs
              client
              ~blocks_per_voting_period:blocks_per_voting_period_alpha
              ~blocks_per_cycle:blocks_per_cycle_alpha
              ~blocks_per_commitment:blocks_per_commitment_alpha
              ~expected_period_kind
              ~expected_period_index
              "head"
          else unit
        in
        let* _ = Node.wait_for_level node (level + i) in
        iter (i + 1))
      else unit
    in
    iter 1
  in
  unit

(* The following four variables need to be updated according to the user's setup. *)

(* path to an existing mainnet context, which will be copied *)
let context = "/tmp/tezos-node-2021-03-31.roll"

(* the protocol to be activated *)
let protocol = "PrxmwK1B8zS4Hh2z4XFcn29eAwuypLtibiWFoQLaKf2FR1rdoK5"

(* the period kind of the first period of the new protocol; in reality
   it should be "proposal", but since we user "user activated
   upgrades" we can activate the protocol in any period *)
let expected_period_kind = "\"cooldown\""

(* The migration block is the one that activates the next
   protocol. For this test, we simulate with a user activated upgrade
   the activation of a protocol at the end of the current voting
   period. So 'levels_till_migration' represents the number of levels
   till the end of the period. E.g. this number is 2 if the imported
   context is for level 6 and the end of the period is at level 8. *)
let levels_till_migration = 2

(* It is recommended that the test is run with the option '--log-level
   info'. *)
let register () =
  migration context protocol levels_till_migration expected_period_kind
