(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(* Testing
   -------
   Component:    [stresstest] client command
   Invocation:   dune exec tezt/manual_tests/main.exe -- --file stresstest_command.ml
   Subject:      Test the [stresstest] client command. Even though this
                 command is only used for testing purposes, its development
                 is both tricky and ongoing. Moreover, it can be complex to
                 debug when called in various contexts. That is why these
                 tests are nice to have. *)

type config = {
  faucet_key : Account.key;
  (* Generate accounts *)
  nb_accounts : int;
  (* Distribute tokens *)
  batch_size : int;
  batch_per_block : int;
  amount : Int64.t; (*micro tz*)
}

let test_stresstest_fund_accounts =
  Protocol.register_test
    ~__FILE__
    ~title:"stresstest fund multiple accounts"
    ~tags:["stresstest"; "fund"; "accounts"]
  @@ fun protocol ->
  let config =
    {
      faucet_key = Constant.bootstrap1;
      nb_accounts = 100;
      batch_size = 10;
      batch_per_block = 2;
      amount = 42_000_000L;
    }
  in
  let* baking_node, baking_client =
    Client.init_with_protocol
      ~nodes_args:[Synchronisation_threshold 0; Connections 1]
      `Client
      ~protocol
      ()
  in
  let* node, client =
    Client.init_with_node
      ~nodes_args:[Synchronisation_threshold 0; Connections 1]
      `Client
      ()
  in
  let* () = Client.Admin.connect_address ~peer:baking_node client in
  let* _ = Node.wait_for_level node 1 in
  let* accounts =
    Client.stresstest_gen_keys ~alias_prefix:"id_" config.nb_accounts client
  in
  let p, r = Lwt.wait () in
  (*
     Blocks to bake to ensure that all accounts were funded:
     init : 1
     reveal : batch_size / batch_per_blocks
     batches : ((nb_accounts - batch_size) / batch_size / batch_per_block )
     final bake
  *)
  let min_blocks_to_bake =
    (*fund starters*)
    1
    (*reveals*) + (config.batch_size / config.batch_per_block)
    + (config.batch_size mod config.batch_per_block)
    +
    (* fund batches*)
    +((config.nb_accounts - config.batch_size)
     / config.batch_size / config.batch_per_block)
    + (config.nb_accounts - config.batch_size)
      / config.batch_size mod config.batch_per_block
  in
  let () =
    Background.register
      (let rec bake_n n =
         let* () = Client.bake_for baking_client in
         Log.info
           "Block %d/%d baked@."
           (min_blocks_to_bake - n)
           min_blocks_to_bake ;
         let* () = Lwt_unix.sleep 1. in
         if n = 0 then unit else bake_n (pred n)
       in
       let* () = bake_n min_blocks_to_bake in
       Lwt.wakeup r () ;
       unit)
  in
  let initial_amount = Tez.of_mutez_int64 config.amount in
  let* () =
    Client.stresstest_fund_accounts_from_source
      ~source_key_pkh:Constant.bootstrap1.public_key_hash
      ~batch_size:config.batch_size
      ~batches_per_block:config.batch_per_block
      ~initial_amount
      client
  in
  let* () = p in
  (* Make sure that accounts are well funded, that is to say, contains
     at least the initial_amount.*)
  Lwt_list.iter_s
    (fun a ->
      let account = a.Account.public_key_hash in
      let* balance = Client.get_balance_for ~account client in
      let error_msg =
        "Account was funded with %L, but at least %R was expected."
      in
      Check.(
        Tez.(mutez_int64 balance >= mutez_int64 initial_amount) int64 ~error_msg) ;
      unit)
    accounts

(* The following part concerns manual testing of the stresstest command on a live
   network.

   Invocation: TEZT_STRESSTEST_CONFIG_FILE=<path_to_config_json_file> dune exec
   tezt/manual_tests/main.exe -- --file stresstest_command.ml stresstest live
   network --verbose

   Subject: This test allows to simply run the stresstest command on a live test
   network from an already existing node data-dir using a fundraiser account
   already existing in a client base-dir and funded with enough balance to
   sustain the test costs. *)

(** Json config file for the test should be of the form:
    {
      "data_dir": "<path_to_node_data_dir>",
      "network": "<network_name>",
      "base_dir": "<path_to_client_base_dir>",
      "fundraiser_account": "<alias_of_the_fundraiser_account>",
      "tps" : <optional_tps>,
      "level_limit" : <max_number_of_block>,
      "transfers" : <max_number_of_transfers>,
      "initial_amount" : <initial_amount>,
      "nb_accounts" : <number_of_accounts_to_create_and_use>
   }
*)
type live_network_config = {
  data_dir : string;
  network : string;
  base_dir : string;
  fundraiser_account : string;
  tps : int option;
  level_limit : int option;
  transfers : int option;
  initial_amount : Int64.t option;
  nb_accounts : int option;
}

let parse_config_file () =
  let config_file = Sys.getenv_opt "TEZT_STRESSTEST_CONFIG_FILE" in
  match config_file with
  | None ->
      Test.fail
        "No config file found in environment variable \
         TEZT_STRESSTEST_CONFIG_FILE"
  | Some config_file ->
      let config_file = JSON.parse_file config_file in
      let data_dir = JSON.(get "data_dir" config_file |> as_string) in
      let network = JSON.(get "network" config_file |> as_string) in
      let base_dir = JSON.(get "base_dir" config_file |> as_string) in
      let fundraiser_account =
        JSON.(get "fundraiser_account" config_file |> as_string)
      in
      let tps = JSON.(get "tps" config_file |> as_int_opt) in
      let level_limit = JSON.(get "level_limit" config_file |> as_int_opt) in
      let transfers = JSON.(get "transfers" config_file |> as_int_opt) in
      let initial_amount =
        JSON.(get "initial_amount" config_file |> as_int64_opt)
      in
      let nb_accounts = JSON.(get "nb_accounts" config_file |> as_int_opt) in
      Log.info
        "%s"
        Format.(
          asprintf
            "@[<v 2>Test will run with the following parameters:@,\
             tps: %a@,\
             level_limit: %a@,\
             transfers: %a@,\
             nb_accounts: %a @]"
            (pp_print_option pp_print_int)
            tps
            (pp_print_option pp_print_int)
            level_limit
            (pp_print_option pp_print_int)
            transfers
            (pp_print_option pp_print_int)
            nb_accounts) ;
      {
        data_dir;
        network;
        base_dir;
        fundraiser_account;
        tps;
        level_limit;
        transfers;
        initial_amount;
        nb_accounts;
      }

let fund_stresstest_accounts config ~source client =
  Client.stresstest_fund_accounts_from_source
    ~batch_size:150
    ~batches_per_block:50
    ~source_key_pkh:source
    ~burn_cap:1
    ?initial_amount:(Option.map Tez.of_mutez_int64 config.initial_amount)
    client

let spawn_stresstest_command config ~accounts client =
  let filename =
    let sources =
      `A
        (List.map
           (fun ({public_key_hash = pkh; _} : Account.key) ->
             `O [("pkh", `String pkh)])
           accounts)
    in
    let base = sf "stresstest-sources.json" in
    let source = Temp.file ?runner:None base in
    let () = JSON.encode_to_file_u source sources in
    source
  in
  let p =
    Client.spawn_stresstest_with_filename
      ~strategy:1 (* 1 mutez exchange between accounts *)
      ?tps:config.tps
      ?level_limit:config.level_limit
      ?transfers:config.transfers
      client
      filename
  in
  Process.check p

let payback ~accounts fundraiser client =
  Lwt_list.iter_s
    (fun ({alias; _} : Account.key) ->
      let* balance = Client.get_balance_for ~account:alias client in
      if Tez.to_mutez balance < 5_000 then unit
        (* Only payback if the account has 5k mutez in balance *)
      else
        let amount =
          Tez.(balance - of_mutez_int 3_000)
          (* substract fees *)
        in
        Client.transfer
          ~amount
          ~giver:alias
          ~receiver:fundraiser.Account.alias
          client)
    accounts

let test_stresstest_live_network () =
  Test.register
    ~__FILE__
    ~title:"stresstest live network"
    ~tags:["stresstest"; "live"; "network"]
  @@ fun () ->
  let config = parse_config_file () in
  let node =
    Node.create
      ~data_dir:config.data_dir
      [Network config.network; Expected_pow 26]
  in
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in

  Log.info "Waiting for the node to be bootstrapped, this can take a while" ;
  let* () = Node.wait_for node "bootstrapped.v0" (fun _json -> Some ()) in

  let client =
    Client.create ~base_dir:config.base_dir ~endpoint:(Node node) ()
  in
  let* account = Client.show_address ~alias:config.fundraiser_account client in
  let* () =
    Client.forget_all_keys client
    (* Just in case, we clear all other account than the fundraiser one *)
  in
  let* () =
    Client.import_secret_key client account.secret_key ~alias:account.alias
  in

  let nb_accounts = Option.value ~default:500 config.nb_accounts in
  Log.info "Generating %d accounts" nb_accounts ;
  let* accounts = Client.stresstest_gen_keys nb_accounts client in

  Log.info "Funding accounts" ;
  let* () =
    fund_stresstest_accounts config ~source:account.public_key_hash client
  in

  Log.info "Stresstesting" ;
  let* () = spawn_stresstest_command config ~accounts client in

  Log.info "Stresstest terminated, payback remaining tz to fundraiser account" ;
  let* lvl = Node.get_level node in
  let* _ = Node.wait_for_level node (lvl + 5) in
  let* () = payback ~accounts account client in
  unit

let register ~protocols =
  test_stresstest_fund_accounts protocols ;
  test_stresstest_live_network ()
