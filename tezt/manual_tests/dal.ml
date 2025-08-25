(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    DAL
   Invocation:   See docstring of the individual tests for instructions on how to execute them.
   Subject:      Test getting informaton about the DAL distribution.
*)

module Dal = Dal_common

module Dal_RPC = struct
  include Dal.RPC

  (* We override call_xx RPCs in Dal.RPC to use a DAL node in this file. *)
  include Dal.RPC.Local
end

(* [is_fake] is true iff the mocked SRS is used. *)
let dal_parameters ~is_fake =
  let mode = if is_fake then "mock" else "real" in
  Test.register
    ~__FILE__
    ~title:(sf "Check the validity of DAL parameters with %s SRS" mode)
    ~tags:[Tag.tezos2; "dal"; "parameters"; mode]
  @@ fun () ->
  let open Dal.Cryptobox in
  let number_of_shards = Cli.get_int "number_of_shards" in
  let slot_size = Cli.get_int "slot_size" in
  let redundancy_factor = Cli.get_int "redundancy_factor" in
  let page_size = Cli.get_int "page_size" in
  let parameters =
    {number_of_shards; redundancy_factor; page_size; slot_size}
  in
  let find_srs_files = Tezos_base.Dal_srs.find_trusted_setup_files in
  let check_make error_msg =
    match make parameters with
    | Ok _ -> unit
    | Error (`Fail s) -> Test.fail "%s. Reason:@.%s@." error_msg s
  in
  let* () = check_make "The set of parameters is invalid for the verifier" in
  let* () =
    let* res = init_prover_dal ~find_srs_files ~fetch_trusted_setup:false () in
    match res with
    | Ok () -> unit
    | Error errs ->
        Test.fail
          "Could not init prover. Reason:@.%a@."
          (Tezos_error_monad.Error_monad.TzTrace.pp_print_top
             Tezos_error_monad.Error_monad.pp)
          errs
  in
  let* () = check_make "The set of parameters is invalid for the prover" in
  unit

(** Start a layer 1 node on the given network, with the given data-dir and
    rpc-port if any. *)
let start_layer_1_node ~network ?data_dir ?net_addr ?rpc_addr ?metrics_addr
    ?net_port ?rpc_port ?metrics_port () =
  let arguments =
    [Node.Network network; Synchronisation_threshold 1; Expected_pow 26]
  in
  let node =
    Node.create
      ?data_dir
      ?rpc_host:rpc_addr
      ?rpc_port
      ?net_port
      ?net_addr
      ?metrics_addr
      ?metrics_port
      arguments
  in
  let* () = Node.config_reset node arguments in
  let* () = Node.run node arguments in
  let* () = Node.wait_for_ready node in
  return node

(** Start a DAL node with the given information and wait until it's ready. *)
let start_dal_node ~peers ?data_dir ?net_addr ?net_port ?rpc_addr ?rpc_port
    ?metrics_addr ?metrics_port ?public_ip_addr ?operator_profiles
    ?attester_profiles ?bootstrap_profile node =
  let listen_addr =
    match (net_addr, net_port) with
    | None, None -> None
    | Some addr, None -> Some (sf "%s:%d" addr @@ Port.fresh ())
    | None, Some port -> Some (sf "%s:%d" Constant.default_host port)
    | Some addr, Some port -> Some (sf "%s:%d" addr port)
  in
  let public_addr =
    Option.map
      (fun ip -> Option.fold net_port ~none:ip ~some:(sf "%s:%d" ip))
      public_ip_addr
  in
  let metrics_addr =
    match (metrics_addr, metrics_port) with
    | None, None -> None
    | Some addr, None -> Some (sf "%s:%d" addr @@ Port.fresh ())
    | None, Some port -> Some (sf "%s:%d" Constant.default_host port)
    | Some addr, Some port -> Some (sf "%s:%d" addr port)
  in

  let dal_node =
    Dal_node.create
      ?data_dir
      ?rpc_port
      ?rpc_host:rpc_addr
      ?listen_addr
      ?public_addr
      ?metrics_addr
      ~node
      ()
  in
  let* () =
    Dal_node.init_config
      ~expected_pow:26.
      ~peers
      ?operator_profiles
      ?attester_profiles
      ?bootstrap_profile
      dal_node
  in
  let* () = Dal_node.run ~wait_ready:true dal_node in
  return dal_node

(** This function determines the teztnet network date depending on the current
    time. *)
let teztnet_network_target =
  let dailynet () = Ptime_clock.now () |> Ptime.to_date in
  let weeklynet () =
    let t = Ptime_clock.now () in
    let days_since_wednesday = (Ptime.weekday_num t + 4) mod 7 in
    let span = Ptime.Span.of_int_s @@ (days_since_wednesday * 3600 * 24) in
    match Ptime.sub_span t span with
    | Some t -> t |> Ptime.to_date
    | _ -> assert false (* Unreachable*)
  in
  fun ~network ~teztnet_network_day ->
    Option.value
      teztnet_network_day
      ~default:
        (let year, month, day =
           match network with
           | "dailynet" -> dailynet ()
           | "weeklynet" -> weeklynet ()
           | s -> Test.fail "Unknown network %s@." s
         in
         Printf.sprintf "%04d-%02d-%02d" year month day)

(** This function allows to run a [main_scenario] function on a teztnet network
    after initializing an L1 and DAL node and bootstrapping them. *)
let scenario_on_teztnet =
  let ( //> ) =
    let ( // ) = Filename.concat in
    fun a_opt b ->
      Option.map
        (fun a ->
          let dir = a // b in
          ignore @@ Sys.command (sf "mkdir -p %s" dir) ;
          dir)
        a_opt
  in
  fun ~network
      ?(dal_bootstrap_peers = [])
      ?operator_profiles
      ?attester_profiles
      ~main_scenario
      ()
    ->
    (* Parse CLI arguments *)
    (* The working-dir option is not mandatory. It allows providing a fix
       directly for nodes and wallet data/base dir. An advantage of this is the
       ability to stop & restart the infra without being obliged to resync
       dailynet or weeklynet from genesis. In fact, Tezt chooses a different
       (random) directory for binaries (usually in /tmp) if they are not
       explicitly specified. *)
    let working_dir = Cli.get_string_opt "working-dir" in
    let public_ip_addr = Cli.get_string_opt "public-ip-addr" in
    let teztnet_network_day = Cli.get_string_opt "teztnet-network-day" in

    (* L1 ports *)
    let net_port = Cli.get_int_opt "net-port" in
    let rpc_port = Cli.get_int_opt "rpc-port" in
    let metrics_port = Cli.get_int_opt "metrics-port" in

    (* DAL ports *)
    let dal_net_port = Cli.get_int_opt "dal-net-port" in
    let dal_rpc_port = Cli.get_int_opt "dal-rpc-port" in
    let dal_metrics_port = Cli.get_int_opt "dal-metrics-port" in

    (* L1 addresses *)
    let net_addr = Cli.get_string_opt "net-addr" in
    let rpc_addr = Cli.get_string_opt "rpc-addr" in
    let metrics_addr = Cli.get_string_opt "metrics-addr" in

    (* DAL addresses *)
    let dal_net_addr = Cli.get_string_opt "dal-net-addr" in
    let dal_rpc_addr = Cli.get_string_opt "dal-rpc-addr" in
    let dal_metrics_addr = Cli.get_string_opt "dal-metrics-addr" in

    (* Determine the right network day *)
    let teztnet_network_day =
      teztnet_network_target ~network ~teztnet_network_day
    in
    (* prepare directories *)
    let working_dir = working_dir //> network //> teztnet_network_day in
    let data_l1 = working_dir //> "layer1-data" in
    let dal_producer = working_dir //> "dal-data" in
    let wallet = working_dir //> "wallet" in

    (* Start L1 node and wait until it's bootstrapped *)
    let* l1_node =
      let network =
        sf "https://teztnets.com/%s-%s" network teztnet_network_day
      in
      start_layer_1_node
        ~network
        ?data_dir:data_l1
        ?net_addr
        ?rpc_addr
        ?metrics_addr
        ?net_port
        ?rpc_port
        ?metrics_port
        ()
    in
    let client =
      Client.create
        ?base_dir:wallet
        ~endpoint:(Node l1_node)
        ~media_type:Json
        ()
    in
    let* () = Client.bootstrapped client in

    (* Start DAL node  *)
    let peer = sf "dal.%s-%s.teztnets.com:11732" network teztnet_network_day in
    let* dal_node =
      start_dal_node
        ~peers:(peer :: dal_bootstrap_peers)
        ?data_dir:dal_producer
        ?operator_profiles
        ?attester_profiles
        ?public_ip_addr
        ?net_addr:dal_net_addr
        ?net_port:dal_net_port
        ?rpc_addr:dal_rpc_addr
        ?rpc_port:dal_rpc_port
        ?metrics_addr:dal_metrics_addr
        ?metrics_port:dal_metrics_port
        l1_node
    in
    (* Prepare airdropper account. Secret key of
       tz1PEhbjTyVvjQ2Zz8g4bYU2XPTbhvG8JMFh, a funded key on periodic testnets. *)
    let airdropper_sk =
      "edsk3AWajGUgzzGi3UrQiNWeRZR1YMRYVxfe642AFSKBTFXaoJp5hu"
    in
    let airdropper_alias = "airdropper" in
    let* () =
      Client.import_secret_key
        client
        (Account.Unencrypted airdropper_sk)
        ~alias:airdropper_alias
        ~force:true
    in
    main_scenario ~airdropper_alias client dal_node l1_node

(** This function allows to injects DAL slots in the given network. *)
let slots_injector_scenario ?publisher_sk ~airdropper_alias client dal_node
    l1_node ~slot_index =
  let open Runnable.Syntax in
  let alias = "publisher" in
  let* () =
    match publisher_sk with
    | None ->
        let* _alias = Client.gen_keys ~alias client ~force:true in
        unit
    | Some account_sk ->
        Client.import_secret_key
          client
          (Account.Unencrypted account_sk)
          ~alias
          ~force:true
  in

  (* Airdrop publisher if needed *)
  let* balance_publisher = Client.get_balance_for ~account:alias client in
  let* () =
    if Tez.to_mutez balance_publisher > 5_000_000 then unit
    else
      Client.transfer
        ~amount:(Tez.of_mutez_int 50_000_000)
        ~giver:airdropper_alias
        ~receiver:alias
        ~burn_cap:(Tez.of_mutez_int 70_000)
        ~wait:"1"
        client
  in
  (* Fetch slots size from protocol parameters *)
  let* proto_parameters =
    Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
  in
  let dal_parameters =
    Dal.Parameters.from_protocol_parameters proto_parameters
  in
  let slot_size = dal_parameters.cryptobox.slot_size in
  (* Endless loop that injects slots *)
  let rec loop level =
    let slot =
      sf "slot=%d/payload=%d" slot_index level
      |> Dal_common.Helpers.make_slot ~slot_size
    in
    let* commitment, proof =
      Dal_common.Helpers.store_slot dal_node ~slot_index slot
    in
    let* level = Node.wait_for_level l1_node (level + 1) in
    let*! () =
      Client.publish_dal_commitment
        ~src:alias
        ~slot_index
        ~commitment
        ~proof
        client
    in
    loop level
  in
  let* level = Client.level client in
  loop level

let stake_or_unstake_half_balance client ~baker_alias =
  let* baker = Client.show_address ~alias:baker_alias client in
  let* full_balance =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_delegate_full_balance
         baker.Account.public_key_hash
  in
  let* available_balance = Client.get_balance_for ~account:baker_alias client in
  (* Stake half of the baker's balance *)
  let frozen_balance = Tez.(full_balance - available_balance) in
  let entrypoint, amount =
    if Tez.to_mutez available_balance > Tez.to_mutez frozen_balance then
      ("stake", Tez.((available_balance - frozen_balance) /! 2L))
    else ("unstake", Tez.((frozen_balance - available_balance) /! 2L))
  in
  if Tez.to_mutez amount = 0 then unit
  else
    Client.transfer
      ~amount
      ~giver:baker_alias
      ~receiver:baker_alias
      ~burn_cap:(Tez.of_mutez_int 140_000)
      ~entrypoint
      ~wait:"1"
      client

(** This function allows to start a baker and attests slots DAL slots in the
    given network. *)
let baker_scenario ?baker_sk ~airdropper_alias client dal_node l1_node =
  (* We'll not airdrop baker account to avoid emptying airdropper. The user
     should either provide a baker secret key with enough tez, or we use
     airdropper as a baker directly. *)
  let* baker_alias =
    match baker_sk with
    | None -> return airdropper_alias
    | Some account_sk ->
        let alias = "baker" in
        let* () =
          Client.import_secret_key
            client
            (Account.Unencrypted account_sk)
            ~alias
            ~force:true
        in
        return alias
  in
  (* No need to check if baker_alias is already delegate. Re-registering an
     already registered delegate doesn't fail. *)
  let* _s = Client.register_delegate ~delegate:baker_alias client in
  (* Manually stake a part of the baker's balance
     after it is declared as delegate. *)
  let* () = stake_or_unstake_half_balance client ~baker_alias in
  let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
  let baker = Agnostic_baker.create ~dal_node_rpc_endpoint l1_node client in
  let* () = Agnostic_baker.run baker in
  Lwt_unix.sleep Float.max_float

(** A slots injector test parameterized by a network. Typically, to run a slot
    injector on dailynet for slot index 10, one could run:

  dune exec tezt/manual_tests/main.exe -- --file dal.ml \
    --title "Join dailynet and inject slots" --verbose \
    -a public-ip-addr="<pub-addr>" \
    -a slot-index=0 \
    -a publisher-sk="edsk4Vi86eAcBVXYLnBawg2p9FaDEKeDxXPHo26UfhsJteVY7P7guq" \
    -a working-dir=./dal-e2e \
    -a rpc-port=30000 \
    -a dal-rpc-port=30001

Replace dailynet with weeklynet to rather join weeklynet.

*)
let slots_injector_test ~network =
  Test.register
    ~__FILE__
    ~title:(sf "Join %s and inject slots" network)
    ~tags:[Tag.tezos2; "dal"; "slot"; "producer"; network]
  @@ fun () ->
  let slot_index = Cli.get_int "slot-index" in
  let publisher_sk = Cli.get_string_opt "publisher-sk" in
  let dal_bootstrap_peers =
    Cli.get_string_opt "dal-bootstrap-peers"
    |> Option.map (String.split_on_char ',')
  in
  scenario_on_teztnet
    ?dal_bootstrap_peers
    ~network
    ~main_scenario:(slots_injector_scenario ~slot_index ?publisher_sk)
    ~operator_profiles:[slot_index]
    ()

(** A baker test parameterized by a network. Typically, to run a baker that
    operates on dailynet, one could run:

  dune exec tezt/manual_tests/main.exe -- --file dal.ml \
    --title "Join dailynet and bake" --verbose \
    -a public-ip-addr="<pub-addr>" \
    -a working-dir=./dal-e2e \
    -a rpc-port=10000 \
    -a baker-sk="edpkuSZ6gD6reoGEuJyPiPk67gp94V7xXtP1EZ83H46fNW9YQBUUdg" \
    -a dal-rpc-port=10001

Replace dailynet with weeklynet to rather join weeklynet.
*)
let baker_test ~network =
  Test.register
    ~__FILE__
    ~title:(sf "Join %s and bake" network)
    ~tags:[Tag.tezos2; "dal"; "baker"; network]
    ~uses:[Constant.octez_agnostic_baker]
  @@ fun () ->
  let baker_sk = Cli.get_string_opt "baker-sk" in
  let dal_bootstrap_peers =
    Cli.get_string_opt "dal-bootstrap-peers"
    |> Option.map (String.split_on_char ',')
  in
  scenario_on_teztnet
    ?dal_bootstrap_peers
    ~network
    ~main_scenario:(baker_scenario ?baker_sk)
    ()

let register () =
  dal_parameters ~is_fake:true ;
  dal_parameters ~is_fake:false ;
  List.iter
    (fun network ->
      slots_injector_test ~network ;
      baker_test ~network)
    ["dailynet"; "weeklynet"] ;
  ()
