(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>               *)
(*                                                                           *)
(*****************************************************************************)

(* Prerequisite:

   In order to be able to run the following test successfully, you need to make
   sure that your environment is well configured. To do so, have a look at the
   tezt/lib_cloud/README.md documentation.

   Additionally, if you are running the test you must ensure that:
   - DAL_TRUSTED_SETUP_PATH contains the expected data -- this can be done by
     running `./scripts/install_dal_trusted_setup.sh`; or use a custom dockerfile
     not requiring this data
*)

open Scenarios_helpers
open Tezos
open Yes_crypto

(** A baker account is its public key (pk) and its hash (pkh) *)
type baker_account = {pk : string; pkh : string}

let wait_next_level ?(offset = 1) node =
  Lwt.bind
    (Network.get_level (Node.as_rpc_endpoint node))
    (fun level -> Node.wait_for_level node (level + offset))

(** Max number of TPS one stresstest node is expected to reach *)
let stresstest_max_tps_pre_node = 50

(** Max number of pkh handled by one stresstest node *)
let stresstest_max_pkh_pre_node = 100

(** For a given [network], returns the number of stesstest node to spawn
    in order to achieve [tps] tps. *)
let nb_stresstester network tps =
  let n1 = tps / stresstest_max_tps_pre_node in
  let n2 = tps * Network.block_time network / stresstest_max_pkh_pre_node in
  max 1 (max n1 n2)

module Node = struct
  open Snapshot_helpers
  include Node

  (* If trying to only bootstrap the network from a snapshot, you will have
     errors about missing block metadata, which is likely (I guess?) to be
     because of data not included in the snapshot.

     > initializing irmin context at /tmp/tezt-2300623/1/baker-0-node/context
     > successfully migrated nonces: legacy nonces are safe to delete
     > Voting pass for liquidity baking toggle vote
     > Voting pass for adaptive issuance vote
     > failed to forge block for
     > baker_17 (tz1gjJfsjbB2ZwBfG7SiCxXUKWLvyLEEvP7U)
     >   Error:
     >     Unable to find block
     >     BKuTmkDidyuRZbUzKUPy1oyatuN3jQb3Hsbp7ZSdecRSyohKBmx's metadata.

     That's why the bootstrap node first syncs for few levels before being
     disconnected from the real network.
  *)
  let init_bootstrap_node_from_snapshot ~peers ~ppx_profiling
      (agent, node, name) snapshot network migration_offset =
    let* snapshot =
      ensure_snapshot ~agent ~name ~network:(Network.to_public network) snapshot
    in
    let* () =
      let toplog s = toplog "/!\\ %s /!\\" s in
      toplog "Bootstrapping node using the real world" ;
      let config =
        [
          Network Network.(to_octez_network_options @@ to_public network);
          Expected_pow 26;
          Cors_origin "*";
          Synchronisation_threshold 0;
        ]
      in
      let* () = Node.config_init node config in
      let* () = import_snapshot ~no_check:true ~name node snapshot in
      let* () =
        (* When bootstrapping from the real network, we want to use the real time. *)
        let env = String_map.(add "FAKETIME" "+0" empty) in
        run ~env node [Synchronisation_threshold 0]
      in
      let* () = wait_for_ready node in
      let* _new_level = wait_next_level ~offset:2 node in
      let* () = terminate node in
      Lwt.return_unit
    in
    toplog "Reset node config for private a yes-crypto network" ;
    let config = Node_helpers.isolated_config ~peers ~network ~delay:0 in
    let* () = Node.config_reset node config in
    let* () =
      Node_helpers.may_add_migration_offset_to_config
        node
        snapshot
        ~migration_offset
        ~network
    in
    let arguments = Node_helpers.isolated_args peers in
    let* () =
      Node.Agent.run
        ~env:yes_crypto_env
        node
        (Synchronisation_threshold 0 :: arguments)
        ~ppx_profiling
    in
    wait_for_ready node

  (** Initialize a node, which means:
      - configure it for a private network with yes-crypto enabled
      - import the relevant snapshot
      - run it with yes-crypto enabled and allowed peer lists
  *)
  let init_node_from_snapshot ~delay ~peers ~ppx_profiling ~snapshot ~network
      ~migration_offset (agent, node, name) =
    let* snapshot =
      ensure_snapshot ~agent ~name ~network:(Network.to_public network) snapshot
    in
    let config = Node_helpers.isolated_config ~peers ~network ~delay in
    let* () = Node.config_init node config in
    let* () =
      Node_helpers.may_add_migration_offset_to_config
        node
        snapshot
        ~migration_offset
        ~network
    in
    let* () = import_snapshot ~no_check:true ~name node snapshot in
    let arguments = Node_helpers.isolated_args peers in
    let synchronisation_waiter =
      Node.wait_for_synchronisation ~statuses:["synced"; "stuck"] node
    in
    let* () =
      Node.Agent.run
        ~env:yes_crypto_env
        ~ppx_profiling
        node
        (Synchronisation_threshold 0 :: arguments)
    in
    let* () = wait_for_ready node in
    (* As we are playing with dates in the past,
       disconnected from real network (i.e. in a frozen state) you are likely to
       be [stuck] (i.e. synchronised with peer but missing newer blocks) until
       your bakers start to bake. *)
    synchronisation_waiter

  let client ~node agent =
    let name = Tezt_cloud.Agent.name agent ^ "-client" in
    Client.Agent.create ~name ~endpoint:(Client.Node node) agent

  (** Initialize the node,
      create the associated client,
      create the yes-wallet.
  *)
  let init_bootstrap_node ?stresstest ~ppx_profiling ~peers ~snapshot ~network
      ~migration_offset (agent, node, name) =
    toplog "Initializing an L1 node (public network): %s" name ;
    let* () =
      init_bootstrap_node_from_snapshot
        ~peers
        ~ppx_profiling
        (agent, node, name)
        snapshot
        network
        migration_offset
    in
    let* client = client ~node agent in
    let* yes_wallet = Node_helpers.yes_wallet agent in
    let* _filename =
      Yes_wallet.create_from_context
        ?rich_accounts:
          (match stresstest with
        | None -> None
        | Some _ -> Some (1, 1_000_000))
          (* If the stresstest argument is set we try to find an account with at least 1M tez *)
        ~node
        ~client
        ~network:(Network.to_string network)
        yes_wallet
    in
    let* delegates, consensus_keys, rich_account_opt =
      let* known_addresses = Client.list_known_addresses client in
      let get_pk alias =
        let* account = Client.show_address ~alias client in
        return account.public_key
      in
      (Lwt_list.fold_left_s
         (fun (delegates, consensus_keys, rich_account_opt) (alias, pkh) ->
           if alias =~ rex "rich_" then
             (* if the alias match "rich_n" *)
             let* pk = get_pk alias in
             return (delegates, consensus_keys, Some {pkh; pk})
           else
             match alias =~* rex "(\\w+)_consensus_key" with
             | None ->
                 (* if the alias does not contain "_consensus_key" *)
                 let* pk = get_pk alias in
                 return
                   ( String_map.add alias {pkh; pk} delegates,
                     consensus_keys,
                     rich_account_opt )
             | Some consensus_key_alias ->
                 (* if the alias match "baker_n_consensus_key" *)
                 let* pk = get_pk alias in
                 return
                   ( delegates,
                     String_map.add consensus_key_alias {pkh; pk} consensus_keys,
                     rich_account_opt )))
        (String_map.empty, String_map.empty, None)
        (List.rev known_addresses)
    in
    let delegates_with_consensus_keys =
      String_map.mapi
        (fun alias address_and_pk ->
          (address_and_pk, String_map.find_opt alias consensus_keys))
        delegates
    in
    let* () = Client.forget_all_keys client in
    Lwt.return (client, delegates_with_consensus_keys, rich_account_opt)

  (** Initialize the node,
      create the associated client,
      create the yes-wallet.
    *)
  let init_baker_node ?(delay = 0) ~accounts ~peers ~ppx_profiling ~snapshot
      ~network ~migration_offset (agent, node, name) =
    toplog "Initializing an L1 node (public network): %s" name ;
    let* () =
      init_node_from_snapshot
        ~delay
        ~peers
        ~ppx_profiling
        ~snapshot
        ~network
        ~migration_offset
        (agent, node, name)
    in
    let* client = client ~node agent in
    let* yes_wallet = Node_helpers.yes_wallet agent in
    let* () =
      Lwt_list.iter_s
        (fun {pkh = alias; pk = public_key} ->
          Client.import_public_key ~alias ~public_key client)
        accounts
    in
    let* () = Yes_wallet.convert_wallet_inplace ~client yes_wallet in
    Lwt.return client

  (** Prerequisite: the chain is running (i.e. bakers are baking blocks) *)
  let init_stresstest_node ?(delay = 0) ~pkh ~pk ~peers ~ppx_profiling ~snapshot
      ~network ~migration_offset ~tps (agent, node, name) =
    let* () =
      init_node_from_snapshot
        ~delay
        ~peers
        ~ppx_profiling
        ~snapshot
        ~network
        ~migration_offset
        (agent, node, name)
    in
    let* client = client ~node agent in
    let* yes_wallet = Node_helpers.yes_wallet agent in
    let* () = Client.forget_all_keys client in
    let* () = Client.import_public_key ~alias:pkh ~public_key:pk client in
    let* () = Yes_wallet.convert_wallet_inplace ~client yes_wallet in
    let* accounts =
      Client.stresstest_gen_keys
        ~alias_prefix:name
        (tps * Network.block_time network)
        client
    in
    Lwt.return (client, accounts)
end

(** Stresstest parameters
    - [tps]: targeted number of transactions per second
    - [seed]: seed used for stresstest traffic generation
 *)
type stresstest_conf = {tps : int; seed : int}

(** Scenario configuration

    - [snapshot]: local path or URL of the snapshot to use for the experiment.
      local path implies to [scp] the snapshot to all the vms.

    - [stake]: stake repartition between baking node, numbers are relatives.

      e.g.: [2,1,1] runs 3 bakers, aggregate delegates from the network,
      spreading them in 3 pools representing roughly 50%, 25% and 25% of the
      total stake of the network. The same stake repartition using the same
      snapshot will result in the same delegate repartition.

      There is a special case using a single number instead of a list, which
      must match the number of delegates. This scenario will launch one node
      per delegate (i.e. more than 300 nodes if testing on mainnet).

    - [maintenance_delay]: number of level which will be multiplied by the
      position in the list of the bakers to define the store merge delay.
      We want it to be the same for two runs with same parameters (not
      random) and we want it not to occur at the same time on every baker.
      Default value is 1.
      Use 0 for disabling delay and have all the bakers to merge their
      store at the beginning of cycles.

    - [migration_offset]: offset that dictates after how many levels a protocol
      upgrade will be performed via a UAU.

    - [stresstest]: See the description of [stresstest_conf]
  *)
type configuration = {
  stake : int list;
  network : Network.t;
  snapshot : Snapshot_helpers.t;
  stresstest : stresstest_conf option;
  maintenance_delay : int;
  migration_offset : int option;
  ppx_profiling : bool;
  ppx_profiling_backends : string list;
  signing_delay : (float * float) option;
  fixed_random_seed : int option;
}

let network_encoding =
  Data_encoding.string_enum [("Mainnet", `Mainnet); ("Ghostnet", `Ghostnet)]

let stresstest_encoding =
  let open Data_encoding in
  conv
    (fun {tps; seed} -> (tps, seed))
    (fun (tps, seed) -> {tps; seed})
    (obj2 (req "tps" int31) (req "seed" int31))

let configuration_encoding =
  let open Data_encoding in
  conv
    (fun {
           stake;
           network;
           snapshot;
           stresstest;
           maintenance_delay;
           migration_offset;
           ppx_profiling;
           ppx_profiling_backends;
           signing_delay;
           fixed_random_seed;
         }
       ->
      ( stake,
        network,
        snapshot,
        stresstest,
        maintenance_delay,
        migration_offset,
        ppx_profiling,
        ppx_profiling_backends,
        signing_delay,
        fixed_random_seed ))
    (fun ( stake,
           network,
           snapshot,
           stresstest,
           maintenance_delay,
           migration_offset,
           ppx_profiling,
           ppx_profiling_backends,
           signing_delay,
           fixed_random_seed )
       ->
      {
        stake;
        network;
        snapshot;
        stresstest;
        maintenance_delay;
        migration_offset;
        ppx_profiling;
        ppx_profiling_backends;
        signing_delay;
        fixed_random_seed;
      })
    (obj10
       (req "stake" (list int31))
       (req "network" network_encoding)
       (req "snapshot" Snapshot_helpers.encoding)
       (opt "stresstest" stresstest_encoding)
       (dft
          "maintenance_delay"
          int31
          Scenarios_cli.Layer1_default.default_maintenance_delay)
       (opt "migration_offset" int31)
       (dft
          "ppx_profiling"
          bool
          Scenarios_cli.Layer1_default.default_ppx_profiling)
       (dft
          "ppx_profiling_backends"
          (list string)
          Scenarios_cli.Layer1_default.default_ppx_profiling_backends)
       (opt "Signing_delay" (tup2 float float))
       (opt "fixed_random_seed" int31))

type bootstrap = {
  agent : Agent.t;
  node : Node.t;
  node_p2p_endpoint : string;
  client : Client.t;
}

type baker = {
  agent : Agent.t;
  node : Node.t;
  baker : Agnostic_baker.t;
  accounts : string list;
}

type stresstester = {
  agent : Agent.t;
  node : Node.t;
  client : Client.t;
  accounts : string list;
}

type 'network t = {
  configuration : configuration;
  cloud : Cloud.t;
  bootstrap : bootstrap;
  bakers : baker list;
  stresstesters : stresstester list;
}

let init_baker_i i (configuration : configuration) cloud ~peers
    (accounts : baker_account list) (agent, node, name) =
  let delay = i * configuration.maintenance_delay in
  let* client =
    toplog "init_baker: Initialize node" ;
    let name = name ^ "-node" in
    Node.init_baker_node
      ~accounts
      ~delay
      ~peers
      ~ppx_profiling:configuration.ppx_profiling
      ~snapshot:configuration.snapshot
      ~network:configuration.network
      ~migration_offset:configuration.migration_offset
      (agent, node, name)
  in
  let* baker =
    toplog "init_baker: Initialize agnostic baker" ;
    let name = name ^ "-agnostic-baker" in
    let open Signing_delay in
    let env = yes_crypto_env in
    let env =
      match configuration.fixed_random_seed with
      | None ->
          Random.self_init () ;
          env
      | Some seed ->
          Random.init seed ;
          (* each baker will have a different seed *)
          fixed_seed_env env
    in
    let env =
      match configuration.signing_delay with
      | None -> env
      | Some (min, max) -> signing_delay_env min max env
    in
    let* agnostic_baker =
      Agnostic_baker.Agent.init
        ~env
        ~name
        ~delegates:(List.map (fun ({pkh; _} : baker_account) -> pkh) accounts)
        ~ppx_profiling:configuration.ppx_profiling
        ~client
        ~allow_fixed_random_seed:
          (Option.is_some configuration.fixed_random_seed)
        ~allow_signing_delay:(Option.is_some configuration.signing_delay)
        node
        cloud
        agent
    in
    let* () = Agnostic_baker.wait_for_ready agnostic_baker in
    toplog "init_baker: %s is ready!" name ;
    Lwt.return agnostic_baker
  in
  Lwt.return
    {
      agent;
      node;
      baker;
      accounts = List.map (fun ({pkh; _} : baker_account) -> pkh) accounts;
    }

let fund_stresstest_accounts ~source client =
  Client.stresstest_fund_accounts_from_source
    ~batch_size:150
    ~batches_per_block:50
    ~env:yes_crypto_env
    ~source_key_pkh:source
    ~initial_amount:(Tez.of_mutez_int64 1_000_000_000L)
    client

let init_stresstest_i i (configuration : configuration) ~pkh ~pk ~peers
    (agent, node, name) tps : stresstester Lwt.t =
  let delay = i * configuration.maintenance_delay in
  let* client, accounts =
    toplog "init_stresstest: Initialize node" ;
    Node.init_stresstest_node
      ~pk
      ~pkh
      ~delay
      ~peers
      ~ppx_profiling:configuration.ppx_profiling
      ~snapshot:configuration.snapshot
      ~network:configuration.network
      ~migration_offset:configuration.migration_offset
      (agent, node, name)
      ~tps
  in
  let accounts = List.map (fun a -> a.Account.public_key_hash) accounts in
  Lwt.return {agent; node; client; accounts}

let init_network ~peers (configuration : configuration) cloud teztale
    ((agent, node, _) as resources) =
  toplog "init_network: Initializing the bootstrap node" ;
  let* client, delegates, rich_account =
    Node.init_bootstrap_node
      ?stresstest:configuration.stresstest
      ~peers
      ~ppx_profiling:configuration.ppx_profiling
      ~snapshot:configuration.snapshot
      ~network:configuration.network
      ~migration_offset:configuration.migration_offset
      resources
  in
  toplog "init_network: Add a Teztale archiver" ;
  let* () =
    Teztale.add_archiver
      teztale
      cloud
      agent
      ~node_name:(Node.name node)
      ~node_port:(Node.rpc_port node)
  in
  let bootstrap =
    {agent; node; node_p2p_endpoint = Node.point_str node; client}
  in
  Lwt.return (bootstrap, delegates, rich_account)

(** Reserves resources for later usage. *)
let agent_and_node next_agent cloud ~name =
  let* agent = next_agent ~name in
  let* node = Node.Agent.create ~metadata_size_limit:false ~name cloud agent in
  Lwt.return (agent, node, name)

(** Distribute the delegate accounts according to stake specification *)
let distribute_delegates stake (baker_accounts : (baker_account * int) list) =
  let sum list = List.fold_left ( + ) 0 list |> float_of_int in
  let baker_accounts =
    (* Order delegates from the most powerful to the least powerful *)
    List.sort (fun (_, p) (_, p') -> Int.compare p' p) baker_accounts
  in
  let total_baking_power =
    List.fold_left (fun acc (_, p) -> acc + p) 0 baker_accounts |> float_of_int
  in
  let stake =
    (* Convert targets list in actual baking power needed *)
    let total_stake = sum stake in
    List.map
      (fun s ->
        int_of_float
        @@ Float.ceil (total_baking_power *. float_of_int s /. total_stake))
      stake
  in
  let distribution =
    (* Order each target from the lowest to the highest *)
    (* Associate each target with an empty set of delegates *)
    (* Convert this list to array for easier update *)
    List.sort Int.compare stake |> List.map (fun s -> (s, [])) |> Array.of_list
  in
  (* [missing i] is the baking power still needed to match the expected target *)
  let missing i = fst distribution.(i) in
  let swap i j =
    let tmp = distribution.(i) in
    distribution.(i) <- distribution.(j) ;
    distribution.(j) <- tmp
  in
  (* [update_order i] update the place of i-th element in order to keep the array
     in ascending order of missing baking power to match the target *)
  let rec update_order i =
    if i > 0 then
      if missing (i - 1) > missing i then (
        swap (i - 1) i ;
        update_order (i - 1))
      else ()
    else if i < Array.length distribution - 1 then
      if missing (i + 1) < missing i then (
        swap (i + 1) i ;
        update_order (i + 1))
      else ()
  in
  (* Find the best place to distribute the baking power [p] which is the place
     where adding this baking power would leave the smallest gap between effective
     power and target while remaining less or equal to the target.
     If no option allow to keep the sum less or equal to the target, we add the
     delegate to the set where the gap would be the smallest in absolute value. *)
  let rec find_best_place p acc i =
    if i = Array.length distribution then acc
    else if missing i >= p then find_best_place p (Some i) (i + 1)
    else acc
  in
  let find_best_place p =
    match find_best_place p None 0 with
    | Some i -> i
    | None -> Array.length distribution - 1
  in
  (* For each delegate, find the best suited place according to its baking power
     while keeping the array order *)
  List.iter
    (fun (a, p) ->
      let i = find_best_place p in
      let target, set = distribution.(i) in
      distribution.(i) <- (target - p, (a, p) :: set) ;
      update_order i)
    baker_accounts ;
  let print_list s list =
    toplog
      "%s: [%s]"
      s
      (List.map
         (fun s -> Printf.sprintf "%.4f" (s /. total_baking_power))
         (List.sort compare list)
      |> String.concat "; ")
  in
  let distribution = Array.to_list distribution |> List.map snd in
  List.map float_of_int stake |> print_list "Target" ;
  List.map (fun s -> sum (List.map snd s)) distribution
  |> print_list "Distribution" ;
  List.map (List.map fst) distribution

let number_of_bakers ~snapshot ~network ~ppx_profiling ~ppx_profiling_backends:_
    cloud agent name =
  let* node =
    Node.Agent.create ~metadata_size_limit:false ~name:"tmp-node" cloud agent
  in
  let* snapshot =
    Snapshot_helpers.ensure_snapshot
      ~agent
      ~name
      ~network:(Network.to_public network)
      snapshot
  in
  let* () =
    Node.config_init
      node
      (Node_helpers.isolated_config ~peers:[] ~network ~delay:0)
  in
  let* () =
    Snapshot_helpers.import_snapshot ~no_check:true ~name node snapshot
  in
  let* () =
    Node.Agent.run node (Node_helpers.isolated_args []) ~ppx_profiling
  in
  let* () = Node.wait_for_ready node in
  let* client =
    Client.Agent.create ~name:"tmp-client" ~endpoint:(Node node) agent
  in
  let* n =
    Client.(
      rpc GET ["chains"; "main"; "blocks"; "head"; "context"; "delegates"])
      client
    |> Lwt.map (fun json -> JSON.(json |> as_list |> List.length))
  in
  let* () = Node.Agent.terminate node in
  Lwt.return n

let init ~(configuration : configuration) cloud next_agent =
  let () = toplog "Init" in
  (* First, we allocate agents and node address/port in order to have the
     peer list known when initializing. *)
  let* ((bootstrap_agent, bootstrap_node, bootstrap_name) as bootstrap) =
    agent_and_node next_agent cloud ~name:"bootstrap"
  in
  let* stresstest_agents =
    match configuration.stresstest with
    | None -> Lwt.return_nil
    | Some {tps; _} ->
        List.init (nb_stresstester configuration.network tps) (fun i -> i)
        |> Lwt_list.map_s @@ fun i ->
           agent_and_node next_agent cloud ~name:(sf "stresstest-%d" i)
  in
  let* baker_agents =
    let* n =
      if configuration.stake = [] then
        number_of_bakers
          ~snapshot:configuration.snapshot
          ~network:configuration.network
          ~ppx_profiling:configuration.ppx_profiling
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
          cloud
          bootstrap_agent
          bootstrap_name
      else
        Lwt.return
          (match configuration.stake with
          | [n] -> n
          | _ -> List.length configuration.stake)
    in
    let () = toplog "Preparing agents for bakers (%d)" n in
    List.init n (fun i -> i)
    |> Lwt_list.map_s @@ fun i ->
       agent_and_node next_agent cloud ~name:(sf "baker-%d" i)
  in
  let* teztale = init_teztale cloud bootstrap_agent in
  let* () = init_explorus cloud bootstrap_node in
  let peers : string list =
    List.map
      (fun (_, node, _) -> Node.point_str node)
      (stresstest_agents @ baker_agents)
  in
  let* bootstrap, baker_accounts, stresstest_rich_account_opt =
    init_network ~peers configuration cloud teztale bootstrap
  in
  let peers = Node.point_str bootstrap_node :: peers in
  let* bakers =
    toplog "Initializing bakers" ;
    let* distribution =
      let baker_accounts =
        String_map.fold (fun _alias a acc -> a :: acc) baker_accounts []
      in
      match configuration.stake with
      | [] ->
          let res =
            List.map
              (fun (baker_account, consensus_key_opt) ->
                match consensus_key_opt with
                | None -> [baker_account]
                | Some consensus_key -> [consensus_key])
              baker_accounts
          in
          Lwt.return res
      | [x] ->
          if List.compare_length_with baker_accounts x != 0 then
            Test.fail
              "The number of agents (%d) is not the same as the number of \
               bakers (%d) retrieved in the yes wallet "
              x
              (List.length baker_accounts) ;
          let res =
            List.map
              (fun (baker_account, consensus_key_opt) ->
                match consensus_key_opt with
                | None -> [baker_account]
                | Some consensus_key -> [consensus_key])
              baker_accounts
          in
          Lwt.return res
      | stake ->
          let* accounts =
            toplog
              "init_network: Fetching baker accounts baking power from client" ;
            Lwt_list.map_s
              (fun ( (baker_account : baker_account),
                     (consensus_key_opt : baker_account option) )
                 ->
                let* power =
                  Client.(
                    rpc
                      GET
                      [
                        "chains";
                        "main";
                        "blocks";
                        "head";
                        "context";
                        "delegates";
                        baker_account.pkh;
                        "current_baking_power";
                      ]
                      bootstrap.client)
                in
                let baker_account =
                  match consensus_key_opt with
                  | None -> baker_account
                  | Some consenusus_key -> consenusus_key
                in
                Lwt.return (baker_account, power |> JSON.as_int))
              baker_accounts
          in
          distribute_delegates stake accounts |> return
    in
    Lwt_list.mapi_p
      (fun i accounts ->
        let ((_, node, _) as agent) = List.nth baker_agents i in
        let peers = List.filter (( <> ) (Node.point_str node)) peers in
        init_baker_i i ~peers configuration cloud accounts agent)
      distribution
  in
  let* stresstesters =
    match (configuration.stresstest, stresstest_rich_account_opt) with
    | None, _ -> Lwt.return_nil
    | Some _, None ->
        Test.fail
          "Stresstest argument was provided but no rich account was found in \
           the yes wallet"
    | Some {tps; seed}, Some {pkh; pk} ->
        let tps = tps / nb_stresstester configuration.network tps in
        let* stresstesters =
          (* init stresstest: init node and create accounts *)
          Lwt_list.mapi_p
            (fun i ((_, node, _) as res) ->
              let peers = List.filter (( <> ) (Node.point_str node)) peers in
              init_stresstest_i i configuration ~pkh ~pk ~peers res tps)
            stresstest_agents
        in
        let* () =
          (* fund accounts *)
          Lwt_list.iter_s
            (fun {client; _} -> fund_stresstest_accounts ~source:pkh client)
            stresstesters
        in
        let* () =
          (* ensure accounts are revealed *)
          (* FIXME: handle this in stresstest command itself *)
          Lwt_list.iter_p
            (fun {client; accounts; node; _} ->
              Lwt_list.iter_s
                (fun pkh ->
                  (* For some reason, [try ... with ...] will make the scenario fail when a exception is raised  *)
                  let* need_reveal_operation =
                    RPC.get_chain_block_context_contract_manager_key ~id:pkh ()
                    |> RPC_core.call (Node.as_rpc_endpoint node)
                    |> Lwt.map JSON.is_null
                  in
                  if need_reveal_operation then
                    Client.reveal ~src:pkh client |> Runnable.run
                  else Lwt.return_unit)
                accounts)
            stresstesters
        in
        let* () =
          (* run the stresstest *)
          Lwt_list.iteri_p
            (fun i {agent; client; accounts; _} ->
              let* filename =
                (* The list of account is too big to be passed by ssh,
                   so we generate the account list locally,
                   copy it the the agent,
                   and pass the filename to stresstest invocation directly *)
                let sources =
                  `A (List.map (fun pkh -> `O [("pkh", `String pkh)]) accounts)
                in
                (* As we run in parrallel, and as Temp.file does not return a unique file name,
                   we need a different base filename name for each stresstester *)
                let base = sf "stresstest-%i-sources.json" i in
                let source = Temp.file ?runner:None base in
                let destination = Temp.file ?runner:(Agent.runner agent) base in
                let () = JSON.encode_to_file_u source sources in
                Tezt_cloud.Agent.copy agent ~destination ~source
              in
              let _ =
                Client.spawn_stresstest_with_filename
                  ~env:yes_crypto_env
                  ~tps
                  ~seed
                  client
                  filename
              in
              Lwt.return_unit)
            stresstesters
        in
        Lwt.return stresstesters
  in
  let* () =
    add_prometheus_source ~node:bootstrap_node cloud bootstrap_agent "bootstrap"
  in
  let* () =
    Lwt_list.iter_s
      (fun ({agent; node; _} : baker) ->
        add_prometheus_source ~node cloud agent (Agent.name agent))
      bakers
  in
  let* () =
    Lwt_list.iter_s
      (fun ({agent; node; _} : stresstester) ->
        add_prometheus_source ~node cloud agent (Agent.name agent))
      stresstesters
  in
  Lwt.return {cloud; configuration; bootstrap; bakers; stresstesters}

let on_new_level =
  let push_level t level =
    Cloud.push_metric
      t.cloud
      ~help:"Level of (HEAD - 1)"
      ~typ:`Counter
      ~name:"level"
      (float_of_int level)
  in
  let push_nb_ops t nb =
    Cloud.push_metric
      t.cloud
      ~help:"Level of (HEAD - 1)"
      ~typ:`Counter
      ~name:"nb_ops"
      (float_of_int nb)
  in
  fun t level ->
    let* _ = Node.wait_for_level t.bootstrap.node level in
    let level = level - 2 in
    toplog "Processing metrics for level %d" level ;
    let* json =
      Client.(rpc GET)
        ["chains"; "main"; "blocks"; string_of_int level]
        t.bootstrap.client
    in
    let level = JSON.(json |-> "header" |-> "level" |> as_int) in
    let nops = JSON.(json |-> "operations" |> as_list |> List.length) in
    push_level t level ;
    push_nb_ops t nops ;
    toplog "Done processing metrics for level %d" level ;
    Lwt.return_unit

type docker_image = Agent.Configuration.docker_image =
  | Gcp of {alias : string}
  | Octez_release of {tag : string}

let docker_image_encoding =
  let open Data_encoding in
  let gcp_tag = 0 and gcp_encoding = string in
  let octez_release_tag = 1 and octez_release_encoding = string in
  matching
    (* optimised encoding function *)
    (function
      | Gcp {alias} -> matched gcp_tag gcp_encoding alias
      | Octez_release {tag} ->
          matched octez_release_tag octez_release_encoding tag)
    (* decoding case list *)
    [
      case
        ~title:"Gcp"
        (Tag gcp_tag)
        gcp_encoding
        (function Gcp {alias} -> Some alias | _ -> None)
        (fun alias -> Gcp {alias});
      case
        ~title:"Octez_release"
        (Tag octez_release_tag)
        octez_release_encoding
        (function Octez_release {tag} -> Some tag | _ -> None)
        (fun tag -> Octez_release {tag});
    ]

type vm_conf = {
  machine_type : string option;
  docker_image : docker_image option;
  max_run_duration : int option;
  binaries_path : string option;
  os : string option;
}

let vm_conf_encoding =
  let open Data_encoding in
  conv
    (fun {machine_type; docker_image; max_run_duration; binaries_path; os} ->
      (machine_type, docker_image, max_run_duration, binaries_path, os))
    (fun (machine_type, docker_image, max_run_duration, binaries_path, os) ->
      {machine_type; docker_image; max_run_duration; binaries_path; os})
    (obj5
       (opt "machine_type" string)
       (opt "docker_image" docker_image_encoding)
       (opt "max_duration_round" int31)
       (opt "binaries_path" string)
       (opt "os" string))

type vms_conf = {
  bootstrap : vm_conf option;
  bakers : vm_conf list option;
  stresstester : vm_conf option;
}

let vms_conf_encoding =
  let open Data_encoding in
  conv
    (fun {bootstrap; bakers; stresstester} -> (bootstrap, bakers, stresstester))
    (fun (bootstrap, bakers, stresstester) -> {bootstrap; bakers; stresstester})
    (obj3
       (opt "bootstrap" vm_conf_encoding)
       (opt "bakers" @@ list vm_conf_encoding)
       (opt "stresstester" vm_conf_encoding))

(** Copy/paste from teztale_server_main.ml *)
let parse_conf encoding file =
  (* [non_dir_file] from [Cmdliner] already checks that the file exists. *)
  let ic = open_in file in
  match Ezjsonm.from_channel_result ic with
  | Error err ->
      (* Must exists somewhere but where ! *)
      let print_location f ((fl, fc), (tl, tc)) =
        if Int.equal fl tl then
          if Int.equal fc tc then Format.fprintf f "line %i character %i" fl fc
          else Format.fprintf f "line %i characters %i-%i" fl fc tc
        else Format.fprintf f "lines %i-%i characters %i-%i" fl tl fc tc
      in
      Format.eprintf
        "Error in %s: %a:@ %s@."
        file
        (Format.pp_print_option print_location)
        (Ezjsonm.read_error_location err)
        (Ezjsonm.read_error_description err) ;
      exit 1
  | Ok json -> (
      try Data_encoding.Json.destruct encoding json
      with e ->
        let () =
          Format.eprintf
            "@[<v>@[Invalid configuration in %s:@ @[%a@]@]@ Configuration file \
             format is@ @[%a@]@]@."
            file
            (Data_encoding.Json.print_error ?print_unknown:None)
            e
            Json_schema.pp
            (Data_encoding.Json.schema encoding)
        in
        exit 1)

let yes_wallet_exe = Uses.path Constant.yes_wallet

let register (module Cli : Scenarios_cli.Layer1) =
  let configuration : configuration =
    let stake = Cli.stake in
    let network = Cli.network in
    let stresstest =
      Option.map (fun (tps, seed) -> {tps; seed}) Cli.stresstest
    in
    let maintenance_delay = Cli.maintenance_delay in
    let snapshot = Cli.snapshot in
    let migration_offset = Cli.migration_offset in
    let ppx_profiling = Cli.ppx_profiling in
    let ppx_profiling_backends = Cli.ppx_profiling_backends in
    let signing_delay = Cli.signing_delay in
    let fixed_random_seed = Cli.fixed_random_seed in
    match Tezt_cloud_cli.scenario_specific_json with
    | Some ("LAYER1", json) ->
        let conf = Data_encoding.Json.destruct configuration_encoding json in
        {
          stake = Option.value ~default:conf.stake stake;
          network = Option.value ~default:conf.network network;
          snapshot = Option.value ~default:conf.snapshot snapshot;
          stresstest =
            (if stresstest <> None then stresstest else conf.stresstest);
          maintenance_delay =
            Option.value ~default:conf.maintenance_delay maintenance_delay;
          migration_offset =
            (if migration_offset <> None then migration_offset
             else conf.migration_offset);
          ppx_profiling = ppx_profiling || conf.ppx_profiling;
          ppx_profiling_backends =
            (if ppx_profiling_backends <> [] then ppx_profiling_backends
             else conf.ppx_profiling_backends);
          signing_delay =
            (if signing_delay <> None then signing_delay else conf.signing_delay);
          fixed_random_seed =
            (if fixed_random_seed <> None then fixed_random_seed
             else conf.fixed_random_seed);
        }
    | _ ->
        {
          stake = Option.get stake;
          network = Option.get network;
          stresstest;
          maintenance_delay =
            Option.value
              ~default:Scenarios_cli.Layer1_default.default_maintenance_delay
              maintenance_delay;
          snapshot = Option.get snapshot;
          migration_offset;
          ppx_profiling;
          ppx_profiling_backends;
          signing_delay;
          fixed_random_seed;
        }
  in
  if configuration.stake = [] then Test.fail "stake parameter cannot be empty" ;
  if configuration.snapshot = Snapshot_helpers.No_snapshot then
    Test.fail "snapshot parameter cannot be empty" ;

  let vms_conf = Option.map (parse_conf vms_conf_encoding) Cli.vms_config in
  toplog "Parsing CLI done" ;
  let vms =
    `Bootstrap
    ::
    (match configuration.stake with
    | [n] -> List.init n (fun i -> `Baker i)
    | stake -> List.mapi (fun i _ -> `Baker i) stake)
    @
    match configuration.stresstest with
    | None -> []
    | Some {tps; _} ->
        let n = nb_stresstester configuration.network tps in
        List.init n (fun i -> `Stresstest i)
  in
  let default_docker_image =
    Option.map
      (fun tag -> Agent.Configuration.Octez_release {tag})
      Cli.octez_release
  in
  let default_vm_configuration ~name =
    Agent.Configuration.make ?docker_image:default_docker_image ~name ()
  in
  let make_vm_conf ~name = function
    | None -> default_vm_configuration ~name
    | Some {machine_type; docker_image; max_run_duration; binaries_path; os} ->
        let docker_image =
          match docker_image with
          | None -> default_docker_image
          | _ -> docker_image
        in
        let os = Option.map Types.Os.of_string_exn os in
        Agent.Configuration.make
          ?machine_type
          ?docker_image
          ~max_run_duration
          ?binaries_path
          ?os
          ~name
          ()
  in
  let vms =
    vms
    |> List.map (fun kind ->
           match kind with
           | `Bootstrap ->
               make_vm_conf ~name:"bootstrap"
               @@ Option.bind vms_conf (fun {bootstrap; _} -> bootstrap)
           | `Stresstest j ->
               make_vm_conf ~name:(Format.sprintf "stresstest-%d" j)
               @@ Option.bind vms_conf (fun {stresstester; _} -> stresstester)
           | `Baker i ->
               make_vm_conf ~name:(Format.sprintf "baker-%d" i)
               @@ Option.bind vms_conf (function
                    | {bakers = Some bakers; _} -> List.nth_opt bakers i
                    | {bakers = None; _} -> None))
  in
  Cloud.register
  (* Docker images are pushed before executing the test in case binaries
     are modified locally. This way we always use the latest ones. *)
    ~vms:(return vms)
    ~proxy_files:
      ([yes_wallet_exe]
      @
      match configuration.snapshot with
      | Local_file snapshot -> [snapshot]
      | _ -> [])
    ~__FILE__
    ~title:"L1 simulation"
    ~tags:[]
  @@ fun cloud ->
  toplog "Creating the agents" ;
  let agents = Cloud.agents cloud in
  toplog "Created %d agents" (List.length agents) ;
  (* We give to the [init] function a sequence of agents. We set their name
     only if the number of agents is the computed one. Otherwise, the user
     has mentioned explicitly a reduced number of agents and it is not
     clear how to give them proper names. *)
  let next_agent ~name =
    List.find (fun agent -> Agent.name agent = name) agents |> Lwt.return
  in
  let* t = init ~configuration cloud next_agent in
  toplog "Starting main loop" ;
  Lwt.bind
    (Network.get_level (Node.as_rpc_endpoint t.bootstrap.node))
    (let rec loop level =
       let level = succ level in
       let* () = on_new_level t level in
       loop level
     in
     loop)
