(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos
open Scenarios_helpers

(** A baker account is its public key (pk) and its hash (pkh) *)
type baker_account = {pk : string; pkh : string}

let add_prometheus_source node cloud agent =
  Scenarios_helpers.add_prometheus_source ~node cloud agent (Agent.name agent)

let get_snapshot_info_level node snapshot_path =
  let* info = Node.snapshot_info node ~json:true snapshot_path in
  let json = JSON.parse ~origin:"snapshot_info" info in
  Lwt.return JSON.(json |-> "snapshot_header" |-> "level" |> as_int)

module Network = struct
  include Network

  type t = [`Mainnet | `Ghostnet]

  let to_string (t : t) = Network.to_string (t :> Network.t)

  let to_octez_network_options (t : t) =
    Network.to_octez_network_options (t :> Network.public)

  let block_time = function `Ghostnet -> 5 | `Mainnet -> 8

  (** Next protocol for both Mainnet and Ghostnet - needs to be updated manually. *)
  let migrate_to _network = Protocol.Alpha
end

let yes_crypto_env =
  String_map.add
    Tezos_crypto.Helpers.yes_crypto_environment_variable
    "y"
    String_map.empty

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
  max n1 n2

module Node = struct
  let runner_of_agent = Agent.runner

  include Node

  (** If snapshot is an URL, download it on the agent's runner.
      If it is a filename, copy it to the agent's runner if needed. *)
  let ensure_snapshot agent snapshot =
    if Re.Str.(string_match (regexp "^https?://.+$") snapshot 0) then
      let url = snapshot in
      let filename = "snapshot" in
      let* () =
        Process.spawn
          ?runner:(runner_of_agent agent)
          "wget"
          ["-q"; "-O"; filename; url]
        |> Process.check
      in
      Lwt.return filename
    else Tezt_cloud.Agent.copy agent ~destination:snapshot ~source:snapshot

  (** We are running a private network with yes-crypto enabled.
      We don't want to connect with the real network.
  *)
  let isolated_config ~peers ~network ~delay =
    [
      No_bootstrap_peers;
      Connections (List.length peers);
      Synchronisation_threshold (if List.length peers < 2 then 1 else 2);
      Network (Network.to_octez_network_options network);
      Expected_pow 0;
      Cors_origin "*";
      Storage_maintenance_delay (string_of_int delay);
    ]

  (** [--private-mode] is mainly useful for the bootstrap node
      because it is used first to bootstrap a node with real network peers
      before being disconnected.
      For the other node, it's an extra security but their ip/identity should
      not be advertised to the external world anyway.
  *)
  let isolated_args peers =
    Private_mode
    :: List.fold_left
         (fun acc peer -> Peer peer :: acc)
         [Allow_yes_crypto; Force_history_mode_switch]
         peers

  (** [add_migration_offset_to_config node snapshot ~migration_offset ~network] adds an
      entry in the configuration file of [node] to trigger a UAU at level [~migration_offset]
      to upgrade to the next protocol of [~network]. This entry is is parametrised by the
      information obtained from [snapshot]. *)
  let add_migration_offset_to_config node snapshot ~migration_offset ~network =
    let* level = get_snapshot_info_level node snapshot in
    match migration_offset with
    | None -> Lwt.return_unit
    | Some migration_offset ->
        let network_config =
          match network with
          | `Mainnet -> Node.Config_file.mainnet_network_config
          | `Ghostnet -> Node.Config_file.ghostnet_network_config
        in
        let migration_level = level + migration_offset in
        toplog "Add UAU entry for level : %d" migration_level ;
        Node.Config_file.update node (fun json ->
            JSON.put
              ( "network",
                JSON.annotate
                  ~origin:"add_migration_offset_to_config"
                  network_config )
              json
            |> Node.Config_file.update_network_with_user_activated_upgrades
                 [(migration_level, Network.migrate_to network)])

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
  let init_bootstrap_node_from_snapshot ~peers (agent, node) snapshot network
      migration_offset =
    let* snapshot = ensure_snapshot agent snapshot in
    let* () =
      let toplog s = toplog "/!\\ %s /!\\" s in
      toplog "Bootstrapping node using the real world" ;
      let config =
        [
          Network (Network.to_octez_network_options network);
          Expected_pow 26;
          Cors_origin "*";
          Synchronisation_threshold 1;
        ]
      in
      let* () = Node.config_init node config in
      let* () = Node.snapshot_import ~no_check:true node snapshot in
      let* () =
        (* When bootstrapping from the real network, we want to use the real time. *)
        let env = String_map.(add "FAKETIME" "+0" empty) in
        run ~env node []
      in
      let* () = wait_for_ready node in
      let* _new_level = wait_next_level ~offset:2 node in
      let* () = terminate node in
      Lwt.return_unit
    in
    toplog "Reset node config for private a yes-crypto network" ;
    let config = isolated_config ~peers ~network ~delay:0 in
    let* () = Node.config_reset node config in
    let* () =
      add_migration_offset_to_config node snapshot ~migration_offset ~network
    in
    let arguments = isolated_args peers in
    let* () = run ~env:yes_crypto_env node arguments in
    wait_for_ready node

  (** Initialize a node, which means:
      - configure it for a private network with yes-crypto enabled
      - import the relevant snapshot
      - run it with yes-crypto enabled and allowed peer lists
  *)
  let init_node_from_snapshot ~delay ~peers ~snapshot ~network ~migration_offset
      (agent, node) =
    let* snapshot = ensure_snapshot agent snapshot in
    let config = isolated_config ~peers ~network ~delay in
    let* () = Node.config_init node config in
    let* () =
      add_migration_offset_to_config node snapshot ~migration_offset ~network
    in
    let* () = Node.snapshot_import ~no_check:true node snapshot in
    let arguments = isolated_args peers in
    let* () = run ~env:yes_crypto_env node arguments in
    let* () = wait_for_ready node in
    (* As we are playing with dates in the past,
       disconnected from real network (i.e. in a frozen state) you are likely to
       be [stuck] (i.e. synchronised with peert but missing newer blocks) until
       your bakers start to bake. *)
    Node.wait_for_synchronisation ~statuses:["synced"; "stuck"] node

  let client ~node agent =
    let name = Tezt_cloud.Agent.name agent ^ "-client" in
    Client.Agent.create ~name ~endpoint:(Client.Node node) agent

  let yes_wallet agent =
    let name = Tezt_cloud.Agent.name agent ^ "-yes-wallet" in
    Yes_wallet.Agent.create ~name agent

  (** Initialize the node,
      create the associated client,
      create the yes-wallet.
  *)
  let init_bootstrap_node ~peers ~snapshot ~network ~migration_offset
      (agent, node, name) =
    toplog "Initializing an L1 node (public network): %s" name ;
    let* () =
      init_bootstrap_node_from_snapshot
        ~peers
        (agent, node)
        snapshot
        network
        migration_offset
    in
    let* client = client ~node agent in
    let* yes_wallet = yes_wallet agent in
    let* _filename =
      Yes_wallet.create_from_context
        ~node
        ~client
        ~network:(Network.to_string network)
        yes_wallet
    in
    let* delegates =
      Client.list_known_addresses client |> Lwt.map (List.map snd)
    in
    let* () = Client.forget_all_keys client in
    Lwt.return (client, delegates)

  (** Initialize the node,
      create the associated client,
      create the yes-wallet.
    *)
  let init_baker_node ?(delay = 0) ~accounts ~peers ~snapshot ~network
      ~migration_offset (agent, node, name) =
    toplog "Initializing an L1 node (public network): %s" name ;
    let* () =
      init_node_from_snapshot
        ~delay
        ~peers
        ~snapshot
        ~network
        ~migration_offset
        (agent, node)
    in
    let* client = client ~node agent in
    let* yes_wallet = yes_wallet agent in
    let* () =
      Lwt_list.iter_s
        (fun {pkh = alias; pk = public_key} ->
          Client.import_public_key ~alias ~public_key client)
        accounts
    in
    let* () = Yes_wallet.convert_wallet_inplace ~client yes_wallet in
    Lwt.return client

  (** Prerequisite: the chain is running (i.e. bakers are baking blocks) *)
  let init_stresstest_node ?(delay = 0) ~pkh ~pk ~peers ~snapshot ~network
      ~migration_offset ~tps (agent, node, name) =
    let* () =
      init_node_from_snapshot
        ~delay
        ~peers
        ~snapshot
        ~network
        ~migration_offset
        (agent, node)
    in
    let* client = client ~node agent in
    let* yes_wallet = yes_wallet agent in
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
    - [pkh]: public key hash of the account to use as a faucet
    - [pk]: public key of the account to use as a faucet
    - [tps]: targeted number of transactions per second
    - [seed]: seed used for stresstest traffic generation
 *)
type stresstest_conf = {pkh : string; pk : string; tps : int; seed : int}

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
  snapshot : string;
  stresstest : stresstest_conf option;
  maintenance_delay : int;
  migration_offset : int option;
}

(** A version of the [configuration] partially defined. *)
type partial_configuration = {
  stake : int list option;
  network : Network.t option;
  snapshot : string option;
  stresstest : stresstest_conf option;
  maintenance_delay : int option;
  migration_offset : int option;
}

let network_encoding =
  Data_encoding.string_enum [("Mainnet", `Mainnet); ("Ghostnet", `Ghostnet)]

let stresstest_encoding =
  let open Data_encoding in
  conv
    (fun {pkh; pk; tps; seed} -> (pkh, pk, tps, seed))
    (fun (pkh, pk, tps, seed) -> {pkh; pk; tps; seed})
    (obj4
       (req "pkh" string)
       (req "pk" string)
       (req "tps" int31)
       (req "seed" int31))

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
         } ->
      (stake, network, snapshot, stresstest, maintenance_delay, migration_offset))
    (fun ( stake,
           network,
           snapshot,
           stresstest,
           maintenance_delay,
           migration_offset ) ->
      {
        stake;
        network;
        snapshot;
        stresstest;
        maintenance_delay;
        migration_offset;
      })
    (obj6
       (opt "stake" @@ list int31)
       (opt "network" network_encoding)
       (opt "snapshot" string)
       (opt "stresstest" stresstest_encoding)
       (opt "maintenance_delay" int31)
       (opt "migration_offset" int31))

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
      ~snapshot:configuration.snapshot
      ~network:configuration.network
      ~migration_offset:configuration.migration_offset
      (agent, node, name)
  in
  let* baker =
    toplog "init_baker: Initialize agnostic baker" ;
    let name = name ^ "-agnostic-baker" in
    let* agnostic_baker =
      Agnostic_baker.Agent.init
        ~env:yes_crypto_env
        ~name
        ~delegates:(List.map (fun ({pkh; _} : baker_account) -> pkh) accounts)
        ~client
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
  let* client, delegates =
    Node.init_bootstrap_node
      ~peers
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
  Lwt.return (bootstrap, delegates)

(** Reserves resources for later usage. *)
let agent_and_node next_agent cloud ~name =
  let* agent = next_agent ~name in
  let* node = Node.Agent.create ~metadata_size_limit:false ~name cloud agent in
  Lwt.return (agent, node, name)

(** Distribute the delegate accounts according to stake specification *)
let distribute_delegates stake baker_accounts =
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

let number_of_bakers ~snapshot ~network cloud agent =
  let* node =
    Node.Agent.create ~metadata_size_limit:false ~name:"tmp-node" cloud agent
  in
  let* snapshot = Node.ensure_snapshot agent snapshot in
  let* () =
    Node.config_init node (Node.isolated_config ~peers:[] ~network ~delay:0)
  in
  let* () = Node.snapshot_import ~no_check:true node snapshot in
  let* () = Node.Agent.run node (Node.isolated_args []) in
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
  let* ((bootstrap_agent, bootstrap_node, _) as bootstrap) =
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
          cloud
          bootstrap_agent
      else Lwt.return (List.length configuration.stake)
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
  let* bootstrap, baker_accounts =
    init_network ~peers configuration cloud teztale bootstrap
  in
  let peers = Node.point_str bootstrap_node :: peers in
  let* bakers =
    toplog "Initializing bakers" ;
    let* distribution =
      match configuration.stake with
      | [] -> List.map (fun a -> [a]) baker_accounts |> Lwt.return
      | stake ->
          let* accounts =
            toplog
              "init_network: Fetching baker accounts baking power from client" ;
            Lwt_list.map_s
              (fun pkh ->
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
                        pkh;
                        "current_baking_power";
                      ]
                      bootstrap.client)
                in
                Lwt.return (pkh, power |> JSON.as_string |> int_of_string))
              baker_accounts
          in
          distribute_delegates stake accounts |> Lwt.return
    in
    let* distribution =
      toplog "init_network: Update consensus keys in use" ;
      Lwt_list.map_s
        (fun accounts ->
          Lwt_list.fold_left_s
            (fun acc pkh ->
              (* FIXME: "pendings" keys if it exists *)
              let* consensus_key =
                Client.rpc
                  RPC_core.GET
                  [
                    "chains";
                    "main";
                    "blocks";
                    "head";
                    "context";
                    "delegates";
                    pkh;
                    "consensus_key";
                  ]
                  bootstrap.client
              in
              let active = JSON.(consensus_key |-> "active") in
              let pkh' = JSON.(active |-> "pkh" |> as_string) in
              let pk = JSON.(active |-> "pk" |> as_string) in
              Lwt.return ({pk; pkh = pkh'} :: acc))
            []
            accounts)
        distribution
    in
    Lwt_list.mapi_p
      (fun i accounts ->
        let ((_, node, _) as agent) = List.nth baker_agents i in
        let peers = List.filter (( <> ) (Node.point_str node)) peers in
        init_baker_i i ~peers configuration cloud accounts agent)
      distribution
  in
  let* stresstesters =
    match configuration.stresstest with
    | None -> Lwt.return_nil
    | Some {pkh; pk; tps; seed} ->
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
  let* () = add_prometheus_source bootstrap_node cloud bootstrap_agent in
  let* () =
    Lwt_list.iter_s
      (fun ({agent; node; _} : baker) -> add_prometheus_source node cloud agent)
      bakers
  in
  let* () =
    Lwt_list.iter_s
      (fun ({agent; node; _} : stresstester) ->
        add_prometheus_source node cloud agent)
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
  let configuration0 : partial_configuration =
    let stake = Cli.stake in
    let network = Cli.network in
    let stresstest =
      Option.map
        (fun (pkh, pk, tps, seed) -> {pkh; pk; tps; seed})
        Cli.stresstest
    in
    let maintenance_delay = Cli.maintenance_delay in
    let snapshot = Cli.snapshot in
    let migration_offset = Cli.migration_offset in
    match Cli.config with
    | Some filename ->
        let conf = parse_conf configuration_encoding filename in
        {
          stake = (if stake <> None then stake else conf.stake);
          network = (if network <> None then network else conf.network);
          snapshot = (if snapshot <> None then snapshot else conf.snapshot);
          stresstest =
            (if stresstest <> None then stresstest else conf.stresstest);
          maintenance_delay =
            (if maintenance_delay <> None then maintenance_delay
             else conf.maintenance_delay);
          migration_offset =
            (if migration_offset <> None then migration_offset
             else conf.migration_offset);
        }
    | None ->
        {
          stake;
          network;
          stresstest;
          maintenance_delay;
          snapshot;
          migration_offset;
        }
  in
  let vms_conf = Option.map (parse_conf vms_conf_encoding) Cli.vms_config in
  toplog "Parsing CLI done" ;
  let vms =
    `Bootstrap
    ::
    (match configuration0.stake with
    | Some [n] -> List.init n (fun i -> `Baker i)
    | Some stake -> List.mapi (fun i _ -> `Baker i) stake
    | None -> [])
    @
    match configuration0.stresstest with
    | None -> []
    | Some {tps; _} ->
        let n =
          match configuration0.network with
          | Some network -> nb_stresstester network tps
          | None -> tps / stresstest_max_tps_pre_node
        in
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
    ~vms
    ~proxy_files:
      ([yes_wallet_exe]
      @
      match configuration0.snapshot with Some snapshot -> [snapshot] | _ -> [])
    ~__FILE__
    ~title:"L1 simulation"
    ~tags:[]
  @@ fun cloud ->
  let configuration : configuration =
    let stake = Option.get configuration0.stake in
    let network = Option.get configuration0.network in
    let snapshot = Option.get configuration0.snapshot in
    let stresstest = configuration0.stresstest in
    let maintenance_delay =
      Option.value
        ~default:Cli.default_maintenance_delay
        configuration0.maintenance_delay
    in
    let migration_offset = configuration0.migration_offset in
    if stake = [] then Test.fail "stake parameter can not be empty" ;
    if snapshot = "" then Test.fail "snapshot parameter can not be empty" ;
    {stake; network; snapshot; stresstest; maintenance_delay; migration_offset}
  in
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
