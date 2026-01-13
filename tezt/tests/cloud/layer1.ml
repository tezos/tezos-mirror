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
open Agent_kind

type baker_account = {
  delegate : Account.key;
  consensus : Account.key list;
  companion : Account.key list;
}

(* Public key hashes of a delegate consensus and companion keys *)
let baking_keys delegate =
  let pkhs_of_account_list =
    List.map (fun account -> account.Account.public_key_hash)
  in
  pkhs_of_account_list delegate.consensus
  @ pkhs_of_account_list delegate.companion

let wait_next_level ?(offset = 1) node =
  Lwt.bind
    (Network.get_level (Node.as_rpc_endpoint node))
    (fun level -> Node.wait_for_level node (level + offset))

let extract_agent_index (r : rex) agent =
  match Agent.name agent =~* r with
  | None ->
      Test.fail "Failed to extract index from agent name %s" (Agent.name agent)
  | Some i -> int_of_string i

let match_agent_name r agent = Agent.name agent =~ r

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
  let init_bootstrap_node_from_snapshot ~peers ~ppx_profiling_verbosity
      ~ppx_profiling_backends (agent, node, name) snapshot network
      migration_offset =
    let* snapshot =
      ensure_snapshot ~agent ~name ~network:(Network.to_public network) snapshot
    in
    let* version = get_snapshot_info_version node snapshot in
    let* () =
      if version >= 9 then
        let config =
          Node_helpers.isolated_config
            ~auto_synchronisation_threshold:false
            ~auto_connections:true
            ~no_bootstrap_peers:true
            ~peers
            ~network
            ~delay:0
        in
        let* () = Node.config_init node config in
        let* () =
          import_snapshot ~env:yes_crypto_env ~no_check:true ~name node snapshot
        in
        Lwt.return_unit
      else
        let toplog s = toplog "/!\\ %s /!\\" s in
        toplog "Bootstrapping node using the real world" ;
        let config =
          [
            Network Network.(to_octez_network_options network);
            Expected_pow 26;
            Cors_origin "*";
          ]
        in
        let* () = Node.config_init node config in
        let* () = import_snapshot ~no_check:true ~name node snapshot in
        let* () =
          (* When bootstrapping from the real network, we want to use the real time. *)
          let env = String_map.(add "FAKETIME" "+0" empty) in
          run ~env node []
        in
        let* () = wait_for_ready node in
        let* _new_level = wait_next_level ~offset:2 node in
        let* () = terminate node in
        toplog "Reset node config for private a yes-crypto network" ;
        let config =
          Node_helpers.isolated_config
            ~auto_synchronisation_threshold:true
            ~auto_connections:true
            ~no_bootstrap_peers:true
            ~peers
            ~network
            ~delay:0
        in
        let* () = Node.config_reset node config in
        let () =
          let peers_file = sf "%s/peers.json" (Node.data_dir node) in
          let runner = Node.runner node in
          if Runner.Sys.file_exists ?runner peers_file then (
            toplog (sf "Removing %s" peers_file) ;
            Runner.Sys.remove ?runner peers_file)
          else toplog (sf "%s Not found" peers_file)
        in
        let () =
          let identity_file = sf "%s/identity.json" (Node.data_dir node) in
          let runner = Node.runner node in
          if Runner.Sys.file_exists ?runner identity_file then (
            toplog (sf "Removing %s" identity_file) ;
            Runner.Sys.remove ?runner identity_file)
          else toplog (sf "%s Not found" identity_file)
        in
        Lwt.return_unit
    in
    let* () =
      Node_helpers.may_add_migration_offset_to_config
        node
        snapshot
        ~migration_offset
        ~network
    in
    let arguments =
      (if version >= 9 then
         (* The auto synchronistaion threshold is set to false so that the
             bootstrap node will be directly bootstraped *)
         [Synchronisation_threshold 0]
       else [ (* Synchronisation_threshold 0 *) ])
      @ Node_helpers.isolated_args ~private_mode:false peers
    in
    let* () =
      Node.Agent.run
        ~env:yes_crypto_env
        node
        arguments
        ~ppx_profiling_verbosity
        ~ppx_profiling_backends
    in
    wait_for_ready node

  (** Initialize a node, which means:
      - configure it for a private network with yes-crypto enabled
      - import the relevant snapshot
      - run it with yes-crypto enabled and allowed peer lists
  *)
  let init_node_from_snapshot ~delay ~peers ~ppx_profiling_verbosity
      ~ppx_profiling_backends ~snapshot ~network ~migration_offset
      (agent, node, name) =
    let* snapshot =
      ensure_snapshot ~agent ~name ~network:(Network.to_public network) snapshot
    in
    let config =
      Node_helpers.isolated_config
        ~auto_synchronisation_threshold:true
        ~auto_connections:true
        ~no_bootstrap_peers:true
        ~peers
        ~network
        ~delay
    in
    let* () = Node.config_init node config in
    let* () =
      Node_helpers.may_add_migration_offset_to_config
        node
        snapshot
        ~migration_offset
        ~network
    in
    let* () =
      import_snapshot ~env:yes_crypto_env ~no_check:true ~name node snapshot
    in
    let arguments = Node_helpers.isolated_args ~private_mode:false peers in
    let synchronisation_waiter =
      Node.wait_for_synchronisation ~statuses:["synced"; "stuck"] node
    in
    let* () =
      Node.Agent.run
        ~env:yes_crypto_env
        ~ppx_profiling_verbosity
        ~ppx_profiling_backends
        node
        arguments
    in
    let* () = wait_for_ready node in
    (* As we are playing with dates in the past,
       disconnected from real network (i.e. in a frozen state) you are likely to
       be [stuck] (i.e. synchronised with peer but missing newer blocks) until
       your bakers start to bake. *)
    synchronisation_waiter

  let client ~local ~node agent =
    let name = Tezt_cloud.Agent.name agent ^ "-client" in
    let endpoint =
      if local then Client.Foreign_endpoint (Node.as_rpc_endpoint ~local node)
      else Client.Node node
    in
    Client.Agent.create ~name ~endpoint agent

  (** Initialize the node,
      create the associated client,
      create the yes-wallet.
  *)
  let init_bootstrap_node ?stresstest ?dal_node_producers
      ~ppx_profiling_verbosity ~ppx_profiling_backends ~peers ~snapshot ~network
      ~migration_offset (agent, node, name) =
    toplog "Initializing an L1 node (public network): %s" name ;
    let* () =
      init_bootstrap_node_from_snapshot
        ~peers
        ~ppx_profiling_verbosity
        ~ppx_profiling_backends
        (agent, node, name)
        snapshot
        network
        migration_offset
    in
    let* client = client ~local:true ~node agent in
    (* Build a temporary yes_wallet from the context to retrieve delegate keys.
       The wallet is cleared afterwards. *)
    let* yes_wallet = Node_helpers.yes_wallet agent in
    let* _filename =
      Yes_wallet.create_from_context
        ?rich_accounts:
          (match (stresstest, dal_node_producers) with
        | None, None -> None
        | Some _, Some l -> Some (List.length l + 1, 1_000_000)
        | Some _, None -> Some (1, 1_000_000)
        | None, Some l -> Some (List.length l, 1_000_000))
          (* If the stresstest argument is set we try to find an account with at least 1M tez *)
        ~node
        ~client
        ~network:(Network.to_octez_network_options network)
        yes_wallet
    in
    let* delegates, consensus_keys, companion_keys, rich_account_opt =
      let* known_addresses = Client.list_known_addresses client in
      let get_account alias = Client.show_address ~alias client in
      let cons key value map =
        String_map.update
          key
          (function
            | None -> Some [value] | Some values -> Some (value :: values))
          map
      in
      (Lwt_list.fold_left_s
         (fun
           ((delegates, consensus_keys, companion_keys, rich_accounts) as acc)
           (alias, _pkh)
         ->
           if alias =~ rex "rich_" then
             (* if the alias match "rich_n" *)
             let* rich_account = get_account alias in
             return
               ( delegates,
                 consensus_keys,
                 companion_keys,
                 rich_account :: rich_accounts )
           else
             match alias =~* rex "(\\w+)_consensus_key_[0-9]+" with
             | Some consensus_key_alias ->
                 (* if the alias match "baker_n_consensus_key" *)
                 let* consensus_account = get_account alias in
                 return
                   ( delegates,
                     cons consensus_key_alias consensus_account consensus_keys,
                     companion_keys,
                     rich_accounts )
             | None -> (
                 match alias =~* rex "(\\w+)_companion_key_[0-9]+" with
                 | Some companion_key_alias ->
                     (* if the alias match "baker_n_companion_key" *)
                     let* consensus_account = get_account alias in
                     return
                       ( delegates,
                         consensus_keys,
                         cons
                           companion_key_alias
                           consensus_account
                           companion_keys,
                         rich_accounts )
                 | None ->
                     if alias =~ rex "^baker_[0-9]+$" then
                       (* otherwise, if the alias match "baker_n" *)
                       let* delegate_account = get_account alias in
                       return
                         ( String_map.add alias delegate_account delegates,
                           consensus_keys,
                           companion_keys,
                           rich_accounts )
                     else (
                       toplog "Discarded unexpected yes_wallet alias: %s" alias ;
                       return acc))))
        (String_map.empty, String_map.empty, String_map.empty, [])
        (List.rev known_addresses)
    in
    let baker_accounts =
      String_map.mapi
        (fun alias delegate ->
          let consensus =
            String_map.find_opt alias consensus_keys
            |> Option.value ~default:[] |> List.cons delegate
          in
          let companion =
            String_map.find_opt alias companion_keys |> Option.value ~default:[]
          in
          {delegate; consensus; companion})
        delegates
    in
    let* () = Client.forget_all_keys client in
    Lwt.return (client, baker_accounts, rich_account_opt)

  (** Initialize the node,
      create the associated client,
      create the yes-wallet.
    *)
  let init_node_with_wallet ?(delay = 0) ~accounts ~peers
      ~ppx_profiling_verbosity ~ppx_profiling_backends ~snapshot ~network
      ~migration_offset (agent, node, name) =
    toplog "Initializing an L1 node (public network): %s" name ;
    let* () =
      init_node_from_snapshot
        ~delay
        ~peers
        ~ppx_profiling_verbosity
        ~ppx_profiling_backends
        ~snapshot
        ~network
        ~migration_offset
        (agent, node, name)
    in
    toplog "L1 node %s initialized" name ;
    let* client =
      (* [~local] is set to false since the client might be called from the localhost/orchestrator *)
      client ~local:false ~node agent
    in
    let* yes_wallet = Node_helpers.yes_wallet agent in
    let* () =
      let client_import_pks =
        Lwt_list.iter_s (fun (account : Account.key) ->
            Client.import_public_key
              ~alias:account.public_key_hash
              ~public_key:account.public_key
              client)
      in
      Lwt_list.iter_s
        (fun account ->
          let* () = client_import_pks account.consensus in
          client_import_pks account.companion)
        accounts
    in
    let* () = Yes_wallet.convert_wallet_inplace ~client yes_wallet in
    Lwt.return client

  (** Prerequisite: the chain is running (i.e. bakers are baking blocks) *)
  let init_stresstest_node ?(delay = 0) ~pkh ~pk ~peers ~ppx_profiling_verbosity
      ~ppx_profiling_backends ~snapshot ~network ~migration_offset ~tps
      (agent, node, name) =
    let* () =
      init_node_from_snapshot
        ~delay
        ~peers
        ~ppx_profiling_verbosity
        ~ppx_profiling_backends
        ~snapshot
        ~network
        ~migration_offset
        (agent, node, name)
    in
    let* client = client ~local:true ~node agent in
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

type bootstrap = {
  agent : Agent.t;
  node : Node.t;
  node_p2p_endpoint : string;
  dal_node : Dal_node.t option;
  dal_node_p2p_endpoint : string option;
  client : Client.t;
}

type baker = {
  agent : Agent.t;
  node : Node.t;
  dal_node : Dal_node.t option;
  baker : Agnostic_baker.t;
  accounts : baker_account list;
}

type stresstester = {
  agent : Agent.t;
  node : Node.t;
  client : Client.t;
  accounts : Account.key list;
}

type 'network t = {
  configuration : Scenarios_configuration.LAYER1.t;
  cloud : Cloud.t;
  bootstrap : bootstrap;
  bakers : baker list;
  producers : Dal_node_helpers.producer list;
  stresstesters : stresstester list;
}

let init_baker_i i (configuration : Scenarios_configuration.LAYER1.t) cloud
    ~peers dal_node_p2p_endpoint (accounts : baker_account list)
    (agent, node, name) =
  let delay = i * configuration.maintenance_delay in
  let* client =
    toplog "init_baker: Initialize node" ;
    let name = name ^ "-node" in
    Node.init_node_with_wallet
      ~accounts
      ~delay
      ~peers
      ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
      ~ppx_profiling_backends:configuration.ppx_profiling_backends
      ~snapshot:configuration.snapshot
      ~network:configuration.network
      ~migration_offset:
        (Option.map
           (fun ({migration_offset; _} : Protocol_migration.t) ->
             migration_offset)
           configuration.migration)
      (agent, node, name)
  in
  let* dal_node =
    if configuration.without_dal then Lwt.return_none
    else (
      toplog "init_baker: Initialize dal node" ;
      let name = name ^ "-dal-node" in
      let* dal_node = Dal_node.Agent.create ~name cloud agent ~node in
      let attester_profiles =
        List.map (fun {delegate; _} -> delegate.public_key_hash) accounts
      in
      let* () =
        Dal_node.init_config
          ~expected_pow:26.
          ~attester_profiles
          ~peers:[dal_node_p2p_endpoint |> Option.get]
          dal_node
      in
      let* () =
        Dal_node.Agent.run
          ~event_level:`Notice
          ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
          dal_node
      in
      Lwt.return_some dal_node)
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
          Random.init (seed + i) ;
          (* each baker will have a different seed *)
          fixed_seed_env env
    in
    let env =
      match configuration.signing_delay with
      | None -> env
      | Some (min, max) -> signing_delay_env min max env
    in
    let dal_node_rpc_endpoint = Option.map Dal_node.as_rpc_endpoint dal_node in
    let* agnostic_baker =
      Agnostic_baker.Agent.init
        ~env
        ~name
        ~delegates:(List.concat_map baking_keys accounts)
        ?dal_node_rpc_endpoint
        ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
        ~ppx_profiling_backends:configuration.ppx_profiling_backends
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
  Lwt.return {agent; node; dal_node; baker; accounts}

let init_producer_i i (configuration : Scenarios_configuration.LAYER1.t)
    slot_index (account : Account.key) cloud ~peers dal_node_p2p_endpoint
    (agent, node, name) =
  let delay = i * configuration.maintenance_delay in
  let () = toplog "Initializing the DAL producer %s" name in
  let* client =
    let name = name ^ "-node" in
    Node.init_node_with_wallet
      ~accounts:[]
      ~delay
      ~peers
      ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
      ~ppx_profiling_backends:configuration.ppx_profiling_backends
      ~snapshot:configuration.snapshot
      ~network:configuration.network
      ~migration_offset:
        (Option.map
           (fun ({migration_offset; _} : Protocol_migration.t) ->
             migration_offset)
           configuration.migration)
      (agent, node, name)
  in
  let* () =
    Client.import_public_key
      ~alias:account.public_key_hash
      ~public_key:account.public_key
      client
  in
  let* dal_node =
    let name = name ^ "-dal-node" in
    let* dal_node = Dal_node.Agent.create ~name cloud agent ~node in
    let* () =
      Dal_node.init_config
        ~expected_pow:26.
        ~observer_profiles:[slot_index]
        ~peers:[dal_node_p2p_endpoint |> Option.get]
        dal_node
    in
    let* () =
      Dal_node.Agent.run
        ~event_level:`Notice
        ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
        ~ppx_profiling_backends:configuration.ppx_profiling_backends
        dal_node
    in
    Lwt.return dal_node
  in
  Lwt.return
    ({client; node; dal_node; account; is_ready = Lwt.return_unit; slot_index}
      : Dal_node_helpers.producer)

let fund_stresstest_accounts ~source client =
  Client.stresstest_fund_accounts_from_source
    ~batch_size:150
    ~batches_per_block:50
    ~env:yes_crypto_env
    ~source_key_pkh:source
    ~initial_amount:(Tez.of_mutez_int64 1_000_000_000L)
    client

let init_stresstest_i i (configuration : Scenarios_configuration.LAYER1.t) ~pkh
    ~pk ~peers (agent, node, name) tps : stresstester Lwt.t =
  let delay = i * configuration.maintenance_delay in
  let* client, accounts =
    toplog "init_stresstest: Initialize node" ;
    Node.init_stresstest_node
      ~pk
      ~pkh
      ~delay
      ~peers
      ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
      ~ppx_profiling_backends:configuration.ppx_profiling_backends
      ~snapshot:configuration.snapshot
      ~network:configuration.network
      ~migration_offset:
        (Option.map
           (fun ({migration_offset; _} : Protocol_migration.t) ->
             migration_offset)
           configuration.migration)
      (agent, node, name)
      ~tps
  in
  Lwt.return {agent; node; client; accounts}

let init_network ~peers (configuration : Scenarios_configuration.LAYER1.t) cloud
    teztale ((agent, node, _) as resources) =
  toplog "init_network: Initializing the bootstrap node" ;
  let* client, delegates, rich_account =
    Node.init_bootstrap_node
      ?stresstest:configuration.stresstest
      ?dal_node_producers:configuration.dal_node_producers
      ~peers
      ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
      ~ppx_profiling_backends:configuration.ppx_profiling_backends
      ~snapshot:configuration.snapshot
      ~network:configuration.network
      ~migration_offset:
        (Option.map
           (fun ({migration_offset; _} : Protocol_migration.t) ->
             migration_offset)
           configuration.migration)
      resources
  in
  let* dal_node =
    if configuration.without_dal then Lwt.return_none
    else (
      toplog "init_network: Initialize the bootstrap dal-node" ;
      let disable_shard_validation = true in
      let* dal_node =
        Dal_node.Agent.create
          ~name:"bootstrap-dal-node"
          cloud
          agent
          ~node
          ~disable_shard_validation
      in
      let* () =
        Dal_node.init_config ~expected_pow:26. ~bootstrap_profile:true dal_node
      in
      let* () = Node.wait_for_ready node in
      let* () =
        Dal_node.Agent.run
          ~wait_ready:false
          ~event_level:`Notice
          ~disable_shard_validation
          ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
          dal_node
      in
      Lwt.return_some dal_node)
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
    {
      agent;
      node;
      node_p2p_endpoint = Node.point_str node;
      dal_node;
      dal_node_p2p_endpoint = Option.map Dal_node.point_str dal_node;
      client;
    }
  in
  Lwt.return (bootstrap, delegates, rich_account)

(** Reserves resources for later usage. *)
let create_node agent cloud =
  let name = Agent.name agent in
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

let run_stresstest stresstesters tps seed =
  (* run the stresstest *)
  Lwt_list.iteri_p
    (fun i {agent; client; accounts; _} ->
      let* filename =
        (* The list of account is too big to be passed by ssh,
                   so we generate the account list locally,
                   copy it the the agent,
                   and pass the filename to stresstest invocation directly *)
        let sources =
          `A
            (List.map
               (fun ({public_key_hash = pkh; _} : Account.key) ->
                 `O [("pkh", `String pkh)])
               accounts)
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

let init ~(configuration : Scenarios_configuration.LAYER1.t) cloud =
  let open Scenarios_configuration.LAYER1 in
  let () = toplog "Init" in
  (* First, we allocate agents and node address/port in order to have the
     peer list known when initializing. *)
  let* ((bootstrap_agent, bootstrap_node, bootstrap_name) as bootstrap) =
    let agent =
      List.find (fun a -> Agent.name a = name_of Bootstrap) (Cloud.agents cloud)
    in
    create_node agent cloud
  in
  let create_nodes fmt =
    Lwt_list.filter_map_p
      (fun agent ->
        if match_agent_name fmt agent then
          create_node agent cloud |> Lwt.map Option.some
        else Lwt.return_none)
      (Cloud.agents cloud)
  in
  let* stresstest_agents = create_nodes Agent_kind.rex_stresstest_index in
  let* baker_agents = create_nodes Agent_kind.rex_baker_index in
  let* producers_agents = create_nodes Agent_kind.rex_producer_index in
  let* teztale = init_teztale cloud bootstrap_agent in
  let* () = init_explorus cloud bootstrap_node in
  let peers : string list =
    List.map
      (fun (_, node, _) -> Node.point_str node)
      (stresstest_agents @ baker_agents @ producers_agents)
  in
  let* bootstrap, baker_accounts, rich_accounts =
    init_network ~peers configuration cloud teztale bootstrap
  in
  let stresstest_rich_account_opt, producer_accounts =
    match configuration.stresstest with
    | None -> (None, rich_accounts)
    | Some _ -> (
        match rich_accounts with a :: l -> (Some a, l) | _ -> assert false)
  in
  let peers =
    [Node.point_str bootstrap_node]
    (* :: peers *)
  in
  let* bakers =
    toplog "Initializing bakers" ;
    let* distribution =
      let baker_accounts =
        String_map.fold (fun _alias a acc -> a :: acc) baker_accounts []
      in
      match configuration.stake with
      | Auto -> Lwt.return (List.map (fun x -> [x]) baker_accounts)
      | Manual [] -> Lwt.return_nil
      | Manual stake ->
          let* accounts =
            toplog
              "init_network: Fetching baker accounts baking power from client" ;
            Lwt_list.map_s
              (fun (baker_account : baker_account) ->
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
                        baker_account.delegate.public_key_hash;
                        "current_baking_power";
                      ]
                      bootstrap.client)
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
        init_baker_i
          i
          ~peers
          configuration
          cloud
          bootstrap.dal_node_p2p_endpoint
          accounts
          agent)
      distribution
  in
  let () =
    toplog "Init: initializing %d DAL producers" (List.length producer_accounts)
  in
  let* producers =
    Lwt_list.mapi_p
      (fun i (((agent, _, _) as producer_info), account) ->
        let slot_index =
          extract_agent_index Agent_kind.rex_producer_index agent
        in
        init_producer_i
          i
          configuration
          slot_index
          account
          cloud
          ~peers
          bootstrap.dal_node_p2p_endpoint
          producer_info)
      (List.combine producers_agents producer_accounts)
  in
  let* stresstesters =
    match (configuration.stresstest, stresstest_rich_account_opt) with
    | None, _ -> Lwt.return_nil
    | Some _, None ->
        Test.fail
          "Stresstest argument was provided but no rich account was found in \
           the yes wallet"
    | ( Some {tps; seed},
        Some ({public_key = pk; public_key_hash = pkh; _} : Account.key) ) ->
        let tps = tps / Stresstest.nb_stresstester configuration.network tps in
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
                (fun ({public_key_hash = pkh; _} : Account.key) ->
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
          match configuration.migration with
          | None -> run_stresstest stresstesters tps seed
          | Some _ -> Lwt.return_unit
        in
        Lwt.return stresstesters
  in
  let* () =
    add_prometheus_source
      ~node:bootstrap_node
      cloud
      bootstrap_agent
      bootstrap_name
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
  Lwt.return {cloud; configuration; bootstrap; bakers; producers; stresstesters}

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
  producers : vm_conf list option;
  stresstester : vm_conf option;
}

let vms_conf_encoding =
  let open Data_encoding in
  conv
    (fun {bootstrap; bakers; producers; stresstester} ->
      (bootstrap, bakers, producers, stresstester))
    (fun (bootstrap, bakers, producers, stresstester) ->
      {bootstrap; bakers; producers; stresstester})
    (obj4
       (opt "bootstrap" vm_conf_encoding)
       (opt "bakers" @@ list vm_conf_encoding)
       (opt "producers" @@ list vm_conf_encoding)
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

let local_snapshot =
  let cache = Hashtbl.create 0 in
  fun ~snapshot ~network ->
    match Hashtbl.find_opt cache (snapshot, network) with
    | Some path -> Lwt.return path
    | None ->
        let* path =
          let open Snapshot_helpers in
          match snapshot with
          | Local_file path -> Lwt.return path
          | Docker_embedded path ->
              toplog "Using locally stored snapshot file: %s" path ;
              Lwt.return path
          | Url url ->
              let path = Temp.file "snapshot_file" in
              download_snapshot ~path ~url ~name:"host" ()
          | No_snapshot ->
              toplog
                "No snapshot provided. Downloading one for network %s"
                (Network.to_string network) ;
              download_snapshot
                ~url:(Network.snapshot_service @@ Network.to_public network)
                ~name:"host"
                ()
        in
        Hashtbl.add cache (snapshot, network) path ;
        Lwt.return path

let snapshot_timestamp ~snapshot ~(network : Network.t) =
  let node = Node.create [] in
  let* snapshot = local_snapshot ~snapshot ~network in
  Snapshot_helpers.get_snapshot_info_timestamp node snapshot

let init_tmp_node snapshot network =
  let* snapshot = local_snapshot ~snapshot ~network in
  let node =
    let name = "tmp-node" in
    let data_dir = Temp.dir name in
    Node.create ~data_dir ~name []
  in
  let* () =
    Node.config_init
      node
      (Node_helpers.isolated_config
         ~auto_synchronisation_threshold:true
         ~auto_connections:true
         ~no_bootstrap_peers:true
         ~peers:[]
         ~network
         ~delay:0)
  in
  let* () =
    Node.snapshot_import ~env:yes_crypto_env ~no_check:true node snapshot
  in
  let* () = Node.run node (Node_helpers.isolated_args ~private_mode:false []) in
  let* () = Node.wait_for_ready node in
  return node

let number_of_bakers ~snapshot ~(network : Network.t) =
  let* node = init_tmp_node snapshot network in
  let* client = Client.init ~endpoint:(Node node) () in
  let* n =
    Lwt.map
      List.length
      (Client.RPC.call client
      @@ RPC.get_chain_block_context_delegates
           ~query_string:[("active", "true")]
           ())
  in
  let* () = Node.terminate node in
  let* () = Tezos_stdlib_unix.Lwt_utils_unix.remove_dir (Node.data_dir node) in
  Lwt.return n

let get_migration_level_offsets (migration : Protocol_migration.t) ~snapshot
    ~(network : Network.t) () =
  let* node = init_tmp_node snapshot network in
  let* client = Client.init ~endpoint:(Node node) () in
  let* current_level =
    Lwt.map
      (fun (current_level : RPC.level) -> current_level.level)
      (Client.RPC.call client @@ RPC.get_chain_block_helper_current_level ())
  in
  let* last_cycle_level =
    Lwt.map
      (fun (levels : RPC.cycle_levels) -> levels.last)
      (Client.RPC.call client
      @@ RPC.get_chain_block_helper_levels_in_current_cycle ())
  in
  let* blocks_per_cycle =
    Lwt.map
      (fun constants -> JSON.(constants |-> "blocks_per_cycle" |> as_int))
      (Client.RPC.call client
      @@ RPC.get_chain_block_context_constants_parametric ())
  in
  let* migration =
    let migration_level_offset =
      match migration.migration_offset with
      | Level_offset l -> l
      | Cycle_offset c ->
          last_cycle_level - current_level + (c * blocks_per_cycle)
    in
    let termination_level_offset =
      match migration.migration_offset with
      | Level_offset l -> l
      | Cycle_offset c -> c * blocks_per_cycle
    in
    return
      ( {
          migration with
          migration_offset = Level_offset migration_level_offset;
          termination_offset = Level_offset termination_level_offset;
        },
        current_level + migration_level_offset,
        current_level + migration_level_offset + termination_level_offset )
  in
  let* () = Node.terminate node in
  let* () = Tezos_stdlib_unix.Lwt_utils_unix.remove_dir (Node.data_dir node) in
  Lwt.return migration

let may_export_snapshot (t : bootstrap)
    (migration : Protocol_migration.t option) level =
  match migration with
  | None -> Lwt.return_unit
  | Some migration when not migration.export_snapshot -> Lwt.return_unit
  | Some _ -> (
      match Tezt_cloud_cli.artifacts_dir with
      | None -> Lwt.return_unit
      | Some artifacts_dir ->
          let snapshot_name = sf "%d.rolling" level in
          let destination =
            Artifact_helpers.local_path
              [artifacts_dir; "migration"; snapshot_name]
          in
          let* () =
            Node.snapshot_export
              ~history_mode:Rolling_history
              ~export_format:Tar
              t.node
              snapshot_name
          in
          Agent.scp
            t.agent
            ~is_directory:true
            ~source:snapshot_name
            ~destination
            `FromRunner)

let register (module Cli : Scenarios_cli.Layer1) =
  let configuration : Scenarios_configuration.LAYER1.t =
    {
      stake = Cli.stake;
      network = Cli.network;
      snapshot = Cli.snapshot;
      stresstest = Cli.stresstest;
      without_dal = Cli.without_dal;
      dal_node_producers = Cli.dal_producers_slot_indices;
      maintenance_delay = Cli.maintenance_delay;
      migration = Cli.migration;
      ppx_profiling_verbosity = Cli.ppx_profiling_verbosity;
      ppx_profiling_backends = Cli.ppx_profiling_backends;
      signing_delay = Cli.signing_delay;
      fixed_random_seed = Tezt.Cli.Options.seed;
      octez_release = Cli.octez_release;
      auto_faketime = Cli.auto_faketime;
    }
  in
  let with_dal_producers =
    match configuration.dal_node_producers with
    | None | Some [] -> false
    | _ -> true
  in
  if configuration.without_dal && with_dal_producers then
    Test.fail
      "Dal producer indices provided but DAL network disabled with \
       `--without-dal`" ;
  let vms_conf = Option.map (parse_conf vms_conf_encoding) Cli.vms_config in
  toplog "Parsing CLI done" ;
  let default_docker_image =
    Option.map
      (fun tag -> Agent.Configuration.Octez_release {tag})
      configuration.octez_release
  in
  let default_vm_configuration ?machine_type ~name () =
    Agent.Configuration.make
      ?machine_type
      ?docker_image:default_docker_image
      ~name
      ()
  in
  let make_vm_conf ~name = function
    | None ->
        default_vm_configuration
          ?machine_type:
            (if String.equal name "bootstrap" then Some "n2-standard-8"
             else None)
          ~name
          ()
    | Some {machine_type; docker_image; max_run_duration; binaries_path; os} ->
        let docker_image =
          match docker_image with
          | None -> default_docker_image
          | _ -> docker_image
        in
        let os = Option.map Types.Os.of_string_exn os in
        Agent.Configuration.make
          ?machine_type:
            (if String.equal name "bootstrap" then Some "n2-standard-8"
             else machine_type)
          ?docker_image
          ~max_run_duration
          ?binaries_path
          ?os
          ~name
          ()
  in
  let vms () =
    let* vms =
      let init n = List.init n (fun i -> Baker i) in
      let* bakers =
        match configuration.stake with
        | Stake_repartition.Layer1.Auto ->
            Lwt.map
              init
              (number_of_bakers
                 ~snapshot:configuration.snapshot
                 ~network:configuration.network)
        | Manual stake -> Lwt.return (init (List.length stake))
      in
      Lwt.return
      @@ Bootstrap
         :: (bakers
            @ (match configuration.dal_node_producers with
              | Some dal_node_producers ->
                  List.map (fun i -> Producer i) dal_node_producers
              | None -> [])
            @
            match configuration.stresstest with
            | None -> []
            | Some {tps; _} ->
                let n = Stresstest.nb_stresstester configuration.network tps in
                List.init n (fun i -> Stresstest i))
    in
    Lwt.return
    @@ List.map
         (fun kind ->
           match kind with
           | Bootstrap ->
               make_vm_conf ~name:(name_of Bootstrap)
               @@ Option.bind vms_conf (fun {bootstrap; _} -> bootstrap)
           | Stresstest j ->
               make_vm_conf ~name:(name_of (Stresstest j))
               @@ Option.bind vms_conf (fun {stresstester; _} -> stresstester)
           | Baker i ->
               make_vm_conf ~name:(name_of (Baker i))
               @@ Option.bind vms_conf (function
                    | {bakers = Some bakers; _} -> List.nth_opt bakers i
                    | {bakers = None; _} -> None)
           | Producer i ->
               make_vm_conf ~name:(name_of (Producer i))
               @@ Option.bind vms_conf (function
                    | {producers = Some producers; _} ->
                        List.nth_opt producers i
                    | {producers = None; _} -> None)
           | _ -> assert false)
         vms
  in
  Cloud.register
  (* Docker images are pushed before executing the test in case binaries
     are modified locally. This way we always use the latest ones. *)
    ~vms
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
  let* configuration, _migration_level, termination_level =
    match configuration.migration with
    | None -> return (configuration, None, None)
    | Some migration ->
        let* migration, migration_level, termination_level =
          get_migration_level_offsets
            migration
            ~snapshot:configuration.snapshot
            ~network:configuration.network
            ()
        in
        return
          ( {configuration with migration = Some migration},
            Some migration_level,
            Some termination_level )
  in
  toplog "Prepare artifacts directory and export the full configuration" ;
  Artifact_helpers.prepare_artifacts
    ~scenario_config:
      ( "LAYER1",
        Data_encoding.Json.construct
          Scenarios_configuration.LAYER1.encoding
          configuration )
    () ;
  toplog "Creating the agents" ;
  let agents = Cloud.agents cloud in
  toplog "Created %d agents" (List.length agents) ;
  let* () =
    if configuration.auto_faketime && Tezt_cloud_cli.faketime = None then
      let* snapshot_timestamp =
        snapshot_timestamp
          ~network:configuration.network
          ~snapshot:configuration.snapshot
      in
      let timestamp, _, _ =
        Result.get_ok (Ptime.of_rfc3339 snapshot_timestamp)
      in
      let now = Ptime_clock.now () in
      let offset =
        Ptime.(to_float_s timestamp -. to_float_s now) |> truncate |> sf "%+d"
      in
      Lwt_list.iter_s
        (fun agent -> Tezt_cloud.Cloud.set_faketime agent offset)
        agents
    else Lwt.return_unit
  in
  let* t = init ~configuration cloud in
  toplog "Starting main loop" ;
  let produce_slot (t : 'network t) level =
    Lwt_list.iter_p
      (fun (producer : Dal_node_helpers.producer) ->
        let index = producer.slot_index in
        toplog "Producing DAL commitment for slot %d" index ;
        let content =
          Format.asprintf "%d:%d" level index
          |> Dal_common.Helpers.make_slot ~padding:false ~slot_size:131072
        in
        let* _ = Node.wait_for_level producer.node level in
        let fee = 800 in
        let* _commitment =
          (* A dry-run of the "publish dal commitment" command for each tz kinds outputs:
             - tz1: fees of 513µtz and 1333 gas consumed
             - tz2: fees of 514µtz and 1318 gas consumed
             - tz3: fees of 543µtz and 1607 gas consumed
             - tz4: fees of 700µtz and 2837 gas consumed
             We added a (quite small) margin to it. *)
          Dal_common.Helpers.publish_and_store_slot
            ~fee
            ~gas_limit:3000
            ~dont_wait:true
            producer.client
            producer.dal_node
            producer.account
            ~force:true
            ~index
            content
        in
        Lwt.return_unit)
      t.producers
  in
  let should_terminate level =
    match termination_level with
    | None -> false
    | Some termination_level -> level >= termination_level
  in
  let may_start_stresstesting t stresstesting_started migration_level level =
    if stresstesting_started then Lwt.return_true
    else
      match migration_level with
      | None -> Lwt.return_true
      | Some migration_level ->
          if level > migration_level - 100 then
            match configuration.stresstest with
            | None -> Lwt.return_true
            | Some {tps; seed} ->
                let tps =
                  tps / Stresstest.nb_stresstester configuration.network tps
                in
                let* () = run_stresstest t.stresstesters tps seed in
                Lwt.return_true
          else Lwt.return stresstesting_started
  in
  Lwt.bind
    (Network.get_level (Node.as_rpc_endpoint t.bootstrap.node))
    (let rec loop stresstesting_started with_producers level =
       let level = succ level in
       toplog "Loop at level %d" level ;
       let* stresstesting_started =
         may_start_stresstesting t stresstesting_started migration_level level
       in
       if should_terminate level then
         let* () =
           may_export_snapshot t.bootstrap configuration.migration level
         in
         Lwt.return_unit
       else
         let* () =
           if with_producers then
             let* _ = Node.wait_for_level t.bootstrap.node level in
             Lwt.return_unit
           else produce_slot t level
         in
         loop stresstesting_started with_producers level
     in
     loop (migration_level = None) (List.compare_length_with t.producers 0 = 0))
