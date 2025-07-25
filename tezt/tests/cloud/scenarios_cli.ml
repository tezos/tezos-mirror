(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Clap = struct
  include Clap

  let list ?(sep = ',') ?(dummy = []) ~name parse show =
    let parse str =
      try str |> String.split_on_char sep |> List.map parse |> Option.some
      with _ -> None
    in
    let show l = l |> List.map show |> String.concat (String.make 1 sep) in
    Clap.typ ~name ~dummy ~parse ~show

  let list_of_int ?dummy name = list ~name ?dummy int_of_string string_of_int
end

let parse_network_simulation_config_from_args simulate_network_arg =
  let is_positive_param p =
    if p > 0 then p
    else
      Test.fail
        "Unexpected value provided, [%d], from argument [%s]. Values must be \
         positive integers.@."
        p
        simulate_network_arg
  in
  let is_arg1_sup_eq_arg2 arg1 arg2 =
    if arg1 >= arg2 then ()
    else
      Test.fail
        "Unexpected value provided for argument [%s]. %d must be greater or \
         equal to %d."
        simulate_network_arg
        arg1
        arg2
  in
  let re_scatter = Str.regexp "\\(scatter\\)(\\([^,]+\\),\\([^)]*\\))" in
  let re_map = Str.regexp "\\(map\\)(\\([^,]+\\),\\([^)]*\\),\\([^)]*\\))" in
  if Str.string_match re_scatter simulate_network_arg 0 then
    let arg1 =
      Str.matched_group 2 simulate_network_arg
      |> int_of_string |> is_positive_param
    in
    let arg2 =
      Str.matched_group 3 simulate_network_arg
      |> int_of_string |> is_positive_param
    in
    let () = is_arg1_sup_eq_arg2 arg1 arg2 in
    Some (Scenarios_configuration.Scatter (arg1, arg2))
  else if Str.string_match re_map simulate_network_arg 0 then
    let arg1 =
      Str.matched_group 2 simulate_network_arg
      |> int_of_string |> is_positive_param
    in
    let arg2 =
      Str.matched_group 3 simulate_network_arg
      |> int_of_string |> is_positive_param
    in
    let arg3 =
      Str.matched_group 4 simulate_network_arg
      |> int_of_string |> is_positive_param
    in
    let () = is_arg1_sup_eq_arg2 arg1 (arg2 + arg3) in
    Some (Scenarios_configuration.Map (arg1, arg2, arg3))
  else
    Test.fail
      "Unexpected network simulation config (--simulation) [%s]"
      simulate_network_arg

let network_typ : Network.t Clap.typ =
  Clap.typ
    ~name:"network"
    ~dummy:`Ghostnet
    ~parse:Network.parse
    ~show:Network.to_string

let snapshot_typ : Snapshot_helpers.t Clap.typ =
  let open Snapshot_helpers in
  Clap.typ
    ~name:"snapshot"
    ~dummy:No_snapshot
    ~parse:(fun snapshot ->
      try Some (parse_snapshot (Some snapshot)) with _exn -> None)
    ~show:to_string

module type Dal = sig
  val blocks_history : int

  val producer_key : string option

  val fundraiser : string option

  val network : Network.t

  val simulate_network_typ :
    Scenarios_configuration.network_simulation_config Clap.typ

  val simulate_network : Scenarios_configuration.network_simulation_config

  val snapshot : Snapshot_helpers.t

  val bootstrap : bool

  val stake : Network.stake_repartition

  val bakers : string list

  val stake_machine_type : string list

  val dal_producers_slot_indices : int list

  val producers : int

  val producers_delay : int

  val producer_machine_type : string option

  val observer_slot_indices : int list

  val observer_pkhs : string list

  val protocol : Protocol.t

  val data_dir : string option

  val etherlink : bool

  val etherlink_sequencer : bool

  val etherlink_producers : int

  val etherlink_chain_id : int option

  val echo_rollup : bool

  val disconnect : (int * int) option

  val etherlink_dal_slots : int list

  val teztale : bool

  val octez_release : string option

  val memtrace : bool

  val bootstrap_node_identity_file : string option

  val bootstrap_dal_node_identity_file : string option

  val refresh_binaries : bool

  val node_external_rpc_server : bool

  val with_dal : bool

  val proxy_localhost : bool

  val disable_shard_validation : bool

  val ignore_pkhs : string list

  val ppx_profiling : bool

  val ppx_profiling_backends : string list

  val enable_network_health_monitoring : bool

  val tezlink : bool
end

module Dal () : Dal = struct
  let section =
    Clap.section
      ~description:
        "All the options related to running DAL scenarios onto the cloud"
      "DAL"

  let config =
    Data_encoding.Json.destruct
      Scenarios_configuration.DAL.encoding
      Tezt_cloud_cli.scenario_specific_json

  let blocks_history =
    Clap.default_int
      ~section
      ~long:"blocks-history"
      ~description:"Number of blocks history kept in memory. Default value: 100"
      (Option.value ~default:100 config.blocks_history)

  let fundraiser =
    let from_cli =
      Clap.optional_string
        ~section
        ~long:"fundraiser"
        ~description:
          "Fundraiser secret key that has enough money on test network"
        ()
    in
    Option.fold ~none:config.fundraiser ~some:Option.some from_cli

  let producer_key =
    let from_cli =
      Clap.optional_string
        ~section
        ~long:"producer-key"
        ~description:"Producer secret key that has enough money"
        ()
    in
    Option.fold ~none:config.producer_key ~some:Option.some from_cli

  let producers_delay =
    Clap.default_int
      ~section
      ~long:"producers-delay"
      ~description:
        "Delay in levels between two slot productions. Default is 1 meaning \
         \"produce every level\"."
      (Option.value ~default:1 config.producers_delay)

  let network_typ : Network.t Clap.typ =
    Clap.typ
      ~name:"network"
      ~dummy:`Ghostnet
      ~parse:Network.parse
      ~show:Network.to_string

  let network =
    Clap.default
      ~section
      ~long:"network"
      ~placeholder:
        "<network> \
         (sandbox,ghostnet,nextnet-YYYY-MM-DD,weeklynet-YYYY-MM-DD,...)"
      ~description:"Allow to specify a network to use for the scenario"
      network_typ
      (Option.value ~default:`Sandbox config.network)

  let simulate_network_typ :
      Scenarios_configuration.network_simulation_config Clap.typ =
    Clap.typ
      ~name:"simulate_network"
      ~dummy:Scenarios_configuration.Disabled
      ~parse:parse_network_simulation_config_from_args
      ~show:Scenarios_configuration.simulate_network_to_string

  let simulate_network =
    Clap.default
      ~section
      ~long:"simulate"
      ~description:
        "This option can be used to simulate a network, relying on the actual \
         distribution of rights that will be found in the imported data \
         (data-dir or snapshot). It requires yes crypto to be enabled.\n\
        \ The simulate option has two modes:\n\
        \      - scatter(x,y): selects the [x] biggest bakers found, and \
         scatters their baking rights, in a round robin fashion, on [y] baker \
         daemons. This is particularly useful to scatter the baking power \
         across several baker daemons.\n\
        \      - map(x,y): maps [y-1] keys from the biggest bakers found onto \
         [y-1] baker daemons (these daemons are handling a single key) and \
         gives the remaining keys [x-y-1] to a single baker daemon. This is \
         particularly useful to simulate the behaviour of an actual network.\n\
         Note that, for both scatter and map, if the [x] arguments exceeds the \
         actual number of active baking accounts found in the imported data, \
         everything works fine: the number of baking accounts used will simply \
         be lower than requested.\n\
         For example:\n\
         - scatter(10,2): [[0;2;4;6;8];[1;3;5;7;9]]\n\
         - map(10,3):[[0];[1];[2;3;4;5;6;7;8;9]]"
      simulate_network_typ
      (Option.value ~default:Disabled config.simulate_network)

  let snapshot =
    Clap.default
      ~section
      ~long:"snapshot"
      ~description:
        "Snapshot file, which is stored locally, to initiate the scenario with \
         some data"
      snapshot_typ
      (Option.value ~default:No_snapshot config.snapshot)

  let bootstrap =
    Clap.flag
      ~section
      ~set_long:"bootstrap"
      (let default = match network with `Sandbox -> true | _ -> false in
       Option.value ~default config.bootstrap)

  let stake_repartition_typ : Network.stake_repartition Clap.typ =
    let open Network in
    let parse_public_network (net : string) : public option =
      try Option.map to_public (parse net) with _ -> None
    in
    Clap.typ
      ~name:"stake_repartition"
      ~dummy:(Custom [100])
      ~parse:(fun str ->
        (* If it is a list of int, then a custom repartition has been selected. *)
        let int_list_regexp = Str.regexp {|\([0-9]+,\( ?\)\)*[0-9]+$|} in
        if Str.string_match int_list_regexp str 0 then
          Some
            (Custom (str |> String.split_on_char ',' |> List.map int_of_string))
          (* Else we expect a network name, potentially followed by how many bakers should be created. *)
        else
          match String.split_on_char '_' str with
          | [network] ->
              Option.map
                (fun network -> Mimic {network; max_nb_bakers = None})
                (parse_public_network network)
          | [network; n_str] -> (
              try
                let n = int_of_string n_str in
                Option.map
                  (fun network -> Mimic {network; max_nb_bakers = Some n})
                  (parse_public_network network)
              with _ -> None)
          | _ -> None)
      ~show:(function
        | Custom l ->
            l |> List.map string_of_int |> String.concat (String.make 1 ',')
        | Mimic {network; max_nb_bakers = None} -> to_string network
        | Mimic {network; max_nb_bakers = Some n} ->
            Format.sprintf "%s_%d" (to_string network) n)

  let stake =
    Clap.default
      ~section
      ~long:"stake"
      ~placeholder:"<integer>, <integer>, <integer>, ...|<network>(_<integer>)?"
      ~description:
        "Specify the stake distribution. If a list of integers is provided, \
         each number specifies the number of shares held by one baker. The \
         total stake is proportional to the sum of all shares. If a network is \
         provided share repartitions is the same as on this network (truncated \
         to the N biggest delegates if <network>_<N> is given)."
      stake_repartition_typ
      (let default =
         if network = `Sandbox && simulate_network = Disabled then
           Network.Custom [100]
         else Custom []
       in
       Option.value ~default config.stake)

  let bakers =
    config.bakers
    @ Clap.list_string
        ~section
        ~long:"bakers"
        ~placeholder:"<unencrypted pkh> <unencrypted pkh>"
        ~description:
          "Specify a baker secret key to bake with. While [--stake] is mostly \
           used for private networks, this one can be used on public networks."
        ()

  let stake_machine_type =
    let stake_machine_type_typ =
      Clap.list ~name:"stake_machine_type" ~dummy:["foo"] Fun.id Fun.id
    in
    let from_cli =
      Clap.optional
        ~section
        ~long:"stake-machine-type"
        ~placeholder:"<machine_type>,<machine_type>,<machine_type>, ..."
        ~description:
          "Specify the machine type used by the stake. The nth machine type \
           will be assigned to the nth stake specified with [--stake]. If less \
           machine types are specified, the default one (or the one specified \
           by --machine-type) will be used."
        stake_machine_type_typ
        ()
    in
    Option.fold ~none:config.stake_machine_type ~some:Fun.id from_cli

  let dal_producers_slot_indices =
    config.dal_producers_slot_indices
    @ Clap.default
        ~section
        ~long:"producer-slot-indices"
        ~description:
          "Specify the slot indices for DAL producers to run. The number of \
           DAL producers run is the size of the list unless `--producers` is \
           also specified, in that case it takes precedence over this \
           argument."
        (Clap.list_of_int "producer_slot_indices")
        []

  let producers =
    Clap.default_int
      ~section
      ~long:"producers"
      ~description:
        "Specify the number of DAL producers for this test. Slot indices are \
         incremented by one, starting from `0`, unless \
         `--producer-slot-indices` is provided. In that case, producers use \
         the specified indices, and if the list is exhausted, the indices \
         continue incrementing from the last specified index. For example, to \
         start 5 producers from index 5, use `--producers 5 \
         --producer-slot-indices 5`."
      (Option.value
         ~default:(List.length dal_producers_slot_indices)
         config.producers)

  let producer_machine_type =
    let from_cli =
      Clap.optional_string
        ~section
        ~long:"producer-machine-type"
        ~description:"Machine type used for the DAL producers"
        ()
    in
    Option.fold ~none:config.producer_machine_type ~some:Option.some from_cli

  let observer_slot_indices =
    config.observer_slot_indices
    @ Clap.default
        ~section
        ~long:"observer-slot-indices"
        ~placeholder:"<slot_index>,<slot_index>,<slot_index>, ..."
        ~description:
          "For each slot index specified, an observer will be created to \
           observe this slot index."
        (Clap.list_of_int "observer_slot_indices")
        []

  let observer_pkhs =
    config.observer_pkhs
    @ Clap.list_string
        ~section
        ~long:"observer-pkh"
        ~placeholder:"<pkh>"
        ~description:
          "Enable to run a DAL node following the same topics as the baker pkh \
           given in input"
        ()

  let protocol =
    let protocol_typ =
      let parse string =
        try
          Data_encoding.Json.from_string string
          |> Result.get_ok
          |> Data_encoding.Json.destruct Protocol.encoding
          |> Option.some
        with _ -> None
      in
      let show = Protocol.name in
      Clap.typ ~name:"protocol" ~dummy:Protocol.Alpha ~parse ~show
    in
    Clap.default
      ~section
      ~long:"protocol"
      ~placeholder:"<protocol_name> (such as alpha, oxford,...)"
      ~description:"Specify the economic protocol used for this test"
      protocol_typ
      (Option.value ~default:(Network.default_protocol network) config.protocol)

  let data_dir =
    let from_cli =
      Clap.optional_string
        ~section
        ~long:"data-dir"
        ~placeholder:"<data_dir>"
        ()
    in
    Option.fold ~none:config.data_dir ~some:Option.some from_cli

  let tezlink =
    Clap.flag
      ~section
      ~set_long:"tezlink"
      ~unset_long:"no-tezlink"
      ~description:"Run Tezlink"
      (Option.value ~default:false config.tezlink)

  let etherlink =
    Clap.flag
      ~section
      ~set_long:"etherlink"
      (* If --tezlink is given, there is no need to also pass --etherlink *)
      (Option.value ~default:tezlink config.etherlink)

  let etherlink_sequencer =
    (* We want the sequencer to be active by default if etherlink is activated. *)
    Clap.flag
      ~section
      ~unset_long:"no-etherlink-sequencer"
      (Option.value ~default:etherlink config.etherlink_sequencer)

  let etherlink_producers =
    Clap.default_int
      ~section
      ~long:"etherlink-producers"
      (Option.value ~default:0 config.etherlink_producers)

  let etherlink_chain_id =
    let from_cli = Clap.optional_int ~section ~long:"etherlink-chain-id" () in
    Option.fold ~none:config.etherlink_chain_id ~some:Option.some from_cli

  let echo_rollup =
    Clap.flag
      ~section
      ~set_long:"echo-rollup"
      (Option.value ~default:false config.echo_rollup)

  let disconnect =
    let disconnect_typ =
      let parse string =
        try
          match String.split_on_char ',' string with
          | [disconnection; reconnection] ->
              Some (int_of_string disconnection, int_of_string reconnection)
          | _ -> None
        with _ -> None
      in
      let show (d, r) = Format.sprintf "%d,%d" d r in
      Clap.typ ~name:"disconnect" ~dummy:(10, 10) ~parse ~show
    in
    let from_cli =
      Clap.optional
        ~section
        ~long:"disconnect"
        ~placeholder:"<disconnect_frequency>,<levels_disconnected>"
        ~description:
          "If this argument is provided, bakers will disconnect in turn each \
           <disconnect_frequency> levels, and each will reconnect after a \
           delay of <levels_disconnected> levels."
        disconnect_typ
        ()
    in
    Option.fold ~none:config.disconnect ~some:Option.some from_cli

  let etherlink_dal_slots =
    config.etherlink_dal_slots
    @ Clap.list_int ~section ~long:"etherlink-dal-slots" ()

  let teztale =
    Clap.flag
      ~section
      ~set_long:"teztale"
      ~unset_long:"no-teztale"
      ~description:"Runs teztale"
      (Option.value ~default:false config.teztale)

  let octez_release =
    let from_cli =
      Clap.optional_string
        ~section
        ~long:"octez-release"
        ~placeholder:"<tag>"
        ~description:
          "Use the octez release <tag> instead of local octez binaries."
        ()
    in
    Option.fold ~none:config.octez_release ~some:Option.some from_cli

  let memtrace =
    Clap.flag
      ~section
      ~set_long:"memtrace"
      ~description:"Use memtrace on all the services"
      (Option.value ~default:false config.memtrace)

  let bootstrap_node_identity_file =
    let from_cli =
      Clap.optional_string
        ~section
        ~long:"bootstrap-node-identity"
        ~description:
          "The bootstrap node identity file. Warning: this argument may be \
           removed in a future release."
        ()
    in
    Option.fold
      ~none:config.bootstrap_node_identity_file
      ~some:Option.some
      from_cli

  let bootstrap_dal_node_identity_file =
    let from_cli =
      Clap.optional_string
        ~section
        ~long:"bootstrap-dal-node-identity"
        ~description:
          "The bootstrap DAL node identity file. Warning: this argument may be \
           removed in a future release."
        ()
    in
    Option.fold
      ~none:config.bootstrap_dal_node_identity_file
      ~some:Option.some
      from_cli

  let refresh_binaries =
    Clap.flag
      ~section
      ~set_long:"refresh-binaries"
      ~description:
        "In proxy mode, when one wants to reuse an already existing VM, \
         binaries are not updated. That's the desired default behaviour, but \
         if the user wants to update them, this option provides a possibility \
         to do so.\n\
         Furthermore, it is not the recommended way to do so, but this option \
         also allows to use a docker image without binaries (like the provided \
         debian one) and to copy the local binaries to the proxy."
      (Option.value ~default:false config.refresh_binaries)

  let node_external_rpc_server =
    Clap.flag
      ~section
      ~set_long:"node-external-rpc-server"
      ~unset_long:"no-node-external-rpc-server"
      ~description:"Use the external RPC server on the L1 nodes"
      (Option.value ~default:false config.node_external_rpc_server)

  let with_dal =
    Clap.flag
      ~section
      ~set_long:"dal"
      ~unset_long:"no-dal"
      ~description:
        "No bootstrap DAL node is run and bakers do not run a DAL node \
         (default is 'false'). DAL nodes can be activated via other options \
         such as [--producers]."
      (Option.value ~default:true config.with_dal)

  let proxy_localhost =
    Clap.flag
      ~section
      ~set_long:"proxy-localhost"
      ~unset_long:"no-proxy-localhost"
      ~description:
        "All agents run on the proxy VM if the proxy mode is activated. This \
         can be used to solve a bug with the Tezt Cloud library. This option \
         will be removed once the bug is fixed"
      (Option.value ~default:false config.proxy_localhost)

  let disable_shard_validation =
    Clap.flag
      ~section
      ~set_long:"disable-shard-validation"
      ~description:"All DAL nodes will bypass the shard validation stage."
      (Option.value ~default:false config.disable_shard_validation)

  let ignore_pkhs =
    config.ignore_pkhs
    @ Clap.list_string
        ~section
        ~long:"ignore-pkhs"
        ~placeholder:"<pkh> <pkh>"
        ~description:
          "Specify a list of public key hashes for which all the producers \
           will not publish the associated shards."
        ()

  let ppx_profiling =
    Clap.flag
      ~section
      ~set_long:"ppx-profiling"
      ~description:
        "Enable PPX profiling on all components. The level of verbosity is by \
         default `Debug` and the format of the output is `txt`. "
      (Option.value ~default:false config.ppx_profiling)

  let ppx_profiling_backends =
    config.ppx_profiling_backends
    @ Clap.list_string
        ~section
        ~long:"ppx-profiling-backends"
        ~description:
          "Select the backends used by the profiler, bypassing the defaults \
           selection: always `txt` and `json`, and also `prometheus` if \
           `--prometheus` and `opentelemetry` if `--opentelemetry`."
        ()

  let enable_network_health_monitoring =
    Clap.flag
      ~section
      ~set_long:"net-health"
      ~set_long_synonyms:["enable-network-health-monitoring"]
      ~description:
        "If specified, the network health monitoring app.\n\
         Recommendation: enable only for public dal bootstrap node deployments"
      (Option.value ~default:false config.enable_network_health_monitoring)
end

module type Layer1 = sig
  val network : Network.t option

  val stake : int list option

  val stresstest : (string * string * int * int) option

  val default_maintenance_delay : int

  val maintenance_delay : int option

  val migration_offset : int option

  val snapshot : Snapshot_helpers.t option

  val octez_release : string option

  val vms_config : string option

  val config : string option
end

module Layer1 () = struct
  (** Keep the CLI arguments optional because they can be defined in a
      config file. Parameters consistency will be checked in the Layer1
      scenario. *)

  let section =
    Clap.section
      ~description:
        "All the options related to running Layer 1 scenarios onto the cloud. \
         Note that in order to run these tests, you need to specify both \
         [cloud] and [layer1] tags on the command line"
      "LAYER1"

  let network : Network.t option =
    Clap.optional
      ~section
      ~long:"network"
      ~placeholder:"<ghostnet|mainnet>"
      ~description:"Allow to specify a network to use for the scenario"
      network_typ
      ()

  let stake =
    Clap.optional
      ~section
      ~long:"stake"
      ~placeholder:"<integer>,<integer>,<integer>,..."
      ~description:
        "By default, each delegate will run its own baker node. If that is \
         what you want, --stake option only takes one integer that is the \
         number of active bakers on the network (you need to know that number \
         before starting the experiment). If you want to aggregate delegates \
         into pools, use a comma-separated list of integers representing \
         relative weights defining the expected stake repartition. Delegates \
         will be distributed amongst pools in order to (approximately) respect \
         the given stake distribution."
      (Clap.list_of_int ~dummy:[] "stake")
      ()

  let stresstest =
    let typ =
      let parse string =
        try
          match string |> String.split_on_char '/' with
          | [pkh; pk; n] -> Some (pkh, pk, int_of_string n, Random.int max_int)
          | [pkh; pk; n; seed] ->
              Some (pkh, pk, int_of_string n, int_of_string seed)
          | _ -> None
        with _ -> None
      in
      let show (pkh, pk, tps, seed) =
        pkh ^ "/" ^ pk ^ "/" ^ string_of_int tps ^ "/" ^ string_of_int seed
      in
      Clap.typ ~name:"stresstest" ~dummy:("", "", 0, 0) ~parse ~show
    in
    Clap.optional
      ~section
      ~long:"stresstest"
      ~placeholder:"pkh/pk/TPS[/seed]"
      ~description:
        "Public key hash / public key of an account used to fund fresh \
         accounts for reaching TPS stresstest traffic generation. A seed for \
         stresstest initialization can also be specified."
      typ
      ()

  let default_maintenance_delay = 1

  let maintenance_delay =
    Clap.optional_int
      ~section
      ~long:"maintenance-delay"
      ~placeholder:"N"
      ~description:
        (sf
           "Each baker has maintenance delayed by (position in the list * N). \
            Default is %d. Use 0 for disabling mainteance delay"
           default_maintenance_delay)
      ()

  let migration_offset =
    Clap.optional_int
      ~section
      ~long:"migration-offset"
      ~description:
        "After how many levels we will perform a UAU to upgrade to the next \
         protocol."
      ()

  let snapshot =
    Clap.optional
      ~section
      ~long:"snapshot"
      ~description:
        "Either a path the a local file or url of the snapshot to use for \
         bootstrapping the experiment"
      snapshot_typ
      ()

  let octez_release =
    Clap.optional_string
      ~section
      ~long:"octez-release"
      ~placeholder:"<tag>"
      ~description:
        "Use the octez release <tag> instead of local octez binaries."
      ()

  let vms_config =
    Clap.optional_string
      ~section
      ~long:"vms"
      ~description:
        "JSON file optionally describing options for each VM involved in the \
         test"
      ()

  let config =
    Clap.optional_string
      ~section
      ~long:"config"
      ~description:
        "JSON file optionally describing options for the test scenario"
      ()
end

module type Tezlink = sig
  val proxy_localhost : bool

  val public_rpc_port : int option
end

module Tezlink () : Tezlink = struct
  let section =
    Clap.section
      ~description:
        "All the options related to running Tezlink sandbox scenarios onto the \
         cloud"
      "Tezlink"

  let proxy_localhost =
    Clap.flag
      ~section
      ~set_long:"proxy-localhost"
      ~unset_long:"no-proxy-localhost"
      ~description:
        "All agents run on the proxy VM if the proxy mode is activated. This \
         can be used to solve a bug with the Tezt Cloud library. This option \
         will be removed once the bug is fixed"
      false

  let public_rpc_port =
    Clap.optional_int
      ~section
      ~long:"public-rpc-port"
      ~description:"Set the port number of the RPC server"
      ()
end
