(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Clap = struct
  include Clap

  let parse_list ~sep parse str =
    try str |> String.split_on_char sep |> List.map parse |> Option.some
    with _ -> None

  let show_list ~sep show l =
    l |> List.map show |> String.concat (String.make 1 sep)

  let list ?(sep = ',') ?(dummy = []) ~name parse show =
    let parse str = parse_list ~sep parse str in
    let show = show_list ~sep show in
    Clap.typ ~name ~dummy ~parse ~show

  let list_of_int ?dummy name = list ~name ?dummy int_of_string string_of_int

  let list_of_list ?(sep_out = ',') ?(sep_in = ';') ?(dummy = []) ~name
      parse_elm show_elm =
    let parse_inner str =
      let trimmed =
        let str = String.trim str in
        let len = String.length str in
        if String.get str 0 = '[' && String.get str (len - 1) = ']' then
          String.sub str 1 (len - 2)
        else str
      in
      match parse_list ~sep:sep_in parse_elm trimmed with
      | Some l -> l
      | None -> raise (Invalid_argument str)
    in
    let show_inner = function
      | [] -> "[]"
      | [e] -> show_elm e
      | l -> "[" ^ show_list ~sep:sep_in show_elm l
    in
    list ~sep:sep_out ~dummy ~name parse_inner show_inner

  let list_of_list_of_int ?dummy name =
    list_of_list ~name ?dummy int_of_string string_of_int
end

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

exception Scenario_mismatch

module type Dal = sig
  val blocks_history : int

  val producer_key : string option

  val fundraiser : string option

  val network : Network.t

  val simulate_network : Network_simulation.t

  val snapshot : Snapshot_helpers.t

  val bootstrap : bool

  val stake : Stake_repartition.Dal.t

  val bakers : string list

  val stake_machine_type : string list

  val dal_producers_slot_indices : int list

  val producers : int

  val producers_delay : int

  val producer_machine_type : string option

  val observer_slot_indices : int list

  val observers_multi_slot_indices : int list list

  val observer_machine_type : string list

  val archivers_slot_indices : int list list

  val observer_pkhs : string list

  val protocol : Protocol.t

  val data_dir : string option

  val etherlink : bool

  val etherlink_sequencer : bool

  val etherlink_producers : int

  val etherlink_chain_id : int option

  val echo_rollups : int

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

  val disable_amplification : bool

  val ignore_pkhs : string list

  val ppx_profiling_verbosity : string option

  val ppx_profiling_backends : string list

  val enable_network_health_monitoring : bool

  val tezlink : bool

  val slot_size : int option

  val number_of_slots : int option

  val attestation_lag : int option

  val config : Scenarios_configuration.DAL.t

  val traps_fraction : Q.t option

  val publish_slots_regularly : bool

  val stresstest : Stresstest.t option
end

module Dal () : Dal = struct
  let scenario_name = "DAL"

  let section =
    Clap.section
      ~description:
        "All the options related to running DAL scenarios onto the cloud"
      "DAL"

  let config =
    match Tezt_cloud_cli.scenario_specific_json with
    | None ->
        Data_encoding.Json.destruct Scenarios_configuration.DAL.encoding (`O [])
    | Some (name, options) when name = scenario_name -> (
        try
          Data_encoding.Json.destruct
            Scenarios_configuration.DAL.encoding
            options
        with
        | Json_encoding.Cannot_destruct (_, e) as exn ->
            Log.error
              "Cannot load config file: %s - %s"
              (Printexc.to_string exn)
              (Printexc.to_string e) ;
            raise exn
        | e -> raise e)
    | Some (name, _options) ->
        Log.error
          "Configuration file mismatch. This config file is for scenario %s \
           whereas the command was launched for scenario %s"
          name
          scenario_name ;
        raise Scenario_mismatch

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
      Network_simulation.typ
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

  let stake =
    let open Stake_repartition.Dal in
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
      typ
      (let default =
         if network = `Sandbox && simulate_network = Network_simulation.Disabled
         then Custom [100]
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

  let observers_multi_slot_indices =
    config.observers_multi_slot_indices
    @ Clap.default
        ~section
        ~long:"observers-multi-slot-indices"
        ~placeholder:"[<slot_index>;..],[<slot_index>;...],..."
        ~description:
          "For each list of slots, an observer will be created to observe them."
        (Clap.list_of_list_of_int "observer_multi_slot_indices")
        []

  let observer_machine_type =
    let observer_machine_type_typ =
      Clap.list ~name:"observer_machine_type" ~dummy:["foo"] Fun.id Fun.id
    in
    let from_cli =
      Clap.optional
        ~section
        ~long:"observer-machine-type"
        ~placeholder:"<machine_type>,<machine_type>,<machine_type>, ..."
        ~description:
          "Specify the machine type used by an observer. The nth machine type \
           will be assigned to the nth observer specified with \
           [--observer_slot_indices] or [--observers_multi_slot_indices]. If \
           less machine types are specified, the default one (or the one \
           specified by --machine-type) will be used.\n\
           Note that observers from [--observer_slot_indices] argument will be \
           mapped to the machine type first."
        observer_machine_type_typ
        ()
    in
    Option.fold ~none:config.observer_machine_type ~some:Fun.id from_cli

  let archivers_slot_indices =
    config.archivers_slot_indices
    @ Clap.default
        ~section
        ~long:"archivers-slot-indices"
        ~placeholder:"[<slot_index>;..],[<slot_index>;...],..."
        ~description:
          "For each list of slots, an operator will be created to observe them."
        (Clap.list_of_list_of_int "archivers_slot_indices")
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

  let echo_rollups =
    Clap.default_int
      ~section
      ~long:"echo-rollups"
      ~description:
        "Number of echo-rollup operators to run (0 = disabled). Each operator \
         will spawn its own DAL node observer for each slot from the list \
         given by [--producer-slot-indices]."
      (Option.value ~default:0 config.echo_rollups)

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

  let disable_amplification =
    Clap.flag
      ~section
      ~set_long:"disable-amplification"
      ~description:
        "DAL nodes will not do the missing-shards reconstruction and \
         propagation."
      (Option.value ~default:false config.disable_amplification)

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

  let ppx_profiling_verbosity =
    Option.fold ~none:config.ppx_profiling_verbosity ~some:Option.some
    @@ Clap.optional_string
         ~section
         ~long:"ppx-profiling-verbosity"
         ~description:
           "Enable PPX profiling on all components, with the given level of \
            verbosity. "
         ()

  let ppx_profiling_backends =
    let default = config.ppx_profiling_backends in
    let from_cli =
      Clap.list_string
        ~section
        ~long:"ppx-profiling-backends"
        ~description:
          (sf
             "Select the backends used by the profiler, bypassing the defaults \
              selection: `%s`, and also `prometheus` if `--prometheus` and \
              `opentelemetry` if `--opentelemetry`."
             (String.concat "," default))
        ()
    in
    Option.fold
      ~none:default
      ~some:Fun.id
      (match from_cli with [] -> None | _ -> Some from_cli)

  let enable_network_health_monitoring =
    Clap.flag
      ~section
      ~set_long:"net-health"
      ~set_long_synonyms:["enable-network-health-monitoring"]
      ~description:
        "If specified, the network health monitoring app.\n\
         Recommendation: enable only for public dal bootstrap node deployments"
      (Option.value ~default:false config.enable_network_health_monitoring)

  let slot_size =
    let open Scenarios_helpers in
    let parse s =
      match int_of_string_opt s with
      | None -> Test.fail "Invalid --slot-size value: %s is not an integer" s
      | Some v ->
          if v <= 0 then
            Test.fail "Invalid --slot-size value: %d: must be positive" v ;
          if v mod default_page_size <> 0 then
            Test.fail
              "Invalid --slot-size value: %d: must be a multiple of the \
               cryptobox page size (%d)."
              v
              default_page_size ;
          Some v
    in
    let typ =
      Clap.typ
        ~name:"slot-size"
        ~dummy:default_slot_size
        ~parse
        ~show:string_of_int
    in
    let from_cli =
      Clap.optional
        ~section
        ~long:"slot-size"
        ~description:
          (* TODO: Make this work for network simulation in sandbox and for other networks, using UAUs. *)
          (Format.sprintf
             "Size in bytes of each DAL slot (must be a positive multiple of \
              the cryptobox page size = %d). This value will be overridden in \
              the DAL parameters only for sandbox experiments with network \
              simulation DISABLED."
             default_page_size)
        typ
        ()
    in
    Option.fold ~none:config.slot_size ~some:Option.some from_cli

  let number_of_slots =
    let from_cli =
      Clap.optional_int
        ~section
        ~long:"number-of-slots"
        ~description:
          (* TODO: Make this work for network simulation in sandbox and for other networks, using UAUs. *)
          "Number of DAL slots to use."
        ()
    in
    Option.fold ~none:config.number_of_slots ~some:Option.some from_cli

  let attestation_lag =
    let from_cli =
      Clap.optional_int
        ~section
        ~long:"attestation-lag"
        ~description:
          "Attestation lag - the number of L1 blocks between a slot's \
           publication and its attestation."
        ()
    in
    Option.fold ~none:config.attestation_lag ~some:Option.some from_cli

  let traps_fraction =
    let typ =
      Clap.typ
        ~name:"Q"
        ~dummy:Q.zero
        ~parse:(fun s -> try Some (Q.of_string s) with _ -> None)
        ~show:Q.to_string
    in
    let from_cli =
      Clap.optional
        ~section
        ~long:"traps-fraction"
        ~description:
          "Set the fraction of shards received by the DAL node considered as \
           traps."
        typ
        ()
    in
    Option.fold ~none:config.traps_fraction ~some:Option.some from_cli

  let publish_slots_regularly =
    Clap.flag
      ~section
      ~set_long:"publish-slots-regularly"
      ~description:
        "Let the DAL node handle autonomously (i.e without orchestration) the \
         publication of slots at a regular frequency"
      config.publish_slots_regularly

  let stresstest =
    let from_cli =
      Clap.optional
        ~section
        ~long:"stresstest"
        ~description:
          "Stresstest the L1 network with TPS operations (transfers) per \
           second. One can also provide a seed with the syntax TPS/SEED"
        Stresstest.typ
        ()
    in
    Option.fold ~none:config.stresstest ~some:Option.some from_cli
end

module type Layer1 = sig
  val network : Network.t

  val stake : Stake_repartition.Layer1.t

  val stresstest : Stresstest.t option

  val maintenance_delay : int

  val migration_offset : int option

  val signing_delay : (float * float) option

  val snapshot : Snapshot_helpers.t

  val octez_release : string option

  val vms_config : string option

  val without_dal : bool

  val dal_producers_slot_indices : int list option

  val ppx_profiling_verbosity : string option

  val ppx_profiling_backends : string list

  val auto_faketime : bool
end

module Layer1 () = struct
  let scenario_name = "LAYER1"

  let section =
    Clap.section
      ~description:
        "All the options related to running Layer 1 scenarios onto the cloud."
      "LAYER1"

  let config =
    match Tezt_cloud_cli.scenario_specific_json with
    | None -> None
    | Some (name, options) when name = scenario_name -> (
        try
          Data_encoding.Json.destruct
            Scenarios_configuration.LAYER1.encoding
            options
          |> Option.some
        with
        | Json_encoding.Cannot_destruct (_, e) as exn ->
            Log.error
              "Cannot load config file: %s - %s"
              (Printexc.to_string exn)
              (Printexc.to_string e) ;
            raise exn
        | e -> raise e)
    | Some (name, _options) ->
        Log.error
          "Configuration file mismatch. This config file is for scenario %s \
           whereas the command was launched for scenario %s"
          name
          scenario_name ;
        raise Scenario_mismatch

  let mandatory_from_cli_or_config (type a) (typ : a Clap.typ) ?section ?last
      ?long ?long_synonyms ?short ?short_synonyms ?placeholder ?description
      ~from_config () =
    (* If the config exists, then the parameter is available as it is required
       in the config. In that case, the parameter is optional. Otherwise, it is
       mandatory. *)
    match Option.map from_config config with
    | Some config_param -> (
        let from_cli =
          Clap.optional
            ?section
            ?last
            ?long
            ?long_synonyms
            ?short
            ?short_synonyms
            ?placeholder
            ?description
            typ
            ()
        in
        match from_cli with None -> config_param | Some p -> p)
    | None ->
        Clap.mandatory
          ?section
          ?last
          ?long
          ?long_synonyms
          ?short
          ?short_synonyms
          ?placeholder
          ?description
          typ
          ()

  let network =
    mandatory_from_cli_or_config
      ~section
      ~long:"network"
      ~placeholder:
        "<network> \
         (sandbox,ghostnet,nextnet-YYYY-MM-DD,weeklynet-YYYY-MM-DD,...)"
      ~description:"Allow to specify a network to use for the scenario"
      network_typ
      ~from_config:(fun config -> config.network)
      ()

  let stake =
    mandatory_from_cli_or_config
      ~section
      ~long:"stake"
      ~placeholder:"AUTO|<nb_of_bakers>|<stake1>,<stake2>,...,<stakeN>"
      ~description:
        "With AUTO, each delegate will run its own baker node. With only one \
         integer, you give the number of bakers to run, and stake will be \
         distributed as evenly as possible among all these bakers. Otherwise, \
         you can specify the stake distribution you want to achieve by giving \
         a list of relative weight. Delegates will be distributed amongst \
         pools in order to (approximately) respect the given stake \
         distribution."
      Stake_repartition.Layer1.typ
      ~from_config:(fun config -> config.stake)
      ()

  let stresstest =
    let from_cli =
      Clap.optional
        ~section
        ~long:"stresstest"
        ~placeholder:"TPS[/seed]"
        ~description:
          "A Public key hash and its public key are automatically retrieved \
           from the yes wallet to fund fresh accounts for reaching TPS \
           stresstest traffic generation. A seed for stresstest initialization \
           can also be specified."
        Stresstest.typ
        ()
    in
    let from_config =
      Option.fold
        ~none:None
        ~some:(fun (c : Scenarios_configuration.LAYER1.t) -> c.stresstest)
        config
    in
    Option.fold ~none:from_config ~some:Option.some from_cli

  let auto_faketime =
    Clap.flag
      ~section
      ~set_long:"auto-faketime"
      ~unset_long:"no-auto-faketime"
      ~description:
        "If set (it is by default), automatically set the FAKETIME based on \
         the snapshot timestamp"
      (Option.value
         ~default:Scenarios_configuration.LAYER1.Default.auto_faketime
      @@ Option.map
           (fun (c : Scenarios_configuration.LAYER1.t) -> c.auto_faketime)
           config)

  let maintenance_delay =
    let default_maintenance_delay =
      Scenarios_configuration.LAYER1.Default.maintenance_delay
    in
    let from_config =
      Option.map
        (fun (c : Scenarios_configuration.LAYER1.t) -> c.maintenance_delay)
        config
    in
    Clap.default_int
      ~section
      ~long:"maintenance-delay"
      ~placeholder:"N"
      ~description:
        (sf
           "Each baker has maintenance delayed by (position in the list * N). \
            Default is %d. Use 0 for disabling mainteance delay"
           default_maintenance_delay)
      (Option.value ~default:default_maintenance_delay from_config)

  let migration_offset =
    let from_cli =
      Clap.optional_int
        ~section
        ~long:"migration-offset"
        ~description:
          "After how many levels we will perform a UAU to upgrade to the next \
           protocol."
        ()
    in
    let from_config =
      Option.fold
        ~none:None
        ~some:(fun (c : Scenarios_configuration.LAYER1.t) -> c.migration_offset)
        config
    in
    Option.fold ~none:from_config ~some:Option.some from_cli

  let signing_delay =
    let typ =
      let parse string =
        try
          match String.split_on_char ',' string with
          | [max] -> Some (0., float_of_string max)
          | [min; max] -> Some (float_of_string min, float_of_string max)
          | _ -> None
        with _ -> None
      in
      let show (min, max) = Format.sprintf "%f,%f" min max in
      Clap.typ ~name:"signing-delay" ~dummy:(0., 0.) ~parse ~show
    in
    let from_cli =
      Clap.optional
        ~section
        ~long:"signing-delay"
        ~placeholder:"<min>,<max>"
        ~description:
          "Introduce a random signing delay between <min> and <max> seconds. \
           This is useful when simulating a network with multiple bakers to \
           avoid having all bakers trying to sign at the same time. If only \
           one value is provided, the minimum is set to 0."
        typ
        ()
    in
    let from_config =
      Option.fold
        ~none:None
        ~some:(fun (c : Scenarios_configuration.LAYER1.t) -> c.signing_delay)
        config
    in
    Option.fold ~none:from_config ~some:Option.some from_cli

  let fixed_random_seed =
    let from_cli = Tezt.Cli.Options.seed in
    let from_config =
      Option.fold
        ~none:None
        ~some:(fun (c : Scenarios_configuration.LAYER1.t) ->
          c.fixed_random_seed)
        config
    in
    Option.fold ~none:from_config ~some:Option.some from_cli

  let snapshot =
    mandatory_from_cli_or_config
      ~section
      ~long:"snapshot"
      ~description:
        "Snapshot file, which is stored locally, to initiate the scenario with \
         some data"
      snapshot_typ
      ~from_config:(fun c -> c.snapshot)
      ()

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
    let from_config =
      Option.fold
        ~none:None
        ~some:(fun (c : Scenarios_configuration.LAYER1.t) -> c.octez_release)
        config
    in
    Option.fold ~none:from_config ~some:Option.some from_cli

  let vms_config =
    Clap.optional_string
      ~section
      ~long:"vms"
      ~description:
        "JSON file optionally describing options for each VM involved in the \
         test"
      ()

  let without_dal =
    let from_config =
      Option.map
        (fun (c : Scenarios_configuration.LAYER1.t) -> c.without_dal)
        config
    in
    Clap.flag
      ~section
      ~set_long:"without-dal"
      ~description:
        "Disable running DAL nodes on bootstrap and bakers nodes. It is set to \
         `false` by default."
      (Option.value
         ~default:Scenarios_configuration.LAYER1.Default.without_dal
         from_config)

  let dal_producers_slot_indices =
    let from_cli =
      Clap.optional
        ~section
        ~long:"producer-slot-indices"
        ~description:
          "Specify the slot indices for DAL producers to run. The number of \
           DAL producers run is the size of the list."
        (Clap.list_of_int ~dummy:[] "producer_slot_indices")
        ()
    in
    let from_config =
      Option.fold
        ~none:None
        ~some:(fun (c : Scenarios_configuration.LAYER1.t) ->
          c.dal_node_producers)
        config
    in
    Option.fold ~none:from_config ~some:Option.some from_cli

  let ppx_profiling_verbosity =
    let from_cli =
      Clap.optional_string
        ~section
        ~long:"ppx-profiling-verbosity"
        ~description:
          "Enable PPX profiling on all components, with the given level of \
           verbosity. "
        ()
    in
    let from_config =
      Option.fold
        ~none:None
        ~some:(fun (c : Scenarios_configuration.LAYER1.t) ->
          c.ppx_profiling_verbosity)
        config
    in
    Option.fold ~none:from_config ~some:Option.some from_cli

  let ppx_profiling_backends =
    let default =
      Scenarios_configuration.LAYER1.Default.ppx_profiling_backends
    in
    let from_config =
      Option.fold
        ~none:default
        ~some:(fun (c : Scenarios_configuration.LAYER1.t) ->
          c.ppx_profiling_backends)
        config
    in
    let from_cli =
      Clap.list_string
        ~section
        ~long:"ppx-profiling-backends"
        ~description:
          (sf
             "Select the backends used by the profiler, bypassing the defaults \
              selection: `%s`, and also `prometheus` if `--prometheus` and \
              `opentelemetry` if `--opentelemetry`."
             (String.concat "," default))
        ()
    in
    from_config @ from_cli
end

module type Tezlink = sig
  val proxy_localhost : bool

  val public_rpc_port : int option

  val tzkt_api_port : int option

  val tzkt : bool

  val faucet : bool

  val verbose : bool

  val time_between_blocks : Tezos.Evm_node.time_between_blocks

  val activate_ssl : bool

  val parent_dns_domain : string option

  val external_sequencer_endpoint : string option

  val deposit_frontend : bool

  val bridge_contract : string option

  val rollup_address : string option

  val l1_network : string option

  val l1_endpoint : string option

  val l1_tzkt_api : string option

  val faucet_private_key : string option
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

  let faucet =
    Clap.flag
      ~section
      ~set_long:"faucet"
      ~unset_long:"no-faucet"
      ~description:"Run a faucet (requires --tzkt)"
      true

  let tzkt_api_port =
    Clap.optional_int
      ~section
      ~long:"tzkt-api-port"
      ~description:
        "Set the port number of the TzKT API. Requires the tzkt option"
      ()

  let tzkt =
    Clap.flag
      ~section
      ~set_long:"tzkt"
      ~unset_long:"no-tzkt"
      ~description:"Run the TzKT indexer and API"
      true

  let verbose =
    Clap.flag
      ~section
      ~set_long:"verbose-node"
      ~unset_long:"discreet-node"
      ~description:"Make the EVM node verbose"
      true

  let time_between_blocks =
    let s =
      Clap.default_string
        ~section
        ~long:"time-between-blocks"
        ~description:
          "Interval (in seconds) at which the sequencer creates an empty block \
           by default. If set to `none`, blocks are produced on demand only \
           (see octez-evm-node private method produceBlock). (default: 8.)"
        ~placeholder:"<float>"
        "8."
    in
    if s = "none" then Tezos.Evm_node.Nothing
    else
      match Float.of_string_opt s with
      | None ->
          Clap.error
            (sf
               "unrecognized value \"%s\": option --time-between-blocks only \
                accepts \"none\" or a float"
               s) ;
          (* `Tezos.Evm_node.Nothing` is a placeholder and will be ignored as we registered an error in Clap *)
          Tezos.Evm_node.Nothing
      | Some f -> Time_between_blocks f

  let activate_ssl =
    Clap.flag
      ~section
      ~set_long:"activate-ssl"
      ~unset_long:"deactivate-ssl"
      ~description:
        "Generate SSL certificates and serve over HTTPS the following \
         services: sequencer RPC endpoint and TzKT API (if enabled). Requires \
         the dns-domain option."
      false

  let parent_dns_domain =
    Clap.optional_string
      ~section
      ~long:"parent-dns-domain"
      ~description:
        "When this option is set, it is used to compute the DNS of all \
         services as subdomains of this parent domain. However, all used \
         domains need to be registered with the dns-domain option."
      ()

  let external_sequencer_endpoint =
    Clap.optional_string
      ~section
      ~long:"external-sequencer-endpoint"
      ~description:
        "Don't run a sequencer and instead have services point to an external \
         one"
      ()

  let deposit_frontend =
    Clap.flag
      ~section
      ~set_long:"deposit-frontend"
      ~unset_long:"no-deposit-frontend"
      ~description:
        "Run the deposit frontend for Tezlink (requires tzkt and \
         external-sequencer-endpoint on a non sandbox Tezlink)"
      false

  let l1_endpoint =
    Clap.optional_string
      ~section
      ~long:"l1-endpoint"
      ~description:"The L1 endpoint on which the Tezlink rollup is running"
      ()

  let l1_network =
    Clap.optional_string
      ~section
      ~long:"l1-network"
      ~description:"The L1 network on which the Tezlink rollup is running"
      ()

  let bridge_contract =
    Clap.optional_string
      ~section
      ~long:"bridge-contract"
      ~description:
        "The bridge contract that will be called by the Tezlink frontend \
         deposit"
      ()

  let rollup_address =
    Clap.optional_string
      ~section
      ~long:"rollup-address"
      ~description:"The address of the Tezlink rollup originated on l1_endpoint"
      ()

  let l1_tzkt_api =
    Clap.optional_string
      ~section
      ~long:"l1-tzkt-api"
      ~description:
        "The url of the tzkt api for the Layer 1 chain on which Tezlink is \
         running"
      ()

  let faucet_private_key =
    Clap.optional_string
      ~section
      ~long:"faucet-private-key"
      ~description:
        "Set the private key used for the faucet account (requires --faucet)"
      ()
end

module type Etherlink = sig
  val kernel : [`Mainnet | `Custom of string]

  val network : string option

  val evm_node_version : [`Latest | `V of string]

  include Etherlink_benchmark_lib.Benchmark_utils.PARAMETERS
end

module Etherlink () : Etherlink = struct
  let section =
    Clap.section
      ~description:
        "All the options related to running Etherlink scenarios onto the cloud"
      "Etherlink"

  let kernel =
    let s =
      Clap.default_string
        ~section
        ~long:"kernel"
        ~description:
          "Path to wasm kernel for Etherlink node. (default: mainnet)"
        ~placeholder:"<mainnet|path>"
        "mainnet"
    in
    if s = "mainnet" then `Mainnet else `Custom s

  let network =
    let typ =
      Clap.enum
        "network"
        (List.map (fun x -> (x, x)) ["mainnet"; "testnet"; "braeburn"])
    in
    Clap.optional
      typ
      ~section
      ~long:"network"
      ~description:"Start a sandbox with a context from this network"
      ~placeholder:"<mainnet|testnet|braeburn>"
      ()

  let evm_node_version =
    Clap.default
      (Clap.typ
         ~name:"EVM node version"
         ~dummy:`Latest
         ~parse:(function
           | "latest" -> Some `Latest
           | s when String.length s >= 1 && s.[0] = 'v' ->
               Some (`V (String.sub s 1 (String.length s - 1)))
           | s -> Some (`V s))
         ~show:(function `Latest -> "latest" | `V s -> "v" ^ s))
      ~section
      ~long:"evm-node-version"
      ~description:"Choose the EVM node version to use for the benchmark"
      ~placeholder:"<latest|vX.Y>"
      `Latest

  include Etherlink_benchmark_lib.Benchmark_utils.Parameters ()
end
