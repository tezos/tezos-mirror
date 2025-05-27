(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
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

module type Dal = sig
  val blocks_history : int

  val producer_key : string option

  val fundraiser : string option

  val network_typ : Network.t Clap.typ

  val network : Network.t

  val bootstrap : bool

  val stake : int list

  val bakers : string list

  val stake_machine_type : string list option

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

  val dal_incentives : bool

  val proxy_localhost : bool

  module Monitoring_app : sig
    val slack_channel_id : string option

    val slack_bot_token : string option
  end
end

module Dal () : Dal = struct
  let section =
    Clap.section
      ~description:
        "All the options related to running DAL scenarios onto the cloud"
      "DAL"

  let blocks_history =
    Clap.default_int
      ~section
      ~long:"blocks-history"
      ~description:"Number of blocks history kept in memory. Default value: 100"
      100

  let fundraiser =
    Clap.optional_string
      ~section
      ~long:"fundraiser"
      ~description:"Fundraiser secret key that has enough money on test network"
      ()

  let producer_key =
    Clap.optional_string
      ~section
      ~long:"producer-key"
      ~description:"Producer secret key that has enough money"
      ()

  let producers_delay =
    Clap.default_int
      ~section
      ~long:"producers-delay"
      ~description:
        "Delay in levels between two slot productions. Default is 1 meaning \
         \"produce every level\"."
      1

  let network_typ : Network.t Clap.typ =
    Clap.typ
      ~name:"network"
      ~dummy:`Ghostnet
      ~parse:(function
        | "mainnet" -> Some `Mainnet
        | "ghostnet" -> Some `Ghostnet
        | "rionet" -> Some `Rionet
        | s when String.length s = 20 && String.sub s 0 10 = "weeklynet-" ->
            (* format:  weeklynet-2025-01-29 (with dashes) *)
            let date = String.sub s 10 10 in
            Some (`Weeklynet date)
        | s when String.length s = 16 && String.sub s 0 8 = "nextnet-" ->
            (* format: nextnet-20250203  (without dashes) *)
            let date = String.sub s 8 8 in
            Some (`Nextnet date)
        | "sandbox" -> Some `Sandbox
        | _ -> None)
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
      `Sandbox

  let bootstrap =
    Clap.flag
      ~section
      ~set_long:"bootstrap"
      (match network with `Sandbox -> true | _ -> false)

  let stake =
    Clap.default
      ~section
      ~long:"stake"
      ~placeholder:"<integer>, <integer>, <integer>, ..."
      ~description:
        "Specify the stake repartition. Each number specifies the number of \
         shares held by one baker. The total stake is given by the sum of all \
         shares."
      (Clap.list_of_int ~dummy:[100] "stake")
      (match network with `Sandbox -> [100] | _ -> [])

  let bakers =
    Clap.list_string
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
    Clap.optional
      ~section
      ~long:"stake-machine-type"
      ~placeholder:"<machine_type>,<machine_type>,<machine_type>, ..."
      ~description:
        "Specify the machine type used by the stake. The nth machine type will \
         be assigned to the nth stake specified with [--stake]. If less \
         machine types are specified, the default one will be used."
      stake_machine_type_typ
      ()

  let dal_producers_slot_indices =
    Clap.default
      ~section
      ~long:"producer-slot-indices"
      ~description:
        "Specify the slot indices for DAL producers to run. The number of DAL \
         producers run is the size of the list unless `--producers` is also \
         specified, in that case it takes precedence over this argument."
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
      (List.length dal_producers_slot_indices)

  let producer_machine_type =
    Clap.optional_string
      ~section
      ~long:"producer-machine-type"
      ~description:"Machine type used for the DAL producers"
      ()

  let observer_slot_indices =
    Clap.default
      ~section
      ~long:"observer-slot-indices"
      ~placeholder:"<slot_index>,<slot_index>,<slot_index>, ..."
      ~description:
        "For each slot index specified, an observer will be created to observe \
         this slot index."
      (Clap.list_of_int "observer_slot_indices")
      []

  let observer_pkhs =
    Clap.list_string
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
      (Network.default_protocol network)

  let data_dir =
    Clap.optional_string ~section ~long:"data-dir" ~placeholder:"<data_dir>" ()

  let etherlink = Clap.flag ~section ~set_long:"etherlink" false

  let etherlink_sequencer =
    (* We want the sequencer to be active by default if etherlink is activated. *)
    Clap.flag ~section ~unset_long:"no-etherlink-sequencer" etherlink

  let etherlink_producers =
    Clap.default_int ~section ~long:"etherlink-producers" 0

  let etherlink_chain_id =
    Clap.optional_int ~section ~long:"etherlink-chain-id" ()

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
    Clap.optional
      ~section
      ~long:"disconnect"
      ~placeholder:"<disconnect_frequency>,<levels_disconnected>"
      ~description:
        "If this argument is provided, bakers will disconnect in turn each \
         <disconnect_frequency> levels, and each will reconnect after a delay \
         of <levels_disconnected> levels."
      disconnect_typ
      ()

  let etherlink_dal_slots =
    Clap.list_int ~section ~long:"etherlink-dal-slots" ()

  let teztale =
    Clap.flag
      ~section
      ~set_long:"teztale"
      ~unset_long:"no-teztale"
      ~description:"Runs teztale"
      false

  let octez_release =
    Clap.optional_string
      ~section
      ~long:"octez-release"
      ~placeholder:"<tag>"
      ~description:
        "Use the octez release <tag> instead of local octez binaries."
      ()

  let memtrace =
    Clap.flag
      ~section
      ~set_long:"memtrace"
      ~description:"Use memtrace on all the services"
      false

  let bootstrap_node_identity_file =
    Clap.optional_string
      ~section
      ~long:"bootstrap-node-identity"
      ~description:
        "The bootstrap node identity file. Warning: this argument may be \
         removed in a future release."
      ()

  let bootstrap_dal_node_identity_file =
    Clap.optional_string
      ~section
      ~long:"bootstrap-dal-node-identity"
      ~description:
        "The bootstrap DAL node identity file. Warning: this argument may be \
         removed in a future release."
      ()

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
      false

  let node_external_rpc_server =
    Clap.flag
      ~section
      ~set_long:"node-external-rpc-server"
      ~unset_long:"no-node-external-rpc-server"
      ~description:"Use the external RPC server on the L1 nodes"
      false

  let with_dal =
    Clap.flag
      ~section
      ~set_long:"dal"
      ~unset_long:"no-dal"
      ~description:
        "No bootstrap DAL node is run and bakers do not run a DAL node \
         (default is 'false'). DAL nodes can be activated via other options \
         such as [--producers]."
      true

  let dal_incentives =
    Clap.flag
      ~section
      ~set_long:"dal-incentives"
      ~unset_long:"no-dal-incentives"
      ~description:"Activate the DAL incentives"
      (* Activate by default DAL incentives on Alpha. *)
      (protocol = Protocol.Alpha)

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

  module Monitoring_app = struct
    let section =
      Clap.section
        ~description:"Define report and alert managing options"
        "Cloud reporting and alerting options"

    let slack_channel_id =
      Clap.optional_string
        ~section
        ~long:"slack-channel-id"
        ~description:"The Slack channel id to send reports and alerts on"
        ()

    let slack_bot_token =
      Clap.optional_string
        ~section
        ~long:"slack-bot-token"
        ~description:"The Slack bot token used to send reports and alerts"
        ()
  end
end

module type Layer1 = sig
  val network : [`Mainnet | `Ghostnet] option

  val stake : int list option

  val stresstest : (string * string * int * int) option

  val default_maintenance_delay : int

  val maintenance_delay : int option

  val migration_offset : int option

  val snapshot : string option

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

  let network : [`Mainnet | `Ghostnet] option =
    let typ =
      Clap.typ
        ~name:"network"
        ~dummy:`Ghostnet
        ~parse:(function
          | "mainnet" -> Some `Mainnet
          | "ghostnet" -> Some `Ghostnet
          | _ -> None)
        ~show:(fun n -> Network.to_string (n :> Network.t))
    in
    Clap.optional
      ~section
      ~long:"network"
      ~placeholder:"<ghostnet|mainnet>"
      ~description:"Allow to specify a network to use for the scenario"
      typ
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
    Clap.optional_string
      ~section
      ~long:"snapshot"
      ~description:
        "Either a path the a local file or url of the snapshot to use for \
         bootstrapping the experiment"
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
