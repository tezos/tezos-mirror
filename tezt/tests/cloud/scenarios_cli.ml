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

let section =
  Clap.section
    ~description:
      "All the options related to running DAL scenarions onto the cloud"
    "Cloud DAL"

let blocks_history =
  Clap.default_int
    ~section
    ~long:"blocks-history"
    ~description:"Number of blocks history kept in memory. Default value: 100"
    100

let metrics_retention =
  let parse str =
    try
      let suffix = String.get str (String.length str - 1) in
      let factor =
        match suffix with
        | 's' -> 1
        | 'm' -> 60
        | 'h' -> 60 * 60
        | 'd' -> 24 * 60 * 60
        | _ -> raise Not_found
      in
      String.sub str 0 (String.length str - 1)
      |> int_of_string |> ( * ) factor |> Option.some
    with _ -> None
  in
  let show d = string_of_int d in
  let typ = Clap.typ ~name:"time" ~dummy:0 ~parse ~show in
  Clap.default
    typ
    ~section
    ~long:"metrics-retention"
    ~placeholder:"<integer>s OR <integer>m OR <integer>h OR <integer>d ..."
    ~description:
      "Define the characteristic time of the exponential moving average. \
       Default value: 12h."
    (* retention is half of a day. *)
    (12 * 60 * 60)

let fundraiser =
  Clap.optional_string
    ~section
    ~long:"fundraiser"
    ~description:"Fundraiser secret key that has enough money on test network"
    ()

let network_typ : Network.t Clap.typ =
  Clap.typ
    ~name:"network"
    ~dummy:`Ghostnet
    ~parse:(function
      | "mainnet" -> Some `Mainnet
      | "ghostnet" -> Some `Ghostnet
      | s when String.length s = 20 && String.sub s 0 10 = "weeklynet-" ->
          let date = String.sub s 10 10 in
          Some (`Weeklynet date)
      | "sandbox" -> Some `Sandbox
      | _ -> None)
    ~show:Network.to_string

let network =
  Clap.default
    ~section
    ~long:"network"
    ~placeholder:"<network> (sandbox,ghostnet,weeklynet-YYYY-MM-DD,...)"
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
       be assigned to the nth stake specified with [--stake]. If less machine \
       types are specified, the default one will be used."
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
       incremented by one, starting from `0`, unless `--producer-slot-indices` \
       is provided. In that case, producers use the specified indices, and if \
       the list is exhausted, the indices continue incrementing from the last \
       specified index. For example, to start 5 producers from index 5, use \
       `--producers 5 --producer-slot-indices 5`."
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
       <disconnect_frequency> levels, and each will reconnect after a delay of \
       <levels_disconnected> levels."
    disconnect_typ
    ()

let etherlink_dal_slots = Clap.list_int ~section ~long:"etherlink-dal-slots" ()

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
    ~description:"Use the octez release <tag> instead of local octez binaries."
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
      "The bootstrap node identity file. Warning: this argument may be removed \
       in a future release."
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
      "In proxy mode, when one wants to reuse an already existing VM, binaries \
       are not updated. That's the desired default behaviour, but if the user \
       wants to update them, this option provides a possibility to do so.\n\
       Furthermore, it is not the recommended way to do so, but this option \
       also allows to use a docker image without binaries (like the provided \
       debian one) and to copy the local binaries to the proxy."
    false

module Alerts = struct
  let section =
    Clap.section
      ~description:"CLI arguments defining alert managing options"
      "Cloud alerting"

  let dal_slack_webhook =
    Clap.optional_string
      ~section
      ~long:"dal-slack-webhook"
      ~description:"The slack webhook url to send the alerts on"
      ()
end
