(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Filename.Infix

let home = try Sys.getenv "HOME" with Not_found -> "/root"

let data_dir_env_name = "TEZOS_NODE_DIR"

let default_data_dir = home // ".tezos-node"

let default_rpc_port = 8732

let default_metrics_port = 9932

let default_p2p_port = 9732

let default_discovery_port = 10732

type chain_name = Distributed_db_version.Name.t

type blockchain_network = {
  alias : string option;
  genesis : Genesis.t;
  genesis_parameters : Genesis.Parameters.t option;
  chain_name : chain_name;
  old_chain_name : chain_name option;
  incompatible_chain_name : chain_name option;
  sandboxed_chain_name : chain_name;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  default_bootstrap_peers : string list;
}

let make_blockchain_network ~alias ~chain_name ?old_chain_name
    ?incompatible_chain_name ~sandboxed_chain_name
    ?(user_activated_upgrades = []) ?(user_activated_protocol_overrides = [])
    ?(default_bootstrap_peers = []) ?genesis_parameters genesis =
  let of_string = Distributed_db_version.Name.of_string in
  {
    alias = Some alias;
    genesis;
    genesis_parameters;
    chain_name = of_string chain_name;
    old_chain_name = Option.map of_string old_chain_name;
    incompatible_chain_name = Option.map of_string incompatible_chain_name;
    sandboxed_chain_name = of_string sandboxed_chain_name;
    user_activated_upgrades =
      List.map
        (fun (l, h) -> (l, Protocol_hash.of_b58check_exn h))
        user_activated_upgrades;
    user_activated_protocol_overrides =
      List.map
        (fun (a, b) ->
          (Protocol_hash.of_b58check_exn a, Protocol_hash.of_b58check_exn b))
        user_activated_protocol_overrides;
    default_bootstrap_peers;
  }

(* The script in scripts/user_activated_upgrade.sh patches the following lines
   when it needs to set the user activated upgrade levels for Mainnet. *)
(* BEGIN_PATCHING_ZONE_FOR_MAINNET_USER_ACTIVATED_UPGRADES *)
let mainnet_user_activated_upgrades =
  [
    (28082l, "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt");
    (204761l, "PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP");
  ]

(* END_PATCHING_ZONE_FOR_MAINNET_USER_ACTIVATED_UPGRADES *)
(* it patches the following lines when it needs to set the user activated
   upgrade levels for a sandbox. *)
(* BEGIN_PATCHING_ZONE_FOR_SANDBOX_USER_ACTIVATED_UPGRADES *)
let sandbox_user_activated_upgrades = []
(* END_PATCHING_ZONE_FOR_SANDBOX_USER_ACTIVATED_UPGRADES *)

let blockchain_network_mainnet =
  let giganode_1 = "116.202.172.21" in
  let giganode_2 = "95.216.45.62" in
  make_blockchain_network
    ~alias:"mainnet"
    {
      time = Time.Protocol.of_notation_exn "2018-06-30T16:07:32Z";
      block =
        Block_hash.of_b58check_exn
          "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2";
      protocol =
        Protocol_hash.of_b58check_exn
          "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P";
    }
    ~chain_name:"TEZOS_MAINNET"
    ~old_chain_name:"TEZOS_BETANET_2018-06-30T16:07:32Z"
    ~incompatible_chain_name:"INCOMPATIBLE"
    ~sandboxed_chain_name:"SANDBOXED_TEZOS_MAINNET"
    ~user_activated_upgrades:mainnet_user_activated_upgrades
    ~user_activated_protocol_overrides:
      [
        ( "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU",
          "PsBabyM1eUXZseaJdmXFApDSBqj8YBfwELoxZHHW77EMcAbbwAS" );
        ( "PtEdoTezd3RHSC31mpxxo1npxFjoWWcFgQtxapi51Z8TLu6v6Uq",
          "PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA" );
        ( "PtHangzHogokSuiMHemCuowEavgYTP8J5qQ9fQS793MHYFpCY3r",
          "PtHangz2aRngywmSRGGvrcTyMbbdpWdpFKuS4uMWxg2RaH9i1qx" );
      ]
    ~default_bootstrap_peers:["boot.tzbeta.net"; giganode_1; giganode_2]

let blockchain_network_hangzhounet =
  make_blockchain_network
    ~alias:"hangzhounet"
    {
      time = Time.Protocol.of_notation_exn "2021-11-04T15:00:00Z";
      block =
        Block_hash.of_b58check_exn
          "BLockGenesisGenesisGenesisGenesisGenesis7e8c4d4snJW";
      protocol =
        Protocol_hash.of_b58check_exn
          "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P";
    }
    ~genesis_parameters:
      {
        context_key = "sandbox_parameter";
        values =
          `O
            [
              ( "genesis_pubkey",
                `String "edpkuYLienS3Xdt5c1vfRX1ibMxQuvfM67ByhJ9nmRYYKGAAoTq1UC"
              );
            ];
      }
    ~chain_name:"TEZOS_HANGZHOUNET_2021-11-04T15:00:00Z"
    ~sandboxed_chain_name:"SANDBOXED_TEZOS"
    ~user_activated_upgrades:
      [(8191l, "PtHangz2aRngywmSRGGvrcTyMbbdpWdpFKuS4uMWxg2RaH9i1qx")]
    ~default_bootstrap_peers:
      [
        "hangzhounet.teztnets.xyz";
        "hangzhounet.kaml.fr";
        "hangzhounet.smartpy.io";
        "hangzhounet.tezos.co.il";
        "hangzhounet.boot.tez.ie";
      ]

let blockchain_network_ithacanet =
  make_blockchain_network
    ~alias:"ithacanet"
    {
      time = Time.Protocol.of_notation_exn "2022-01-25T15:00:00Z";
      block =
        Block_hash.of_b58check_exn
          "BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9";
      protocol =
        Protocol_hash.of_b58check_exn
          "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P";
    }
    ~genesis_parameters:
      {
        context_key = "sandbox_parameter";
        values =
          `O
            [
              ( "genesis_pubkey",
                `String "edpkuYLienS3Xdt5c1vfRX1ibMxQuvfM67ByhJ9nmRYYKGAAoTq1UC"
              );
            ];
      }
    ~chain_name:"TEZOS_ITHACANET_2022-01-25T15:00:00Z"
    ~sandboxed_chain_name:"SANDBOXED_TEZOS"
    ~user_activated_upgrades:
      [(8191l, "Psithaca2MLRFYargivpo7YvUr7wUDqyxrdhC5CQq78mRvimz6A")]
    ~default_bootstrap_peers:
      [
        "ithacanet.teztnets.xyz";
        "ithacanet.smartpy.io";
        "ithacanet.kaml.fr";
        "ithacanet.boot.ecadinfra.com";
      ]

let blockchain_network_jakartanet =
  make_blockchain_network
    ~alias:"jakartanet"
    {
      time = Time.Protocol.of_notation_exn "2022-04-27T15:00:00Z";
      block =
        Block_hash.of_b58check_exn
          "BLockGenesisGenesisGenesisGenesisGenesisbd16dciJxo9";
      protocol =
        Protocol_hash.of_b58check_exn
          "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P";
    }
    ~genesis_parameters:
      {
        context_key = "sandbox_parameter";
        values =
          `O
            [
              ( "genesis_pubkey",
                `String "edpkuYLienS3Xdt5c1vfRX1ibMxQuvfM67ByhJ9nmRYYKGAAoTq1UC"
              );
            ];
      }
    ~chain_name:"TEZOS_JAKARTANET_2022-04-27T15:00:00Z"
    ~sandboxed_chain_name:"SANDBOXED_TEZOS"
    ~user_activated_upgrades:
      [(8192l, "PtJakart2xVj7pYXJBXrqHgd82rdkLey5ZeeGwDgPp9rhQUbSqY")]
    ~default_bootstrap_peers:
      [
        "jakartanet.teztnets.xyz";
        "jakartanet.boot.ecadinfra.com";
        "jakartanet.kaml.fr";
        "jakartanet.visualtez.com";
      ]

let blockchain_network_sandbox =
  make_blockchain_network
    ~alias:"sandbox"
    {
      time = Time.Protocol.of_notation_exn "2018-06-30T16:07:32Z";
      block =
        Block_hash.of_b58check_exn
          "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2";
      protocol =
        Protocol_hash.of_b58check_exn
          "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im";
    }
    ~genesis_parameters:
      (* Genesis public key corresponds to the following private key:
         unencrypted:edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6 *)
      {
        context_key = "sandbox_parameter";
        values =
          `O
            [
              ( "genesis_pubkey",
                `String "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2"
              );
            ];
      }
    ~chain_name:"TEZOS"
    ~sandboxed_chain_name:"SANDBOXED_TEZOS"
    ~user_activated_upgrades:sandbox_user_activated_upgrades

let blockchain_network_encoding : blockchain_network Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           alias = _;
           genesis;
           genesis_parameters;
           chain_name;
           old_chain_name;
           incompatible_chain_name;
           sandboxed_chain_name;
           user_activated_upgrades;
           user_activated_protocol_overrides;
           default_bootstrap_peers;
         } ->
      ( genesis,
        genesis_parameters,
        chain_name,
        old_chain_name,
        incompatible_chain_name,
        sandboxed_chain_name,
        user_activated_upgrades,
        user_activated_protocol_overrides,
        default_bootstrap_peers ))
    (fun ( genesis,
           genesis_parameters,
           chain_name,
           old_chain_name,
           incompatible_chain_name,
           sandboxed_chain_name,
           user_activated_upgrades,
           user_activated_protocol_overrides,
           default_bootstrap_peers ) ->
      {
        alias = None;
        genesis;
        genesis_parameters;
        chain_name;
        old_chain_name;
        incompatible_chain_name;
        sandboxed_chain_name;
        user_activated_upgrades;
        user_activated_protocol_overrides;
        default_bootstrap_peers;
      })
    (let chain = Distributed_db_version.Name.encoding in
     obj9
       (req "genesis" Genesis.encoding)
       (opt "genesis_parameters" Genesis.Parameters.encoding)
       (req "chain_name" chain)
       (opt "old_chain_name" chain)
       (opt "incompatible_chain_name" chain)
       (req "sandboxed_chain_name" chain)
       (dft "user_activated_upgrades" User_activated.upgrades_encoding [])
       (dft
          "user_activated_protocol_overrides"
          User_activated.protocol_overrides_encoding
          [])
       (dft
          "default_bootstrap_peers"
          ~description:
            "List of hosts to use if p2p.bootstrap_peers is unspecified."
          (list string)
          []))

let builtin_blockchain_networks_with_tags =
  [
    (1, blockchain_network_sandbox);
    (4, blockchain_network_mainnet);
    (16, blockchain_network_hangzhounet);
    (17, blockchain_network_ithacanet);
    (18, blockchain_network_jakartanet);
  ]
  |> List.map (fun (tag, network) ->
         match network.alias with
         | None -> assert false (* all built-in networks must have aliases *)
         | Some alias -> (tag, alias, network))

let builtin_blockchain_networks =
  List.map
    (fun (_, name, network) -> (name, network))
    builtin_blockchain_networks_with_tags

let sugared_blockchain_network_encoding : blockchain_network Data_encoding.t =
  let open Data_encoding in
  let builtin_encoding (tag, network_alias, network) =
    case
      (Tag tag)
      ~title:network_alias
      (constant network_alias)
      (fun candidate ->
        match candidate.alias with
        | None -> None
        | Some candidate_alias ->
            if String.equal candidate_alias network_alias then Some () else None)
      (fun () -> network)
  in
  (* It is important that built-in networks are listed before the Custom case,
     so that they have priority. Indeed, if possible we want to store the alias
     in the configuration file, not the full network description. Not just because
     it is prettier, but also in case user-activated upgrades are added to the built-in
     network: by writing the alias we ensure that new upgrades are used without having
     to update the configuration file manually. *)
  union
    ~tag_size:`Uint8
    (List.map builtin_encoding builtin_blockchain_networks_with_tags
    @ [
        case
          (Tag 0)
          ~title:"Custom"
          blockchain_network_encoding
          (fun x -> Some x)
          (fun x -> x);
      ])

type t = {
  data_dir : string;
  disable_config_validation : bool;
  p2p : p2p;
  rpc : rpc;
  log : Lwt_log_sink_unix.cfg;
  internal_events : Internal_event_config.t;
  shell : shell;
  blockchain_network : blockchain_network;
  metrics_addr : string list;
}

and p2p = {
  expected_pow : float;
  bootstrap_peers : string list option;
  listen_addr : string option;
  advertised_net_port : int option;
  discovery_addr : string option;
  private_mode : bool;
  limits : P2p.limits;
  disable_mempool : bool;
  enable_testchain : bool;
  reconnection_config : P2p_point_state.Info.reconnection_config;
}

and rpc = {
  listen_addrs : string list;
  cors_origins : string list;
  cors_headers : string list;
  tls : tls option;
  acl : RPC_server.Acl.policy;
  media_type : Media_type.Command_line.t;
}

and tls = {cert : string; key : string}

and shell = {
  block_validator_limits : Block_validator.limits;
  prevalidator_limits : Prevalidator.limits;
  peer_validator_limits : Peer_validator.limits;
  chain_validator_limits : Chain_validator.limits;
  history_mode : History_mode.t option;
}

let default_p2p_limits : P2p.limits =
  let greylist_timeout = Time.System.Span.of_seconds_exn 86400. (* one day *) in
  {
    connection_timeout = Time.System.Span.of_seconds_exn 10.;
    authentication_timeout = Time.System.Span.of_seconds_exn 5.;
    greylist_timeout;
    maintenance_idle_time =
      Time.System.Span.of_seconds_exn 120. (* two minutes *);
    min_connections = 10;
    expected_connections = 50;
    max_connections = 100;
    backlog = 20;
    max_incoming_connections = 20;
    max_download_speed = None;
    max_upload_speed = None;
    read_buffer_size = 1 lsl 14;
    read_queue_size = None;
    write_queue_size = None;
    incoming_app_message_queue_size = None;
    incoming_message_queue_size = None;
    outgoing_message_queue_size = None;
    max_known_points = Some (400, 300);
    max_known_peer_ids = Some (400, 300);
    peer_greylist_size = 1023 (* historical value *);
    ip_greylist_size_in_kilobytes =
      2 * 1024 (* two megabytes has shown good properties in simulation *);
    ip_greylist_cleanup_delay = greylist_timeout;
    swap_linger = Time.System.Span.of_seconds_exn 30.;
    binary_chunks_size = None;
  }

let default_p2p =
  {
    expected_pow = 26.;
    bootstrap_peers = None;
    listen_addr = Some ("[::]:" ^ string_of_int default_p2p_port);
    advertised_net_port = None;
    discovery_addr = None;
    private_mode = false;
    limits = default_p2p_limits;
    disable_mempool = false;
    enable_testchain = false;
    reconnection_config = P2p_point_state.Info.default_reconnection_config;
  }

let default_rpc =
  {
    listen_addrs = [];
    cors_origins = [];
    cors_headers = [];
    tls = None;
    acl = RPC_server.Acl.empty_policy;
    media_type = Media_type.Command_line.Any;
  }

let default_shell =
  {
    block_validator_limits = Node.default_block_validator_limits;
    prevalidator_limits = Node.default_prevalidator_limits;
    peer_validator_limits = Node.default_peer_validator_limits;
    chain_validator_limits = Node.default_chain_validator_limits;
    history_mode = None;
  }

let default_disable_config_validation = false

let default_config =
  {
    data_dir = default_data_dir;
    p2p = default_p2p;
    rpc = default_rpc;
    log = Lwt_log_sink_unix.default_cfg;
    internal_events = Internal_event_config.default;
    shell = default_shell;
    blockchain_network = blockchain_network_mainnet;
    disable_config_validation = default_disable_config_validation;
    metrics_addr = [];
  }

let limit : P2p.limits Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           P2p.connection_timeout;
           authentication_timeout;
           greylist_timeout;
           maintenance_idle_time;
           min_connections;
           expected_connections;
           max_connections;
           backlog;
           max_incoming_connections;
           max_download_speed;
           max_upload_speed;
           read_buffer_size;
           read_queue_size;
           write_queue_size;
           incoming_app_message_queue_size;
           incoming_message_queue_size;
           outgoing_message_queue_size;
           max_known_points;
           max_known_peer_ids;
           peer_greylist_size;
           ip_greylist_size_in_kilobytes;
           ip_greylist_cleanup_delay;
           swap_linger;
           binary_chunks_size;
         } ->
      ( ( ( connection_timeout,
            authentication_timeout,
            min_connections,
            expected_connections,
            max_connections,
            backlog,
            max_incoming_connections,
            max_download_speed,
            max_upload_speed,
            swap_linger ),
          ( binary_chunks_size,
            read_buffer_size,
            read_queue_size,
            write_queue_size,
            incoming_app_message_queue_size,
            incoming_message_queue_size,
            outgoing_message_queue_size,
            max_known_points ) ),
        ( max_known_peer_ids,
          peer_greylist_size,
          ip_greylist_size_in_kilobytes,
          ip_greylist_cleanup_delay,
          greylist_timeout,
          maintenance_idle_time ) ))
    (fun ( ( ( connection_timeout,
               authentication_timeout,
               min_connections,
               expected_connections,
               max_connections,
               backlog,
               max_incoming_connections,
               max_download_speed,
               max_upload_speed,
               swap_linger ),
             ( binary_chunks_size,
               read_buffer_size,
               read_queue_size,
               write_queue_size,
               incoming_app_message_queue_size,
               incoming_message_queue_size,
               outgoing_message_queue_size,
               max_known_points ) ),
           ( max_known_peer_ids,
             peer_greylist_size,
             ip_greylist_size_in_kilobytes,
             ip_greylist_cleanup_delay,
             greylist_timeout,
             maintenance_idle_time ) ) ->
      {
        connection_timeout;
        authentication_timeout;
        greylist_timeout;
        maintenance_idle_time;
        min_connections;
        expected_connections;
        max_connections;
        backlog;
        max_incoming_connections;
        max_download_speed;
        max_upload_speed;
        read_buffer_size;
        read_queue_size;
        write_queue_size;
        incoming_app_message_queue_size;
        incoming_message_queue_size;
        outgoing_message_queue_size;
        max_known_points;
        max_known_peer_ids;
        peer_greylist_size;
        ip_greylist_size_in_kilobytes;
        ip_greylist_cleanup_delay;
        swap_linger;
        binary_chunks_size;
      })
    (merge_objs
       (merge_objs
          (obj10
             (dft
                "connection-timeout"
                ~description:
                  "Delay acceptable when initiating a connection to a new \
                   peer, in seconds."
                Time.System.Span.encoding
                default_p2p_limits.authentication_timeout)
             (dft
                "authentication-timeout"
                ~description:
                  "Delay granted to a peer to perform authentication, in \
                   seconds."
                Time.System.Span.encoding
                default_p2p_limits.authentication_timeout)
             (dft
                "min-connections"
                ~description:
                  "Strict minimum number of connections (triggers an urgent \
                   maintenance)."
                uint16
                default_p2p_limits.min_connections)
             (dft
                "expected-connections"
                ~description:
                  "Targeted number of connections to reach when bootstrapping \
                   / maintaining."
                uint16
                default_p2p_limits.expected_connections)
             (dft
                "max-connections"
                ~description:
                  "Maximum number of connections (exceeding peers are \
                   disconnected)."
                uint16
                default_p2p_limits.max_connections)
             (dft
                "backlog"
                ~description:
                  "Number above which pending incoming connections are \
                   immediately rejected."
                uint8
                default_p2p_limits.backlog)
             (dft
                "max-incoming-connections"
                ~description:
                  "Number above which pending incoming connections are \
                   immediately rejected."
                uint8
                default_p2p_limits.max_incoming_connections)
             (opt
                "max-download-speed"
                ~description:"Max download speeds in KiB/s."
                int31)
             (opt
                "max-upload-speed"
                ~description:"Max upload speeds in KiB/s."
                int31)
             (dft
                "swap-linger"
                Time.System.Span.encoding
                default_p2p_limits.swap_linger))
          (obj8
             (opt "binary-chunks-size" uint8)
             (dft
                "read-buffer-size"
                ~description:"Size of the buffer passed to read(2)."
                int31
                default_p2p_limits.read_buffer_size)
             (opt "read-queue-size" int31)
             (opt "write-queue-size" int31)
             (opt "incoming-app-message-queue-size" int31)
             (opt "incoming-message-queue-size" int31)
             (opt "outgoing-message-queue-size" int31)
             (opt "max_known_points" (tup2 uint16 uint16))))
       (obj6
          (opt
             "max_known_peer_ids"
             ~description:"The max and target size for the known address table."
             (tup2 uint16 uint16))
          (dft
             "peer_greylist_size"
             ~description:"The number of peer_ids kept in the peer_id greylist."
             uint16
             default_p2p_limits.peer_greylist_size)
          (dft
             "ip_greylist_size_in_kilobytes"
             ~description:"The size of the IP address greylist (in kilobytes)."
             uint16
             default_p2p_limits.ip_greylist_size_in_kilobytes)
          (dft
             "ip_greylist_cleanup_delay"
             ~description:"The time an IP address is kept in the greylist."
             Time.System.Span.encoding
             default_p2p_limits.ip_greylist_cleanup_delay)
          (dft
             "greylist-timeout"
             ~description:"GC delay for the greylists tables, in seconds."
             Time.System.Span.encoding
             default_p2p_limits.greylist_timeout)
          (dft
             "maintenance-idle-time"
             ~description:
               "How long to wait at most, in seconds, before running a \
                maintenance loop."
             Time.System.Span.encoding
             default_p2p_limits.maintenance_idle_time)))

let p2p =
  let open Data_encoding in
  conv
    (fun {
           expected_pow;
           bootstrap_peers;
           listen_addr;
           advertised_net_port;
           discovery_addr;
           private_mode;
           limits;
           disable_mempool;
           enable_testchain;
           reconnection_config;
         } ->
      ( expected_pow,
        bootstrap_peers,
        listen_addr,
        advertised_net_port,
        discovery_addr,
        private_mode,
        limits,
        disable_mempool,
        enable_testchain,
        reconnection_config ))
    (fun ( expected_pow,
           bootstrap_peers,
           listen_addr,
           advertised_net_port,
           discovery_addr,
           private_mode,
           limits,
           disable_mempool,
           enable_testchain,
           reconnection_config ) ->
      {
        expected_pow;
        bootstrap_peers;
        listen_addr;
        advertised_net_port;
        discovery_addr;
        private_mode;
        limits;
        disable_mempool;
        enable_testchain;
        reconnection_config;
      })
    (obj10
       (dft
          "expected-proof-of-work"
          ~description:
            "Floating point number between 0 and 256 that represents a \
             difficulty, 24 signifies for example that at least 24 leading \
             zeroes are expected in the hash."
          float
          default_p2p.expected_pow)
       (opt
          "bootstrap-peers"
          ~description:
            "List of hosts. Tezos can connect to both IPv6 and IPv4 hosts. If \
             the port is not specified, default port 9732 will be assumed."
          (list string))
       (opt
          "listen-addr"
          ~description:
            "Host to listen to. If the port is not specified, the default port \
             9732 will be assumed."
          string)
       (opt
          "advertised-net-port"
          ~description:
            "Alternative port advertised to other peers to connect to. If the \
             port is not specified, the port from listen-addr will be assumed."
          uint16)
       (dft
          "discovery-addr"
          ~description:
            "Host for local peer discovery. If the port is not specified, the \
             default port 10732 will be assumed."
          (option string)
          default_p2p.discovery_addr)
       (dft
          "private-mode"
          ~description:
            "Specify if the node is in private mode or not. A node in private \
             mode rejects incoming connections from untrusted peers and only \
             opens outgoing connections to peers listed in 'bootstrap-peers' \
             or provided with '--peer' option. Moreover, these peers will keep \
             the identity and the address of the private node secret."
          bool
          false)
       (dft "limits" ~description:"Network limits" limit default_p2p_limits)
       (dft
          "disable_mempool"
          ~description:
            "If set to [true], the node will not participate in the \
             propagation of pending operations (mempool). Default value is \
             [false]. It can be used to decrease the memory and computation \
             footprints of the node."
          bool
          false)
       (dft
          "enable_testchain"
          ~description:
            "If set to [true], the node will spawn a testchain during the \
             protocol's testing voting period. Default value is [false]. It is \
             disabled to decrease the node storage usage and computation by \
             dropping the validation of the test network blocks."
          bool
          false)
       (let open P2p_point_state.Info in
       dft
         "greylisting_config"
         ~description:
           "The reconnection policy regulates the frequency with which the \
            node tries to reconnect to an old known peer."
         reconnection_config_encoding
         default_reconnection_config))

let rpc : rpc Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {cors_origins; cors_headers; listen_addrs; tls; acl; media_type} ->
      let cert, key =
        match tls with
        | None -> (None, None)
        | Some {cert; key} -> (Some cert, Some key)
      in
      ( Some listen_addrs,
        None,
        cors_origins,
        cors_headers,
        cert,
        key,
        acl,
        media_type ))
    (fun ( listen_addrs,
           legacy_listen_addr,
           cors_origins,
           cors_headers,
           cert,
           key,
           acl,
           media_type ) ->
      let tls =
        match (cert, key) with
        | None, _ | _, None -> None
        | Some cert, Some key -> Some {cert; key}
      in
      let listen_addrs =
        match (listen_addrs, legacy_listen_addr) with
        | Some addrs, None -> addrs
        | None, Some addr -> [addr]
        | None, None -> default_rpc.listen_addrs
        | Some _, Some _ ->
            Stdlib.failwith
              "Config file: Use only \"listen-addrs\" and not (legacy) \
               \"listen-addr\"."
      in
      {listen_addrs; cors_origins; cors_headers; tls; acl; media_type})
    (obj8
       (opt
          "listen-addrs"
          ~description:
            "Hosts to listen to. If the port is not specified, the default \
             port 8732 will be assumed."
          (list string))
       (opt "listen-addr" ~description:"Legacy value: Host to listen to" string)
       (dft
          "cors-origin"
          ~description:
            "Cross Origin Resource Sharing parameters, see \
             https://en.wikipedia.org/wiki/Cross-origin_resource_sharing."
          (list string)
          default_rpc.cors_origins)
       (dft
          "cors-headers"
          ~description:
            "Cross Origin Resource Sharing parameters, see \
             https://en.wikipedia.org/wiki/Cross-origin_resource_sharing."
          (list string)
          default_rpc.cors_headers)
       (opt
          "crt"
          ~description:"Certificate file (necessary when TLS is used)."
          string)
       (opt "key" ~description:"Key file (necessary when TLS is used)." string)
       (dft
          "acl"
          ~description:"A list of RPC ACLs for specific listening addresses."
          RPC_server.Acl.policy_encoding
          default_rpc.acl)
       (dft
          "media-type"
          ~description:"The media types supported by the server."
          Media_type.Command_line.encoding
          default_rpc.media_type))

let timeout_encoding = Time.System.Span.encoding

let block_validator_limits_encoding =
  let open Data_encoding in
  conv
    (fun {Block_validator.protocol_timeout; operation_metadata_size_limit} ->
      (protocol_timeout, operation_metadata_size_limit))
    (fun (protocol_timeout, operation_metadata_size_limit) ->
      {protocol_timeout; operation_metadata_size_limit})
    (obj2
       (dft
          "protocol_request_timeout"
          timeout_encoding
          default_shell.block_validator_limits.protocol_timeout)
       (dft
          "operation_metadata_size_limit"
          (union
             [
               case
                 ~title:"unlimited"
                 (Tag 0)
                 (constant "unlimited")
                 (function None -> Some () | _ -> None)
                 (fun () -> None);
               case
                 ~title:"limited"
                 (Tag 1)
                 int31
                 (function Some i -> Some i | None -> None)
                 (fun i -> Some i);
             ])
          default_shell.block_validator_limits.operation_metadata_size_limit))

let prevalidator_limits_encoding =
  let open Data_encoding in
  conv
    (fun {
           Prevalidator.operation_timeout;
           max_refused_operations;
           operations_batch_size;
           disable_precheck;
         } ->
      ( operation_timeout,
        max_refused_operations,
        operations_batch_size,
        disable_precheck ))
    (fun ( operation_timeout,
           max_refused_operations,
           operations_batch_size,
           disable_precheck ) ->
      {
        operation_timeout;
        max_refused_operations;
        operations_batch_size;
        disable_precheck;
      })
    (obj4
       (dft
          "operations_request_timeout"
          timeout_encoding
          default_shell.prevalidator_limits.operation_timeout)
       (dft
          "max_refused_operations"
          uint16
          default_shell.prevalidator_limits.max_refused_operations)
       (dft
          "operations_batch_size"
          int31
          default_shell.prevalidator_limits.operations_batch_size)
       (dft
          "disable_precheck"
          bool
          default_shell.prevalidator_limits.disable_precheck))

let peer_validator_limits_encoding =
  let open Data_encoding in
  let default_limits = default_shell.peer_validator_limits in
  conv
    (fun {
           Peer_validator.block_header_timeout;
           block_operations_timeout;
           protocol_timeout;
           new_head_request_timeout;
         } ->
      ( block_header_timeout,
        block_operations_timeout,
        protocol_timeout,
        new_head_request_timeout ))
    (fun ( block_header_timeout,
           block_operations_timeout,
           protocol_timeout,
           new_head_request_timeout ) ->
      {
        block_header_timeout;
        block_operations_timeout;
        protocol_timeout;
        new_head_request_timeout;
      })
    (obj4
       (dft
          "block_header_request_timeout"
          timeout_encoding
          default_limits.block_header_timeout)
       (dft
          "block_operations_request_timeout"
          timeout_encoding
          default_limits.block_operations_timeout)
       (dft
          "protocol_request_timeout"
          timeout_encoding
          default_limits.protocol_timeout)
       (dft
          "new_head_request_timeout"
          timeout_encoding
          default_limits.new_head_request_timeout))

let synchronisation_heuristic_encoding default_latency default_threshold =
  let open Data_encoding in
  conv
    (fun {Chain_validator.latency; threshold} -> (latency, threshold))
    (fun (latency, threshold) -> {latency; threshold})
    (obj2
       (dft
          "latency"
          ~description:
            "[latency] is the time interval (in seconds) used to determine if \
             a peer is synchronized with a chain. For instance, a peer whose \
             known head has a timestamp T is considered synchronized if T >= \
             now - latency. This parameter depends on the baking rate and the \
             latency of the network."
          uint16
          default_latency)
       (dft
          "synchronisation_threshold"
          ~description:
            "The minimal number of peers this peer should be synchronized with \
             in order to be bootstrapped."
          uint8
          default_threshold))

let chain_validator_limits_encoding =
  let open Data_encoding in
  conv
    (fun {Chain_validator.synchronisation} -> synchronisation)
    (fun synchronisation -> {synchronisation})
    (* Use a union to support both the deprecated
       bootstrap_threshold and the new synchronisation_threshold
       options when parsing.  When printing, use the new
       synchronisation_threshold option. *)
    (union
       [
         case
           ~title:"synchronisation_heuristic_encoding"
           Json_only
           (synchronisation_heuristic_encoding
              default_shell.chain_validator_limits.synchronisation.latency
              default_shell.chain_validator_limits.synchronisation.threshold)
           (fun x -> Some x)
           (fun x -> x);
         case
           ~title:"legacy_bootstrap_threshold_encoding"
           Json_only
           (obj1
              (dft
                 "bootstrap_threshold"
                 ~description:
                   "[DEPRECATED] Set the number of peers with whom a chain \
                    synchronisation must be completed to bootstrap the node."
                 uint8
                 4))
           (fun _ -> None) (* This is used for legacy *)
           (fun x ->
             Chain_validator.
               {
                 threshold = x;
                 latency =
                   default_shell.chain_validator_limits.synchronisation.latency;
               });
       ])

let shell =
  let open Data_encoding in
  conv
    (fun {
           peer_validator_limits;
           block_validator_limits;
           prevalidator_limits;
           chain_validator_limits;
           history_mode;
         } ->
      ( peer_validator_limits,
        block_validator_limits,
        prevalidator_limits,
        chain_validator_limits,
        history_mode ))
    (fun ( peer_validator_limits,
           block_validator_limits,
           prevalidator_limits,
           chain_validator_limits,
           history_mode ) ->
      {
        peer_validator_limits;
        block_validator_limits;
        prevalidator_limits;
        chain_validator_limits;
        history_mode;
      })
    (obj5
       (dft
          "peer_validator"
          peer_validator_limits_encoding
          default_shell.peer_validator_limits)
       (dft
          "block_validator"
          block_validator_limits_encoding
          default_shell.block_validator_limits)
       (dft
          "prevalidator"
          prevalidator_limits_encoding
          default_shell.prevalidator_limits)
       (dft
          "chain_validator"
          chain_validator_limits_encoding
          default_shell.chain_validator_limits)
       (opt "history_mode" History_mode.encoding))

let encoding =
  let open Data_encoding in
  conv
    (fun {
           data_dir;
           disable_config_validation;
           rpc;
           p2p;
           log;
           internal_events;
           shell;
           blockchain_network;
           metrics_addr;
         } ->
      ( data_dir,
        disable_config_validation,
        rpc,
        p2p,
        log,
        internal_events,
        shell,
        blockchain_network,
        metrics_addr ))
    (fun ( data_dir,
           disable_config_validation,
           rpc,
           p2p,
           log,
           internal_events,
           shell,
           blockchain_network,
           metrics_addr ) ->
      {
        disable_config_validation;
        data_dir;
        rpc;
        p2p;
        log;
        internal_events;
        shell;
        blockchain_network;
        metrics_addr;
      })
    (obj9
       (dft
          "data-dir"
          ~description:"Location of the data dir on disk."
          string
          default_data_dir)
       (dft
          "disable-config-validation"
          ~description:"Disable the node configuration validation."
          bool
          default_disable_config_validation)
       (dft
          "rpc"
          ~description:"Configuration of rpc parameters"
          rpc
          default_rpc)
       (dft
          "p2p"
          ~description:"Configuration of network parameters"
          p2p
          default_p2p)
       (dft
          "log"
          ~description:
            "Configuration of the Lwt-log sink (part of the logging framework)"
          Lwt_log_sink_unix.cfg_encoding
          Lwt_log_sink_unix.default_cfg)
       (dft
          "internal-events"
          ~description:"Configuration of the structured logging framework"
          Internal_event_config.encoding
          Internal_event_config.default)
       (dft
          "shell"
          ~description:"Configuration of network parameters"
          shell
          default_shell)
       (dft
          "network"
          ~description:"Configuration of which network/blockchain to connect to"
          sugared_blockchain_network_encoding
          blockchain_network_mainnet)
       (dft
          "metrics_addr"
          ~description:"Configuration of the Prometheus metrics endpoint"
          (list string)
          default_config.metrics_addr))

(* Abstract version of [Json_encoding.Cannot_destruct]: first argument is the
   string representation of the path, second argument is the error message
   of the actual exception which was raised (as [Cannot_destruct] takes an [exn]
   as second argument). *)
type error += Invalid_content of string option * string

let () =
  register_error_kind
    `Permanent
    ~id:"node_config_file.invalid_content"
    ~title:"Invalid config file"
    ~description:"Invalid content in node config file"
    ~pp:(fun ppf (path, exn) ->
      match path with
      | Some path ->
          Format.fprintf
            ppf
            "@[<hov>Invalid configuration file:@ at %s:@ %s@]"
            path
            exn
      | None ->
          Format.fprintf ppf "@[<hov>Invalid configuration file:@ %s@]" exn)
    Data_encoding.(obj2 (req "path" (option string)) (req "error" string))
    (function Invalid_content (p, e) -> Some (p, e) | _ -> None)
    (fun (p, e) -> Invalid_content (p, e))

module Event = struct
  include Internal_event.Simple

  let section = ["node"; "main"]

  let level = Internal_event.Warning

  let cannot_convert_to_ipv4 =
    Internal_event.Simple.declare_1
      ~section
      ~level
      ~name:"cannot_convert_to_ipv4"
      ~msg:"failed to convert {addr} to an ipv4 address"
      ~pp1:(fun ppf -> Format.fprintf ppf "%S")
      ("addr", Data_encoding.string)

  let all_rpc_allowed =
    declare_1
      ~level:Error
      ~section
      ~name:"all_rpc_allowed"
      ~msg:"FULL access to RPC enabled; this is very risky."
      ~pp1:
        Format.(
          pp_print_list
            ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
            P2p_point.Id.pp_addr_port_id)
      ("addresses", Data_encoding.(list P2p_point.Id.addr_port_id_encoding))
end

let string_of_json_encoding_error exn =
  Format.asprintf "%a" (Json_encoding.print_error ?print_unknown:None) exn

let read fp =
  let open Lwt_result_syntax in
  if Sys.file_exists fp then
    let* json = Lwt_utils_unix.Json.read_file fp in
    try return (Data_encoding.Json.destruct encoding json) with
    | Json_encoding.Cannot_destruct (path, exn) ->
        let path = Json_query.json_pointer_of_path path in
        let exn = string_of_json_encoding_error exn in
        tzfail (Invalid_content (Some path, exn))
    | ( Json_encoding.Unexpected _ | Json_encoding.No_case_matched _
      | Json_encoding.Bad_array_size _ | Json_encoding.Missing_field _
      | Json_encoding.Unexpected_field _ | Json_encoding.Bad_schema _ ) as exn
      ->
        let exn = string_of_json_encoding_error exn in
        tzfail (Invalid_content (None, exn))
  else return default_config

let write fp cfg =
  let open Lwt_result_syntax in
  let* () = Node_data_version.ensure_data_dir (Filename.dirname fp) in
  Lwt_utils_unix.Json.write_file fp (Data_encoding.Json.construct encoding cfg)

let to_string cfg =
  Data_encoding.Json.to_string (Data_encoding.Json.construct encoding cfg)

let update ?(disable_config_validation = false) ?data_dir ?min_connections
    ?expected_connections ?max_connections ?max_download_speed ?max_upload_speed
    ?binary_chunks_size ?peer_table_size ?expected_pow ?bootstrap_peers
    ?listen_addr ?advertised_net_port ?discovery_addr ?(rpc_listen_addrs = [])
    ?(allow_all_rpc = []) ?(media_type = Media_type.Command_line.Any)
    ?(metrics_addr = []) ?operation_metadata_size_limit ?(private_mode = false)
    ?(disable_mempool = false)
    ?(disable_mempool_precheck =
      default_shell.prevalidator_limits.disable_precheck)
    ?(enable_testchain = false) ?(cors_origins = []) ?(cors_headers = [])
    ?rpc_tls ?log_output ?synchronisation_threshold ?history_mode ?network
    ?latency cfg =
  let open Lwt_result_syntax in
  let disable_config_validation =
    cfg.disable_config_validation || disable_config_validation
  in
  let data_dir = Option.value ~default:cfg.data_dir data_dir in
  let*! () =
    if List.compare_length_with allow_all_rpc 1 >= 0 then
      Event.(emit all_rpc_allowed allow_all_rpc)
    else Lwt.return_unit
  in
  let* () = Node_data_version.ensure_data_dir data_dir in
  let peer_table_size = Option.map (fun i -> (i, i / 4 * 3)) peer_table_size in
  let unopt_list ~default = function [] -> default | l -> l in
  let limits : P2p.limits =
    {
      cfg.p2p.limits with
      min_connections =
        Option.value ~default:cfg.p2p.limits.min_connections min_connections;
      expected_connections =
        Option.value
          ~default:cfg.p2p.limits.expected_connections
          expected_connections;
      max_connections =
        Option.value ~default:cfg.p2p.limits.max_connections max_connections;
      max_download_speed =
        Option.either max_download_speed cfg.p2p.limits.max_download_speed;
      max_upload_speed =
        Option.either max_upload_speed cfg.p2p.limits.max_upload_speed;
      max_known_points =
        Option.either peer_table_size cfg.p2p.limits.max_known_points;
      max_known_peer_ids =
        Option.either peer_table_size cfg.p2p.limits.max_known_peer_ids;
      binary_chunks_size = Option.map (fun x -> x lsl 10) binary_chunks_size;
    }
  in
  let acl =
    (* Take addresses listed in allow_all_rpc and add each of them with allow_all
       ACL to the policy. *)
    List.fold_right
      RPC_server.Acl.put_policy
      (List.map (fun addr -> (addr, RPC_server.Acl.allow_all)) allow_all_rpc)
      cfg.rpc.acl
  in
  let p2p : p2p =
    {
      expected_pow = Option.value ~default:cfg.p2p.expected_pow expected_pow;
      bootstrap_peers =
        Option.value ~default:cfg.p2p.bootstrap_peers bootstrap_peers;
      listen_addr = Option.either listen_addr cfg.p2p.listen_addr;
      advertised_net_port =
        Option.either advertised_net_port cfg.p2p.advertised_net_port;
      discovery_addr = Option.either discovery_addr cfg.p2p.discovery_addr;
      private_mode = cfg.p2p.private_mode || private_mode;
      limits;
      disable_mempool = cfg.p2p.disable_mempool || disable_mempool;
      enable_testchain = cfg.p2p.enable_testchain || enable_testchain;
      reconnection_config = cfg.p2p.reconnection_config;
    }
  and rpc : rpc =
    {
      listen_addrs = unopt_list ~default:cfg.rpc.listen_addrs rpc_listen_addrs;
      cors_origins = unopt_list ~default:cfg.rpc.cors_origins cors_origins;
      cors_headers = unopt_list ~default:cfg.rpc.cors_headers cors_headers;
      tls = Option.either rpc_tls cfg.rpc.tls;
      acl;
      media_type;
    }
  and metrics_addr = unopt_list ~default:cfg.metrics_addr metrics_addr
  and log : Lwt_log_sink_unix.cfg =
    {cfg.log with output = Option.value ~default:cfg.log.output log_output}
  and shell : shell =
    {
      peer_validator_limits = cfg.shell.peer_validator_limits;
      block_validator_limits =
        {
          cfg.shell.block_validator_limits with
          operation_metadata_size_limit =
            Option.value
              ~default:
                cfg.shell.block_validator_limits.operation_metadata_size_limit
              operation_metadata_size_limit;
        };
      prevalidator_limits =
        {
          cfg.shell.prevalidator_limits with
          disable_precheck =
            cfg.shell.prevalidator_limits.disable_precheck
            || disable_mempool_precheck;
        };
      chain_validator_limits =
        (let synchronisation : Chain_validator.synchronisation_limits =
           {
             latency =
               Option.value
                 ~default:
                   cfg.shell.chain_validator_limits.synchronisation.latency
                 latency;
             threshold =
               Option.value
                 ~default:
                   cfg.shell.chain_validator_limits.synchronisation.threshold
                 synchronisation_threshold;
           }
         in
         {synchronisation});
      history_mode = Option.either history_mode cfg.shell.history_mode;
    }
  in
  (* If --network is specified it overrides the "network" entry of the
     configuration file, which itself defaults to mainnet. *)
  let blockchain_network =
    Option.value ~default:cfg.blockchain_network network
  in
  return
    {
      cfg with
      disable_config_validation;
      data_dir;
      p2p;
      rpc;
      log;
      shell;
      blockchain_network;
      metrics_addr;
    }

type Error_monad.error += Failed_to_parse_address of (string * string)

let () =
  (* Parsing of an address failed with an explanation *)
  Error_monad.register_error_kind
    `Permanent
    ~id:"node_config_file.parsing_address_failed"
    ~title:"Parsing of an address failed"
    ~description:"Parsing an address failed with an explanation."
    ~pp:(fun ppf (addr, explanation) ->
      Format.fprintf ppf "Failed to parse address '%s': %s@." addr explanation)
    Data_encoding.(obj2 (req "addr" string) (req "explanation" string))
    (function Failed_to_parse_address s -> Some s | _ -> None)
    (fun s -> Failed_to_parse_address s)

let to_ipv4 ipv6_l =
  let open Lwt_syntax in
  let convert_or_warn (ipv6, port) =
    let ipv4 = Ipaddr.v4_of_v6 ipv6 in
    match ipv4 with
    | None ->
        let* () =
          Event.(emit cannot_convert_to_ipv4) (Ipaddr.V6.to_string ipv6)
        in
        return_none
    | Some ipv4 -> return_some (ipv4, port)
  in
  List.filter_map_s convert_or_warn ipv6_l

(* Parse an address.

   - [peer] is a string representing the peer.

   - if [no_peer_id_expected] is true, then parsing a representation
   containing a peer id will result in an error.

   - [default_addr] is the used if no hostname or IP is given or if
   the hostname "_" is used.

   - [default_port] is the used if port is given. *)
let resolve_addr ~default_addr ?(no_peer_id_expected = true) ?default_port
    ?(passive = false) peer :
    (P2p_point.Id.t * P2p_peer.Id.t option) list tzresult Lwt.t =
  let open Lwt_result_syntax in
  match P2p_point.Id.parse_addr_port_id peer with
  | (Error (P2p_point.Id.Bad_id_format _) | Ok {peer_id = Some _; _})
    when no_peer_id_expected ->
      tzfail
        (Failed_to_parse_address
           (peer, "no peer identity should be specified here"))
  | Error err ->
      tzfail
        (Failed_to_parse_address (peer, P2p_point.Id.string_of_parsing_error err))
  | Ok {addr; port; peer_id} ->
      let service_port =
        match (port, default_port) with
        | Some port, _ -> port
        | None, Some default_port -> default_port
        | None, None -> default_p2p_port
      in
      let service = string_of_int service_port in
      let node = if addr = "" || addr = "_" then default_addr else addr in
      let*! l = Lwt_utils_unix.getaddrinfo ~passive ~node ~service in
      return (List.map (fun point -> (point, peer_id)) l)

let resolve_addrs ?default_port ?passive ?no_peer_id_expected ~default_addr
    addrs =
  List.concat_map_es
    (resolve_addr ~default_addr ?default_port ?passive ?no_peer_id_expected)
    addrs

let resolve_discovery_addrs discovery_addr =
  let open Lwt_result_syntax in
  let* addrs =
    resolve_addr
      ~default_addr:Ipaddr.V4.(to_string broadcast)
      ~default_port:default_discovery_port
      ~passive:true
      discovery_addr
  in
  let*! addrs = to_ipv4 (List.map fst addrs) in
  return addrs

let resolve_listening_addrs listen_addr =
  let open Lwt_result_syntax in
  let+ addrs =
    resolve_addr
      ~default_addr:"::"
      ~default_port:default_p2p_port
      ~passive:true
      listen_addr
  in
  List.map fst addrs

let resolve_rpc_listening_addrs listen_addr =
  let open Lwt_result_syntax in
  let+ addrs =
    resolve_addr
      ~default_addr:"localhost"
      ~default_port:default_rpc_port
      ~passive:true
      listen_addr
  in
  List.map fst addrs

let resolve_metrics_addrs metrics_addr =
  let open Lwt_result_syntax in
  let+ addrs =
    resolve_addr
      ~default_addr:"localhost"
      ~default_port:default_metrics_port
      ~passive:true
      metrics_addr
  in
  List.map fst addrs

let resolve_bootstrap_addrs peers =
  resolve_addrs
    ~no_peer_id_expected:false
    ~default_addr:"::"
    ~default_port:default_p2p_port
    peers

let bootstrap_peers config =
  Option.value
    ~default:config.blockchain_network.default_bootstrap_peers
    config.p2p.bootstrap_peers
