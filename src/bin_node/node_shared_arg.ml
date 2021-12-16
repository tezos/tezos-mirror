(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Cmdliner
open Filename.Infix

type net_config =
  | BuiltIn of Node_config_file.blockchain_network
  | Url of Uri.t
  | Filename of string

type t = {
  disable_config_validation : bool;
  data_dir : string option;
  config_file : string;
  network : net_config option;
  connections : int option;
  max_download_speed : int option;
  max_upload_speed : int option;
  binary_chunks_size : int option;
  peer_table_size : int option;
  expected_pow : float option;
  peers : string list;
  no_bootstrap_peers : bool;
  listen_addr : string option;
  advertised_net_port : int option;
  discovery_addr : string option;
  rpc_listen_addrs : string list;
  private_mode : bool;
  disable_mempool : bool;
  disable_mempool_precheck : bool;
  enable_testchain : bool;
  cors_origins : string list;
  cors_headers : string list;
  rpc_tls : Node_config_file.tls option;
  log_output : Lwt_log_sink_unix.Output.t option;
  bootstrap_threshold : int option;
  history_mode : History_mode.t option;
  synchronisation_threshold : int option;
  latency : int option;
  allow_all_rpc : P2p_point.Id.addr_port_id list;
}

type error +=
  | Invalid_network_config of string * string (* filename, exception raised *)
  | Network_http_error of (Cohttp.Code.status_code * string)

let () =
  register_error_kind
    `Permanent
    ~id:"node.network.invalid_config"
    ~title:"Invalid network config"
    ~description:"The network config provided by --network argument is invalid."
    ~pp:(fun ppf (path, error) ->
      Format.fprintf ppf "The network config at %s is invalid (%s)." path error)
    Data_encoding.(
      obj2 (req "path" Data_encoding.string) (req "error" Data_encoding.string))
    (function
      | Invalid_network_config (path, exn) -> Some (path, exn) | _ -> None)
    (fun (path, exn) -> Invalid_network_config (path, exn)) ;
  let http_status_enc =
    let open Data_encoding in
    let open Cohttp.Code in
    conv code_of_status status_of_code int31
  in
  register_error_kind
    `Permanent
    ~id:"node.network.http_error"
    ~title:"HTTP error when downloading network config"
    ~description:
      "The node encountered an HTTP error when downloading the network config."
    ~pp:(fun ppf (status, body) ->
      Format.fprintf
        ppf
        "Downloading network config resulted in: %s (%s)."
        (Cohttp.Code.string_of_status status)
        body)
    Data_encoding.(
      obj2 (req "status" http_status_enc) (req "body" Data_encoding.string))
    (function
      | Network_http_error (status, body) -> Some (status, body) | _ -> None)
    (fun (status, body) -> Network_http_error (status, body))

let decode_net_config source json =
  try
    Data_encoding.Json.destruct
      Node_config_file.blockchain_network_encoding
      json
    |> return
  with
  | Json_encoding.Cannot_destruct (path, exn) ->
      let path = Json_query.json_pointer_of_path path in
      fail (Invalid_network_config (path, Printexc.to_string exn))
  | ( Json_encoding.Unexpected _ | Json_encoding.No_case_matched _
    | Json_encoding.Bad_array_size _ | Json_encoding.Missing_field _
    | Json_encoding.Unexpected_field _ | Json_encoding.Bad_schema _ ) as exn ->
      fail (Invalid_network_config (source, Printexc.to_string exn))

let load_net_config =
  let open Lwt_tzresult_syntax in
  function
  | BuiltIn net -> return net
  | Url uri ->
      let*! (resp, body) = Cohttp_lwt_unix.Client.get uri in
      let*! body_str = Cohttp_lwt.Body.to_string body in
      let* netconfig =
        match resp.status with
        | `OK -> (
            try return (Ezjsonm.from_string body_str)
            with Ezjsonm.Parse_error (_, msg) ->
              fail (Invalid_network_config (Uri.to_string uri, msg)))
        | #Cohttp.Code.status_code ->
            fail (Network_http_error (resp.status, body_str))
      in
      decode_net_config (Uri.to_string uri) netconfig
  | Filename filename ->
      let* netconfig = Lwt_utils_unix.Json.read_file filename in
      decode_net_config filename netconfig

let wrap data_dir config_file network connections max_download_speed
    max_upload_speed binary_chunks_size peer_table_size listen_addr
    advertised_net_port discovery_addr peers no_bootstrap_peers
    bootstrap_threshold private_mode disable_mempool disable_mempool_precheck
    enable_testchain expected_pow rpc_listen_addrs rpc_tls cors_origins
    cors_headers log_output history_mode synchronisation_threshold latency
    disable_config_validation allow_all_rpc =
  let actual_data_dir =
    Option.value ~default:Node_config_file.default_data_dir data_dir
  in
  let config_file =
    Option.value
      ~default:(actual_data_dir // Node_data_version.default_config_file_name)
      config_file
  in
  let rpc_tls =
    Option.map (fun (cert, key) -> {Node_config_file.cert; key}) rpc_tls
  in
  {
    disable_config_validation;
    data_dir;
    config_file;
    network;
    connections;
    max_download_speed;
    max_upload_speed;
    binary_chunks_size;
    expected_pow;
    peers;
    no_bootstrap_peers;
    listen_addr;
    advertised_net_port;
    discovery_addr;
    rpc_listen_addrs;
    private_mode;
    disable_mempool;
    disable_mempool_precheck;
    enable_testchain;
    cors_origins;
    cors_headers;
    rpc_tls;
    log_output;
    peer_table_size;
    bootstrap_threshold;
    history_mode;
    synchronisation_threshold;
    latency;
    allow_all_rpc;
  }

module Manpage = struct
  let misc_section = "MISC OPTIONS"

  let p2p_section = "P2P OPTIONS"

  let rpc_section = "RPC OPTIONS"

  let args = [`S p2p_section; `S rpc_section; `S misc_section]

  let bugs =
    [
      `S "BUGS"; `P "Check bug reports at https://gitlab.com/tezos/tezos/issues.";
    ]
end

module Term = struct
  let log_output_converter =
    ( (fun s ->
        match Lwt_log_sink_unix.Output.of_string s with
        | Some res -> `Ok res
        | None -> `Error s),
      Lwt_log_sink_unix.Output.pp )

  let history_mode_converter =
    let open History_mode in
    (* Parses the history mode given as a string through [arg] from
       the command line and returns the corresponding well formed
       history mode. The colon punctuation mark (:) is used to delimit
       offsets such as: "full:5" -> Full { offset = 5 }. *)
    let parse_history_mode s =
      let delim = ':' in
      let args = String.split_on_char delim s in
      match args with
      | ["archive"] | ["Archive"] -> Some Archive
      | ["full"] | ["Full"] -> Some default_full
      | ["full"; n] | ["Full"; n] ->
          Option.map (fun offset -> Full (Some {offset})) (int_of_string_opt n)
      | ["rolling"] | ["Rolling"] -> Some default_rolling
      | ["rolling"; n] | ["Rolling"; n] ->
          Option.map
            (fun offset -> Rolling (Some {offset}))
            (int_of_string_opt n)
      | ["experimental-rolling"] -> Some default_rolling
      | _ -> None
    in
    ( (fun arg ->
        match parse_history_mode arg with
        | Some hm -> `Ok hm
        | None -> `Error arg),
      pp )

  let network_printer ppf = function
    | BuiltIn ({alias; _} : Node_config_file.blockchain_network) ->
        (* Should not fail by construction of Node_config_file.block_chain_network *)
        let alias = WithExceptions.Option.get ~loc:__LOC__ alias in
        Format.fprintf ppf "built-in network: %s" alias
    | Url url -> Format.fprintf ppf "URL network: %s" (Uri.to_string url)
    | Filename file -> Format.fprintf ppf "local file network: %s" file

  let network_parser =
    let parse_network_name s =
      List.assoc_opt
        ~equal:String.equal
        (String.lowercase_ascii s)
        Node_config_file.builtin_blockchain_networks
      |> Option.map (fun net -> Result.ok (BuiltIn net))
    in
    let parse_network_url s =
      let uri = Uri.of_string s in
      match Uri.scheme uri with
      | Some "http" | Some "https" -> Some (Ok (Url uri))
      | Some _ | None -> None
    in
    let parse_file_config filename =
      if Sys.file_exists filename then Some (Result.ok (Filename filename))
      else None
    in
    let parse_error s =
      Error
        (`Msg
          (Format.asprintf
             "invalid value '%s', expected one of '%a', a URL or an existing \
              filename"
             s
             (Format.pp_print_list
                ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
                Format.pp_print_string)
             (List.map fst Node_config_file.builtin_blockchain_networks)))
    in
    let parser s =
      let ( <||> ) = Option.either_f
      and ( <|!> ) opt default = Option.value_f ~default opt in
      (* Select the first parsing result that is not None. *)
      ( (parse_network_name s <||> fun () -> parse_network_url s) <||> fun () ->
        parse_file_config s )
      <|!> fun () -> parse_error s
    in
    ( (parser : string -> (net_config, [`Msg of string]) result),
      (network_printer : net_config Cmdliner.Arg.printer) )

  (* misc args *)

  let docs = Manpage.misc_section

  let disable_config_validation =
    let doc = "Disable the node configuration validation." in
    Arg.(value & flag & info ~docs ~doc ["disable-config-validation"])

  let history_mode =
    let doc =
      Format.sprintf
        "Set the mode for the chain's data history storage. Possible values \
         are $(i,archive), $(i,full) $(b,(default)), $(i,full:N), \
         $(i,rolling), $(i,rolling:N). Archive mode retains all data since the \
         genesis block. Full mode only maintains block headers and operations \
         allowing replaying the chain since the genesis if wanted. Rolling \
         mode retains only the most recent data and deletes the rest. For both \
         Full and Rolling modes, it is possible to adjust the number of cycles \
         to preserve by using the $(i,:N) annotation. The default number of \
         preserved cycles is %d. The value $(i,experimental-rolling) is \
         deprecated but is equivalent to $(i,rolling) which should be used \
         instead."
        History_mode.default_additional_cycles.offset
    in
    Arg.(
      value
      & opt (some history_mode_converter) None
      & info ~docs ~doc ~docv:"<mode>" ["history-mode"])

  let log_output =
    let doc =
      "Log output. Either $(i,stdout), $(i,stderr), $(i,syslog:<facility>) or \
       a file path."
    in
    Arg.(
      value
      & opt (some log_output_converter) None
      & info ~docs ~docv:"OUTPUT" ~doc ["log-output"])

  let data_dir =
    let doc =
      "The directory where the Tezos node will store all its data. Parent \
       directories are created if necessary."
    in
    let env = Arg.env_var Node_config_file.data_dir_env_name in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~env ~doc ~docv:"DIR" ["data-dir"; "d"])

  let config_file =
    let doc = "The main configuration file." in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~doc ~docv:"FILE" ["config-file"])

  let network =
    let open Cmdliner in
    let doc =
      "Select which network to run. Possible values are: "
      ^ String.concat
          ", "
          (List.map fst Node_config_file.builtin_blockchain_networks)
      ^ ". Default is mainnet. You can also specify custom networks by passing \
         a path to a file containing the custom network configuration, or by \
         passing a URL from which such a file can be downloaded. If you have a \
         file named after a built-in network, you can prefix its name with \
         './' so that the node treats it as a file. Otherwise it will be \
         treated as a proper name of the built-in network. With commands other \
         than 'config init', specifying this option causes the node to fail if \
         the configuration implies another network."
    in
    Arg.(
      value
      & opt (some (conv network_parser)) None
      & info ~docs ~doc ~docv:"NETWORK" ["network"])

  (* P2p args *)

  let docs = Manpage.p2p_section

  let connections =
    let doc =
      "Sets min_connections, expected_connections, max_connections to NUM / 2, \
       NUM, (3 * NUM) / 2, respectively. Sets peer_table_size to 8 * NUM \
       unless it is already defined on the command line. Sets \
       synchronisation_threshold to max(NUM / 4, 2) unless it is already \
       defined on the command line."
    in
    Arg.(
      value & opt (some int) None & info ~docs ~doc ~docv:"NUM" ["connections"])

  let max_download_speed =
    let doc = "The maximum number of bytes read per second." in
    Arg.(
      value
      & opt (some int) None
      & info ~docs ~doc ~docv:"NUM" ["max-download-speed"])

  let max_upload_speed =
    let doc = "The maximum number of bytes sent per second." in
    Arg.(
      value
      & opt (some int) None
      & info ~docs ~doc ~docv:"NUM" ["max-upload-speed"])

  let binary_chunks_size =
    let doc =
      "Size limit (in kB) of binary blocks that are sent to other peers."
    in
    Arg.(
      value
      & opt (some int) None
      & info ~docs ~doc ~docv:"NUM" ["binary-chunks-size"])

  let peer_table_size =
    let doc =
      "Maximum size of internal peer tables, used to store metadata/logs about \
       a peer or about a to-be-authenticated host:port couple."
    in
    Arg.(
      value
      & opt (some int) None
      & info ~docs ~doc ~docv:"NUM" ["peer-table-size"])

  let listen_addr =
    let doc =
      "The TCP address and port at which this instance can be reached."
    in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~doc ~docv:"ADDR:PORT" ["net-addr"])

  let advertised_net_port =
    let doc =
      "The alternative TCP port at which this instance can be reached. This \
       instance does not actually binds to it. The port may be used by a NAT \
       server to forward connections to the instance listenning port."
    in
    Arg.(
      value
      & opt (some int) None
      & info ~docs ~doc ~docv:"PORT" ["advertised-net-port"])

  let discovery_addr =
    let doc = "The UDP address and port used for local peer discovery." in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~doc ~docv:"ADDR:PORT" ["discovery-addr"])

  let no_bootstrap_peers =
    let doc =
      "Ignore the peers found in the config file (or the hard-coded bootstrap \
       peers in the absence of config file)."
    in
    Arg.(value & flag & info ~docs ~doc ["no-bootstrap-peers"])

  let bootstrap_threshold =
    let doc =
      "[DEPRECATED: use synchronisation_threshold instead] The number of peers \
       to synchronize with before declaring the node bootstrapped."
    in
    Arg.(
      value
      & opt (some int) None
      & info ~docs ~doc ~docv:"NUM" ["bootstrap-threshold"])

  let peers =
    let doc =
      "A peer to bootstrap the network from. Can be used several times to add \
       several peers. Optionally, the expected identity of the peer can be \
       given using the b58 hash format of its public key."
    in
    Arg.(
      value & opt_all string []
      & info ~docs ~doc ~docv:"ADDR:PORT[#ID]" ["peer"])

  let expected_pow =
    let doc = "Expected level of proof-of-work for peers identity." in
    Arg.(
      value
      & opt (some float) None
      & info ~docs ~doc ~docv:"FLOAT" ["expected-pow"])

  let private_mode =
    let doc =
      "Only open outgoing/accept incoming connections to/from peers listed in \
       'bootstrap-peers' or provided with '--peer' option."
    in
    Arg.(value & flag & info ~docs ~doc ["private-mode"])

  let disable_mempool =
    let doc =
      "If set to [true], the node will not participate in the propagation of \
       pending operations (mempool). Default value is [false]. It can be used \
       to decrease the memory and computation footprints of the node."
    in
    Arg.(value & flag & info ~docs ~doc ["disable-mempool"])

  let disable_mempool_precheck =
    let doc =
      "If set to [true], the node's prevalidator will fully execute operations \
       before gossiping valid operations over the network. Default value is \
       [false], in which case the node's prevalidator only performs a fast \
       check over operations before gossiping them. If set to [true], this \
       option can slow down your node and should be used for testing or \
       debugging purposes."
    in
    Arg.(value & flag & info ~docs ~doc ["disable-mempool-precheck"])

  let enable_testchain =
    let doc =
      "If set to [true], the node will spawn a testchain during the protocol's \
       testing voting period. Default value is [false]. It will increase the \
       node storage usage and computation by additionally validating the test \
       network blocks."
    in
    Arg.(value & flag & info ~docs ~doc ["enable-testchain"])

  let synchronisation_threshold =
    let doc =
      "Set the number of peers with whom a chain synchronization must be \
       completed to bootstrap the node"
    in
    Arg.(
      value
      & opt (some int) None
      & info ~docs ~doc ~docv:"NUM" ["synchronisation-threshold"])

  let latency =
    let doc =
      "[latency] is the time interval (in seconds) used to determine if a peer \
       is synchronized with a chain. For instance, a peer whose known head has \
       a timestamp T is considered synchronized if T >= now - max_latency. \
       This parameter's default value was set with the chain's current \
       protocol's baking rate in mind (and some allowance for network \
       latency)."
    in
    Arg.(
      value & opt (some int) None & info ~docs ~doc ~docv:"NUM" ["sync-latency"])

  (* rpc args *)
  let docs = Manpage.rpc_section

  let rpc_listen_addrs =
    let doc =
      "The TCP socket address at which this RPC server instance can be reached."
    in
    Arg.(
      value & opt_all string [] & info ~docs ~doc ~docv:"ADDR:PORT" ["rpc-addr"])

  let rpc_tls =
    let doc =
      "Enable TLS for this RPC server with the provided certificate and key."
    in
    Arg.(
      value
      & opt (some (pair string string)) None
      & info ~docs ~doc ~docv:"crt,key" ["rpc-tls"])

  let cors_origins =
    let doc =
      "CORS origin allowed by the RPC server via Access-Control-Allow-Origin; \
       may be used multiple times"
    in
    Arg.(
      value & opt_all string [] & info ~docs ~doc ~docv:"ORIGIN" ["cors-origin"])

  let cors_headers =
    let doc =
      "Header reported by Access-Control-Allow-Headers reported during CORS \
       preflighting; may be used multiple times"
    in
    Arg.(
      value & opt_all string [] & info ~docs ~doc ~docv:"HEADER" ["cors-header"])

  let allow_all_rpc =
    let addr_port_id str =
      match P2p_point.Id.parse_addr_port_id str with
      | Ok addr -> `Ok addr
      | Error e -> `Error (P2p_point.Id.string_of_parsing_error e)
    in
    let doc =
      "Apply allow-all policy to a given RPC listening address rather than the \
       safe default."
    in
    Arg.(
      value
      & opt_all (addr_port_id, P2p_point.Id.pp_addr_port_id) []
      & info ~docs ~doc ~docv:"ADDR:PORT" ["allow-all-rpc"])

  (* Args. *)

  let args =
    let open Term in
    const wrap $ data_dir $ config_file $ network $ connections
    $ max_download_speed $ max_upload_speed $ binary_chunks_size
    $ peer_table_size $ listen_addr $ advertised_net_port $ discovery_addr
    $ peers $ no_bootstrap_peers $ bootstrap_threshold $ private_mode
    $ disable_mempool $ disable_mempool_precheck $ enable_testchain
    $ expected_pow $ rpc_listen_addrs $ rpc_tls $ cors_origins $ cors_headers
    $ log_output $ history_mode $ synchronisation_threshold $ latency
    $ disable_config_validation $ allow_all_rpc
end

let read_config_file args =
  if Sys.file_exists args.config_file then
    Node_config_file.read args.config_file
  else return Node_config_file.default_config

let read_data_dir args =
  let open Lwt_tzresult_syntax in
  let* cfg = read_config_file args in
  let {data_dir; _} = args in
  let data_dir = Option.value ~default:cfg.data_dir data_dir in
  return data_dir

type error +=
  | Network_configuration_mismatch of {
      configuration_file_chain_name : Distributed_db_version.Name.t;
      command_line_chain_name : Distributed_db_version.Name.t;
    }

type error += Invalid_command_line_arguments of string

let () =
  register_error_kind
    `Permanent
    ~id:"node.config.network_configuration_mismatch"
    ~title:"Network configuration mismatch"
    ~description:
      "You specified a --network argument on the command line, but it does not \
       match your current configuration"
    ~pp:(fun ppf (configuration_file_chain_name, command_line_chain_name) ->
      Format.fprintf
        ppf
        "@[Specified@ --network@ has@ chain@ name@ %s,@ but@ current@ \
         configuration@ implies@ expected@ chain@ name@ %s.@ Use:@ tezos-node \
         config init --network <NETWORK>@ to@ configure@ your@ node.@]"
        command_line_chain_name
        configuration_file_chain_name)
    Data_encoding.(
      obj2
        (req "configuration_file_chain_name" string)
        (req "command_line_chain_name" string))
    (function
      | Network_configuration_mismatch
          {configuration_file_chain_name; command_line_chain_name} ->
          Some
            ( (configuration_file_chain_name :> string),
              (command_line_chain_name :> string) )
      | _ -> None)
    (fun (configuration_file_chain_name, command_line_chain_name) ->
      Network_configuration_mismatch
        {
          configuration_file_chain_name =
            Distributed_db_version.Name.of_string configuration_file_chain_name;
          command_line_chain_name =
            Distributed_db_version.Name.of_string command_line_chain_name;
        }) ;
  register_error_kind
    `Permanent
    ~id:"node.config.invalidcommandlinearguments"
    ~title:"Invalid command line arguments"
    ~description:"Given command line arguments are invalid"
    ~pp:(fun ppf explanation ->
      Format.fprintf
        ppf
        "@[Specified command line arguments are invalid: %s@]"
        explanation)
    Data_encoding.(obj1 (req "explanation" string))
    (function Invalid_command_line_arguments x -> Some x | _ -> None)
    (fun explanation -> Invalid_command_line_arguments explanation)

module Event = struct
  include Internal_event.Simple

  let disabled_bootstrap_peers =
    Internal_event.Simple.declare_0
      ~section:["node"; "main"]
      ~name:"disabled_bootstrap_peers"
      ~msg:"disabled bootstrap peers"
      ()
end

let read_and_patch_config_file ?(may_override_network = false)
    ?(ignore_bootstrap_peers = false) args =
  let open Lwt_tzresult_syntax in
  let* cfg = read_config_file args in
  let {
    data_dir;
    disable_config_validation;
    connections;
    max_download_speed;
    max_upload_speed;
    binary_chunks_size;
    peer_table_size;
    expected_pow;
    peers;
    no_bootstrap_peers;
    listen_addr;
    advertised_net_port;
    private_mode;
    discovery_addr;
    disable_mempool;
    disable_mempool_precheck;
    enable_testchain;
    rpc_listen_addrs;
    rpc_tls;
    cors_origins;
    cors_headers;
    log_output;
    bootstrap_threshold;
    history_mode;
    network;
    config_file = _;
    synchronisation_threshold;
    latency;
    allow_all_rpc;
  } =
    args
  in
  let* synchronisation_threshold =
    match (bootstrap_threshold, synchronisation_threshold) with
    | (Some _, Some _) ->
        fail
          (Invalid_command_line_arguments
             "--bootstrap-threshold is deprecated; use \
              --synchronisation-threshold instead. Do not use both at the same \
              time.")
    | (None, Some threshold) | (Some threshold, None) -> return_some threshold
    | (None, None) -> return_none
  in
  let* network_data =
    match network with
    | None -> return None
    | Some n ->
        let* x = load_net_config n in
        return (Some x)
  in
  (* Overriding the network with [--network] is a bad idea if the configuration
     file already specifies it. Essentially, [--network] tells the node
     "if there is no config file, use this network; otherwise, check that the
     config file uses the network I expect". This behavior can be overridden
     by [may_override_network], which is used when doing [config init]. *)
  let* () =
    match network_data with
    | None -> return_unit
    | Some net ->
        if may_override_network then return_unit
        else if
          Distributed_db_version.Name.equal
            cfg.blockchain_network.chain_name
            net.chain_name
        then return_unit
        else
          fail
            (Network_configuration_mismatch
               {
                 configuration_file_chain_name =
                   cfg.blockchain_network.chain_name;
                 command_line_chain_name = net.chain_name;
               })
  in
  (* Update bootstrap peers must take into account the updated config file
     with the [--network] argument, so we cannot use [Node_config_file]. *)
  let* bootstrap_peers =
    if no_bootstrap_peers || ignore_bootstrap_peers then
      let*! () = Event.(emit disabled_bootstrap_peers) () in
      return peers
    else
      let cfg_peers =
        match cfg.p2p.bootstrap_peers with
        | Some peers -> peers
        | None -> (
            match network_data with
            | Some net -> net.default_bootstrap_peers
            | None -> cfg.blockchain_network.default_bootstrap_peers)
      in
      return (cfg_peers @ peers)
  in
  let* () =
    Option.iter_es
      (fun connections ->
        fail_when
          (connections > 100 && disable_config_validation = false)
          (Invalid_command_line_arguments
             "The number of expected connections is limited to `100`. This \
              maximum cap may be overridden by manually modifying the \
              configuration file. However, this should be done carefully. \
              Exceeding this number of connections may degrade the performance \
              of your node."))
      connections
  in
  (* when `--connections` is used,
     override all the bounds defined in the configuration file. *)
  let ( synchronisation_threshold,
        min_connections,
        expected_connections,
        max_connections,
        peer_table_size ) =
    match connections with
    | None -> (synchronisation_threshold, None, None, None, peer_table_size)
    | Some x -> (
        let peer_table_size =
          match peer_table_size with
          | None -> Some (8 * x)
          | Some _ -> peer_table_size
        in
        (* connections sets a new value for the
           [synchronisation_threshold] except if a value for it was
           specified on the command line. *)
        match synchronisation_threshold with
        | None ->
            (* We want to synchronise with at least 2 peers and to a
               number of people proportional to the number of peers we
               are connected with. Because a heuristic is used, we only
               need to be synchronised with a sufficiently large number
               of our peers. To avoid the lack of connections when x is
               1, we define the minimum to 1 manually. (x/4) is enough
               if the `synchronisation-threshold` is not set. *)
            ( Some (max (x / 4) 2),
              Some (if x = 1 then x else x / 2),
              Some x,
              Some (3 * x / 2),
              peer_table_size )
        | Some threshold ->
            ( Some threshold,
              Some (if x = 1 then x else x / 2),
              Some x,
              Some (3 * x / 2),
              peer_table_size ))
  in
  Node_config_file.update
    ~disable_config_validation
    ?data_dir
    ?min_connections
    ?expected_connections
    ?max_connections
    ?max_download_speed
    ?max_upload_speed
    ?binary_chunks_size
    ?peer_table_size
    ?expected_pow
    ~bootstrap_peers:(Some bootstrap_peers)
    ?listen_addr
    ?advertised_net_port
    ?discovery_addr
    ~rpc_listen_addrs
    ~allow_all_rpc
    ~private_mode
    ~disable_mempool
    ~disable_mempool_precheck
    ~enable_testchain
    ~cors_origins
    ~cors_headers
    ?rpc_tls
    ?log_output
    ?synchronisation_threshold
    ?history_mode
    ?network:network_data
    ?latency
    cfg
