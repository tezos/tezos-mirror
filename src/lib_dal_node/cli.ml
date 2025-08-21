(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Types = Tezos_dal_node_services.Types

let env_value_starts_with_yes ~env_var =
  match Sys.getenv_opt env_var with
  | None -> false
  | Some x -> (
      match String.lowercase_ascii x with "yes" | "y" -> true | _ -> false)

(** This variable is used to disable DAL shard validation at runtime. When activated,
    Gossipsub messages (i.e. shards) are always considered valid. This can be risky
    as the DAL node would no longer validate the shards and therefore should be used
    only for testing purposes and/or with extreme care. *)
let disable_shard_validation_environment_variable =
  "TEZOS_DISABLE_SHARD_VALIDATION_I_KNOW_WHAT_I_AM_DOING"

let disable_shard_validation =
  env_value_starts_with_yes
    ~env_var:disable_shard_validation_environment_variable

(** This variable is used to instruct the DAL node to not propagate messages
    belonging to certain topics. This activates only when the variable is used
    in conjunction with the corresponding CLI argument. *)
let env_var_ignore_topics = "TEZOS_IGNORE_TOPICS_I_KNOW_WHAT_I_AM_DOING"

let env_ignore_topics = env_value_starts_with_yes ~env_var:env_var_ignore_topics

module Term = struct
  type env = {docs : string; doc : string; name : string}

  type 'a arg = {
    default : 'a option;
    short : char option;
    long : string;
    extra_long : string list;
    parse : string -> ('a, string) result;
    doc : string;
    placeholder : string;
    pp : Format.formatter -> 'a -> unit;
    env : env option;
  }

  type 'a arg_list = {
    default : 'a list;
    short : char option;
    long : string;
    extra_long : string list;
    parse : string -> ('a, string) result;
    doc : string;
    placeholder : string;
    pp : Format.formatter -> 'a -> unit;
    env : env option;
  }

  type switch = {long : string; extra_long : string list; doc : string}

  let make_env ~docs ~doc name = {docs; doc; name}

  let make_arg ?default ?short ~parse ~doc ?(placeholder = "VAL") ~pp ?env
      ?(extra_long = []) long : 'a arg =
    {default; short; long; extra_long; parse; doc; placeholder; pp; env}

  let make_arg_list ?(default = []) ?short ~parse ~doc ?(placeholder = "VAL")
      ~pp ?env ?(extra_long = []) long : 'a arg_list =
    {default; short; long; extra_long; parse; doc; placeholder; pp; env}

  let make_switch ~doc ?(extra_long = []) long = {long; extra_long; doc}

  let docs = "OPTIONS"

  let env_to_cmdliner {docs; doc; name} =
    let open Cmdliner in
    Cmd.Env.info ~docs ~doc name

  let arg_to_cmdliner
      ({default; short; long; extra_long; parse; doc; placeholder; pp; env} :
        'a arg) =
    let open Cmdliner in
    let parser =
      let parser s = match parse s with Ok x -> `Ok x | Error s -> `Error s in
      (parser, pp)
    in
    let names =
      match short with
      | None -> long :: extra_long
      | Some short -> String.make 1 short :: long :: extra_long
    in
    Arg.(
      value
      & opt (some parser) default
      & info
          ~doc
          ~docs
          ?env:(Option.map env_to_cmdliner env)
          ~docv:placeholder
          names)

  let arg_list_to_cmdliner
      ({default; short; long; extra_long; parse; doc; placeholder; pp; env} :
        'a arg_list) =
    let open Cmdliner in
    let parser =
      let parser s = match parse s with Ok x -> `Ok x | Error s -> `Error s in
      (parser, pp)
    in
    let names =
      match short with
      | None -> long :: extra_long
      | Some short -> String.make 1 short :: long :: extra_long
    in
    Arg.(
      value
      & opt (list parser) default
      & info
          ~doc
          ~docs
          ?env:(Option.map env_to_cmdliner env)
          ~docv:placeholder
          names)

  let switch_to_cmdliner {long; extra_long; doc} =
    let open Cmdliner in
    Arg.(value & flag & info ~docs ~doc (long :: extra_long))

  let arg_list (parse, printer) =
    let parse s =
      let l = String.split_on_char ',' s in
      let rec traverse acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs -> (
            match parse x with
            | Ok x -> traverse (x :: acc) xs
            | Error err -> Error err)
      in
      traverse [] l
    in
    ( parse,
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",")
        printer )

  let p2p_point_format ~default_port =
    let decoder str =
      match P2p_point.Id.of_string ~default_port str with
      | Ok x -> Ok x
      | Error msg -> (
          (* Let's check if the user has entered a port *)
          match String.split_on_char ':' str with
          | [""; port] -> (
              try
                let port = int_of_string port in
                let default =
                  (fst Configuration_file.default.public_addr, port)
                in
                Ok default
              with Failure _ ->
                Error
                  (Format.asprintf "The port provided: '%s' is invalid" port))
          | _ -> Error msg)
    in
    let printer = P2p_point.Id.pp in
    (decoder, printer)

  let data_dir_arg =
    make_arg
      ~doc:
        "The directory where the Octez DAL node will store all its data. \
         Parent directories are created if necessary."
      ~short:'d'
      ~parse:Result.ok
      ~placeholder:"DIR"
      ~pp:Format.pp_print_string
      "data-dir"

  let data_dir = arg_to_cmdliner data_dir_arg

  let config_file_arg =
    make_arg
      ~doc:"The configuration file of the octez DAL node."
      ~parse:Result.ok
      ~placeholder:"FILE"
      ~pp:Format.pp_print_string
      "config-file"

  let config_file = arg_to_cmdliner config_file_arg

  let rpc_addr_arg =
    let default_port = Configuration_file.default.rpc_addr |> snd in
    let parse, pp = p2p_point_format ~default_port in
    make_arg
      ~doc:
        "The TCP address and optionally the port at which the RPC server of \
         this instance can be reached. The default address is 0.0.0.0. The \
         default port is 10732."
      ~parse
      ~placeholder:"ADDR[:PORT]"
      ~pp
      "rpc-addr"

  let rpc_addr = arg_to_cmdliner rpc_addr_arg

  let expected_pow_arg =
    make_arg
      ~doc:
        "The expected proof-of-work difficulty level for the peers' identity."
      ~parse:(fun s -> Result.ok (Float.of_string s))
      ~placeholder:"FLOAT"
      ~pp:Format.pp_print_float
      "expected-pow"

  let expected_pow = arg_to_cmdliner expected_pow_arg

  let net_addr_arg =
    let default_port = Configuration_file.default.listen_addr |> snd in
    let parse, pp = p2p_point_format ~default_port in
    make_arg
      ~doc:
        "The TCP address and optionally the port bound by the DAL node. If \
         --public-addr is not provided, this is also the address and port at \
         which this instance can be reached by other P2P nodes. The default \
         address is 0.0.0.0. The default port is 11732."
      ~parse
      ~placeholder:"ADDR[:PORT]"
      ~pp
      "net-addr"

  let net_addr = arg_to_cmdliner net_addr_arg

  let public_addr_arg =
    let default_port = Configuration_file.default.public_addr |> snd in
    let parse, pp = p2p_point_format ~default_port in
    make_arg
      ~doc:
        "The TCP address and optionally the port at which this instance can be \
         reached by other P2P nodes. By default, the point is \
         '127.0.0.1:11732'. You can override the port using the syntax \
         ':2222'. If the IP address is detected as a special address (such as \
         a localhost one) it won't be advertised, only the port will."
      ~parse
      ~placeholder:"ADDR[:PORT]"
      ~pp
      "public-addr"

  let public_addr = arg_to_cmdliner public_addr_arg

  let uri_format =
    let decoder string =
      try Uri.of_string string |> Result.ok
      with _ -> Error "The string '%s' is not a valid URI"
    in
    let printer = Uri.pp_hum in
    (decoder, printer)

  let endpoint_arg =
    let decoder, printer = uri_format in
    make_arg
      ~doc:
        "The endpoint (an URI) of the Tezos node that the DAL node should \
         connect to. The default endpoint is 'http://localhost:8732'."
      ~short:'E'
      ~parse:decoder
      ~placeholder:"URI"
      ~pp:printer
      "endpoint"

  let endpoint = arg_to_cmdliner endpoint_arg

  let slots_backup_uris_arg =
    let decoder, printer = uri_format in
    make_arg_list
      ~doc:
        "List of base URIs to fetch missing DAL slots if they are unavailable \
         locally or cannot be reconstructed from shards. Supported URI schemes \
         include 'http://', 'https://', and 'file://'. The option accepts a \
         list of fallback sources separated with commas."
      ~placeholder:"URI"
      ~parse:decoder
      ~pp:printer
      "slots-backup-uri"

  let slots_backup_uris = arg_list_to_cmdliner slots_backup_uris_arg

  let trust_slots_backup_uris_switch =
    make_switch
      ~doc:
        "If set, skip cryptographic verification of slots downloaded from the \
         backup URIs provided via --slots-backup-uri. This can speed up slot \
         retrieval when replaying history or for debugging purposes. Use with \
         caution during normal operation or when data integrity is critical, \
         unless the backup source is fully trusted."
      "trust-slots-backup-uris"

  let trust_slots_backup_uris =
    switch_to_cmdliner trust_slots_backup_uris_switch

  let ignore_l1_config_peers_switch =
    make_switch
      ~doc:"Ignore the boot(strap) peers provided by L1 config."
      "ignore-l1-config-peers"

  let ignore_l1_config_peers = switch_to_cmdliner ignore_l1_config_peers_switch

  let attester_profile_printer = Signature.Public_key_hash.pp

  let producer_profile_printer = Format.pp_print_int

  let attester_profile_format =
    let decoder arg =
      let arg =
        (* If the argument is wrapped with quotes, unwrap it. *)
        if
          String.starts_with ~prefix:"\"" arg
          && String.ends_with ~suffix:"\"" arg
        then String.sub arg 1 (String.length arg - 2)
        else arg
      in
      match Signature.Public_key_hash.of_b58check_opt arg with
      | None -> Error "Unrecognized pkh format"
      | Some pkh -> Ok pkh
    in
    (decoder, attester_profile_printer)

  let producer_profile_format =
    let decoder string =
      let error () =
        Format.kasprintf
          (fun s -> Error s)
          "Unrecognized profile for producer (expected non-negative integer, \
           got %s)"
          string
      in
      match int_of_string_opt string with
      | None -> error ()
      | Some i when i < 0 -> error ()
      | Some slot_index -> Ok slot_index
    in
    (decoder, producer_profile_printer)

  let observer_profile_format =
    let decoder string =
      let error () =
        Format.kasprintf
          (fun s -> Error s)
          "Unrecognized profile for observer (expected nonnegative integer, \
           got %s)"
          string
      in
      match int_of_string_opt string with
      | None -> error ()
      | Some i when i < 0 -> error ()
      | Some slot_index -> Ok slot_index
    in
    (decoder, producer_profile_printer)

  let attester_profile_arg =
    let parse, pp = attester_profile_format in
    make_arg_list
      ~doc:"The Octez DAL node attester profiles for given public key hashes."
      ~placeholder:"PKH1,PKH2,..."
      ~pp
      ~parse
      ~extra_long:["attester"]
      "attester-profiles"

  let attester_profile = arg_list_to_cmdliner attester_profile_arg

  let operator_profile_arg =
    let parse, pp = producer_profile_format in
    make_arg_list
      ~doc:
        "The Octez DAL node operator profiles for given slot indexes. These \
         were previously known as producer profiles, however this name now \
         refers to both operator and observer profiles."
      ~placeholder:"INDEX1,INDEX2,..."
      ~parse
      ~pp
      ~extra_long:["producer-profiles"; "producer"; "operator"]
      "operator-profiles"

  let operator_profile = arg_list_to_cmdliner operator_profile_arg

  let observer_profile_arg =
    let parse, pp = arg_list observer_profile_format in
    make_arg
      ~doc:"The Octez DAL node observer profiles for given slot indexes."
      ~placeholder:"INDEX1,INDEX2,..."
      ~parse
      ~pp
      ~extra_long:["observer"]
      "observer-profiles"

  let observer_profile = arg_to_cmdliner observer_profile_arg

  let bootstrap_profile_switch =
    make_switch
      ~doc:
        "The Octez DAL node bootstrap node profile. Note that a bootstrap node \
         cannot also be an attester or a slot producer"
      ~extra_long:["bootstrap"]
      "bootstrap-profile"

  let bootstrap_profile = switch_to_cmdliner bootstrap_profile_switch

  let peers_arg =
    let default_list = Configuration_file.default.peers in
    make_arg_list
      ~doc:
        "An additional list of peers (bootstrap or not) to connect to, \
         expanding the one from the DAL node's configuration parameter 'peers' \
         and the one from the Octez node's configuration parameter \
         'dal_config.bootstrap_peers'."
      ~placeholder:"ADDR:PORT,..."
      ~parse:Result.ok
      ~pp:Format.pp_print_string
      ~default:default_list
      "peers"

  let peers = arg_list_to_cmdliner peers_arg

  let metrics_addr_arg =
    let default_port = Configuration_file.default_metrics_port in
    let parse, pp = p2p_point_format ~default_port in
    make_arg
      ~doc:
        "The TCP address and optionally the port of the node's metrics server. \
         The default address is 0.0.0.0. The default port is 11733."
      ~placeholder:"ADDR[:PORT]"
      ~pp
      ~parse
      "metrics-addr"

  let metrics_addr = arg_to_cmdliner metrics_addr_arg

  let history_mode_arg =
    let open Result_syntax in
    let doc =
      "The duration for the shards to be kept in the node storage. Either a \
       number, the string \"full\" or the string \"auto\". A number is \
       interpreted as the number of blocks the shards should be kept; the \
       string \"full\" means no shard deletion, the string \"auto\" means the \
       default of the profile: 3 months for an operator, twice the attestation \
       lag for an attester and other profiles."
    in
    let decoder =
      Configuration_file.(
        function
        | "full" -> return Full
        | "auto" -> return @@ Rolling {blocks = `Auto}
        | s -> (
            match int_of_string_opt s with
            | Some i -> return @@ Rolling {blocks = `Some i}
            | None -> Error ("Invalid argument " ^ s ^ " for history-mode.")))
    in
    let printer fmt = function
      | Configuration_file.Full -> Format.fprintf fmt "full"
      | Rolling {blocks = `Auto} -> Format.fprintf fmt "auto"
      | Rolling {blocks = `Some i} -> Format.fprintf fmt "%d" i
    in
    make_arg ~doc ~parse:decoder ~pp:printer "history-mode"

  let history_mode = arg_to_cmdliner history_mode_arg

  let service_name_env =
    make_env
      ~docs:"Opentelemetry"
      ~doc:"Enable to provide an opentelemetry service name"
      "OTEL_SERVICE_NAME"

  let service_name_arg =
    make_arg
      ~doc:
        "A name that can be used to identify this node. This name can appear \
         in observability data such as traces."
      ~env:service_name_env
      ~parse:Result.ok
      ~pp:Format.pp_print_string
      "service-name"

  let service_name = arg_to_cmdliner service_name_arg

  let service_namespace_env =
    make_env
      ~docs:"Opentelemetry"
      ~doc:"Enable to provide an opentelemetry service namespace"
      "OTEL_SERVICE_NAMESPACE"

  let service_namespace_arg =
    make_arg
      ~doc:
        "A namespace associated with the node. This namespace can appear in \
         observability data such as traces."
      ~env:service_namespace_env
      ~parse:Result.ok
      ~pp:Format.pp_print_string
      "service-namespace"

  let service_namespace = arg_to_cmdliner service_namespace_arg

  let fetch_trusted_setup_arg =
    make_arg
      ~doc:
        "Should the DAL node fetch the trusted setup when it needs it. By \
         default, it does so."
      ~placeholder:"true | false"
      ~parse:(fun s -> Result.ok (bool_of_string s))
      ~pp:Format.pp_print_bool
      "fetch-trusted-setup"

  let fetch_trusted_setup = arg_to_cmdliner fetch_trusted_setup_arg

  let disable_shard_validation_switch =
    make_switch
      ~doc:
        "Disable the shard verification. This is used conjoyintly with the \
         `TEZOS_DISABLE_SHARD_VERIFICATION_I_KNOW_WHAT_I_AM_DOING` environment \
         variable. To actually disable the shard verification this option must \
         be used and the environment variable must be set. If only the \
         environment variable is set, the DAL node will refuse to start. "
      "disable-shard-validation"

  let disable_shard_validation =
    switch_to_cmdliner disable_shard_validation_switch

  let verbose_switch =
    make_switch
      ~doc:
        "Controls the verbosity of some emitted events. Default value is false."
      "verbose"

  let verbose = switch_to_cmdliner verbose_switch

  let disable_amplification_switch =
    make_switch
      ~doc:"Disables shard amplification. Default value is false."
      "disable-amplification"

  let disable_amplification = switch_to_cmdliner disable_amplification_switch

  let ignore_topics_arg =
    let parse, pp = attester_profile_format in
    make_arg_list
      ~doc:
        "The producer Octez DAL node will not publish shards for the provided \
         pkhs. This argument is for testing purposes only."
      ~placeholder:"PKH1,PKH2,..."
      ~pp
      ~parse
      "ignore-topics"

  let ignore_topics = arg_list_to_cmdliner ignore_topics_arg

  let term process =
    Cmdliner.Term.(
      ret
        (const process $ data_dir $ config_file $ rpc_addr $ expected_pow
       $ net_addr $ public_addr $ endpoint $ slots_backup_uris
       $ trust_slots_backup_uris $ metrics_addr $ attester_profile
       $ operator_profile $ observer_profile $ bootstrap_profile $ peers
       $ history_mode $ service_name $ service_namespace $ fetch_trusted_setup
       $ disable_shard_validation $ verbose $ ignore_l1_config_peers
       $ disable_amplification $ ignore_topics))
end

type t = Run | Config_init | Config_update | Debug_print_store_schemas

module Run = struct
  let description =
    [`S "DESCRIPTION"; `P "This command runs an Octez DAL node."]

  let man = description

  let info =
    let version = Tezos_version_value.Bin_version.octez_version_string in
    Cmdliner.Cmd.info ~doc:"Run the Octez DAL node" ~man ~version "run"

  let cmd run = Cmdliner.Cmd.v info (Term.term (run Run))
end

module Config = struct
  let description =
    [
      `S "CONFIG DESCRIPTION";
      `P
        "Entry point for initializing, configuring and running an Octez DAL \
         node.";
    ]

  let man = description

  module Init = struct
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command creates a configuration file with the parameters \
           provided on the command-line, if no configuration file exists \
           already in the specified or default location. Otherwise, the \
           command-line parameters override the existing ones, and old \
           parameters are lost. This configuration is then used by the run \
           command.";
      ]

    let info =
      let version = Tezos_version_value.Bin_version.octez_version_string in
      Cmdliner.Cmd.info ~doc:"Configuration initialisation" ~man ~version "init"

    let cmd run = Cmdliner.Cmd.v info (Term.term (run Config_init))
  end

  module Update = struct
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command updates the configuration file with the parameters \
           provided on the command-line. If no configuration file exists \
           already, the command will fail.";
      ]

    let info =
      let version = Tezos_version_value.Bin_version.octez_version_string in
      Cmdliner.Cmd.info ~doc:"Configuration update" ~man ~version "update"

    let cmd run = Cmdliner.Cmd.v info (Term.term (run Config_update))
  end

  let cmd run =
    let default = Cmdliner.Term.(ret (const (`Help (`Pager, None)))) in
    let info =
      let version = Tezos_version_value.Bin_version.octez_version_string in
      Cmdliner.Cmd.info
        ~doc:"Manage the Octez DAL node configuration"
        ~man
        ~version
        "config"
    in
    Cmdliner.Cmd.group ~default info [Init.cmd run; Update.cmd run]
end

module Debug = struct
  let man = [`S "DEBUG DESCRIPTION"; `P "Entrypoint for the debug commands."]

  module Print = struct
    let man =
      [`S "PRINT DESCRIPTION"; `P "Entrypoint for printing debug information."]

    module Store = struct
      let man =
        [
          `S "STORE DESCRIPTION";
          `P
            "Entrypoint for printing debug information related to the DAL node \
             store.";
        ]

      module Schemas = struct
        let man =
          [
            `S "DESCRIPTION";
            `P
              "Print SQL statements describing the tables created in the store.";
          ]

        let info =
          let version = Tezos_version_value.Bin_version.octez_version_string in
          Cmdliner.Cmd.info ~doc:"Print SQL statements" ~man ~version "schemas"

        let cmd run = Cmdliner.Cmd.v info (Term.term run)
      end

      let cmd run =
        let default = Cmdliner.Term.(ret (const (`Help (`Pager, None)))) in
        let info =
          let version = Tezos_version_value.Bin_version.octez_version_string in
          Cmdliner.Cmd.info
            ~doc:"Print DAL node store debug information"
            ~man
            ~version
            "store"
        in
        Cmdliner.Cmd.group ~default info [Schemas.cmd run]
    end

    let cmd run =
      let default = Cmdliner.Term.(ret (const (`Help (`Pager, None)))) in
      let info =
        let version = Tezos_version_value.Bin_version.octez_version_string in
        Cmdliner.Cmd.info ~doc:"Print debug information" ~man ~version "print"
      in
      Cmdliner.Cmd.group ~default info [Store.cmd run]
  end

  let cmd run =
    let default = Cmdliner.Term.(ret (const (`Help (`Pager, None)))) in
    let info =
      let version = Tezos_version_value.Bin_version.octez_version_string in
      Cmdliner.Cmd.info ~doc:"Debug commands" ~man ~version "debug"
    in
    Cmdliner.Cmd.group ~default info [Print.cmd (run Debug_print_store_schemas)]
end

type experimental_features = unit

type options = {
  data_dir : string option;
  config_file : string option;
  rpc_addr : P2p_point.Id.t option;
  expected_pow : float option;
  listen_addr : P2p_point.Id.t option;
  public_addr : P2p_point.Id.t option;
  endpoint : Uri.t option;
  slots_backup_uris : Uri.t list;
  trust_slots_backup_uris : bool;
  profile : Profile_manager.unresolved_profile option;
  metrics_addr : P2p_point.Id.t option;
  peers : string list;
  history_mode : Configuration_file.history_mode option;
  service_name : string option;
  service_namespace : string option;
  experimental_features : experimental_features;
  fetch_trusted_setup : bool option;
  disable_shard_validation : bool;
  verbose : bool;
  ignore_l1_config_peers : bool;
  disable_amplification : bool;
  ignore_topics : Signature.public_key_hash list;
}

let cli_options_to_options data_dir config_file rpc_addr expected_pow
    listen_addr public_addr endpoint slots_backup_uris trust_slots_backup_uris
    metrics_addr attesters operators observers bootstrap_flag peers history_mode
    service_name service_namespace fetch_trusted_setup disable_shard_validation
    verbose ignore_l1_config_peers disable_amplification ignore_topics =
  let open Result_syntax in
  let profile = Controller_profiles.make ~attesters ~operators ?observers () in
  let* profile =
    match (bootstrap_flag, observers, profile) with
    | false, None, profiles when Controller_profiles.is_empty profiles ->
        return_none
    | false, Some _, profiles when Controller_profiles.is_empty profiles ->
        (* The user only mentioned '--observer' without any slot and
           without any other profile. It will be assigned to random
           slots. *)
        return_some Profile_manager.random_observer
    | false, _, _ -> return_some (Profile_manager.controller profile)
    | true, None, profiles when Controller_profiles.is_empty profiles ->
        return_some Profile_manager.bootstrap
    | true, _, _ ->
        fail
          ( false,
            "a bootstrap node (option '--bootstrap') cannot be an attester \
             (option '--attester'), an operator (option '--operator') nor an \
             observer (option '--observer')" )
  in
  return
    {
      data_dir;
      config_file;
      rpc_addr;
      expected_pow;
      listen_addr;
      public_addr;
      endpoint;
      slots_backup_uris;
      trust_slots_backup_uris;
      profile;
      metrics_addr;
      peers;
      history_mode;
      service_name;
      service_namespace;
      experimental_features = ();
      fetch_trusted_setup;
      disable_shard_validation;
      verbose;
      ignore_l1_config_peers;
      disable_amplification;
      ignore_topics;
    }

let merge_experimental_features _ _configuration = ()

let merge
    {
      data_dir;
      config_file = _;
      rpc_addr;
      expected_pow;
      listen_addr;
      public_addr;
      endpoint;
      slots_backup_uris;
      trust_slots_backup_uris;
      metrics_addr;
      profile;
      peers;
      history_mode;
      service_name;
      service_namespace;
      experimental_features;
      fetch_trusted_setup;
      disable_shard_validation = _;
      verbose;
      ignore_l1_config_peers;
      disable_amplification;
      ignore_topics = _;
    } configuration =
  let profile =
    match profile with
    | None -> configuration.Configuration_file.profile
    | Some from_cli ->
        (* Note that the profile from the CLI is prioritized over
           the profile provided in the config file. *)
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6110
           Improve profile configuration UX for when we have conflicting CLI and config file. *)
        Profile_manager.merge_profiles
          ~lower_prio:configuration.profile
          ~higher_prio:from_cli
  in
  let slots_backup_uris = slots_backup_uris @ configuration.slots_backup_uris in
  let trust_slots_backup_uris =
    trust_slots_backup_uris || configuration.trust_slots_backup_uris
  in
  {
    configuration with
    data_dir = Option.value ~default:configuration.data_dir data_dir;
    rpc_addr = Option.value ~default:configuration.rpc_addr rpc_addr;
    listen_addr = Option.value ~default:configuration.listen_addr listen_addr;
    public_addr = Option.value ~default:configuration.public_addr public_addr;
    expected_pow = Option.value ~default:configuration.expected_pow expected_pow;
    endpoint = Option.value ~default:configuration.endpoint endpoint;
    slots_backup_uris;
    trust_slots_backup_uris;
    profile;
    (* metrics are disabled unless a metrics_addr option is specified *)
    metrics_addr;
    peers = peers @ configuration.peers;
    history_mode = Option.value ~default:configuration.history_mode history_mode;
    service_name = Option.value ~default:configuration.service_name service_name;
    service_namespace =
      Option.value ~default:configuration.service_namespace service_namespace;
    fetch_trusted_setup =
      Option.value
        ~default:configuration.fetch_trusted_setup
        fetch_trusted_setup;
    experimental_features =
      merge_experimental_features
        experimental_features
        configuration.experimental_features;
    verbose = configuration.verbose || verbose;
    ignore_l1_config_peers =
      configuration.ignore_l1_config_peers || ignore_l1_config_peers;
    disable_amplification =
      configuration.disable_amplification || disable_amplification;
  }

let wrap_with_error main_promise =
  let open Lwt_syntax in
  let* r = Lwt_exit.wrap_and_exit main_promise in
  match r with
  | Ok () ->
      let* _ = Lwt_exit.exit_and_wait 0 in
      Lwt.return (`Ok ())
  | Error err ->
      let* _ = Lwt_exit.exit_and_wait 1 in
      Lwt.return @@ `Error (false, Format.asprintf "%a" pp_print_trace err)

let run subcommand cli_options =
  let open Lwt_result_syntax in
  let data_dir =
    Option.value
      ~default:Configuration_file.default.data_dir
      cli_options.data_dir
  in
  let config_file =
    Option.value
      cli_options.config_file
      ~default:(Configuration_file.default_config_file data_dir)
  in
  match subcommand with
  | Run ->
      let* () =
        if disable_shard_validation && not cli_options.disable_shard_validation
        then
          failwith
            "DAL shard validation is disabled but the option \
             '--disable-shard-validation' was not provided."
        else if
          (not disable_shard_validation) && cli_options.disable_shard_validation
        then
          failwith
            "DAL shard validation is enabled but the environment variable %s \
             was not set."
            disable_shard_validation_environment_variable
        else return_unit
      in
      let* ignore_pkhs =
        if env_ignore_topics && List.is_empty cli_options.ignore_topics then
          failwith
            "The environment variable to ignore topics %s was set, but the \
             option '--ignore-topics' was not provided."
            env_var_ignore_topics
        else if
          (not env_ignore_topics)
          && (not @@ List.is_empty cli_options.ignore_topics)
        then
          failwith
            "The option '--ignore-topics' was provided, but the environment \
             variable to ignore topics %s was not set."
            env_var_ignore_topics
        else return cli_options.ignore_topics
      in
      Daemon.run
        ~disable_shard_validation
        ~ignore_pkhs
        ~data_dir
        ~config_file
        ~configuration_override:(merge cli_options)
        ()
  | Config_init ->
      Configuration_file.save
        ~config_file
        (merge cli_options Configuration_file.default)
  | Config_update ->
      let config_file =
        Option.value
          cli_options.config_file
          ~default:
            (let data_dir =
               Option.value
                 ~default:Configuration_file.default.data_dir
                 cli_options.data_dir
             in
             Filename.concat data_dir "config.json")
      in
      let* configuration = Configuration_file.load ~config_file in
      Configuration_file.save ~config_file (merge cli_options configuration)
  | Debug_print_store_schemas ->
      Lwt_utils_unix.with_tempdir "store" @@ fun data_dir ->
      let* schemas = Store.Skip_list_cells.schemas data_dir in
      let output = String.concat ";\n\n" schemas in
      Format.printf "%s\n" output ;
      return_unit

let main_run subcommand cli_options =
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  Tezos_base_unix.Event_loop.main_run ~process_name:"dal node" @@ fun () ->
  wrap_with_error @@ run subcommand cli_options

let commands =
  let run subcommand data_dir config_file rpc_addr expected_pow listen_addr
      public_addr endpoint slots_backup_uris trust_slots_backup_uris
      metrics_addr attesters operators observers bootstrap_flag peers
      history_mode service_name service_namespace fetch_trusted_setup
      disable_shard_validation verbose ignore_l1_config_peers
      disable_amplification ignore_pkhs =
    match
      cli_options_to_options
        data_dir
        config_file
        rpc_addr
        expected_pow
        listen_addr
        public_addr
        endpoint
        slots_backup_uris
        trust_slots_backup_uris
        metrics_addr
        attesters
        operators
        observers
        bootstrap_flag
        peers
        history_mode
        service_name
        service_namespace
        fetch_trusted_setup
        disable_shard_validation
        verbose
        ignore_l1_config_peers
        disable_amplification
        ignore_pkhs
    with
    | Ok options -> main_run subcommand options
    | Error msg -> `Error msg
  in
  let default = Cmdliner.Term.(ret (const (`Help (`Pager, None)))) in
  let info =
    let version = Tezos_version_value.Bin_version.octez_version_string in
    Cmdliner.Cmd.info ~doc:"The Octez DAL node" ~version "octez-dal-node"
  in
  Cmdliner.Cmd.group ~default info [Run.cmd run; Config.cmd run; Debug.cmd run]
