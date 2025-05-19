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

module Term = struct
  let p2p_point_arg ~default_port =
    let open Cmdliner in
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
                  (`Msg
                    (Format.asprintf "The port provided: '%s' is invalid" port))
              )
          | _ -> Error (`Msg msg))
    in
    let printer = P2p_point.Id.pp in
    Arg.conv (decoder, printer)

  let docs = "OPTIONS"

  let data_dir =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The directory where the Octez DAL node will store all its data. \
         Parent directories are created if necessary."
    in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~docv:"DIR" ~doc ["data-dir"; "d"])

  let rpc_addr =
    let open Cmdliner in
    let default_port = Configuration_file.default.rpc_addr |> snd in
    let doc =
      Format.asprintf
        "The TCP address and optionally the port at which the RPC server of \
         this instance can be reached. The default address is 0.0.0.0. The \
         default port is 10732."
    in
    Arg.(
      value
      & opt (some (p2p_point_arg ~default_port)) None
      & info ~docs ~doc ~docv:"ADDR[:PORT]" ["rpc-addr"])

  let expected_pow =
    let open Cmdliner in
    let doc =
      "The expected proof-of-work difficulty level for the peers' identity."
    in
    Arg.(
      value
      & opt (some float) None
      & info ~docs ~doc ~docv:"FLOAT" ["expected-pow"])

  let net_addr =
    let open Cmdliner in
    let default_port = Configuration_file.default.listen_addr |> snd in
    let doc =
      Format.asprintf
        "The TCP address and optionally the port bound by the DAL node. If \
         --public-addr is not provided, this is also the address and port at \
         which this instance can be reached by other P2P nodes. The default \
         address is 0.0.0.0. The default port is 11732."
    in
    Arg.(
      value
      & opt (some (p2p_point_arg ~default_port)) None
      & info ~docs ~doc ~docv:"ADDR[:PORT]" ["net-addr"])

  let public_addr =
    let open Cmdliner in
    let default_port = Configuration_file.default.public_addr |> snd in
    let doc =
      Format.asprintf
        "The TCP address and optionally the port at which this instance can be \
         reached by other P2P nodes. By default, the point is \
         '127.0.0.1:11732'. You can override the port using the syntax \
         ':2222'. If the IP address is detected as a special address (such as \
         a localhost one) it won't be advertised, only the port will."
    in
    Arg.(
      value
      & opt (some (p2p_point_arg ~default_port)) None
      & info ~docs ~doc ~docv:"ADDR[:PORT]" ["public-addr"])

  let uri_arg =
    let open Cmdliner in
    let decoder string =
      try Uri.of_string string |> Result.ok
      with _ -> Error (`Msg "The string '%s' is not a valid URI")
    in
    let printer = Uri.pp_hum in
    Arg.conv (decoder, printer)

  let endpoint =
    let open Cmdliner in
    let doc =
      "The endpoint (an URI) of the Tezos node that the DAL node should \
       connect to. The default endpoint is 'http://localhost:8732'."
    in
    Arg.(
      value
      & opt (some uri_arg) None
      & info ~docs ~doc ~docv:"URI" ["endpoint"; "E"])

  let http_backup_uris =
    let open Cmdliner in
    let doc =
      "List of HTTP base URIs to fetch missing DAL slots if they are \
       unavailable locally or cannot be reconstructed from shards. This option \
       can be specified multiple times to provide fallback sources."
    in
    Arg.(value & opt_all uri_arg [] & info ~doc ~docv:"URI" ["http-backup"])

  let trust_http_backup_uris =
    let open Cmdliner in
    let doc =
      "If set, skip cryptographic verification of slots downloaded from HTTP \
       backup URIs. Default is false. This can speed up slot retrieval when \
       replaying history or for debugging purposes, but should be used with \
       caution for normal operation or in the context of refutation games \
       (unless the HTTP source is fully trusted)."
    in
    Arg.(value & flag & info ~doc ["trust-http-backup-uris"])

  let ignore_l1_config_peers =
    let open Cmdliner in
    let doc = "Ignore the boot(strap) peers provided by L1 config." in
    Arg.(value & flag & info ~docs ~doc ["ignore-l1-config-peers"])

  let attester_profile_printer = Signature.Public_key_hash.pp

  let producer_profile_printer = Format.pp_print_int

  let attester_profile_arg =
    let open Cmdliner in
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
      | None -> Error (`Msg "Unrecognized profile")
      | Some pkh -> Ok pkh
    in
    Arg.conv (decoder, attester_profile_printer)

  let producer_profile_arg =
    let open Cmdliner in
    let decoder string =
      let error () =
        Format.kasprintf
          (fun s -> Error (`Msg s))
          "Unrecognized profile for producer (expected non-negative integer, \
           got %s)"
          string
      in
      match int_of_string_opt string with
      | None -> error ()
      | Some i when i < 0 -> error ()
      | Some slot_index -> Ok slot_index
    in
    Arg.conv (decoder, producer_profile_printer)

  let observer_profile_arg =
    let open Cmdliner in
    let decoder string =
      let error () =
        Format.kasprintf
          (fun s -> Error (`Msg s))
          "Unrecognized profile for observer (expected nonnegative integer, \
           got %s)"
          string
      in
      match int_of_string_opt string with
      | None -> error ()
      | Some i when i < 0 -> error ()
      | Some slot_index -> Ok slot_index
    in
    Arg.conv (decoder, producer_profile_printer)

  let attester_profile =
    let open Cmdliner in
    let doc =
      "The Octez DAL node attester profiles for given public key hashes."
    in
    Arg.(
      value
      & opt (list attester_profile_arg) []
      & info ~docs ~doc ~docv:"PKH1,PKH2,..." ["attester-profiles"; "attester"])

  let operator_profile =
    let open Cmdliner in
    let doc =
      "The Octez DAL node operator profiles for given slot indexes. These were \
       previously known as producer profiles, however this name now refers to \
       both operator and observer profiles."
    in
    Arg.(
      value
      & opt (list producer_profile_arg) []
      & info
          ~docs
          ~doc
          ~docv:"INDEX1,INDEX2,..."
          ["producer-profiles"; "producer"; "operator-profiles"; "operator"])

  let observer_profile =
    let open Cmdliner in
    let doc = "The Octez DAL node observer profiles for given slot indexes." in
    Arg.(
      value
      & opt (some' (list observer_profile_arg)) None
      & info
          ~docs
          ~doc
          ~docv:"INDEX1,INDEX2,..."
          ["observer-profiles"; "observer"])

  let bootstrap_profile =
    let open Cmdliner in
    let doc =
      "The Octez DAL node bootstrap node profile. Note that a bootstrap node \
       cannot also be an attester or a slot producer"
    in
    Arg.(value & flag & info ~docs ~doc ["bootstrap-profile"; "bootstrap"])

  let peers =
    let open Cmdliner in
    let default_list = Configuration_file.default.peers in
    let doc =
      "An additional list of peers (bootstrap or not) to connect to, expanding \
       the one from the DAL node's configuration parameter 'peers' and the one \
       from the Octez node's configuration parameter \
       'dal_config.bootstrap_peers'."
    in
    Arg.(
      value
      & opt (list string) default_list
      & info ~docs ~doc ~docv:"ADDR:PORT,..." ["peers"])

  let metrics_addr =
    let open Cmdliner in
    let doc =
      "The TCP address and optionally the port of the node's metrics server. \
       The default address is 0.0.0.0. The default port is 11733."
    in
    let default_port = Configuration_file.default_metrics_port in
    Arg.(
      value
      & opt (some (p2p_point_arg ~default_port)) None
      & info ~docs ~doc ~docv:"ADDR[:PORT]" ["metrics-addr"])

  let history_mode =
    let open Cmdliner in
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
            | None ->
                Error (`Msg ("Invalid argument " ^ s ^ " for history-mode."))))
    in
    let printer fmt = function
      | Configuration_file.Full -> Format.fprintf fmt "full"
      | Rolling {blocks = `Auto} -> Format.fprintf fmt "auto"
      | Rolling {blocks = `Some i} -> Format.fprintf fmt "%d" i
    in
    let history_mode_arg = Arg.conv (decoder, printer) in
    Arg.(
      value
      & opt (some history_mode_arg) None
      & info ~docs ~doc ["history-mode"])

  let service_name_env =
    let open Cmdliner in
    Cmd.Env.info
      ~docs:"Opentelemetry"
      ~doc:"Enable to provide an opentelemetry service name"
      "OTEL_SERVICE_NAME"

  let service_name =
    let open Cmdliner in
    let doc =
      "A name that can be used to identify this node. This name can appear in \
       observability data such as traces."
    in
    let service_name_arg = Arg.string in
    Arg.(
      value
      & opt (some service_name_arg) None
      & info ~docs ~doc ~env:service_name_env ["service-name"])

  let service_namespace_env =
    let open Cmdliner in
    Cmd.Env.info
      ~docs:"Opentelemetry"
      ~doc:"Enable to provide an opentelemetry service namespace"
      "OTEL_SERVICE_NAMESPACE"

  let service_namespace =
    let open Cmdliner in
    let doc =
      "A namespace associated with the node. This namespace can appear in \
       observability data such as traces."
    in
    let service_namespace_arg = Arg.string in
    Arg.(
      value
      & opt (some service_namespace_arg) None
      & info ~docs ~doc ~env:service_namespace_env ["service-namespace"])

  let fetch_trusted_setup =
    let open Cmdliner in
    let doc =
      "Should the DAL node fetch the trusted setup when it needs it. By \
       default, it does so."
    in
    Arg.(
      value
      & opt (some bool) None
      & info ~docs ~doc ~docv:"true | false" ["fetch-trusted-setup"])

  let verbose =
    let open Cmdliner in
    let doc =
      "Controls the verbosity of some emitted events. Default value is false."
    in
    Arg.(value & flag & info ~docs ~doc ["verbose"])

  (* Experimental features. *)

  let term process =
    Cmdliner.Term.(
      ret
        (const process $ data_dir $ rpc_addr $ expected_pow $ net_addr
       $ public_addr $ endpoint $ http_backup_uris $ trust_http_backup_uris
       $ metrics_addr $ attester_profile $ operator_profile $ observer_profile
       $ bootstrap_profile $ peers $ history_mode $ service_name
       $ service_namespace $ fetch_trusted_setup $ verbose
       $ ignore_l1_config_peers))
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
  rpc_addr : P2p_point.Id.t option;
  expected_pow : float option;
  listen_addr : P2p_point.Id.t option;
  public_addr : P2p_point.Id.t option;
  endpoint : Uri.t option;
  http_backup_uris : Uri.t list;
  trust_http_backup_uris : bool;
  profile : Profile_manager.unresolved_profile option;
  metrics_addr : P2p_point.Id.t option;
  peers : string list;
  history_mode : Configuration_file.history_mode option;
  service_name : string option;
  service_namespace : string option;
  experimental_features : experimental_features;
  fetch_trusted_setup : bool option;
  verbose : bool;
  ignore_l1_config_peers : bool;
}

let make ~run =
  let run subcommand data_dir rpc_addr expected_pow listen_addr public_addr
      endpoint http_backup_uris trust_http_backup_uris metrics_addr attesters
      operators observers bootstrap_flag peers history_mode service_name
      service_namespace fetch_trusted_setup verbose ignore_l1_config_peers =
    let run profile =
      run
        subcommand
        {
          data_dir;
          rpc_addr;
          expected_pow;
          listen_addr;
          public_addr;
          endpoint;
          http_backup_uris;
          trust_http_backup_uris;
          profile;
          metrics_addr;
          peers;
          history_mode;
          service_name;
          service_namespace;
          experimental_features = ();
          fetch_trusted_setup;
          verbose;
          ignore_l1_config_peers;
        }
    in
    let profile =
      Controller_profiles.make ~attesters ~operators ?observers ()
    in
    match (bootstrap_flag, observers, profile) with
    | false, None, profiles when Controller_profiles.is_empty profiles ->
        run None
    | false, Some _, profiles when Controller_profiles.is_empty profiles ->
        (* The user only mentioned '--observer' without any slot and
           without any other profile. It will be assigned to random
           slots. *)
        run (Some Profile_manager.random_observer)
    | false, _, _ -> run @@ Some (Profile_manager.controller profile)
    | true, None, profiles when Controller_profiles.is_empty profiles ->
        run @@ Some Profile_manager.bootstrap
    | true, _, _ ->
        `Error
          ( false,
            "a bootstrap node (option '--bootstrap') cannot be an attester \
             (option '--attester'), an operator (option '--operator') nor an \
             observer (option '--observer')" )
  in
  let default = Cmdliner.Term.(ret (const (`Help (`Pager, None)))) in
  let info =
    let version = Tezos_version_value.Bin_version.octez_version_string in
    Cmdliner.Cmd.info ~doc:"The Octez DAL node" ~version "octez-dal-node"
  in
  Cmdliner.Cmd.group ~default info [Run.cmd run; Config.cmd run; Debug.cmd run]
