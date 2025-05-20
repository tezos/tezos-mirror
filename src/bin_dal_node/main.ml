(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let merge_experimental_features _ _configuration = ()

let merge
    Cli.
      {
        data_dir;
        rpc_addr;
        expected_pow;
        listen_addr;
        public_addr;
        endpoint;
        http_backup_uris;
        trust_http_backup_uris;
        metrics_addr;
        profile;
        peers;
        history_mode;
        service_name;
        service_namespace;
        experimental_features;
        fetch_trusted_setup;
        verbose;
        ignore_l1_config_peers;
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
  let http_backup_uris, trust_http_backup_uris =
    (* backup URIs from the CLI, if any, are favored over the ones in the
       config file. *)
    if List.is_empty http_backup_uris then
      (configuration.http_backup_uris, configuration.trust_http_backup_uris)
    else (http_backup_uris, trust_http_backup_uris)
  in

  {
    configuration with
    data_dir = Option.value ~default:configuration.data_dir data_dir;
    rpc_addr = Option.value ~default:configuration.rpc_addr rpc_addr;
    listen_addr = Option.value ~default:configuration.listen_addr listen_addr;
    public_addr = Option.value ~default:configuration.public_addr public_addr;
    expected_pow = Option.value ~default:configuration.expected_pow expected_pow;
    endpoint = Option.value ~default:configuration.endpoint endpoint;
    http_backup_uris;
    trust_http_backup_uris;
    profile;
    (* metrics are disabled unless a metrics_addr option is specified *)
    metrics_addr;
    peers = peers @ configuration.peers;
    history_mode = Option.value ~default:configuration.history_mode history_mode;
    service_name = Option.either service_name configuration.service_name;
    service_namespace =
      Option.either service_namespace configuration.service_namespace;
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
  match subcommand with
  | Cli.Run ->
      let data_dir =
        Option.value
          ~default:Configuration_file.default.data_dir
          cli_options.Cli.data_dir
      in
      Lwt.Exception_filter.(set handle_all_except_runtime) ;
      Tezos_base_unix.Event_loop.main_run @@ fun () ->
      wrap_with_error
      @@ Daemon.run ~data_dir ~configuration_override:(merge cli_options)
  | Config_init ->
      Lwt.Exception_filter.(set handle_all_except_runtime) ;
      Tezos_base_unix.Event_loop.main_run @@ fun () ->
      wrap_with_error
      @@ Configuration_file.save (merge cli_options Configuration_file.default)
  | Config_update ->
      Lwt.Exception_filter.(set handle_all_except_runtime) ;
      Tezos_base_unix.Event_loop.main_run @@ fun () ->
      wrap_with_error
      @@
      let open Lwt_result_syntax in
      let data_dir =
        Option.value
          ~default:Configuration_file.default.data_dir
          cli_options.data_dir
      in
      let* configuration = Configuration_file.load ~data_dir in
      Configuration_file.save (merge cli_options configuration)
  | Debug_print_store_schemas ->
      let open Lwt_result_syntax in
      Tezos_base_unix.Event_loop.main_run @@ fun () ->
      wrap_with_error
      @@ Lwt_utils_unix.with_tempdir "store"
      @@ fun data_dir ->
      let* schemas = Store.Skip_list_cells.schemas data_dir in
      let output = String.concat ";\n\n" schemas in
      Format.printf "%s\n" output ;
      return_unit

let _ =
  (* Memtrace can be activated via the environment variable MEMTRACE
     whose value is the file collecting the trace. The trace can then
     be observed with [memtrace-viewer]. *)
  Memtrace.trace_if_requested () ;
  let commands = Cli.make ~run in
  exit @@ Cmdliner.Cmd.eval commands
