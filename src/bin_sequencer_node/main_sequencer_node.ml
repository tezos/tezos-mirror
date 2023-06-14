(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

module Sc_sequencer = Octez_smart_rollup_sequencer
module Sc_rollup_node = Octez_smart_rollup_node_alpha

let group =
  {
    Tezos_clic.name = "sc_sequencer.node";
    title = "Commands related to the smart rollup sequencer node.";
  }

include Cli.Binary_dependent_args (struct
  let binary_name = "sequencer node"
end)

let sc_operator_pkh next =
  let open Lwt_result_syntax in
  Tezos_clic.param
    ~name:"operator"
    ~desc:"Public key hash, or alias, of a sequencer node operator."
    ( Tezos_clic.parameter @@ fun cctxt s ->
      let parse_pkh s =
        let from_alias s = Client_keys.Public_key_hash.find cctxt s in
        let from_key s =
          match Signature.Public_key_hash.of_b58check_opt s with
          | None ->
              failwith "Could not read public key hash for sequencer operator"
          | Some pkh -> return pkh
        in
        Client_aliases.parse_alternatives
          [("alias", from_alias); ("key", from_key)]
          s
      in
      match String.split ~limit:1 ':' s with
      | [_] ->
          let+ pkh = parse_pkh s in
          `Default pkh
      | [_purpose; _operator_s] ->
          failwith "Purposes are not supported for a sequencer operator"
      | _ ->
          (* cannot happen due to String.split's implementation. *)
          assert false )
    next

let config_init_command =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  let open Cli in
  command
    ~group
    ~desc:"Configure the sequencer node."
    (args13
       force_switch
       data_dir_arg
       rpc_addr_arg
       rpc_port_arg
       metrics_addr_arg
       reconnection_delay_arg
       dac_observer_endpoint_arg
       dac_timeout_arg
       injector_retention_period_arg
       injector_attempts_arg
       injection_ttl_arg
       log_kernel_debug_arg
       boot_sector_file_arg)
    (prefix "init"
    @@ prefixes ["config"; "for"]
    @@ sc_rollup_address_param
    @@ prefixes ["with"; "operator"]
    @@ sc_operator_pkh stop)
    (fun ( force,
           data_dir,
           rpc_addr,
           rpc_port,
           metrics_addr,
           reconnection_delay,
           dac_observer_endpoint,
           dac_timeout,
           injector_retention_period,
           injector_attempts,
           injection_ttl,
           log_kernel_debug,
           boot_sector_file )
         sc_rollup_address
         sc_sequencer_operator
         cctxt ->
      let*? config =
        Configuration.Cli.configuration_from_args
          ~rpc_addr
          ~rpc_port
          ~metrics_addr
          ~loser_mode:None
          ~reconnection_delay
          ~dal_node_endpoint:None
          ~dac_observer_endpoint
          ~dac_timeout
          ~injector_retention_period
          ~injector_attempts
          ~injection_ttl
          ~mode:Configuration.Batcher
          ~sc_rollup_address
          ~boot_sector_file
          ~sc_rollup_node_operators:[sc_sequencer_operator]
          ~log_kernel_debug
      in
      let* () = Configuration.save ~force ~data_dir config in
      let*! () =
        cctxt#message
          "Smart rollup node configuration written in %s"
          (Configuration.config_filename ~data_dir)
      in
      return_unit)

let run_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  let open Cli in
  command
    ~group
    ~desc:
      "Run the sequencer node daemon. Arguments overwrite values provided in \
       the configuration file."
    (args13
       data_dir_arg
       rpc_addr_arg
       rpc_port_arg
       metrics_addr_arg
       reconnection_delay_arg
       dac_observer_endpoint_arg
       dac_timeout_arg
       injector_retention_period_arg
       injector_attempts_arg
       injection_ttl_arg
       log_kernel_debug_arg
       log_kernel_debug_file_arg
       boot_sector_file_arg)
    (prefixes ["run"] @@ prefixes ["for"] @@ sc_rollup_address_param
    @@ prefixes ["with"; "operator"]
    @@ sc_operator_pkh stop)
    (fun ( data_dir,
           rpc_addr,
           rpc_port,
           metrics_addr,
           reconnection_delay,
           dac_observer_endpoint,
           dac_timeout,
           injector_retention_period,
           injector_attempts,
           injection_ttl,
           log_kernel_debug,
           log_kernel_debug_file,
           boot_sector_file )
         sc_rollup_address
         sc_sequencer_operator
         cctxt ->
      let* configuration =
        Configuration.Cli.create_or_read_config
          ~data_dir
          ~rpc_addr
          ~rpc_port
          ~metrics_addr
          ~loser_mode:None
          ~reconnection_delay
          ~dal_node_endpoint:None
          ~dac_observer_endpoint
          ~dac_timeout
          ~injector_retention_period
          ~injector_attempts
          ~injection_ttl
          ~mode:(Some Configuration.Batcher)
          ~sc_rollup_address:(Some sc_rollup_address)
          ~sc_rollup_node_operators:[sc_sequencer_operator]
          ~log_kernel_debug
          ~boot_sector_file
      in
      Sc_rollup_node.Daemon.run
        ~data_dir
        ?log_kernel_debug_file
        configuration
        ~daemon_components:(module Sc_sequencer.Components.Daemon_components)
        (new Protocol_client_context.wrap_full cctxt))

(** Command to dump the rollup node metrics. *)
let dump_metrics =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group
    ~desc:"dump the sequencer available metrics in CSV format."
    no_options
    (prefixes ["dump-metrics"] @@ stop)
    (fun () (cctxt : Client_context.full) ->
      let*! metrics =
        Prometheus.CollectorRegistry.collect Metrics.sc_rollup_node_registry
      in
      let*! () = cctxt#message "%a@." Metrics.print_csv_metrics metrics in
      return_unit)

let commands () = [config_init_command; run_command; dump_metrics]

let select_commands _ctxt _ =
  Lwt_result_syntax.return (commands () @ Client_helpers_commands.commands ())

let () = Client_main_run.run (module Daemon_config) ~select_commands
