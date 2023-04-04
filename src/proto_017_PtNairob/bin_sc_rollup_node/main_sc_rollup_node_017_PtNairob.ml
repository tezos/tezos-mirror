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

let force_switch =
  Tezos_clic.switch
    ~long:"force"
    ~doc:"Overwrites the configuration file when it exists."
    ()

let sc_rollup_address_parameter =
  Tezos_clic.parameter (fun _ s ->
      match Protocol.Alpha_context.Sc_rollup.Address.of_b58check_opt s with
      | None -> failwith "Invalid smart rollup address"
      | Some addr -> return addr)

let sc_rollup_address_param =
  Tezos_clic.param
    ~name:"smart-rollup-address"
    ~desc:"The smart rollup address"
    sc_rollup_address_parameter

let sc_rollup_address_arg =
  Tezos_clic.arg
    ~long:"rollup"
    ~placeholder:"smart-rollup-address"
    ~doc:"The smart rollup address (required when no configuration file exists)"
    sc_rollup_address_parameter

let sc_rollup_node_operator_param =
  let open Lwt_result_syntax in
  Tezos_clic.param
    ~name:"operator"
    ~desc:
      (Printf.sprintf
         "Public key hash, or alias, of a smart rollup node operator. An \
          operator can be specialized to a particular purpose by prefixing its \
          key or alias by said purpose, e.g. publish:alias_of_my_operator. The \
          possible purposes are: %s."
         (String.concat ", "
         @@ Configuration.(List.map string_of_purpose purposes)))
  @@ Tezos_clic.parameter
  @@ fun cctxt s ->
  let parse_pkh s =
    let from_alias s = Client_keys.Public_key_hash.find cctxt s in
    let from_key s =
      match Signature.Public_key_hash.of_b58check_opt s with
      | None ->
          failwith "Could not read public key hash for rollup node operator"
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
  | [purpose; operator_s] -> (
      match Configuration.purpose_of_string purpose with
      | Some purpose ->
          let+ pkh = parse_pkh operator_s in
          `Purpose (purpose, pkh)
      | None ->
          let+ pkh = parse_pkh s in
          `Default pkh)
  | _ ->
      (* cannot happen due to String.split's implementation. *)
      assert false

let possible_modes = List.map Configuration.string_of_mode Configuration.modes

let mode_parameter =
  Tezos_clic.parameter
    ~autocomplete:(fun _ -> return possible_modes)
    (fun _ m -> Lwt.return (Configuration.mode_of_string m))

let mode_doc =
  Format.asprintf
    "The mode for the rollup node (%s)@\n%a"
    (String.concat ", " possible_modes)
    (Format.pp_print_list (fun fmt mode ->
         Format.fprintf
           fmt
           "- %s: %s"
           (Configuration.string_of_mode mode)
           (Configuration.description_of_mode mode)))
    Configuration.modes

let mode_param = Tezos_clic.param ~name:"mode" ~desc:mode_doc mode_parameter

let mode_arg =
  Tezos_clic.arg
    ~long:"mode"
    ~placeholder:"mode"
    ~doc:(mode_doc ^ "\n(required when no configuration file exists)")
    mode_parameter

let rpc_addr_arg =
  let default = Configuration.default_rpc_addr in
  Tezos_clic.arg
    ~long:"rpc-addr"
    ~placeholder:"rpc-address|ip"
    ~doc:
      (Format.sprintf
         "The address the smart rollup node listens to. Default value is %s"
         default)
    Client_proto_args.string_parameter

let metrics_addr_arg =
  Tezos_clic.arg
    ~long:"metrics-addr"
    ~placeholder:
      "ADDR:PORT or :PORT (by default ADDR is localhost and PORT is 9933)"
    ~doc:"The address of the smart rollup node metrics server."
    Client_proto_args.string_parameter

let dal_node_endpoint_arg =
  Tezos_clic.arg
    ~long:"dal-node"
    ~placeholder:"dal-node-endpoint"
    ~doc:
      (Format.sprintf
         "The address of the dal node from which the smart rollup node \
          downloads slots. When not provided, the rollup node will not support \
          the DAL. In production, a DAL node must be provided if DAL is \
          enabled and used in the rollup.")
    (Tezos_clic.parameter (fun _ s -> Lwt.return_ok (Uri.of_string s)))

let rpc_port_arg =
  let default = Configuration.default_rpc_port |> string_of_int in
  Tezos_clic.arg
    ~long:"rpc-port"
    ~placeholder:"rpc-port"
    ~doc:
      (Format.sprintf
         "The port the smart rollup node listens to. Default value is %s"
         default)
    Client_proto_args.int_parameter

let data_dir_arg =
  let default = Configuration.default_data_dir in
  Tezos_clic.default_arg
    ~long:"data-dir"
    ~placeholder:"data-dir"
    ~doc:
      (Format.sprintf
         "The path to the smart rollup node data directory. Default value is %s"
         default)
    ~default
    Client_proto_args.string_parameter

let loser_mode_arg =
  Tezos_clic.arg
    ~long:"loser-mode"
    ~placeholder:"mode"
    ~doc:"Set the rollup node failure points (for test only!)."
    (Tezos_clic.parameter (fun _ s ->
         match Loser_mode.make s with
         | Some t -> return t
         | None -> failwith "Invalid syntax for failure points"))

let reconnection_delay_arg =
  let default =
    Format.sprintf "%.1f" Configuration.default_reconnection_delay
  in
  let doc =
    Format.asprintf
      "The first reconnection delay, in seconds, to wait before reconnecting \
       to the Tezos node. The default delay is %s.\n\
       The actual delay varies to follow a randomized exponential backoff \
       (capped to 1.5h): [1.5^reconnection_attempt * delay ± 50%%]."
      default
  in
  Tezos_clic.arg
    ~long:"reconnection-delay"
    ~placeholder:"delay"
    ~doc
    (Tezos_clic.parameter (fun _ p ->
         try return (float_of_string p) with _ -> failwith "Cannot read float"))

let injector_retention_period_arg =
  Tezos_clic.arg
    ~long:"injector-retention-period"
    ~placeholder:"blocks"
    ~doc:
      (Format.sprintf
         "The number of blocks the injector keeps in memory. Decrease to free \
          memory, and increase to be able to query information about included \
          messages for longer. Default value is %d"
         Configuration.default_injector_retention_period)
  @@ Tezos_clic.map_parameter Client_proto_args.int_parameter ~f:(fun p ->
         if p > Configuration.max_injector_retention_period then
           Format.ksprintf
             Stdlib.failwith
             "injector-retention-period should be smaller than %d"
             Configuration.max_injector_retention_period ;
         p)

let group =
  {
    Tezos_clic.name = "sc_rollup.node";
    title = "Commands related to the smart rollup node.";
  }

let make_operators sc_rollup_node_operators =
  let open Configuration in
  let purposed_operators, default_operators =
    List.partition_map
      (function
        | `Purpose p_operator -> Left p_operator
        | `Default operator -> Right operator)
      sc_rollup_node_operators
  in
  let default_operator =
    match default_operators with
    | [] -> None
    | [default_operator] -> Some default_operator
    | _ -> Stdlib.failwith "Multiple default operators"
  in
  make_purpose_map purposed_operators ~default:default_operator

let configuration_from_args ~rpc_addr ~rpc_port ~metrics_addr ~loser_mode
    ~reconnection_delay ~dal_node_endpoint ~injector_retention_period ~mode
    ~sc_rollup_address ~sc_rollup_node_operators =
  let open Configuration in
  let sc_rollup_node_operators = make_operators sc_rollup_node_operators in
  let config =
    {
      sc_rollup_address;
      sc_rollup_node_operators;
      rpc_addr = Option.value ~default:default_rpc_addr rpc_addr;
      rpc_port = Option.value ~default:default_rpc_port rpc_port;
      reconnection_delay =
        Option.value ~default:default_reconnection_delay reconnection_delay;
      dal_node_endpoint;
      metrics_addr;
      fee_parameters = Operator_purpose_map.empty;
      mode;
      loser_mode = Option.value ~default:Loser_mode.no_failures loser_mode;
      batcher = Configuration.default_batcher;
      injector_retention_period =
        Option.value
          ~default:Configuration.default_injector_retention_period
          injector_retention_period;
      l2_blocks_cache_size = Configuration.default_l2_blocks_cache_size;
    }
  in
  check_mode config

let patch_configuration_from_args configuration ~rpc_addr ~rpc_port
    ~metrics_addr ~loser_mode ~reconnection_delay ~dal_node_endpoint
    ~injector_retention_period ~mode ~sc_rollup_address
    ~sc_rollup_node_operators =
  let open Configuration in
  let new_sc_rollup_node_operators = make_operators sc_rollup_node_operators in
  (* Merge operators *)
  let sc_rollup_node_operators =
    Operator_purpose_map.merge
      (fun _purpose new_operator _old_operator -> new_operator)
      new_sc_rollup_node_operators
      configuration.sc_rollup_node_operators
  in
  let configuration =
    Configuration.
      {
        configuration with
        sc_rollup_address =
          Option.value
            ~default:configuration.sc_rollup_address
            sc_rollup_address;
        sc_rollup_node_operators;
        mode = Option.value ~default:configuration.mode mode;
        rpc_addr = Option.value ~default:configuration.rpc_addr rpc_addr;
        rpc_port = Option.value ~default:configuration.rpc_port rpc_port;
        dal_node_endpoint =
          Option.either dal_node_endpoint configuration.dal_node_endpoint;
        reconnection_delay =
          Option.value
            ~default:configuration.reconnection_delay
            reconnection_delay;
        injector_retention_period =
          Option.value
            ~default:configuration.injector_retention_period
            injector_retention_period;
        loser_mode = Option.value ~default:configuration.loser_mode loser_mode;
        metrics_addr = Option.either metrics_addr configuration.metrics_addr;
      }
  in
  Configuration.check_mode configuration

let create_or_read_config ~data_dir ~rpc_addr ~rpc_port ~metrics_addr
    ~loser_mode ~reconnection_delay ~dal_node_endpoint
    ~injector_retention_period ~mode ~sc_rollup_address
    ~sc_rollup_node_operators =
  let open Lwt_result_syntax in
  let config_file = Configuration.config_filename ~data_dir in
  let*! exists_config = Lwt_unix.file_exists config_file in
  if exists_config then
    (* Read configuration from file and patch if user wanted to override
       some fields with values provided by arguments. *)
    let* configuration = Configuration.load ~data_dir in
    let*? configuration =
      patch_configuration_from_args
        configuration
        ~rpc_addr
        ~rpc_port
        ~metrics_addr
        ~loser_mode
        ~reconnection_delay
        ~dal_node_endpoint
        ~injector_retention_period
        ~mode
        ~sc_rollup_address
        ~sc_rollup_node_operators
    in
    return configuration
  else
    (* Build configuration from arguments only. *)
    let*? mode =
      Option.value_e
        mode
        ~error:
          (TzTrace.make
          @@ error_of_fmt
               "Argument --mode is required when configuration file is not \
                present.")
    in
    let*? sc_rollup_address =
      Option.value_e
        sc_rollup_address
        ~error:
          (TzTrace.make
          @@ error_of_fmt
               "Argument --rollup is required when configuration file is not \
                present.")
    in
    let*? config =
      configuration_from_args
        ~rpc_addr
        ~rpc_port
        ~metrics_addr
        ~loser_mode
        ~reconnection_delay
        ~dal_node_endpoint
        ~injector_retention_period
        ~mode
        ~sc_rollup_address
        ~sc_rollup_node_operators
    in
    return config

let config_init_command =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  command
    ~group
    ~desc:"Configure the smart rollup node."
    (args9
       force_switch
       data_dir_arg
       rpc_addr_arg
       rpc_port_arg
       metrics_addr_arg
       loser_mode_arg
       reconnection_delay_arg
       dal_node_endpoint_arg
       injector_retention_period_arg)
    (prefix "init" @@ mode_param
    @@ prefixes ["config"; "for"]
    @@ sc_rollup_address_param
    @@ prefixes ["with"; "operators"]
    @@ seq_of_param @@ sc_rollup_node_operator_param)
    (fun ( force,
           data_dir,
           rpc_addr,
           rpc_port,
           metrics_addr,
           loser_mode,
           reconnection_delay,
           dal_node_endpoint,
           injector_retention_period )
         mode
         sc_rollup_address
         sc_rollup_node_operators
         cctxt ->
      let*? config =
        configuration_from_args
          ~rpc_addr
          ~rpc_port
          ~metrics_addr
          ~loser_mode
          ~reconnection_delay
          ~dal_node_endpoint
          ~injector_retention_period
          ~mode
          ~sc_rollup_address
          ~sc_rollup_node_operators
      in
      let* () = Configuration.save ~force ~data_dir config in
      let*! () =
        cctxt#message
          "Smart rollup node configuration written in %s"
          (Configuration.config_filename ~data_dir)
      in
      return_unit)

let legacy_run_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group
    ~desc:"Run the rollup node daemon (deprecated)."
    (args10
       data_dir_arg
       mode_arg
       sc_rollup_address_arg
       rpc_addr_arg
       rpc_port_arg
       metrics_addr_arg
       loser_mode_arg
       reconnection_delay_arg
       dal_node_endpoint_arg
       injector_retention_period_arg)
    (prefixes ["run"] @@ stop)
    (fun ( data_dir,
           mode,
           sc_rollup_address,
           rpc_addr,
           rpc_port,
           metrics_addr,
           loser_mode,
           reconnection_delay,
           dal_node_endpoint,
           injector_retention_period )
         cctxt ->
      let* configuration =
        create_or_read_config
          ~data_dir
          ~rpc_addr
          ~rpc_port
          ~metrics_addr
          ~loser_mode
          ~reconnection_delay
          ~dal_node_endpoint
          ~injector_retention_period
          ~mode
          ~sc_rollup_address
          ~sc_rollup_node_operators:[]
      in
      Daemon.run ~data_dir configuration cctxt)

let run_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group
    ~desc:
      "Run the rollup node daemon. Arguments overwrite values provided in the \
       configuration file."
    (args8
       data_dir_arg
       rpc_addr_arg
       rpc_port_arg
       metrics_addr_arg
       loser_mode_arg
       reconnection_delay_arg
       dal_node_endpoint_arg
       injector_retention_period_arg)
    (prefixes ["run"] @@ mode_param @@ prefixes ["for"]
   @@ sc_rollup_address_param
    @@ prefixes ["with"; "operators"]
    @@ seq_of_param @@ sc_rollup_node_operator_param)
    (fun ( data_dir,
           rpc_addr,
           rpc_port,
           metrics_addr,
           loser_mode,
           reconnection_delay,
           dal_node_endpoint,
           injector_retention_period )
         mode
         sc_rollup_address
         sc_rollup_node_operators
         cctxt ->
      let* configuration =
        create_or_read_config
          ~data_dir
          ~rpc_addr
          ~rpc_port
          ~metrics_addr
          ~loser_mode
          ~reconnection_delay
          ~dal_node_endpoint
          ~injector_retention_period
          ~mode:(Some mode)
          ~sc_rollup_address:(Some sc_rollup_address)
          ~sc_rollup_node_operators
      in
      Daemon.run ~data_dir configuration cctxt)

(** Command to dump the rollup node metrics. *)
let dump_metrics =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group
    ~desc:"dump the rollup node available metrics in CSV format."
    no_options
    (prefixes ["dump-metrics"] @@ stop)
    (fun () (cctxt : Protocol_client_context.full) ->
      let*! metrics =
        Prometheus.CollectorRegistry.collect Metrics.sc_rollup_node_registry
      in
      let*! () = cctxt#message "%a@." Metrics.print_csv_metrics metrics in
      return_unit)

let sc_rollup_commands () =
  List.map
    (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
    [config_init_command; run_command; legacy_run_command; dump_metrics]

let select_commands _ctxt _ =
  Lwt_result_syntax.return
    (sc_rollup_commands () @ Client_helpers_commands.commands ())

let () = Client_main_run.run (module Daemon_config) ~select_commands
