(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

let force_switch : (bool, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"force"
    ~doc:"Overwrites the configuration file when it exists."
    ()

let import_force_switch : (bool, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"force"
    ~doc:"Import into an already populated data dir."
    ()

let sc_rollup_address_param x =
  Smart_rollup_alias.Address.param
    ~name:"smart-rollup-address"
    ~desc:"The smart rollup address"
    x

let sc_rollup_address_arg : (_, Client_context.full) Tezos_clic.arg =
  Tezos_clic.arg
    ~long:"rollup"
    ~placeholder:"smart-rollup-address"
    ~doc:"The smart rollup address (required when no configuration file exists)"
    (Smart_rollup_alias.Address.parameter ())

(* Rollup node only arguments *)

let operator_param next =
  let open Lwt_result_syntax in
  let desc =
    Format.asprintf
      "Public key hash, or alias, of a smart rollup node operator. An operator \
       can be specialized to a particular purpose by prefixing its key or \
       alias by said purpose, e.g. operating:<alias_of_my_operator>. The \
       possible purposes are: @[<h>%a@]."
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
         Purpose.pp_ex_purpose)
      Purpose.all
  in
  let parse_default cctxt s =
    let* key = Client_keys.Public_key_hash.parse_source_string cctxt s in
    return (`Default key)
  in
  let parse_purpose purpose cctxt s =
    let* key = Client_keys.Public_key_hash.parse_source_string cctxt s in
    return (`Purpose (purpose, key))
  in
  let all_purpose_case cctxt =
    List.map
      (fun purpose ->
        (Purpose.to_string_ex_purpose purpose, parse_purpose purpose cctxt))
      Purpose.all
  in
  Tezos_clic.param
    ~name:"operator"
    ~desc
    (Tezos_clic.parameter (fun (cctxt : #Client_context.full) s ->
         Client_aliases.parse_alternatives
           (("default", parse_default cctxt) :: all_purpose_case cctxt)
           s))
    next

let possible_modes = List.map Configuration.string_of_mode Configuration.modes

let mode_parameter =
  Tezos_clic.parameter
    ~autocomplete:(fun (_cctxt : Client_context.full) ->
      Lwt_result.return possible_modes)
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

let mode_param params =
  Tezos_clic.param ~name:"mode" ~desc:mode_doc mode_parameter params

let mode_arg =
  Tezos_clic.arg
    ~long:"mode"
    ~placeholder:"mode"
    ~doc:(mode_doc ^ "\n(required when no configuration file exists)")
    mode_parameter

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
    (Tezos_clic.parameter (fun (_cctxt : Client_context.full) s ->
         Lwt.return_ok (Uri.of_string s)))

let pre_images_endpoint_arg =
  Tezos_clic.arg
    ~long:"pre-images-endpoint"
    ~placeholder:"url"
    ~doc:
      (Format.sprintf
         "The address of a service which provides pre-images for the rollup. \
          Missing pre-images will be downloaded remotely if they are not \
          already present on disk.")
    (Tezos_clic.parameter (fun (_cctxt : Client_context.full) s ->
         Lwt.return_ok (Uri.of_string s)))

let loser_mode_arg =
  Tezos_clic.arg
    ~long:"loser-mode"
    ~placeholder:"mode"
    ~doc:"Set the rollup node failure points (for test only!)."
    (Tezos_clic.parameter (fun (_cctxt : Client_context.full) s ->
         match Loser_mode.make s with
         | Some t -> Lwt_result.return t
         | None -> failwith "Invalid syntax for failure points"))

(* Primitive argument parsers *)
let string_parameter =
  Tezos_clic.parameter (fun (_cctxt : Client_context.full) x ->
      Lwt_result.return x)

let hex_or_file_parameter =
  Tezos_clic.parameter (fun (cctxt : Client_context.full) h ->
      let open Lwt_result_syntax in
      match String.remove_prefix ~prefix:"file:" h with
      | Some path ->
          Lwt.catch
            (fun () -> Lwt_result.ok @@ Lwt_utils_unix.read_file path)
            (fun exn ->
              cctxt#error
                "%s is not a valid file (%s)"
                path
                (Printexc.to_string exn))
      | None -> (
          match Hex.to_string (`Hex h) with
          | None -> cctxt#error "Parameter is not a valid hex-encoded string"
          | Some b -> return b))

let int_parameter =
  Tezos_clic.parameter (fun (cctxt : Client_context.full) p ->
      try Lwt_result.return (int_of_string p)
      with _ -> cctxt#error "Cannot read int")

let z_parameter =
  Tezos_clic.parameter (fun (cctxt : Client_context.full) s ->
      try
        let open Lwt_result_syntax in
        let v = Z.of_string s in
        return v
      with _ -> cctxt#error "Invalid number, must be a non negative number.")

let int32_parameter =
  Tezos_clic.parameter (fun (cctxt : Client_context.full) p ->
      try Lwt_result.return (Int32.of_string p)
      with _ -> cctxt#error "Cannot read int")

module Binary_dependent_args (P : sig
  val binary_name : string
end) =
struct
  open P

  let rpc_addr_arg =
    let default = Configuration.default_rpc_addr in
    Tezos_clic.arg
      ~long:"rpc-addr"
      ~placeholder:"rpc-address|ip"
      ~doc:
        (Format.sprintf
           "The address the %s listens to. Default value is %s"
           binary_name
           default)
      string_parameter

  let metrics_addr_arg =
    Tezos_clic.arg
      ~long:"metrics-addr"
      ~placeholder:"ADDR:PORT|:PORT"
      ~doc:
        (Format.sprintf
           "Start the %s metrics server at the specified address ADDR:PORT. \
            Default value is localhost:%d"
           binary_name
           Configuration.default_metrics_port)
      string_parameter

  let enable_performance_metrics_arg :
      (bool, Client_context.full) Tezos_clic.arg =
    Tezos_clic.switch
      ~long:"enable-performance-metrics"
      ~doc:
        "Enable performance metrics when the metrics server is started and the \
         system supports it. DEPRECATED (enabled by default when the system \
         supports it)."
      ()

  let disable_performance_metrics_arg :
      (bool, Client_context.full) Tezos_clic.arg =
    Tezos_clic.switch
      ~long:"disable-performance-metrics"
      ~doc:
        "Disable performance metrics when the metrics server is started \
         (enabled by default when system supports it)."
      ()

  let dac_observer_endpoint_arg =
    Tezos_clic.arg
      ~long:"dac-observer"
      ~placeholder:"dac-observer-endpoint"
      ~doc:
        (Format.sprintf
           "The address of the DAC observer node from which the %s downloads \
            preimages requested through the reveal channel."
           P.binary_name)
      (Tezos_clic.parameter (fun (_cctxt : Client_context.full) s ->
           Lwt.return_ok (Uri.of_string s)))

  let rpc_port_arg =
    let default = Configuration.default_rpc_port |> string_of_int in
    Tezos_clic.arg
      ~long:"rpc-port"
      ~placeholder:"rpc-port"
      ~doc:
        (Format.sprintf
           "The port the %s listens to. Default value is %s"
           binary_name
           default)
      int_parameter

  let acl_override_arg : ([`Allow_all | `Secure] option, _) Tezos_clic.arg =
    Tezos_clic.arg
      ~long:"acl-override"
      ~placeholder:"kind"
      ~doc:
        "Specify a different ACL for the rpc server to override the default \
         one. Possible values are 'secure' and 'allow-all'"
      (Tezos_clic.parameter (fun (_cctxt : Client_context.full) ->
           let open Lwt_result_syntax in
           function
           | "secure" -> return `Secure
           | "allow-all" | "allow_all" -> return `Allow_all
           | _ ->
               failwith
                 "Bad value for acl-override, possible values are 'secure' and \
                  'allow-all'"))

  let data_dir_arg =
    let default = Configuration.default_data_dir in
    Tezos_clic.default_arg
      ~long:"data-dir"
      ~placeholder:"data-dir"
      ~doc:
        (Format.sprintf
           "The path to the %s data directory. Default value is %s"
           binary_name
           default)
      ~default
      (Tezos_clic.parameter (fun (_cctxt : Client_context.full) data_dir ->
           let open Lwt_result_syntax in
           let open Filename.Infix in
           (* Check if the data directory of the smart rollup node is not the
              one of Octez node *)
           let*! identity_file_in_data_dir_exists =
             Lwt_unix.file_exists (data_dir // "identity.json")
           in
           if identity_file_in_data_dir_exists then
             failwith
               "Invalid data directory. This is a data directory for an Octez \
                node, please choose a different directory for the smart rollup \
                node data."
           else return data_dir))

  let config_file_arg =
    Tezos_clic.arg
      ~long:"config-file"
      ~placeholder:"config.json"
      ~doc:
        "Location of the configuration file for the rollup node. Defaults to \
         `<data-dir>/config.json`."
      string_parameter

  let boot_sector_file_arg =
    Tezos_clic.arg
      ~long:"boot-sector-file"
      ~placeholder:"file"
      ~doc:
        (Format.sprintf
           "Path to the boot sector. The argument is optional, if the rollup \
            was originated via the smart rollup originate operation, the %s \
            will fetch the boot sector itself. This argument is required only \
            if it's a bootstrapped smart rollup."
           binary_name)
      (Tezos_clic.parameter (fun (_cctxt : Client_context.full) path ->
           let open Lwt_result_syntax in
           let*! exists = Lwt_unix.file_exists path in
           if exists then return path
           else failwith "Boot sector not found at path %S" path))
end

let dac_timeout_arg =
  Tezos_clic.arg
    ~long:"dac-timeout"
    ~placeholder:"seconds"
    ~doc:
      "Timeout in seconds for which the DAC observer client will wait for a \
       preimage"
    z_parameter

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
    (Tezos_clic.parameter (fun (_cctxt : Client_context.full) p ->
         try Lwt_result.return (float_of_string p)
         with _ -> failwith "Cannot read float"))

let injector_retention_period_arg =
  Tezos_clic.arg
    ~long:"injector-retention-period"
    ~placeholder:"blocks"
    ~doc:
      (Format.sprintf
         "The number of blocks the injector keeps in memory. Decrease to free \
          memory, and increase to be able to query information about included \
          messages for longer. Default value is %d"
         Configuration.default_injector.retention_period)
  @@ Tezos_clic.map_parameter int_parameter ~f:(fun p ->
         if p > Configuration.max_injector_retention_period || p < 0 then
           Format.ksprintf
             Stdlib.failwith
             "injector-retention-period should be a positive number smaller \
              than %d"
             Configuration.max_injector_retention_period ;
         p)

let injector_attempts_arg =
  Tezos_clic.arg
    ~long:"injector-attempts"
    ~placeholder:"number"
    ~doc:
      (Format.sprintf
         "The number of attempts that the injector will make to inject an \
          operation when it fails. Default value is %d"
         Configuration.default_injector.attempts)
  @@ Tezos_clic.map_parameter int_parameter ~f:(fun p ->
         if p < 0 then
           Format.ksprintf
             Stdlib.failwith
             "injector-attempts should be positive" ;
         p)

let injection_ttl_arg =
  Tezos_clic.arg
    ~long:"injection-ttl"
    ~placeholder:"number"
    ~doc:
      (Format.sprintf
         "The number of blocks after which an operation that is injected but \
          never included is retried. Default value is %d"
         Configuration.default_injector.injection_ttl)
  @@ Tezos_clic.map_parameter int_parameter ~f:(fun p ->
         if p < 1 then Stdlib.failwith "injection-ttl should be > 1" ;
         p)

let positive_int_parameter =
  Tezos_clic.parameter (fun (cctxt : Client_context.full) p ->
      match int_of_string_opt p with
      | Some i when i > 0 -> Lwt_result.return i
      | None | Some _ ->
          cctxt#error "Expected a valid positive integer, provided %s instead" p)

let positive_int32_parameter =
  Tezos_clic.parameter (fun (cctxt : Client_context.full) p ->
      match Int32.of_string_opt p with
      | Some i when Compare.Int32.(i > 0l) -> Lwt_result.return i
      | None | Some _ ->
          cctxt#error "Expected a valid positive integer, provided %s instead" p)

let index_buffer_size_arg =
  Tezos_clic.arg
    ~long:"index-buffer-size"
    ~placeholder:"<nb_entries>"
    ~doc:
      "The maximum cache size in memory before it is flushed to disk, used for \
       indexes of the store. (Deprecated)"
    positive_int_parameter

let irmin_cache_size_arg =
  Tezos_clic.arg
    ~long:"irmin-cache-size"
    ~placeholder:"<nb_entries>"
    ~doc:"Size of Irmin cache in number of entries"
    positive_int_parameter

let log_kernel_debug_arg : (bool, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"log-kernel-debug"
    ~doc:"Log the kernel debug output to kernel.log in the data directory"
    ()

let log_kernel_debug_file_arg =
  Tezos_clic.arg
    ~long:"log-kernel-debug-file"
    ~placeholder:"file"
    ~doc:""
    string_parameter

let no_degraded_arg : (bool, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"no-degraded"
    ~doc:
      "Prevent the rollup node from entering degraded mode on error. The \
       rollup node will instead stop."
    ()

let gc_frequency_arg =
  Tezos_clic.arg
    ~long:"gc-frequency"
    ~placeholder:"blocks"
    ~doc:
      "The number of blocks between each launch of the garbage collection. \
       Default is protocol constant challenge_window_in_blocks / 5."
    positive_int32_parameter

let history_mode_parameter =
  Tezos_clic.parameter
    ~autocomplete:(fun (_cctxt : Client_context.full) ->
      Lwt_result.return ["archive"; "full"])
    (fun _ m -> Lwt_result.return (Configuration.history_mode_of_string m))

let history_mode_arg =
  Tezos_clic.arg
    ~long:"history-mode"
    ~placeholder:"history_mode"
    ~doc:
      (Format.sprintf
         "The history mode for the rollup node (archive, full) (default is %s)"
         Configuration.(string_of_history_mode default_history_mode))
    history_mode_parameter

let wasm_dump_file_param next =
  Tezos_clic.param
    ~name:"dump.<json|yaml>"
    ~desc:"YAML or JSON file containing the dumped durable storage"
    string_parameter
    next

let snapshot_dir_arg =
  Tezos_clic.arg
    ~long:"dest"
    ~placeholder:"path"
    ~doc:
      "Directory in which to export the snapshot (defaults to current \
       directory)"
    string_parameter

let snapshot_file_param next =
  Tezos_clic.param
    ~name:"snapshot_file"
    ~desc:"Snapshot archive file"
    string_parameter
    next

let snapshot_file_or_url_param next =
  Tezos_clic.param
    ~name:"snapshot_file_or_url"
    ~desc:
      "Snapshot archive file name, URL to download the snapshot or stdin (when \
       given `-`)"
    string_parameter
    next

let no_checks_arg : (bool, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"no-check"
    ~doc:"Don't check integrity of the snapshot."
    ()

let compress_on_the_fly_arg : (bool, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"compress-on-the-fly"
    ~doc:
      "Produce a compressed snapshot on the fly. The rollup node will use less \
       disk space to produce the snapshot but will lock the rollup node (if \
       running) for a longer time. Without this option, producing a snaphsot \
       requires the available disk space to be around the size of the data \
       dir."
    ()

let uncompressed : (bool, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"uncompressed"
    ~doc:"Produce an uncompressed snapshot."
    ()

let compact : (bool, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"compact"
    ~doc:"Produce a compact snapshot with a single commit for the context."
    ()

let rollup_node_endpoint_arg =
  Tezos_clic.arg
    ~long:"rollup-node-endpoint"
    ~placeholder:"uri"
    ~doc:(Format.sprintf "The address of the running rollup node.")
    (Tezos_clic.parameter (fun (_cctxt : Client_context.full) s ->
         Lwt.return_ok (Uri.of_string s)))

let string_list =
  Tezos_clic.parameter (fun (_cctxt : Client_context.full) s ->
      let list = String.split ',' s in
      Lwt_result.return list)

let cors_allowed_headers_arg =
  Tezos_clic.arg
    ~long:"cors-headers"
    ~placeholder:"ALLOWED_HEADERS"
    ~doc:"List of accepted cors headers."
    string_list

let cors_allowed_origins_arg =
  Tezos_clic.arg
    ~long:"cors-origins"
    ~placeholder:"ALLOWED_ORIGINS"
    ~doc:"List of accepted cors origins."
    string_list

let protocol_hash_parameter =
  Tezos_clic.parameter (fun (_cctxt : Client_context.full) p ->
      Lwt.return (Protocol_hash.of_b58check p))

let protocol_hash_arg =
  Tezos_clic.arg
    ~long:"protocol"
    ~short:'P'
    ~placeholder:"Proto"
    ~doc:
      "Protocol hash in base58-check. If not provided, the export will be for \
       the last registered protocol in the rollup node which may be different \
       between different versions of the node."
    protocol_hash_parameter

let protocol_hash_param next =
  Tezos_clic.param
    ~name:"protocol"
    ~desc:"Protocol hash"
    protocol_hash_parameter
    next

let apply_unsafe_patches_switch : (bool, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"apply-unsafe-patches"
    ~doc:
      "Apply unsafe PVM patches in the configuration or hardcoded by the node."
    ()

let bail_on_disagree_switch : (bool, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"bail-on-disagree"
    ~doc:
      "Make an observer rollup node bail when it sees a commitment it disagree \
       with on L1."
    ()

let unsafe_disable_wasm_kernel_checks_switch :
    (bool, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"unsafe-disable-wasm-kernel-checks"
    ~doc:
      "Allow to run the kernel without checking for its validity. Only for \
       debug, do not use in production!"
    ()

let level_param next =
  Tezos_clic.param ~name:"level" ~desc:"Level" positive_int32_parameter next

let block_hash_or_level_param next =
  Tezos_clic.param
    ~name:"block_or_level"
    ~desc:"An L1 block hash or a level"
    (Tezos_clic.parameter (fun (_cctxt : Client_context.full) s ->
         let open Lwt_result_syntax in
         match Int32.of_string_opt s with
         | Some l -> return (`Level l)
         | None ->
             let*? b = Block_hash.of_b58check s in
             return (`Hash b)))
    next

let profiling_arg : (bool option, Client_context.full) Tezos_clic.arg =
  Tezos_clic.arg
    ~long:"profiling"
    ~placeholder:"BOOL"
    ~doc:"Enable or disable profiling with opentelemetry"
  @@ Tezos_clic.parameter (fun (_cctxt : Client_context.full) -> function
       | "true" -> Lwt_result_syntax.return_true
       | "false" -> Lwt_result_syntax.return_false
       | s -> failwith "Invalid value %S for --profiling" s)

let etherlink_switch : (bool, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"etherlink"
    ~doc:"Force this rollup to be detected as an Etherlink rollup"
    ()

let l1_monitor_finalized_switch :
    (bool option, Client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"l1-monitor-finalized"
    ~doc:
      "The rollup node will only monitor finalized blocks of the L1 node. This \
       option has no effect for RISC-V rollups."
    ()
  |> Tezos_clic.map_arg ~f:(fun _ -> function
       | true -> Lwt_result_syntax.return_some true
       | false -> Lwt_result_syntax.return_none)
