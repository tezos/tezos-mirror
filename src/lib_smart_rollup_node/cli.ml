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
       alias by said purpose, e.g. operating:alias_of_my_operator. The \
       possible purposes are: @[<h>%a@]."
      (Format.pp_print_list Purpose.pp_ex_purpose)
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
      ~placeholder:
        "ADDR:PORT or :PORT (by default ADDR is localhost and PORT is 9933)"
      ~doc:(Format.sprintf "The address of the %s metrics server." binary_name)
      string_parameter

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
       indexes of the store."
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
      (Format.sprintf
         "The number of blocks between each launch of the garbage collection. \
          Default value is %ld."
         Configuration.default_gc_parameters.frequency_in_blocks)
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
