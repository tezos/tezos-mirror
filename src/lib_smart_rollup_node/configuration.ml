(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
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

type mode =
  | Observer
  | Accuser
  | Bailout
  | Batcher
  | Maintenance
  | Operator
  | Custom of Operation_kind.t list

type batcher = {
  min_batch_elements : int;
  min_batch_size : int;
  max_batch_elements : int;
  max_batch_size : int option;
}

type injector = {retention_period : int; attempts : int; injection_ttl : int}

type fee_parameters = Injector_common.fee_parameter Operation_kind.Map.t

type gc_parameters = {
  frequency_in_blocks : int32 option;
  context_splitting_period : int option;
}

type history_mode = Store.State.history_mode = Archive | Full

type outbox_destination_filter =
  | Any_destination
  | Destination_among of string list

type outbox_entrypoint_filter =
  | Any_entrypoint
  | Entrypoint_among of string list

type outbox_message_filter =
  | Transaction of {
      destination : outbox_destination_filter;
      entrypoint : outbox_entrypoint_filter;
    }

type t = {
  sc_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  etherlink : bool;
  boot_sector_file : string option;
  operators : Purpose.operators;
  rpc_addr : string;
  rpc_port : int;
  acl : Tezos_rpc_http_server.RPC_server.Acl.policy;
  metrics_addr : string option;
  performance_metrics : bool;
  reconnection_delay : float;
  fee_parameters : fee_parameters;
  mode : mode;
  loser_mode : Loser_mode.t;
  apply_unsafe_patches : bool;
  unsafe_pvm_patches : Pvm_patches.unsafe_patch list;
  execute_outbox_messages_filter : outbox_message_filter list;
  dal_node_endpoint : Uri.t option;
  pre_images_endpoint : Uri.t option;
  batcher : batcher;
  injector : injector;
  l1_blocks_cache_size : int;
  l2_blocks_cache_size : int;
  prefetch_blocks : int option;
  l1_monitor_finalized : bool;
  l1_rpc_timeout : float;
  loop_retry_delay : float;
  dal_slot_status_max_fetch_attempts : int;
  index_buffer_size : int option;
  irmin_cache_size : int option;
  log_kernel_debug : bool;
  log_kernel_debug_file : string option;
  unsafe_disable_wasm_kernel_checks : bool;
  no_degraded : bool;
  gc_parameters : gc_parameters;
  history_mode : history_mode option;
  cors : Resto_cohttp.Cors.t;
  bail_on_disagree : bool;
  opentelemetry : Octez_telemetry.Opentelemetry_config.t;
}

type error += Empty_operation_kinds_for_custom_mode

let () =
  register_error_kind
    ~id:"sc_rollup_node.empty_operation_kinds_for_custom_mode"
    ~title:"Empty operation kinds for custom mode"
    ~description:
      "Empty operation kinds are not allowed for custom modes, just like in \
       observer mode"
    ~pp:(fun ppf _s ->
      Format.pp_print_string ppf "Operation kinds for custom mode are empty.")
    `Permanent
    Data_encoding.unit
    (function Empty_operation_kinds_for_custom_mode -> Some () | _ -> None)
    (fun () -> Empty_operation_kinds_for_custom_mode)

let default_data_dir =
  Filename.concat (Sys.getenv "HOME") ".tezos-smart-rollup-node"

let storage_dir = "storage"

let context_dir = "context"

let default_storage_dir data_dir = Filename.concat data_dir storage_dir

let default_context_dir data_dir = Filename.concat data_dir context_dir

let config_filename ~data_dir = function
  | Some path -> path
  | None -> Filename.concat data_dir "config.json"

let default_rpc_addr = "127.0.0.1"

let default_rpc_port = 8932

let default_metrics_port = 9933

let default_acl = Tezos_rpc_http_server.RPC_server.Acl.empty_policy

let default_reconnection_delay = 2.0 (* seconds *)

let mutez mutez = {Injector_common.mutez}

let tez t = mutez Int64.(mul (of_int t) 1_000_000L)

(* The below default fee and burn limits are computed by taking into account
   the worst fee found in the tests for the rollup node.

   We take as base the cost of commitment cementation, which is 719 mutez in fees:
   - Commitment publishing is 1.37 times more expensive.
   - Message submission is 0.7 times more expensive, so cheaper but it depends on
     the size of the message.
   - For refutation games:
     - Open is 1.55 times more expensive.
     - Dissection move is 2.31 times more expensive.
     - Proof move is 1.47 times more expensive but depends on the size of the proof.
     - Timeout move is 1.34 times more expensive.

   We set a fee limit of 1 tz for cementation (instead of 719 mutez) which
   should be plenty enough even if the gas price or gas consumption
   increases. We adjust the other limits in proportion.
*)
let default_fee : Operation_kind.t -> Injector_common.tez = function
  | Cement -> tez 1
  | Recover -> tez 1
  | Publish -> tez 2
  | Add_messages ->
      (* We keep this limit even though it depends on the size of the message
         because the rollup node pays the fees for messages submitted by the
         **users**. *)
      tez 1
  | Timeout -> tez 2
  | Refute ->
      (* Should be 3 based on comment above but we want to make sure we inject
         refutation moves even if the proof is large. The stake is high (we can
         lose the 10k deposit or we can get the reward). *)
      tez 5
  | Execute_outbox_message -> tez 1
  | Publish_dal_commitment -> tez 1

let default_burn : Operation_kind.t -> Injector_common.tez = function
  | Publish ->
      (* The first commitment can store data. *)
      tez 1
  | Add_messages -> tez 0
  | Cement -> tez 0
  | Recover -> tez 0
  | Timeout -> tez 0
  | Refute ->
      (* A refutation move can store data, e.g. opening a game. *)
      tez 1
  | Execute_outbox_message -> tez 1
  | Publish_dal_commitment -> tez 1

(* Copied from src/proto_alpha/lib_plugin/mempool.ml *)
let default_fee_parameter operation_kind =
  {
    Injector_common.minimal_fees = mutez 100L;
    minimal_nanotez_per_byte = Q.of_int 1000;
    minimal_nanotez_per_gas_unit = Q.of_int 100;
    force_low_fee = false;
    fee_cap = default_fee operation_kind;
    burn_cap = default_burn operation_kind;
  }

let default_fee_parameters =
  List.fold_left
    (fun acc operation_kind ->
      Operation_kind.Map.add
        operation_kind
        (default_fee_parameter operation_kind)
        acc)
    Operation_kind.Map.empty
    Operation_kind.all

let default_batcher_min_batch_elements = 10

let default_batcher_min_batch_size = 10

let default_batcher_max_batch_elements = (1 lsl 30) - 1

let default_batcher =
  {
    min_batch_elements = default_batcher_min_batch_elements;
    min_batch_size = default_batcher_min_batch_size;
    max_batch_elements = default_batcher_max_batch_elements;
    max_batch_size = None;
  }

let default_injector =
  {retention_period = 2048; attempts = 10; injection_ttl = 120}

let max_injector_retention_period =
  5 * 8192 (* Preserved cycles (5) for mainnet *)

let default_l1_blocks_cache_size = 64

let default_l2_blocks_cache_size = 64

let default_l1_rpc_timeout = 60. (* seconds *)

let default_dal_slot_status_max_fetch_attempts = 15 (* 15 * 1 sec*)

let default_l1_monitor_finalized = false

let default_loop_retry_delay = 10. (* seconds *)

let default_gc_parameters =
  {frequency_in_blocks = None; context_splitting_period = None}

let default_history_mode = Full

let default_execute_outbox_filter = []

let string_of_history_mode = function Archive -> "archive" | Full -> "full"

let history_mode_of_string = function
  | "archive" -> Archive
  | "full" -> Full
  | s -> invalid_arg ("history_mode_of_string " ^ s)

let modes =
  [
    Observer;
    Accuser;
    Bailout;
    Batcher;
    Maintenance;
    Operator;
    Custom Operation_kind.all;
  ]

let string_of_mode = function
  | Observer -> "observer"
  | Accuser -> "accuser"
  | Bailout -> "bailout"
  | Batcher -> "batcher"
  | Maintenance -> "maintenance"
  | Operator -> "operator"
  | Custom _op_kinds -> "custom"

let mode_of_string s =
  match s with
  | "observer" -> Ok Observer
  | "accuser" -> Ok Accuser
  | "bailout" -> Ok Bailout
  | "batcher" -> Ok Batcher
  | "maintenance" -> Ok Maintenance
  | "operator" -> Ok Operator
  | "custom" -> Ok (Custom [])
  | s when String.starts_with ~prefix:"custom:" s ->
      let kinds = String.sub s 7 (String.length s - 7) in
      let operation_kinds_strs = String.split_on_char ',' kinds in
      let operation_kinds =
        List.map Operation_kind.of_string_exn operation_kinds_strs
      in
      Ok (Custom operation_kinds)
  | _ -> Error [Exn (Failure "Invalid mode")]

let description_of_mode = function
  | Observer -> "Only follows the chain, reconstructs and interprets inboxes"
  | Accuser ->
      "Only publishes commitments for conflicts and play refutation games"
  | Bailout -> "Only defends and cements, does not publish any new commitments"
  | Batcher -> "Accepts transactions in its queue and batches them on the L1"
  | Maintenance ->
      "Follows the chain and publishes commitments, cement and refute"
  | Operator -> "Equivalent to maintenance + batcher"
  | Custom op_kinds ->
      let op_kinds_desc =
        List.map Operation_kind.to_string op_kinds |> String.concat ", "
      in
      Printf.sprintf
        "In this mode, the system handles only the specific operation kinds: \
         [%s]. This allows for tailored control and flexibility."
        op_kinds_desc

let mode_encoding =
  let open Data_encoding in
  let operation_kinds_encoding = list Operation_kind.encoding in
  let constant_case mode =
    let title = string_of_mode mode in
    case
      ~title
      Json_only
      (constant title)
      (fun m -> if m = mode then Some () else None)
      (fun () -> mode)
  in
  let custom_case =
    case
      ~title:"custom"
      Json_only
      (obj1 (req "custom" operation_kinds_encoding))
      (function Custom operation_kinds -> Some operation_kinds | _ -> None)
      (fun operation_kinds -> Custom operation_kinds)
  in
  let all_cases =
    custom_case
    :: List.map
         constant_case
         [Observer; Accuser; Bailout; Batcher; Maintenance; Operator]
  in
  def "sc_rollup_node_mode" @@ union all_cases

let batcher_encoding =
  let open Data_encoding in
  conv_with_guard
    (fun {
           min_batch_elements;
           min_batch_size;
           max_batch_elements;
           max_batch_size;
         }
       ->
      (min_batch_elements, min_batch_size, max_batch_elements, max_batch_size))
    (fun (min_batch_elements, min_batch_size, max_batch_elements, max_batch_size)
       ->
      let open Result_syntax in
      let error_when c s = if c then Error s else return_unit in
      let* () =
        error_when (min_batch_size <= 0) "min_batch_size must be positive"
      in
      let* () =
        match max_batch_size with
        | Some m when m < min_batch_size ->
            Error "max_batch_size must be greater than min_batch_size"
        | _ -> return_unit
      in
      let* () =
        error_when (min_batch_elements <= 0) "min_batch_size must be positive"
      in
      let+ () =
        error_when
          (max_batch_elements < min_batch_elements)
          "max_batch_elements must be greater than min_batch_elements"
      in
      {min_batch_elements; min_batch_size; max_batch_elements; max_batch_size})
  @@ obj4
       (dft "min_batch_elements" int31 default_batcher_min_batch_elements)
       (dft "min_batch_size" int31 default_batcher_min_batch_size)
       (dft "max_batch_elements" int31 default_batcher_max_batch_elements)
       (opt "max_batch_size" int31)

let injector_encoding : injector Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {retention_period; attempts; injection_ttl} ->
      (retention_period, attempts, injection_ttl))
    (fun (retention_period, attempts, injection_ttl) ->
      if retention_period > max_injector_retention_period then
        Format.ksprintf
          Stdlib.failwith
          "injector.retention_period should be smaller than %d"
          max_injector_retention_period ;
      if injection_ttl < 1 then
        Stdlib.failwith "injector.injection_ttl should be at least 1" ;
      {retention_period; attempts; injection_ttl})
  @@ obj3
       (dft "retention_period" uint16 default_injector.retention_period)
       (dft "attempts" uint16 default_injector.attempts)
       (dft "injection_ttl" uint16 default_injector.injection_ttl)

let fee_parameters_encoding =
  Operation_kind.map_encoding (fun operation_kind ->
      Injector_common.fee_parameter_encoding
        ~default_fee_parameter:(default_fee_parameter operation_kind))

let gc_parameters_encoding : gc_parameters Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {frequency_in_blocks; context_splitting_period} ->
      (frequency_in_blocks, context_splitting_period))
    (fun (frequency_in_blocks, context_splitting_period) ->
      {frequency_in_blocks; context_splitting_period})
  @@ obj2 (opt "frequency" int32) (opt "context_splitting_period" int31)

let history_mode_encoding : history_mode Data_encoding.t =
  Data_encoding.string_enum [("archive", Archive); ("full", Full)]

let cors_encoding : Resto_cohttp.Cors.t Data_encoding.t =
  let open Resto_cohttp.Cors in
  let open Data_encoding in
  conv
    (fun {allowed_headers; allowed_origins} ->
      (allowed_headers, allowed_origins))
    (fun (allowed_headers, allowed_origins) ->
      {allowed_headers; allowed_origins})
  @@ obj2
       (req "allowed_headers" (list string))
       (req "allowed_origins" (list string))

let outbox_destination_filter_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"any_destination"
        ~description:"Accept any destination."
        (Tag 0)
        (constant "any")
        (function Any_destination -> Some () | _ -> None)
        (fun () -> Any_destination);
      case
        ~title:"destination_among"
        ~description:
          "Accept destination that matches the given list (in base58-check)."
        (Tag 1)
        (list string)
        (function Destination_among l -> Some l | _ -> None)
        (fun l -> Destination_among l);
    ]

let outbox_entrypoint_filter_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"any_entrypoint"
        ~description:"Accept any entrypoint."
        (Tag 0)
        (constant "any")
        (function Any_entrypoint -> Some () | _ -> None)
        (fun () -> Any_entrypoint);
      case
        ~title:"entrypoint_among"
        ~description:"Accept entrypoint of the given list."
        (Tag 1)
        (list string)
        (function Entrypoint_among l -> Some l | _ -> None)
        (fun l -> Entrypoint_among l);
    ]

let outbox_messages_filter_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"transaction_filter"
        ~description:
          "Accept transactions which match the filter on their destination and \
           entrypoint."
        (Tag 0)
        (obj1
           (req
              "transaction"
              (obj2
                 (req "destination" outbox_destination_filter_encoding)
                 (req "entrypoint" outbox_entrypoint_filter_encoding))))
        (function
          | Transaction {destination; entrypoint} ->
              Some (destination, entrypoint))
        (fun (destination, entrypoint) -> Transaction {destination; entrypoint});
    ]

let execute_outbox_messages_filter_encoding =
  Data_encoding.list outbox_messages_filter_encoding

let encoding default_display : t Data_encoding.t =
  let open Data_encoding in
  let dft =
    match default_display with
    | `Hide -> dft
    | `Show ->
        fun ?title ?description name enc _default ->
          req ?title ?description name enc
  in
  conv
    (fun {
           sc_rollup_address;
           etherlink;
           boot_sector_file;
           operators;
           rpc_addr;
           rpc_port;
           acl;
           metrics_addr;
           performance_metrics;
           reconnection_delay;
           fee_parameters;
           mode;
           loser_mode;
           apply_unsafe_patches;
           execute_outbox_messages_filter;
           unsafe_pvm_patches;
           dal_node_endpoint;
           pre_images_endpoint;
           batcher;
           injector;
           l1_blocks_cache_size;
           l2_blocks_cache_size;
           prefetch_blocks;
           l1_monitor_finalized;
           l1_rpc_timeout;
           loop_retry_delay;
           index_buffer_size;
           irmin_cache_size;
           log_kernel_debug;
           log_kernel_debug_file;
           unsafe_disable_wasm_kernel_checks;
           no_degraded;
           gc_parameters;
           history_mode;
           cors;
           bail_on_disagree;
           opentelemetry;
           dal_slot_status_max_fetch_attempts;
         }
       ->
      ( ( ( sc_rollup_address,
            Some etherlink,
            boot_sector_file,
            operators,
            rpc_addr,
            rpc_port,
            acl ),
          ( metrics_addr,
            performance_metrics,
            reconnection_delay,
            fee_parameters,
            mode,
            loser_mode,
            apply_unsafe_patches,
            unsafe_pvm_patches,
            execute_outbox_messages_filter ) ),
        ( ( dal_node_endpoint,
            pre_images_endpoint,
            batcher,
            injector,
            l1_blocks_cache_size,
            l2_blocks_cache_size,
            prefetch_blocks,
            l1_monitor_finalized ),
          ( ( l1_rpc_timeout,
              loop_retry_delay,
              index_buffer_size,
              irmin_cache_size,
              log_kernel_debug,
              log_kernel_debug_file,
              unsafe_disable_wasm_kernel_checks ),
            ( no_degraded,
              gc_parameters,
              history_mode,
              cors,
              bail_on_disagree,
              opentelemetry,
              dal_slot_status_max_fetch_attempts ) ) ) ))
    (fun ( ( ( sc_rollup_address,
               etherlink,
               boot_sector_file,
               operators,
               rpc_addr,
               rpc_port,
               acl ),
             ( metrics_addr,
               performance_metrics,
               reconnection_delay,
               fee_parameters,
               mode,
               loser_mode,
               apply_unsafe_patches,
               unsafe_pvm_patches,
               execute_outbox_messages_filter ) ),
           ( ( dal_node_endpoint,
               pre_images_endpoint,
               batcher,
               injector,
               l1_blocks_cache_size,
               l2_blocks_cache_size,
               prefetch_blocks,
               l1_monitor_finalized ),
             ( ( l1_rpc_timeout,
                 loop_retry_delay,
                 index_buffer_size,
                 irmin_cache_size,
                 log_kernel_debug,
                 log_kernel_debug_file,
                 unsafe_disable_wasm_kernel_checks ),
               ( no_degraded,
                 gc_parameters,
                 history_mode,
                 cors,
                 bail_on_disagree,
                 opentelemetry,
                 dal_slot_status_max_fetch_attempts ) ) ) )
       ->
      {
        sc_rollup_address;
        etherlink =
          (match etherlink with
          | Some b -> b
          | None -> Address.is_etherlink sc_rollup_address);
        boot_sector_file;
        operators;
        rpc_addr;
        rpc_port;
        acl;
        metrics_addr;
        performance_metrics;
        reconnection_delay;
        fee_parameters;
        mode;
        loser_mode;
        apply_unsafe_patches;
        unsafe_pvm_patches;
        execute_outbox_messages_filter;
        dal_node_endpoint;
        pre_images_endpoint;
        batcher;
        injector;
        l1_blocks_cache_size;
        l2_blocks_cache_size;
        prefetch_blocks;
        l1_monitor_finalized;
        l1_rpc_timeout;
        loop_retry_delay;
        index_buffer_size;
        irmin_cache_size;
        log_kernel_debug;
        log_kernel_debug_file;
        unsafe_disable_wasm_kernel_checks;
        no_degraded;
        gc_parameters;
        history_mode;
        cors;
        bail_on_disagree;
        opentelemetry;
        dal_slot_status_max_fetch_attempts;
      })
    (merge_objs
       (merge_objs
          (obj7
             (req
                "smart-rollup-address"
                ~description:"Smart rollup address"
                Tezos_crypto.Hashed.Smart_rollup_address.encoding)
             (opt
                "etherlink"
                ~description:
                  "Whether the rollup is an Etherlink rollup. Defaults to \
                   identification based on known Etherlink addresses."
                bool)
             (opt "boot-sector" ~description:"Boot sector" string)
             (dft
                "smart-rollup-node-operator"
                ~description:
                  "Operators that sign operations of the smart rollup, by \
                   purpose"
                Purpose.operators_encoding
                Purpose.no_operators)
             (dft "rpc-addr" ~description:"RPC address" string default_rpc_addr)
             (dft "rpc-port" ~description:"RPC port" uint16 default_rpc_port)
             (dft
                "acl"
                ~description:"Access control list"
                Tezos_rpc_http_server.RPC_server.Acl.policy_encoding
                default_acl))
          (obj9
             (opt "metrics-addr" ~description:"Metrics address" string)
             (dft "performance-metrics" bool true)
             (dft
                "reconnection_delay"
                ~description:
                  "The reconnection (to the tezos node) delay in seconds"
                float
                default_reconnection_delay)
             (dft
                "fee-parameters"
                ~description:
                  "The fee parameters for each purpose used when injecting \
                   operations in L1"
                fee_parameters_encoding
                default_fee_parameters)
             (req
                ~description:"The mode for this rollup node"
                "mode"
                mode_encoding)
             (dft
                "loser-mode"
                ~description:
                  "If enabled, the rollup node will issue wrong commitments \
                   (for test only!)"
                Loser_mode.encoding
                Loser_mode.no_failures)
             (dft
                "apply-unsafe-patches"
                ~description:
                  "Apply the unsafe PVM patches for this rollup (either user \
                   provided or hardcoded)."
                bool
                false)
             (dft
                "unsafe-pvm-patches"
                ~description:
                  "Unsafe patches to apply to the PVM. For tests only, don't \
                   set this value in production."
                (list Pvm_patches.unsafe_patch_encoding)
                [])
             (dft
                "execute-outbox-messages-filter"
                ~description:
                  "A filter to select which outbox messages the rollup will \
                   execute automatically (must be in maintenance or operator \
                   mode)."
                execute_outbox_messages_filter_encoding
                default_execute_outbox_filter)))
       (merge_objs
          (obj8
             (opt "DAL node endpoint" Tezos_rpc.Encoding.uri_encoding)
             (opt "pre-images-endpoint" Tezos_rpc.Encoding.uri_encoding)
             (dft "batcher" batcher_encoding default_batcher)
             (dft "injector" injector_encoding default_injector)
             (dft "l1_blocks_cache_size" int31 default_l1_blocks_cache_size)
             (dft "l2_blocks_cache_size" int31 default_l2_blocks_cache_size)
             (opt "prefetch_blocks" int31)
             (dft
                "l1_monitor_finalized"
                bool
                default_l1_monitor_finalized
                ~description:
                  "When set to true, the rollup node only monitors L1 head \
                   which are finalized. This option has no effect for RISC-V \
                   rollups."))
          (merge_objs
             (obj7
                (dft
                   "l1_rpc_timeout"
                   Data_encoding.float
                   default_l1_rpc_timeout)
                (dft
                   "loop_retry_delay"
                   Data_encoding.float
                   default_loop_retry_delay)
                (opt "index_buffer_size" int31 ~description:"Deprecated")
                (opt "irmin_cache_size" int31)
                (dft "log-kernel-debug" Data_encoding.bool false)
                (opt "log-kernel-debug-file" Data_encoding.string)
                (dft
                   "unsafe-disable-wasm-kernel-checks"
                   Data_encoding.bool
                   false))
             (obj7
                (dft "no-degraded" Data_encoding.bool false)
                (dft
                   "gc-parameters"
                   gc_parameters_encoding
                   default_gc_parameters)
                (opt "history-mode" history_mode_encoding)
                (dft "cors" cors_encoding Resto_cohttp.Cors.default)
                (dft "bail-on-disagree" bool false)
                (dft
                   "opentelemetry"
                   ~description:"Enable or disable opentelemetry profiling"
                   Octez_telemetry.Opentelemetry_config.encoding
                   Octez_telemetry.Opentelemetry_config.default)
                (dft
                   "dal_slot_status_max_fetch_attempts"
                   Data_encoding.uint8
                   default_dal_slot_status_max_fetch_attempts)))))

let encoding_no_default = encoding `Show

let encoding = encoding `Hide

(** Maps a mode to their corresponding purposes. The Custom mode
    returns each purposes where it has at least one operation kind
    from (i.e. {!purposes_of_operation_kinds}). *)
let purposes_of_mode mode : Purpose.ex_purpose list =
  match mode with
  | Observer -> []
  | Batcher -> [Purpose Batching]
  | Accuser -> [Purpose Operating]
  | Bailout -> [Purpose Operating; Purpose Cementing; Purpose Recovering]
  | Maintenance ->
      [Purpose Operating; Purpose Cementing; Purpose Executing_outbox]
  | Operator ->
      [
        Purpose Operating;
        Purpose Cementing;
        Purpose Executing_outbox;
        Purpose Batching;
      ]
  | Custom op_kinds -> Purpose.of_operation_kind op_kinds

let operation_kinds_of_mode mode =
  match mode with
  | Custom op_kinds -> op_kinds
  | _ ->
      let purposes = purposes_of_mode mode in
      List.map Purpose.operation_kind purposes |> List.flatten

let check_custom_mode mode =
  error_when (mode = Custom []) Empty_operation_kinds_for_custom_mode

let can_inject mode (op_kind : Operation_kind.t) =
  let allowed_operations = operation_kinds_of_mode mode in
  List.mem ~equal:Stdlib.( = ) op_kind allowed_operations

let purposes_matches_mode (type k) mode (purposes : k Purpose.t list) =
  List.exists
    (fun p ->
      List.mem ~equal:Stdlib.( = ) (Purpose.Purpose p) (purposes_of_mode mode))
    purposes

let refutation_player_buffer_levels = 5

let default_index_buffer_size = 10_000

let default_irmin_cache_size = 300_000

let loser_warning_message config =
  if config.loser_mode <> Loser_mode.no_failures then
    Format.printf
      {|
************ WARNING *************
This rollup node is in loser mode.
This should be used for test only!
************ WARNING *************
|}

let override_acl ~rpc_addr ~rpc_port acl = function
  | None -> acl
  | Some kind ->
      let new_acl =
        match kind with
        | `Secure -> Rpc_server.Acl.secure
        | `Allow_all -> Rpc_server.Acl.allow_all
      in
      let addr =
        P2p_point.Id.{addr = rpc_addr; port = Some rpc_port; peer_id = None}
      in
      Tezos_rpc_http_server.RPC_server.Acl.put_policy (addr, new_acl) acl

let save ~force ~config_file config =
  loser_warning_message config ;
  let open Lwt_result_syntax in
  let json = Data_encoding.Json.construct encoding config in
  let*! exists = Lwt_unix.file_exists config_file in
  if exists && not force then
    failwith
      "Configuration file %S already exists. Use --force to overwrite."
      config_file
  else
    let*! () = Lwt_utils_unix.create_dir (Filename.dirname config_file) in
    Lwt_utils_unix.Json.write_file config_file json

let load ~config_file =
  let open Lwt_result_syntax in
  let+ json = Lwt_utils_unix.Json.read_file config_file in
  let config = Data_encoding.Json.destruct encoding json in
  loser_warning_message config ;
  config

module Cli = struct
  let get_purposed_and_default_operators operators =
    let open Result_syntax in
    List.fold_left_e
      (fun (purposed_operator, default_operator_opt) -> function
        | `Purpose p_operator ->
            return (p_operator :: purposed_operator, default_operator_opt)
        | `Default operator ->
            if Option.is_none default_operator_opt then
              return (purposed_operator, Some operator)
            else tzfail (error_of_fmt "Multiple default operators"))
      ([], None)
      operators

  let configuration_from_args ~rpc_addr ~rpc_port ~acl_override ~metrics_addr
      ~disable_performance_metrics ~loser_mode ~reconnection_delay
      ~dal_node_endpoint ~pre_images_endpoint ~injector_retention_period
      ~injector_attempts ~injection_ttl ~mode ~sc_rollup_address
      ~boot_sector_file ~operators ~index_buffer_size ~irmin_cache_size
      ~log_kernel_debug ~log_kernel_debug_file ~no_degraded ~gc_frequency
      ~history_mode ~allowed_origins ~allowed_headers ~apply_unsafe_patches
      ~unsafe_disable_wasm_kernel_checks ~bail_on_disagree ~profiling
      ~force_etherlink ~l1_monitor_finalized =
    let open Lwt_result_syntax in
    let*? purposed_operators, default_operator =
      get_purposed_and_default_operators operators
    in
    let* operators =
      Purpose.make_operators
        ?default_operator
        ~needed_purposes:(purposes_of_mode mode)
        purposed_operators
    in
    let rpc_addr = Option.value ~default:default_rpc_addr rpc_addr in
    let rpc_port = Option.value ~default:default_rpc_port rpc_port in
    let acl = override_acl ~rpc_addr ~rpc_port default_acl acl_override in
    let*? () = check_custom_mode mode in
    return
      {
        sc_rollup_address;
        etherlink = force_etherlink || Address.is_etherlink sc_rollup_address;
        boot_sector_file;
        operators;
        rpc_addr;
        rpc_port;
        acl;
        reconnection_delay =
          Option.value ~default:default_reconnection_delay reconnection_delay;
        dal_node_endpoint;
        pre_images_endpoint;
        metrics_addr;
        performance_metrics = not disable_performance_metrics;
        fee_parameters = Operation_kind.Map.empty;
        mode;
        loser_mode = Option.value ~default:Loser_mode.no_failures loser_mode;
        apply_unsafe_patches;
        unsafe_pvm_patches = [];
        execute_outbox_messages_filter = default_execute_outbox_filter;
        batcher = default_batcher;
        injector =
          {
            retention_period =
              Option.value
                ~default:default_injector.retention_period
                injector_retention_period;
            attempts =
              Option.value ~default:default_injector.attempts injector_attempts;
            injection_ttl =
              Option.value ~default:default_injector.injection_ttl injection_ttl;
          };
        l1_blocks_cache_size = default_l1_blocks_cache_size;
        l2_blocks_cache_size = default_l2_blocks_cache_size;
        prefetch_blocks = None;
        l1_monitor_finalized =
          Option.value
            l1_monitor_finalized
            ~default:default_l1_monitor_finalized;
        l1_rpc_timeout = default_l1_rpc_timeout;
        loop_retry_delay = default_loop_retry_delay;
        index_buffer_size;
        irmin_cache_size;
        log_kernel_debug;
        log_kernel_debug_file;
        unsafe_disable_wasm_kernel_checks;
        no_degraded;
        gc_parameters =
          {frequency_in_blocks = gc_frequency; context_splitting_period = None};
        history_mode;
        cors =
          Resto_cohttp.Cors.
            {
              allowed_headers =
                Option.value ~default:default.allowed_headers allowed_headers;
              allowed_origins =
                Option.value ~default:default.allowed_origins allowed_origins;
            };
        bail_on_disagree;
        opentelemetry =
          (match profiling with
          | None -> Octez_telemetry.Opentelemetry_config.default
          | Some enable ->
              {Octez_telemetry.Opentelemetry_config.default with enable});
        dal_slot_status_max_fetch_attempts =
          default_dal_slot_status_max_fetch_attempts;
      }

  let patch_configuration_from_args configuration ~rpc_addr ~rpc_port
      ~acl_override ~metrics_addr ~disable_performance_metrics ~loser_mode
      ~reconnection_delay ~dal_node_endpoint ~pre_images_endpoint
      ~injector_retention_period ~injector_attempts ~injection_ttl ~mode
      ~sc_rollup_address ~boot_sector_file ~operators ~index_buffer_size
      ~irmin_cache_size ~log_kernel_debug ~log_kernel_debug_file ~no_degraded
      ~gc_frequency ~history_mode ~allowed_origins ~allowed_headers
      ~apply_unsafe_patches ~unsafe_disable_wasm_kernel_checks ~bail_on_disagree
      ~profiling ~force_etherlink ~l1_monitor_finalized =
    let open Lwt_result_syntax in
    let mode = Option.value ~default:configuration.mode mode in
    let*? () = check_custom_mode mode in
    let*? purposed_operators, default_operator =
      get_purposed_and_default_operators operators
    in
    let* operators =
      Purpose.replace_operators
        ?default_operator
        ~needed_purposes:(purposes_of_mode mode)
        purposed_operators
        configuration.operators
    in
    let rpc_addr = Option.value ~default:configuration.rpc_addr rpc_addr in
    let rpc_port = Option.value ~default:configuration.rpc_port rpc_port in
    let acl = override_acl ~rpc_addr ~rpc_port configuration.acl acl_override in
    let* () =
      when_ ((not apply_unsafe_patches) && configuration.apply_unsafe_patches)
      @@ fun () ->
      failwith
        "Configuration is registered to apply unsafe PVM patches. Flag \
         --apply-unsafe-patches must always be given on command line."
    in
    return
      {
        configuration with
        sc_rollup_address =
          Option.value
            ~default:configuration.sc_rollup_address
            sc_rollup_address;
        etherlink = force_etherlink || configuration.etherlink;
        boot_sector_file =
          Option.either boot_sector_file configuration.boot_sector_file;
        operators;
        mode;
        rpc_addr;
        rpc_port;
        acl;
        dal_node_endpoint =
          Option.either dal_node_endpoint configuration.dal_node_endpoint;
        pre_images_endpoint =
          Option.either pre_images_endpoint configuration.pre_images_endpoint;
        reconnection_delay =
          Option.value
            ~default:configuration.reconnection_delay
            reconnection_delay;
        injector =
          {
            retention_period =
              Option.value
                ~default:configuration.injector.retention_period
                injector_retention_period;
            attempts =
              Option.value
                ~default:configuration.injector.attempts
                injector_attempts;
            injection_ttl =
              Option.value
                ~default:configuration.injector.injection_ttl
                injection_ttl;
          };
        loser_mode = Option.value ~default:configuration.loser_mode loser_mode;
        apply_unsafe_patches;
        metrics_addr = Option.either metrics_addr configuration.metrics_addr;
        performance_metrics =
          (not disable_performance_metrics) && configuration.performance_metrics;
        index_buffer_size =
          Option.either index_buffer_size configuration.index_buffer_size;
        irmin_cache_size =
          Option.either irmin_cache_size configuration.irmin_cache_size;
        log_kernel_debug = log_kernel_debug || configuration.log_kernel_debug;
        log_kernel_debug_file =
          Option.either
            log_kernel_debug_file
            configuration.log_kernel_debug_file;
        unsafe_disable_wasm_kernel_checks =
          unsafe_disable_wasm_kernel_checks
          || configuration.unsafe_disable_wasm_kernel_checks;
        no_degraded = no_degraded || configuration.no_degraded;
        gc_parameters =
          {
            frequency_in_blocks =
              Option.either
                gc_frequency
                configuration.gc_parameters.frequency_in_blocks;
            context_splitting_period =
              configuration.gc_parameters.context_splitting_period;
          };
        history_mode = Option.either history_mode configuration.history_mode;
        cors =
          Resto_cohttp.Cors.
            {
              allowed_headers =
                Option.value
                  ~default:configuration.cors.allowed_headers
                  allowed_headers;
              allowed_origins =
                Option.value
                  ~default:configuration.cors.allowed_origins
                  allowed_origins;
            };
        bail_on_disagree = bail_on_disagree || configuration.bail_on_disagree;
        opentelemetry =
          (match profiling with
          | None -> configuration.opentelemetry
          | Some enable -> {configuration.opentelemetry with enable});
        l1_monitor_finalized =
          Option.value
            l1_monitor_finalized
            ~default:configuration.l1_monitor_finalized;
      }

  let create_or_read_config ~config_file ~rpc_addr ~rpc_port ~acl_override
      ~metrics_addr ~disable_performance_metrics ~loser_mode ~reconnection_delay
      ~dal_node_endpoint ~pre_images_endpoint ~injector_retention_period
      ~injector_attempts ~injection_ttl ~mode ~sc_rollup_address
      ~boot_sector_file ~operators ~index_buffer_size ~irmin_cache_size
      ~log_kernel_debug ~log_kernel_debug_file ~no_degraded ~gc_frequency
      ~history_mode ~allowed_origins ~allowed_headers ~apply_unsafe_patches
      ~unsafe_disable_wasm_kernel_checks ~bail_on_disagree ~profiling
      ~force_etherlink ~l1_monitor_finalized =
    let open Lwt_result_syntax in
    let*! exists_config = Lwt_unix.file_exists config_file in
    if exists_config then
      (* Read configuration from file and patch if user wanted to override
         some fields with values provided by arguments. *)
      let* configuration = load ~config_file in
      let* configuration =
        patch_configuration_from_args
          configuration
          ~rpc_addr
          ~rpc_port
          ~acl_override
          ~metrics_addr
          ~disable_performance_metrics
          ~loser_mode
          ~reconnection_delay
          ~dal_node_endpoint
          ~pre_images_endpoint
          ~injector_retention_period
          ~injector_attempts
          ~injection_ttl
          ~mode
          ~sc_rollup_address
          ~boot_sector_file
          ~operators
          ~index_buffer_size
          ~irmin_cache_size
          ~log_kernel_debug
          ~log_kernel_debug_file
          ~no_degraded
          ~gc_frequency
          ~history_mode
          ~allowed_origins
          ~allowed_headers
          ~apply_unsafe_patches
          ~unsafe_disable_wasm_kernel_checks
          ~bail_on_disagree
          ~profiling
          ~force_etherlink
          ~l1_monitor_finalized
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
      let* config =
        configuration_from_args
          ~rpc_addr
          ~rpc_port
          ~acl_override
          ~metrics_addr
          ~disable_performance_metrics
          ~loser_mode
          ~reconnection_delay
          ~dal_node_endpoint
          ~pre_images_endpoint
          ~injector_retention_period
          ~injector_attempts
          ~injection_ttl
          ~mode
          ~sc_rollup_address
          ~boot_sector_file
          ~operators
          ~index_buffer_size
          ~irmin_cache_size
          ~log_kernel_debug
          ~log_kernel_debug_file
          ~no_degraded
          ~gc_frequency
          ~history_mode
          ~allowed_headers
          ~allowed_origins
          ~apply_unsafe_patches
          ~unsafe_disable_wasm_kernel_checks
          ~bail_on_disagree
          ~profiling
          ~force_etherlink
          ~l1_monitor_finalized
      in
      return config
end
