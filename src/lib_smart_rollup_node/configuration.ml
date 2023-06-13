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

type mode = Observer | Accuser | Batcher | Maintenance | Operator | Custom

type purpose = Publish | Add_messages | Cement | Timeout | Refute

let purposes = [Publish; Add_messages; Cement; Timeout; Refute]

module Operator_purpose_map = Map.Make (struct
  type t = purpose

  let compare = Stdlib.compare
end)

type operators = Signature.Public_key_hash.t Operator_purpose_map.t

type fee_parameters = Injector_sigs.fee_parameter Operator_purpose_map.t

type batcher = {
  simulate : bool;
  min_batch_elements : int;
  min_batch_size : int;
  max_batch_elements : int;
  max_batch_size : int option;
}

type injector = {retention_period : int; attempts : int; injection_ttl : int}

type t = {
  sc_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  boot_sector_file : string option;
  sc_rollup_node_operators : operators;
  rpc_addr : string;
  rpc_port : int;
  metrics_addr : string option;
  reconnection_delay : float;
  fee_parameters : fee_parameters;
  mode : mode;
  loser_mode : Loser_mode.t;
  dal_node_endpoint : Uri.t option;
  dac_observer_endpoint : Uri.t option;
  dac_timeout : Z.t option;
  batcher : batcher;
  injector : injector;
  l1_blocks_cache_size : int;
  l2_blocks_cache_size : int;
  prefetch_blocks : int option;
  log_kernel_debug : bool;
}

type error +=
  | Missing_mode_operators of {mode : string; missing_operators : string list}

let () =
  register_error_kind
    ~id:"sc_rollup.node.missing_mode_operators"
    ~title:"Missing operators for the chosen mode"
    ~description:"Missing operators for the chosen mode."
    ~pp:(fun ppf (mode, missing_operators) ->
      Format.fprintf
        ppf
        "@[<hov>Missing operators %a for mode %s.@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           Format.pp_print_string)
        missing_operators
        mode)
    `Permanent
    Data_encoding.(
      obj2 (req "mode" string) (req "missing_operators" (list string)))
    (function
      | Missing_mode_operators {mode; missing_operators} ->
          Some (mode, missing_operators)
      | _ -> None)
    (fun (mode, missing_operators) ->
      Missing_mode_operators {mode; missing_operators})

let default_data_dir =
  Filename.concat (Sys.getenv "HOME") ".tezos-smart-rollup-node"

let storage_dir = "storage"

let context_dir = "context"

let default_storage_dir data_dir = Filename.concat data_dir storage_dir

let default_context_dir data_dir = Filename.concat data_dir context_dir

let config_filename ~data_dir = Filename.concat data_dir "config.json"

let default_rpc_addr = "127.0.0.1"

let default_rpc_port = 8932

let default_metrics_port = 9933

let default_reconnection_delay = 2.0 (* seconds *)

let mutez mutez = {Injector_sigs.mutez}

let tez t = mutez Int64.(mul (of_int t) 1_000_000L)

(* Copied from src/proto_alpha/lib_plugin/mempool.ml *)

let default_minimal_fees = mutez 100L

let default_minimal_nanotez_per_gas_unit = Q.of_int 100

let default_minimal_nanotez_per_byte = Q.of_int 1000

let default_force_low_fee = false

let default_fee_cap = tez 1

let default_burn_cap = mutez 0L

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
let default_fee = function
  | Cement -> tez 1
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

let default_burn = function
  | Publish ->
      (* The first commitment can store data. *)
      tez 1
  | Add_messages -> tez 0
  | Cement -> tez 0
  | Timeout -> tez 0
  | Refute ->
      (* A refutation move can store data, e.g. opening a game. *)
      tez 1

let default_fee_parameter ?purpose () =
  let fee_cap, burn_cap =
    match purpose with
    | None -> (default_fee_cap, default_burn_cap)
    | Some purpose -> (default_fee purpose, default_burn purpose)
  in
  {
    Injector_sigs.minimal_fees = default_minimal_fees;
    minimal_nanotez_per_byte = default_minimal_nanotez_per_byte;
    minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit;
    force_low_fee = default_force_low_fee;
    fee_cap;
    burn_cap;
  }

let default_fee_parameters =
  List.fold_left
    (fun acc purpose ->
      Operator_purpose_map.add purpose (default_fee_parameter ~purpose ()) acc)
    Operator_purpose_map.empty
    purposes

let default_batcher_simulate = true

let default_batcher_min_batch_elements = 10

let default_batcher_min_batch_size = 10

let default_batcher_max_batch_elements = max_int

let default_batcher =
  {
    simulate = default_batcher_simulate;
    min_batch_elements = default_batcher_min_batch_elements;
    min_batch_size = default_batcher_min_batch_size;
    max_batch_elements = default_batcher_max_batch_elements;
    max_batch_size = None;
  }

let default_injector =
  {retention_period = 2048; attempts = 100; injection_ttl = 120}

let max_injector_retention_period =
  5 * 8192 (* Preserved cycles (5) for mainnet *)

let default_l1_blocks_cache_size = 64

let default_l2_blocks_cache_size = 64

let string_of_purpose = function
  | Publish -> "publish"
  | Add_messages -> "add_messages"
  | Cement -> "cement"
  | Timeout -> "timeout"
  | Refute -> "refute"

let purpose_of_string = function
  | "publish" -> Some Publish
  | "add_messages" -> Some Add_messages
  | "cement" -> Some Cement
  | "timeout" -> Some Timeout
  | "refute" -> Some Refute
  | _ -> None

let purpose_of_string_exn s =
  match purpose_of_string s with
  | Some p -> p
  | None -> invalid_arg ("purpose_of_string " ^ s)

let add_fallbacks map fallbacks =
  List.fold_left
    (fun map (missing_purpose, fallback_purpose) ->
      if Operator_purpose_map.mem missing_purpose map then
        (* No missing purpose, don't fallback *)
        map
      else
        match Operator_purpose_map.find fallback_purpose map with
        | None ->
            (* Nothing to fallback on *)
            map
        | Some operator -> Operator_purpose_map.add missing_purpose operator map)
    map
    fallbacks

let make_purpose_map ~default bindings =
  let map = Operator_purpose_map.of_seq @@ List.to_seq bindings in
  let map = add_fallbacks map [(Timeout, Refute)] in
  match default with
  | None -> map
  | Some default ->
      List.fold_left
        (fun map purpose ->
          if Operator_purpose_map.mem purpose map then map
          else Operator_purpose_map.add purpose default map)
        map
        purposes

let operator_purpose_map_encoding encoding =
  let open Data_encoding in
  let schema =
    let open Json_schema in
    let v_schema p = Data_encoding.Json.schema (encoding p) in
    let v_schema_r p = root (v_schema p) in
    let kind =
      Object
        {
          properties =
            List.map
              (fun purpose ->
                (string_of_purpose purpose, v_schema_r purpose, false, None))
              purposes;
          pattern_properties = [];
          additional_properties = None;
          min_properties = 0;
          max_properties = None;
          schema_dependencies = [];
          property_dependencies = [];
        }
    in
    update (element kind) (v_schema Publish (* Dummy for definitions *))
  in
  conv
    ~schema
    (fun map ->
      let fields =
        Operator_purpose_map.bindings map
        |> List.map (fun (p, v) ->
               (string_of_purpose p, Data_encoding.Json.construct (encoding p) v))
      in
      `O fields)
    (function
      | `O fields ->
          List.map
            (fun (p, v) ->
              let purpose = purpose_of_string_exn p in
              (purpose, Data_encoding.Json.destruct (encoding purpose) v))
            fields
          |> List.to_seq |> Operator_purpose_map.of_seq
      | _ -> assert false)
    Data_encoding.Json.encoding

let operators_encoding =
  operator_purpose_map_encoding (fun _ -> Signature.Public_key_hash.encoding)

(* Encoding for Tez amounts, replicated from mempool. *)
let tez_encoding =
  let open Data_encoding in
  let decode {Injector_sigs.mutez} = Z.of_int64 mutez in
  let encode =
    Json.wrap_error (fun i -> {Injector_sigs.mutez = Z.to_int64 i})
  in
  Data_encoding.def
    "mutez"
    ~title:"A millionth of a tez"
    ~description:"One million mutez make a tez (1 tez = 1e6 mutez)"
    (conv decode encode n)

(* Encoding for nano-Tez amounts, replicated from mempool. *)
let nanotez_encoding =
  let open Data_encoding in
  def
    "nanotez"
    ~title:"A thousandth of a mutez"
    ~description:"One thousand nanotez make a mutez (1 tez = 1e9 nanotez)"
    (conv
       (fun q -> (q.Q.num, q.Q.den))
       (fun (num, den) -> {Q.num; den})
       (tup2 z z))

let fee_parameter_encoding purpose =
  let open Data_encoding in
  conv
    (fun {
           Injector_sigs.minimal_fees;
           minimal_nanotez_per_byte;
           minimal_nanotez_per_gas_unit;
           force_low_fee;
           fee_cap;
           burn_cap;
         } ->
      ( minimal_fees,
        minimal_nanotez_per_byte,
        minimal_nanotez_per_gas_unit,
        force_low_fee,
        fee_cap,
        burn_cap ))
    (fun ( minimal_fees,
           minimal_nanotez_per_byte,
           minimal_nanotez_per_gas_unit,
           force_low_fee,
           fee_cap,
           burn_cap ) ->
      {
        minimal_fees;
        minimal_nanotez_per_byte;
        minimal_nanotez_per_gas_unit;
        force_low_fee;
        fee_cap;
        burn_cap;
      })
    (obj6
       (dft
          "minimal-fees"
          ~description:"Exclude operations with lower fees"
          tez_encoding
          default_minimal_fees)
       (dft
          "minimal-nanotez-per-byte"
          ~description:"Exclude operations with lower fees per byte"
          nanotez_encoding
          default_minimal_nanotez_per_byte)
       (dft
          "minimal-nanotez-per-gas-unit"
          ~description:"Exclude operations with lower gas fees"
          nanotez_encoding
          default_minimal_nanotez_per_gas_unit)
       (dft
          "force-low-fee"
          ~description:
            "Don't check that the fee is lower than the estimated default"
          bool
          default_force_low_fee)
       (dft
          "fee-cap"
          ~description:"The fee cap"
          tez_encoding
          (default_fee purpose))
       (dft
          "burn-cap"
          ~description:"The burn cap"
          tez_encoding
          (default_burn purpose)))

let fee_parameters_encoding =
  operator_purpose_map_encoding fee_parameter_encoding

let modes = [Observer; Batcher; Maintenance; Operator; Custom]

let string_of_mode = function
  | Observer -> "observer"
  | Accuser -> "accuser"
  | Batcher -> "batcher"
  | Maintenance -> "maintenance"
  | Operator -> "operator"
  | Custom -> "custom"

let mode_of_string = function
  | "observer" -> Ok Observer
  | "accuser" -> Ok Accuser
  | "batcher" -> Ok Batcher
  | "maintenance" -> Ok Maintenance
  | "operator" -> Ok Operator
  | "custom" -> Ok Custom
  | _ -> Error [Exn (Failure "Invalid mode")]

let description_of_mode = function
  | Observer -> "Only follows the chain, reconstructs and interprets inboxes"
  | Accuser ->
      "Only publishes commitments for conflicts and play refutation games"
  | Batcher -> "Accepts transactions in its queue and batches them on the L1"
  | Maintenance ->
      "Follows the chain and publishes commitments, cement and refute"
  | Operator -> "Equivalent to maintenance + batcher"
  | Custom ->
      "In this mode, only operations that have a corresponding operator/signer \
       are injected"

let mode_encoding =
  Data_encoding.string_enum
    [
      ("observer", Observer);
      ("accuser", Accuser);
      ("batcher", Batcher);
      ("maintenance", Maintenance);
      ("operator", Operator);
      ("custom", Custom);
    ]

let batcher_encoding =
  let open Data_encoding in
  conv_with_guard
    (fun {
           simulate;
           min_batch_elements;
           min_batch_size;
           max_batch_elements;
           max_batch_size;
         } ->
      ( simulate,
        min_batch_elements,
        min_batch_size,
        max_batch_elements,
        max_batch_size ))
    (fun ( simulate,
           min_batch_elements,
           min_batch_size,
           max_batch_elements,
           max_batch_size ) ->
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
      {
        simulate;
        min_batch_elements;
        min_batch_size;
        max_batch_elements;
        max_batch_size;
      })
  @@ obj5
       (dft "simulate" bool default_batcher_simulate)
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

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           sc_rollup_address;
           boot_sector_file;
           sc_rollup_node_operators;
           rpc_addr;
           rpc_port;
           metrics_addr;
           reconnection_delay;
           fee_parameters;
           mode;
           loser_mode;
           dal_node_endpoint;
           dac_observer_endpoint;
           dac_timeout;
           batcher;
           injector;
           l1_blocks_cache_size;
           l2_blocks_cache_size;
           prefetch_blocks;
           log_kernel_debug;
         } ->
      ( ( sc_rollup_address,
          boot_sector_file,
          sc_rollup_node_operators,
          rpc_addr,
          rpc_port,
          metrics_addr,
          reconnection_delay,
          fee_parameters,
          mode,
          loser_mode ),
        ( dal_node_endpoint,
          dac_observer_endpoint,
          dac_timeout,
          batcher,
          injector,
          l1_blocks_cache_size,
          l2_blocks_cache_size,
          prefetch_blocks,
          log_kernel_debug ) ))
    (fun ( ( sc_rollup_address,
             boot_sector_file,
             sc_rollup_node_operators,
             rpc_addr,
             rpc_port,
             metrics_addr,
             reconnection_delay,
             fee_parameters,
             mode,
             loser_mode ),
           ( dal_node_endpoint,
             dac_observer_endpoint,
             dac_timeout,
             batcher,
             injector,
             l1_blocks_cache_size,
             l2_blocks_cache_size,
             prefetch_blocks,
             log_kernel_debug ) ) ->
      {
        sc_rollup_address;
        boot_sector_file;
        sc_rollup_node_operators;
        rpc_addr;
        rpc_port;
        metrics_addr;
        reconnection_delay;
        fee_parameters;
        mode;
        loser_mode;
        dal_node_endpoint;
        dac_observer_endpoint;
        dac_timeout;
        batcher;
        injector;
        l1_blocks_cache_size;
        l2_blocks_cache_size;
        prefetch_blocks;
        log_kernel_debug;
      })
    (merge_objs
       (obj10
          (req
             "smart-rollup-address"
             ~description:"Smart rollup address"
             Tezos_crypto.Hashed.Smart_rollup_address.encoding)
          (opt "boot-sector" ~description:"Boot sector" string)
          (req
             "smart-rollup-node-operator"
             ~description:
               "Operators that sign operations of the smart rollup, by purpose"
             operators_encoding)
          (dft "rpc-addr" ~description:"RPC address" string default_rpc_addr)
          (dft "rpc-port" ~description:"RPC port" uint16 default_rpc_port)
          (opt "metrics-addr" ~description:"Metrics address" string)
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
               "If enabled, the rollup node will issue wrong commitments (for \
                test only!)"
             Loser_mode.encoding
             Loser_mode.no_failures))
       (obj9
          (opt "DAL node endpoint" Tezos_rpc.Encoding.uri_encoding)
          (opt "dac-observer-client" Tezos_rpc.Encoding.uri_encoding)
          (opt "dac-timeout" Data_encoding.z)
          (dft "batcher" batcher_encoding default_batcher)
          (dft "injector" injector_encoding default_injector)
          (dft "l1_blocks_cache_size" int31 default_l1_blocks_cache_size)
          (dft "l2_blocks_cache_size" int31 default_l2_blocks_cache_size)
          (opt "prefetch_blocks" int31)
          (dft "log-kernel-debug" Data_encoding.bool false)))

let check_mode config =
  let open Result_syntax in
  let check_purposes purposes =
    let missing_operators =
      List.filter
        (fun p ->
          not (Operator_purpose_map.mem p config.sc_rollup_node_operators))
        purposes
    in
    if missing_operators <> [] then
      let mode = string_of_mode config.mode in
      let missing_operators = List.map string_of_purpose missing_operators in
      tzfail (Missing_mode_operators {mode; missing_operators})
    else return_unit
  in
  let narrow_purposes purposes =
    let+ () = check_purposes purposes in
    let sc_rollup_node_operators =
      Operator_purpose_map.filter
        (fun op_purpose _ -> List.mem ~equal:Stdlib.( = ) op_purpose purposes)
        config.sc_rollup_node_operators
    in
    {config with sc_rollup_node_operators}
  in
  match config.mode with
  | Observer -> narrow_purposes []
  | Batcher -> narrow_purposes [Add_messages]
  | Accuser -> narrow_purposes [Publish; Refute]
  | Maintenance -> narrow_purposes [Publish; Cement; Refute]
  | Operator -> narrow_purposes [Publish; Cement; Add_messages; Refute]
  | Custom -> return config

let refutation_player_buffer_levels = 5

let loser_warning_message config =
  if config.loser_mode <> Loser_mode.no_failures then
    Format.printf
      {|
************ WARNING *************
This rollup node is in loser mode.
This should be used for test only!
************ WARNING *************
|}

let save ~force ~data_dir config =
  loser_warning_message config ;
  let open Lwt_result_syntax in
  let json = Data_encoding.Json.construct encoding config in
  let config_file = config_filename ~data_dir in
  let*! exists = Lwt_unix.file_exists config_file in
  if exists && not force then
    failwith
      "Configuration file %S already exists. Use --force to overwrite."
      config_file
  else
    let*! () = Lwt_utils_unix.create_dir data_dir in
    Lwt_utils_unix.Json.write_file config_file json

let load ~data_dir =
  let open Lwt_result_syntax in
  let+ json = Lwt_utils_unix.Json.read_file (config_filename ~data_dir) in
  let config = Data_encoding.Json.destruct encoding json in
  loser_warning_message config ;
  config

module Cli = struct
  let make_operators sc_rollup_node_operators =
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
      ~reconnection_delay ~dal_node_endpoint ~dac_observer_endpoint ~dac_timeout
      ~injector_retention_period ~injector_attempts ~injection_ttl ~mode
      ~sc_rollup_address ~boot_sector_file ~sc_rollup_node_operators
      ~log_kernel_debug =
    let sc_rollup_node_operators = make_operators sc_rollup_node_operators in
    let config =
      {
        sc_rollup_address;
        boot_sector_file;
        sc_rollup_node_operators;
        rpc_addr = Option.value ~default:default_rpc_addr rpc_addr;
        rpc_port = Option.value ~default:default_rpc_port rpc_port;
        reconnection_delay =
          Option.value ~default:default_reconnection_delay reconnection_delay;
        dal_node_endpoint;
        dac_observer_endpoint;
        dac_timeout;
        metrics_addr;
        fee_parameters = Operator_purpose_map.empty;
        mode;
        loser_mode = Option.value ~default:Loser_mode.no_failures loser_mode;
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
        log_kernel_debug;
      }
    in
    check_mode config

  let patch_configuration_from_args configuration ~rpc_addr ~rpc_port
      ~metrics_addr ~loser_mode ~reconnection_delay ~dal_node_endpoint
      ~dac_observer_endpoint ~dac_timeout ~injector_retention_period
      ~injector_attempts ~injection_ttl ~mode ~sc_rollup_address
      ~boot_sector_file ~sc_rollup_node_operators ~log_kernel_debug =
    let new_sc_rollup_node_operators =
      make_operators sc_rollup_node_operators
    in
    (* Merge operators *)
    let sc_rollup_node_operators =
      Operator_purpose_map.merge
        (fun _purpose -> Option.either)
        new_sc_rollup_node_operators
        configuration.sc_rollup_node_operators
    in
    let configuration =
      {
        configuration with
        sc_rollup_address =
          Option.value
            ~default:configuration.sc_rollup_address
            sc_rollup_address;
        boot_sector_file =
          Option.either boot_sector_file configuration.boot_sector_file;
        sc_rollup_node_operators;
        mode = Option.value ~default:configuration.mode mode;
        rpc_addr = Option.value ~default:configuration.rpc_addr rpc_addr;
        rpc_port = Option.value ~default:configuration.rpc_port rpc_port;
        dal_node_endpoint =
          Option.either dal_node_endpoint configuration.dal_node_endpoint;
        dac_observer_endpoint =
          Option.either
            dac_observer_endpoint
            configuration.dac_observer_endpoint;
        dac_timeout = Option.either dac_timeout configuration.dac_timeout;
        reconnection_delay =
          Option.value
            ~default:configuration.reconnection_delay
            reconnection_delay;
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
        loser_mode = Option.value ~default:configuration.loser_mode loser_mode;
        metrics_addr = Option.either metrics_addr configuration.metrics_addr;
        log_kernel_debug = log_kernel_debug || configuration.log_kernel_debug;
      }
    in
    check_mode configuration

  let create_or_read_config ~data_dir ~rpc_addr ~rpc_port ~metrics_addr
      ~loser_mode ~reconnection_delay ~dal_node_endpoint ~dac_observer_endpoint
      ~dac_timeout ~injector_retention_period ~injector_attempts ~injection_ttl
      ~mode ~sc_rollup_address ~boot_sector_file ~sc_rollup_node_operators
      ~log_kernel_debug =
    let open Lwt_result_syntax in
    let config_file = config_filename ~data_dir in
    let*! exists_config = Lwt_unix.file_exists config_file in
    if exists_config then
      (* Read configuration from file and patch if user wanted to override
         some fields with values provided by arguments. *)
      let* configuration = load ~data_dir in
      let*? configuration =
        patch_configuration_from_args
          configuration
          ~rpc_addr
          ~rpc_port
          ~metrics_addr
          ~loser_mode
          ~reconnection_delay
          ~dal_node_endpoint
          ~dac_observer_endpoint
          ~dac_timeout
          ~injector_retention_period
          ~injector_attempts
          ~injection_ttl
          ~mode
          ~sc_rollup_address
          ~boot_sector_file
          ~sc_rollup_node_operators
          ~log_kernel_debug
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
          ~dac_observer_endpoint
          ~dac_timeout
          ~injector_retention_period
          ~injector_attempts
          ~injection_ttl
          ~mode
          ~sc_rollup_address
          ~boot_sector_file
          ~sc_rollup_node_operators
          ~log_kernel_debug
      in
      return config
end
