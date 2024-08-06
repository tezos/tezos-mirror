(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
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

type time_between_blocks = Nothing | Time_between_blocks of float

type mode =
  | Observer of {
      initial_kernel : string;
      preimages_dir : string;
      rollup_node_endpoint : string;
    }
  | Threshold_encryption_observer of {
      initial_kernel : string;
      preimages_dir : string;
      rollup_node_endpoint : string;
      bundler_node_endpoint : string;
    }
  | Sequencer of {
      initial_kernel : string;
      preimage_dir : string option;
      private_rpc_port : int option;
      time_between_blocks : time_between_blocks option;
      sequencer : string;
      genesis_timestamp : Client.timestamp option;
      max_blueprints_lag : int option;
      max_blueprints_ahead : int option;
      max_blueprints_catchup : int option;
      catchup_cooldown : int option;
      max_number_of_chunks : int option;
      wallet_dir : string option;
      tx_pool_timeout_limit : int option;
      tx_pool_addr_limit : int option;
      tx_pool_tx_per_addr_limit : int option;
      dal_slots : int list option;
    }
  | Sandbox of {
      initial_kernel : string;
      preimage_dir : string option;
      private_rpc_port : int option;
      time_between_blocks : time_between_blocks option;
      genesis_timestamp : Client.timestamp option;
      max_number_of_chunks : int option;
      wallet_dir : string option;
      tx_pool_timeout_limit : int option;
      tx_pool_addr_limit : int option;
      tx_pool_tx_per_addr_limit : int option;
    }
  | Threshold_encryption_sequencer of {
      initial_kernel : string;
      preimage_dir : string option;
      private_rpc_port : int option;
      time_between_blocks : time_between_blocks option;
      sequencer : string;
      genesis_timestamp : Client.timestamp option;
      max_blueprints_lag : int option;
      max_blueprints_ahead : int option;
      max_blueprints_catchup : int option;
      catchup_cooldown : int option;
      max_number_of_chunks : int option;
      wallet_dir : string option;
      tx_pool_timeout_limit : int option;
      tx_pool_addr_limit : int option;
      tx_pool_tx_per_addr_limit : int option;
      sequencer_sidecar_endpoint : string;
      dal_slots : int list option;
    }
  | Proxy of {finalized_view : bool}
  | Rpc of mode

module Per_level_map = Map.Make (Int)

module Parameters = struct
  type persistent_state = {
    arguments : string list;
    mutable pending_ready : unit option Lwt.u list;
    mutable last_injected_level : int;
    mutable pending_blueprint_injected : unit option Lwt.u list Per_level_map.t;
    mutable last_applied_level : int;
    mutable pending_blueprint_applied : unit option Lwt.u list Per_level_map.t;
    mode : mode;
    data_dir : string;
    rpc_addr : string;
    rpc_port : int;
    endpoint : string;
    runner : Runner.t option;
    restricted_rpcs : string option;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "evm_node"

  let default_colors = Log.Color.[|FG.green; FG.yellow; FG.cyan; FG.magenta|]
end

open Parameters
include Daemon.Make (Parameters)

let mode t = t.persistent_state.mode

let is_sequencer t =
  match t.persistent_state.mode with
  | Sequencer _ | Sandbox _ | Threshold_encryption_sequencer _ -> true
  | Observer _ | Threshold_encryption_observer _ | Proxy _ | Rpc _ -> false

let initial_kernel t =
  let rec from_mode = function
    | Sandbox {initial_kernel; _}
    | Sequencer {initial_kernel; _}
    | Threshold_encryption_sequencer {initial_kernel; _}
    | Observer {initial_kernel; _}
    | Threshold_encryption_observer {initial_kernel; _} ->
        initial_kernel
    | Rpc mode -> from_mode mode
    | Proxy _ -> Test.fail "cannot start a RPC node from a proxy node"
  in
  from_mode t.persistent_state.mode

let can_apply_blueprint t =
  match t.persistent_state.mode with
  | Sequencer _ | Sandbox _ | Threshold_encryption_sequencer _ | Observer _
  | Threshold_encryption_observer _ ->
      true
  | Proxy _ | Rpc _ -> false

let connection_arguments ?rpc_addr ?rpc_port ?runner () =
  let rpc_port =
    match rpc_port with None -> Port.fresh () | Some port -> port
  in
  let rpc_host =
    match rpc_addr with Some addr -> addr | None -> Runner.address runner
  in
  let rpc_addr_arg =
    match (rpc_addr, runner) with
    | None, None -> []
    | None, Some _ -> ["--rpc-addr"; Unix.(string_of_inet_addr inet_addr_any)]
    | Some addr, _ -> ["--rpc-addr"; addr]
  in
  (["--rpc-port"; string_of_int rpc_port] @ rpc_addr_arg, rpc_host, rpc_port)

let trigger_ready sc_node value =
  let pending = sc_node.persistent_state.pending_ready in
  sc_node.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let trigger_blueprint_injected evm_node level =
  let pending = evm_node.persistent_state.pending_blueprint_injected in
  let pending_for_level = Per_level_map.find_opt level pending in
  evm_node.persistent_state.last_injected_level <- level ;
  evm_node.persistent_state.pending_blueprint_injected <-
    Per_level_map.remove level pending ;
  List.iter (fun pending -> Lwt.wakeup_later pending (Some ()))
  @@ Option.value ~default:[] pending_for_level

let trigger_blueprint_applied evm_node level =
  let pending = evm_node.persistent_state.pending_blueprint_applied in
  let pending_for_level = Per_level_map.find_opt level pending in
  evm_node.persistent_state.last_applied_level <- level ;
  evm_node.persistent_state.pending_blueprint_applied <-
    Per_level_map.remove level pending ;
  List.iter (fun pending -> Lwt.wakeup_later pending (Some ()))
  @@ Option.value ~default:[] pending_for_level

let set_ready evm_node =
  (match evm_node.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready evm_node (Some ())

let event_ready_name = "is_ready.v0"

let event_blueprint_injected_name = "blueprint_injection.v0"

let event_blueprint_applied_name = "blueprint_application.v0"

let handle_is_ready_event (evm_node : t) {name; value = _; timestamp = _} =
  if name = event_ready_name then set_ready evm_node else ()

let handle_blueprint_injected_event (evm_node : t) {name; value; timestamp = _}
    =
  if name = event_blueprint_injected_name then
    trigger_blueprint_injected evm_node JSON.(value |> as_int)
  else ()

let handle_blueprint_applied_event (evm_node : t) {name; value; timestamp = _} =
  if name = event_blueprint_applied_name then
    trigger_blueprint_applied evm_node
    @@ JSON.(
         match value |-> "level" |> as_int_opt with
         | Some i -> i (* in devmode. To delete at next upgrade *)
         | None -> value |> as_int (* in prod *))
  else ()

let resolve_or_timeout ?(timeout = 30.) evm_node ~name promise =
  let res = ref None in
  let promise =
    let* result = promise in
    res := Some result ;
    unit
  in
  let* () = Lwt.pick [promise; Lwt_unix.sleep timeout] in
  match !res with
  | Some v -> return v
  | None -> Test.fail "Timeout waiting for %s of %s" name evm_node.name

let wait_for_event ?timeout evm_node ~event f =
  resolve_or_timeout ?timeout evm_node ~name:event @@ wait_for evm_node event f

let raise_terminated_when_none ?where evm_node ~event promise =
  let* res = promise in
  match res with
  | Some x -> return x
  | None ->
      raise (Terminated_before_event {daemon = evm_node.name; event; where})

let wait_for_event_listener ?timeout evm_node ~event promise =
  resolve_or_timeout ?timeout evm_node ~name:event
  @@ raise_terminated_when_none evm_node ~event promise

let wait_for_ready ?timeout evm_node =
  match evm_node.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      evm_node.persistent_state.pending_ready <-
        resolver :: evm_node.persistent_state.pending_ready ;
      wait_for_event_listener ?timeout evm_node ~event:event_ready_name promise

let wait_for_blueprint_injected ?timeout evm_node level =
  match evm_node.status with
  | Running {session_state = {ready = true; _}; _} when is_sequencer evm_node ->
      let current_level = evm_node.persistent_state.last_injected_level in
      if level <= current_level then unit
      else
        let promise, resolver = Lwt.task () in
        evm_node.persistent_state.pending_blueprint_injected <-
          Per_level_map.update
            level
            (fun pending -> Some (resolver :: Option.value ~default:[] pending))
            evm_node.persistent_state.pending_blueprint_injected ;
        wait_for_event_listener
          ?timeout
          evm_node
          ~event:event_blueprint_injected_name
          promise
  | Running {session_state = {ready = true; _}; _} ->
      failwith "EVM node is not a sequencer"
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      failwith "EVM node is not ready"

let wait_for_blueprint_applied ?timeout evm_node level =
  match evm_node.status with
  | Running {session_state = {ready = true; _}; _}
    when can_apply_blueprint evm_node ->
      let current_level = evm_node.persistent_state.last_applied_level in
      if level <= current_level then unit
      else
        let promise, resolver = Lwt.task () in
        evm_node.persistent_state.pending_blueprint_applied <-
          Per_level_map.update
            level
            (fun pending -> Some (resolver :: Option.value ~default:[] pending))
            evm_node.persistent_state.pending_blueprint_applied ;
        wait_for_event_listener
          ?timeout
          evm_node
          ~event:event_blueprint_applied_name
          promise
  | Running {session_state = {ready = true; _}; _} ->
      failwith "EVM node cannot produce blueprints"
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      failwith "EVM node is not ready"

let wait_for_blueprint_invalid ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"blueprint_invalid.v0"
  @@ Fun.const (Some ())

let wait_for_pending_upgrade ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"pending_upgrade.v0"
  @@ JSON.(
       fun json ->
         let root_hash = json |-> "root_hash" |> as_string in
         let timestamp = json |-> "timestamp" |> as_string in
         Some (root_hash, timestamp))

let wait_for_successful_upgrade ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"applied_upgrade.v0"
  @@ JSON.(
       fun json ->
         let root_hash = json |-> "root_hash" |> as_string in
         let level = json |-> "level" |> as_int in
         Some (root_hash, level))

let wait_for_block_producer_locked ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"block_producer_locked.v0"
  @@ Fun.const (Some ())

let wait_for_block_producer_tx_injected ?timeout evm_node =
  wait_for_event
    ?timeout
    evm_node
    ~event:"block_producer_transaction_injected.v0"
  @@ fun json ->
  let hash = JSON.(json |> as_string) in
  Some hash

let wait_for_retrying_connect ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"retrying_connect.v0"
  @@ Fun.const (Some ())

type delayed_transaction_kind = Deposit | Transaction | FaDeposit

let delayed_transaction_kind_of_string = function
  | "transaction" -> Transaction
  | "deposit" -> Deposit
  | "fa_deposit" -> FaDeposit
  | s -> Test.fail "%s is neither a transaction or deposit" s

let wait_for_rollup_node_follower_connection_acquired ?timeout evm_node =
  wait_for_event
    ?timeout
    evm_node
    ~event:"rollup_node_follower_connection_acquired.v0"
  @@ Fun.const (Some ())

type 'a evm_event_kind =
  | Kernel_upgrade : (string * Client.Time.t) evm_event_kind
  | Sequencer_upgrade : (string * Hex.t * Client.Time.t) evm_event_kind
  | Blueprint_applied : (int * string) evm_event_kind
  | New_delayed_transaction : (delayed_transaction_kind * string) evm_event_kind

let string_of_evm_event_kind : type a. a evm_event_kind -> string = function
  | Kernel_upgrade -> "kernel_upgrade"
  | Sequencer_upgrade -> "sequencer_upgrade"
  | Blueprint_applied -> "blueprint_applied"
  | New_delayed_transaction -> "new_delayed_transaction"

let parse_evm_event_kind : type a. a evm_event_kind -> JSON.t -> a option =
 fun kind json ->
  let open JSON in
  match kind with
  | Kernel_upgrade -> (
      match as_list (json |-> "event") with
      | [hash; timestamp] ->
          let hash = as_string hash in
          let timestamp = as_string timestamp |> Client.Time.of_notation_exn in
          Some (hash, timestamp)
      | _ ->
          Test.fail
            ~__LOC__
            "invalid json for the evm event kind kernel upgrade")
  | Sequencer_upgrade -> (
      match as_list (json |-> "event") with
      | [hash; pool_address; timestamp] ->
          let hash = as_string hash in
          let pool_address = as_string pool_address |> Hex.of_string in
          let timestamp = as_string timestamp |> Client.Time.of_notation_exn in
          Some (hash, pool_address, timestamp)
      | _ ->
          Test.fail
            ~__LOC__
            "invalid json for the evm event kind sequencer upgrade")
  | Blueprint_applied -> (
      match as_list (json |-> "event") with
      | [number; hash] ->
          let number = as_int number in
          let hash = as_string hash in
          Some (number, hash)
      | _ ->
          Test.fail
            ~__LOC__
            "invalid json for the evm event kind blueprint applied")
  | New_delayed_transaction -> (
      match as_list_opt (json |-> "event") with
      | Some [kind; hash; _raw] ->
          let kind = delayed_transaction_kind_of_string (as_string kind) in
          let hash = as_string hash in
          Some (kind, hash)
      | _ ->
          Test.fail
            ~__LOC__
            "invalid json for the evm event kind new delayed transaction")

let wait_for_evm_event ?timeout event ?(check = parse_evm_event_kind event)
    evm_node =
  wait_for_event ?timeout evm_node ~event:"evm_events_new_event.v0"
  @@ JSON.(
       fun json ->
         let found_event_kind = json |-> "kind" |> as_string in
         let expected_event_kind = string_of_evm_event_kind event in
         if expected_event_kind = found_event_kind then check json else None)

let wait_for_shutdown_event evm_node =
  wait_for evm_node "shutting_down.v0" @@ fun json ->
  JSON.(json |> as_int |> Option.some)

let wait_for_diverged evm_node =
  wait_for evm_node "evm_events_follower_diverged.v0" @@ fun json ->
  let open JSON in
  let level = json |-> "level" |> as_int in
  let expected_hash = json |-> "expected_hash" |> as_string in
  let found_hash = json |-> "found_hash" |> as_string in
  Some (level, expected_hash, found_hash)

let wait_for_missing_blueprint evm_node =
  wait_for evm_node "evm_events_follower_missing_blueprint.v0" @@ fun json ->
  let open JSON in
  let level = json |-> "level" |> as_int in
  let expected_hash = json |-> "expected_hash" |> as_string in
  Some (level, expected_hash)

let wait_for_rollup_node_ahead evm_node =
  wait_for evm_node "evm_events_follower_rollup_node_ahead.v0" @@ fun json ->
  let open JSON in
  let level = json |> as_int in
  Some level

let wait_for_tx_pool_add_transaction ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"tx_pool_add_transaction.v0"
  @@ JSON.as_string_opt

let create ?(path = Uses.path Constant.octez_evm_node) ?name ?runner
    ?(mode = Proxy {finalized_view = false}) ?data_dir ?rpc_addr ?rpc_port
    ?restricted_rpcs endpoint =
  let arguments, rpc_addr, rpc_port =
    connection_arguments ?rpc_addr ?rpc_port ?runner ()
  in
  let new_name () =
    match mode with
    | Proxy _ -> "proxy_" ^ fresh_name ()
    | Sequencer _ -> "sequencer_" ^ fresh_name ()
    | Sandbox _ -> "sandbox_" ^ fresh_name ()
    | Threshold_encryption_sequencer _ -> "te_sequencer_" ^ fresh_name ()
    | Observer _ -> "observer_" ^ fresh_name ()
    | Threshold_encryption_observer _ ->
        "threshold_encryption_observer_" ^ fresh_name ()
    | Rpc _ -> "rpc_" ^ fresh_name ()
  in
  let name = Option.value ~default:(new_name ()) name in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let evm_node =
    create
      ~path
      ~name
      {
        arguments;
        pending_ready = [];
        last_injected_level = 0;
        pending_blueprint_injected = Per_level_map.empty;
        last_applied_level = 0;
        pending_blueprint_applied = Per_level_map.empty;
        mode;
        data_dir;
        rpc_addr;
        rpc_port;
        endpoint;
        restricted_rpcs;
        runner;
      }
  in
  on_event evm_node (handle_is_ready_event evm_node) ;
  on_event evm_node (handle_blueprint_injected_event evm_node) ;
  on_event evm_node (handle_blueprint_applied_event evm_node) ;
  evm_node

let name evm_node = evm_node.name

let rpc_port evm_node = evm_node.persistent_state.rpc_port

let data_dir evm_node = evm_node.persistent_state.data_dir

let data_dir_arg evm_node = ["--data-dir"; evm_node.persistent_state.data_dir]

(* assume a valid config for the given command and uses new latest run
   command format. *)
let run_args evm_node =
  let shared_args =
    data_dir_arg evm_node @ evm_node.persistent_state.arguments
  in
  let mode_args =
    match evm_node.persistent_state.mode with
    | Proxy {finalized_view} ->
        ["run"; "proxy"]
        @ Cli_arg.optional_switch "finalized-view" finalized_view
    | Sequencer {initial_kernel; genesis_timestamp; wallet_dir; _} ->
        ["run"; "sequencer"; "--initial-kernel"; initial_kernel]
        @ Cli_arg.optional_arg
            "genesis-timestamp"
            (fun timestamp ->
              Client.time_of_timestamp timestamp |> Client.Time.to_notation)
            genesis_timestamp
        @ Cli_arg.optional_arg "wallet-dir" Fun.id wallet_dir
    | Sandbox {initial_kernel; genesis_timestamp; wallet_dir; _} ->
        ["run"; "sandbox"; "--initial-kernel"; initial_kernel]
        @ Cli_arg.optional_arg
            "genesis-timestamp"
            (fun timestamp ->
              Client.time_of_timestamp timestamp |> Client.Time.to_notation)
            genesis_timestamp
        @ Cli_arg.optional_arg "wallet-dir" Fun.id wallet_dir
    | Threshold_encryption_sequencer
        {initial_kernel; genesis_timestamp; wallet_dir; _} ->
        [
          "run";
          "threshold";
          "encryption";
          "sequencer";
          "--initial-kernel";
          initial_kernel;
        ]
        @ Cli_arg.optional_arg
            "genesis-timestamp"
            (fun timestamp ->
              Client.time_of_timestamp timestamp |> Client.Time.to_notation)
            genesis_timestamp
        @ Cli_arg.optional_arg "wallet-dir" Fun.id wallet_dir
    | Observer {initial_kernel; _} ->
        ["run"; "observer"; "--initial-kernel"; initial_kernel]
    | Threshold_encryption_observer {initial_kernel; bundler_node_endpoint; _}
      ->
        [
          "run";
          "observer";
          "--initial-kernel";
          initial_kernel;
          "--bundler-node-endpoint";
          bundler_node_endpoint;
        ]
    | Rpc _ -> ["experimental"; "run"; "rpc"]
  in
  mode_args @ shared_args

let run ?(wait = true) ?(extra_arguments = []) evm_node =
  let on_terminate _status =
    (* Cancel all event listeners. *)
    trigger_ready evm_node None ;
    let pending_blueprint_injected =
      evm_node.persistent_state.pending_blueprint_injected
    in
    evm_node.persistent_state.pending_blueprint_injected <- Per_level_map.empty ;
    Per_level_map.iter
      (fun _ pending_list ->
        List.iter (fun pending -> Lwt.wakeup_later pending None) pending_list)
      pending_blueprint_injected ;
    let pending_blueprint_applied =
      evm_node.persistent_state.pending_blueprint_applied
    in
    evm_node.persistent_state.pending_blueprint_applied <- Per_level_map.empty ;
    Per_level_map.iter
      (fun _ pending_list ->
        List.iter (fun pending -> Lwt.wakeup_later pending None) pending_list)
      pending_blueprint_applied ;
    unit
  in
  let* () =
    run
      ?runner:evm_node.persistent_state.runner
      ~event_level:`Debug
      evm_node
      {ready = false}
      (run_args evm_node @ extra_arguments)
      ~on_terminate
  in
  let* () = if wait then wait_for_ready evm_node else unit in
  unit

let spawn_command evm_node args =
  Process.spawn
    ~name:evm_node.name
    ~color:evm_node.color
    ?runner:evm_node.persistent_state.runner
    evm_node.path
  @@ args

let spawn_run ?(extra_arguments = []) evm_node =
  spawn_command evm_node (run_args evm_node @ extra_arguments)

module Config_file = struct
  let filename evm_node =
    Filename.concat evm_node.persistent_state.data_dir "config.json"

  let read evm_node =
    match evm_node.persistent_state.runner with
    | None -> Lwt.return (JSON.parse_file (filename evm_node))
    | Some runner ->
        let* content =
          Process.spawn ~runner "cat" [filename evm_node]
          |> Process.check_and_read_stdout
        in
        JSON.parse ~origin:"Evm_node.config_file.read" content |> Lwt.return

  let write node config =
    match node.persistent_state.runner with
    | None -> Lwt.return (JSON.encode_to_file (filename node) config)
    | Some runner ->
        let content = JSON.encode config in
        let cmd =
          Runner.Shell.(
            redirect_stdout (cmd [] "echo" [content]) (filename node))
        in
        let cmd, args = Runner.wrap_with_ssh runner cmd in
        Process.run cmd args

  let update node update =
    let* config = read node in
    let config = update config in
    write node config
end

let spawn_init_config ?(extra_arguments = []) evm_node =
  let shared_args =
    data_dir_arg evm_node @ evm_node.persistent_state.arguments
    @ Cli_arg.optional_arg
        "restricted-rpcs"
        Fun.id
        evm_node.persistent_state.restricted_rpcs
  in
  let time_between_blocks_fmt = function
    | Nothing -> "none"
    | Time_between_blocks f -> Format.sprintf "%.3f" f
  in
  let mode_args =
    match evm_node.persistent_state.mode with
    | Proxy _ -> ["--rollup-node-endpoint"; evm_node.persistent_state.endpoint]
    | Rpc _ -> []
    | Sequencer
        {
          initial_kernel = _;
          preimage_dir;
          private_rpc_port;
          time_between_blocks;
          sequencer;
          genesis_timestamp = _;
          max_blueprints_lag;
          max_blueprints_ahead;
          max_blueprints_catchup;
          catchup_cooldown;
          max_number_of_chunks;
          wallet_dir;
          tx_pool_timeout_limit;
          tx_pool_addr_limit;
          tx_pool_tx_per_addr_limit;
          dal_slots;
        } ->
        [
          "--rollup-node-endpoint";
          evm_node.persistent_state.endpoint;
          "--sequencer-key";
          sequencer;
        ]
        @ Cli_arg.optional_arg "preimages-dir" Fun.id preimage_dir
        @ Cli_arg.optional_arg "private-rpc-port" string_of_int private_rpc_port
        @ Cli_arg.optional_arg
            "maximum-blueprints-lag"
            string_of_int
            max_blueprints_lag
        @ Cli_arg.optional_arg
            "maximum-blueprints-ahead"
            string_of_int
            max_blueprints_ahead
        @ Cli_arg.optional_arg
            "maximum-blueprints-catch-up"
            string_of_int
            max_blueprints_catchup
        @ Cli_arg.optional_arg
            "catch-up-cooldown"
            string_of_int
            catchup_cooldown
        @ Cli_arg.optional_arg
            "time-between-blocks"
            time_between_blocks_fmt
            time_between_blocks
        @ Cli_arg.optional_arg
            "max-number-of-chunks"
            string_of_int
            max_number_of_chunks
        @ Cli_arg.optional_arg "wallet-dir" Fun.id wallet_dir
        @ Cli_arg.optional_arg
            "tx-pool-timeout-limit"
            string_of_int
            tx_pool_timeout_limit
        @ Cli_arg.optional_arg
            "tx-pool-addr-limit"
            string_of_int
            tx_pool_addr_limit
        @ Cli_arg.optional_arg
            "tx-pool-tx-per-addr-limit"
            string_of_int
            tx_pool_tx_per_addr_limit
        @ Cli_arg.optional_arg
            "dal-slots"
            (fun l -> String.concat "," (List.map string_of_int l))
            dal_slots
    | Sandbox
        {
          initial_kernel = _;
          preimage_dir;
          private_rpc_port;
          time_between_blocks;
          genesis_timestamp = _;
          max_number_of_chunks;
          wallet_dir;
          tx_pool_timeout_limit;
          tx_pool_addr_limit;
          tx_pool_tx_per_addr_limit;
        } ->
        [
          (* These two fields are not necessary for the sandbox mode, however,
             the init configuration needs them. *)
          "--sequencer-key";
          "unencrypted:edsk3tNH5Ye6QaaRQev3eZNcXgcN6sjCJRXChYFz42L6nKfRVwuL1n";
          "--rollup-node-endpoint";
          evm_node.persistent_state.endpoint;
        ]
        @ Cli_arg.optional_arg "preimages-dir" Fun.id preimage_dir
        @ Cli_arg.optional_arg "private-rpc-port" string_of_int private_rpc_port
        @ Cli_arg.optional_arg
            "time-between-blocks"
            time_between_blocks_fmt
            time_between_blocks
        @ Cli_arg.optional_arg
            "max-number-of-chunks"
            string_of_int
            max_number_of_chunks
        @ Cli_arg.optional_arg "wallet-dir" Fun.id wallet_dir
        @ Cli_arg.optional_arg
            "tx-pool-timeout-limit"
            string_of_int
            tx_pool_timeout_limit
        @ Cli_arg.optional_arg
            "tx-pool-addr-limit"
            string_of_int
            tx_pool_addr_limit
        @ Cli_arg.optional_arg
            "tx-pool-tx-per-addr-limit"
            string_of_int
            tx_pool_tx_per_addr_limit
    | Threshold_encryption_sequencer
        {
          initial_kernel = _;
          preimage_dir;
          private_rpc_port;
          time_between_blocks;
          sequencer;
          genesis_timestamp = _;
          max_blueprints_lag;
          max_blueprints_ahead;
          max_blueprints_catchup;
          catchup_cooldown;
          max_number_of_chunks;
          wallet_dir;
          tx_pool_timeout_limit;
          tx_pool_addr_limit;
          tx_pool_tx_per_addr_limit;
          sequencer_sidecar_endpoint;
          dal_slots;
        } ->
        [
          "--rollup-node-endpoint";
          evm_node.persistent_state.endpoint;
          "--sequencer-key";
          sequencer;
          "--sequencer-sidecar-endpoint";
          sequencer_sidecar_endpoint;
        ]
        @ Cli_arg.optional_arg "preimages-dir" Fun.id preimage_dir
        @ Cli_arg.optional_arg "private-rpc-port" string_of_int private_rpc_port
        @ Cli_arg.optional_arg
            "maximum-blueprints-lag"
            string_of_int
            max_blueprints_lag
        @ Cli_arg.optional_arg
            "maximum-blueprints-ahead"
            string_of_int
            max_blueprints_ahead
        @ Cli_arg.optional_arg
            "maximum-blueprints-catch-up"
            string_of_int
            max_blueprints_catchup
        @ Cli_arg.optional_arg
            "catch-up-cooldown"
            string_of_int
            catchup_cooldown
        @ Cli_arg.optional_arg
            "time-between-blocks"
            time_between_blocks_fmt
            time_between_blocks
        @ Cli_arg.optional_arg
            "max-number-of-chunks"
            string_of_int
            max_number_of_chunks
        @ Cli_arg.optional_arg "wallet-dir" Fun.id wallet_dir
        @ Cli_arg.optional_arg
            "tx-pool-timeout-limit"
            string_of_int
            tx_pool_timeout_limit
        @ Cli_arg.optional_arg
            "tx-pool-addr-limit"
            string_of_int
            tx_pool_addr_limit
        @ Cli_arg.optional_arg
            "tx-pool-tx-per-addr-limit"
            string_of_int
            tx_pool_tx_per_addr_limit
        @ Cli_arg.optional_arg
            "dal-slots"
            (fun l -> String.concat "," (List.map string_of_int l))
            dal_slots
    | Observer {preimages_dir; initial_kernel = _; rollup_node_endpoint} ->
        [
          "--evm-node-endpoint";
          evm_node.persistent_state.endpoint;
          "--rollup-node-endpoint";
          rollup_node_endpoint;
          "--preimages-dir";
          preimages_dir;
        ]
    | Threshold_encryption_observer
        {
          preimages_dir;
          initial_kernel = _;
          rollup_node_endpoint;
          bundler_node_endpoint;
        } ->
        [
          "--evm-node-endpoint";
          evm_node.persistent_state.endpoint;
          "--rollup-node-endpoint";
          rollup_node_endpoint;
          "--bundler-node-endpoint";
          bundler_node_endpoint;
          "--preimages-dir";
          preimages_dir;
        ]
  in
  spawn_command evm_node @@ ["init"; "config"] @ mode_args @ shared_args
  @ extra_arguments

let rpc_endpoint ?(local = false) ?(private_ = false) (evm_node : t) =
  let addr, port, path =
    let host =
      if local then Constant.default_host
      else Runner.address evm_node.persistent_state.runner
    in
    if private_ then
      match evm_node.persistent_state.mode with
      | Sequencer {private_rpc_port = Some private_rpc_port; _}
      | Sandbox {private_rpc_port = Some private_rpc_port; _} ->
          (host, private_rpc_port, "/private")
      | Sequencer {private_rpc_port = None; _}
      | Sandbox {private_rpc_port = None; _} ->
          Test.fail "Sequencer doesn't have a private RPC server"
      | Threshold_encryption_sequencer
          {private_rpc_port = Some private_rpc_port; _} ->
          (host, private_rpc_port, "/private")
      | Threshold_encryption_sequencer {private_rpc_port = None; _} ->
          Test.fail
            "Threshold encryption sequencer doesn't have a private RPC server"
      | Proxy _ -> Test.fail "Proxy doesn't have a private RPC server"
      | Observer _ -> Test.fail "Observer doesn't have a private RPC server"
      | Rpc _ -> Test.fail "Rpc node doesn't have a private RPC server"
      | Threshold_encryption_observer _ ->
          Test.fail
            "Threshold encryption observer doesn't have a private RPC server"
    else (host, evm_node.persistent_state.rpc_port, "")
  in
  Format.sprintf "http://%s:%d%s" addr port path

let endpoint = rpc_endpoint ?local:None

let patch_config_with_experimental_feature
    ?(drop_duplicate_when_injection = false)
    ?(node_transaction_validation = false) () =
  let conditional_json_put ~name cond value_json json =
    if cond then
      JSON.put
        ( name,
          JSON.annotate ~origin:"evm_node.experimental_config_patch"
          @@ value_json )
        json
    else json
  in

  JSON.update "experimental_features" @@ fun json ->
  conditional_json_put
    drop_duplicate_when_injection
    ~name:"drop_duplicate_on_injection"
    (`Bool true)
    json
  |> conditional_json_put
       node_transaction_validation
       ~name:"node_transaction_validation"
       (`Bool true)

let init ?patch_config ?name ?runner ?mode ?data_dir ?rpc_addr ?rpc_port
    ?restricted_rpcs rollup_node =
  let evm_node =
    create
      ?name
      ?runner
      ?mode
      ?data_dir
      ?rpc_addr
      ?rpc_port
      ?restricted_rpcs
      rollup_node
  in
  let* () = Process.check @@ spawn_init_config evm_node in
  let* () =
    match patch_config with
    | Some patch_config -> Config_file.update evm_node patch_config
    | None -> unit
  in
  let* () = run evm_node in
  return evm_node

let init_from_rollup_node_data_dir ?reconstruct evm_node rollup_node =
  let rollup_node_data_dir = Sc_rollup_node.data_dir rollup_node in
  let process =
    spawn_command
      evm_node
      (["init"; "from"; "rollup"; "node"; rollup_node_data_dir]
      @ data_dir_arg evm_node
      @ Cli_arg.optional_arg "reconstruct" Fun.id reconstruct)
  in
  Process.check process

type request = {method_ : string; parameters : JSON.u}

let request_to_JSON {method_; parameters} : JSON.u =
  `O
    ([
       ("jsonrpc", `String "2.0");
       ("method", `String method_);
       ("id", `String "0");
     ]
    @ if parameters == `Null then [] else [("params", parameters)])

let build_request request =
  request_to_JSON request |> JSON.annotate ~origin:"evm_node"

let batch_requests requests =
  `A (List.map request_to_JSON requests) |> JSON.annotate ~origin:"evm_node"

(* We keep both encoding (with a single object or an array of objects) and both
   function on purpose, to ensure both encoding are supported by the server. *)
let call_evm_rpc ?(private_ = false) evm_node request =
  let endpoint = endpoint ~private_ evm_node in
  Curl.post endpoint (build_request request) |> Runnable.run

let batch_evm_rpc ?(private_ = false) evm_node requests =
  let endpoint = endpoint ~private_ evm_node in
  Curl.post endpoint (batch_requests requests) |> Runnable.run

let extract_result json = JSON.(json |-> "result")

let extract_error_message json = JSON.(json |-> "error" |-> "message")

let fetch_contract_code evm_node contract_address =
  let* code =
    call_evm_rpc
      evm_node
      {
        method_ = "eth_getCode";
        parameters = `A [`String contract_address; `String "latest"];
      }
  in
  return (extract_result code |> JSON.as_string)

let upgrade_payload ~root_hash ~activation_timestamp =
  let args =
    [
      "make";
      "upgrade";
      "payload";
      "with";
      "root";
      "hash";
      root_hash;
      "at";
      "activation";
      "timestamp";
      activation_timestamp;
    ]
  in
  let process = Process.spawn (Uses.path Constant.octez_evm_node) @@ args in
  let* payload = Process.check_and_read_stdout process in
  return (String.trim payload)

let transform_dump ~dump_json ~dump_rlp =
  let args = ["transform"; "dump"; dump_json; "to"; "rlp"; dump_rlp] in
  let process = Process.spawn (Uses.path Constant.octez_evm_node) @@ args in
  Process.check process

let reset evm_node ~l2_level =
  let args =
    ["reset"; "at"; string_of_int l2_level; "--force"] @ data_dir_arg evm_node
  in
  let process = Process.spawn evm_node.path @@ args in
  Process.check process

let sequencer_upgrade_payload ?client ~public_key ~pool_address
    ~activation_timestamp () =
  let args =
    [
      "make";
      "sequencer";
      "upgrade";
      "payload";
      "with";
      "pool";
      "address";
      pool_address;
      "at";
      "activation";
      "timestamp";
      activation_timestamp;
      "for";
      public_key;
    ]
  in
  let process =
    Process.spawn (Uses.path Constant.octez_evm_node)
    @@ args
    @ Cli_arg.optional_arg
        "wallet-dir"
        Fun.id
        (Option.map Client.base_dir client)
  in
  let* payload = Process.check_and_read_stdout process in
  return (String.trim payload)

let chunk_data ~rollup_address ?sequencer_key ?timestamp ?parent_hash ?number
    ?client data =
  let args = "chunk" :: "data" :: data in
  let sequencer =
    match sequencer_key with
    | None -> []
    | Some key -> ["--as-blueprint"; "--sequencer-key"; key]
  in
  let rollup_address = ["--rollup-address"; Fun.id rollup_address] in
  let timestamp = Cli_arg.optional_arg "timestamp" Fun.id timestamp in
  let parent_hash = Cli_arg.optional_arg "parent-hash" Fun.id parent_hash in
  let number = Cli_arg.optional_arg "number" string_of_int number in
  let process =
    Process.spawn (Uses.path Constant.octez_evm_node)
    @@ args @ rollup_address @ sequencer @ timestamp @ parent_hash @ number
    @ Cli_arg.optional_arg
        "wallet-dir"
        Fun.id
        (Option.map Client.base_dir client)
  in
  let* output = Process.check_and_read_stdout process in
  (* `tl` will remove the first line `Chunked_transactions :` *)
  let chunks = String.split_on_char '\n' (String.trim output) |> List.tl in
  return chunks

let patch_kernel evm_node path =
  match evm_node.status with
  | Running _ -> Test.fail "Cannot patch the kernel of a running node"
  | Not_running ->
      let args =
        [
          "patch";
          "kernel";
          "with";
          path;
          "--force";
          "--data-dir";
          data_dir evm_node;
        ]
      in
      let process = Process.spawn (Uses.path Constant.octez_evm_node) @@ args in
      Process.check process

let patch_state evm_node ~key ~value =
  match evm_node.status with
  | Running _ -> Test.fail "Cannot patch the state of a running node"
  | Not_running ->
      let args =
        [
          "patch";
          "state";
          "at";
          key;
          "with";
          value;
          "--data-dir";
          data_dir evm_node;
          "--force";
        ]
      in
      let process = Process.spawn (Uses.path Constant.octez_evm_node) @@ args in
      Process.check process

let wait_termination (evm_node : t) =
  match evm_node.status with
  | Not_running -> unit
  | Running {process; _} ->
      let* _status = Process.wait process in
      unit

let make_kernel_installer_config ?(mainnet_compat = false)
    ?(remove_whitelist = false) ?kernel_root_hash ?chain_id ?bootstrap_balance
    ?bootstrap_accounts ?sequencer ?delayed_bridge ?ticketer ?administrator
    ?sequencer_governance ?kernel_governance ?kernel_security_governance
    ?minimum_base_fee_per_gas ?(da_fee_per_byte = Wei.zero)
    ?delayed_inbox_timeout ?delayed_inbox_min_levels ?sequencer_pool_address
    ?maximum_allowed_ticks ?maximum_gas_per_transaction
    ?(max_blueprint_lookahead_in_seconds = 157_680_000L)
    ?(enable_fa_bridge = false) ?(enable_dal = false) ?dal_slots ~output () =
  let cmd =
    ["make"; "kernel"; "installer"; "config"; output]
    @ Cli_arg.optional_switch "mainnet-compat" mainnet_compat
    @ Cli_arg.optional_switch "remove-whitelist" remove_whitelist
    @ Cli_arg.optional_arg "kernel-root-hash" Fun.id kernel_root_hash
    @ Cli_arg.optional_arg "chain-id" string_of_int chain_id
    @ Cli_arg.optional_arg "sequencer" Fun.id sequencer
    @ Cli_arg.optional_arg "delayed-bridge" Fun.id delayed_bridge
    @ Cli_arg.optional_arg "ticketer" Fun.id ticketer
    @ Cli_arg.optional_arg "admin" Fun.id administrator
    @ Cli_arg.optional_arg "sequencer-governance" Fun.id sequencer_governance
    @ Cli_arg.optional_arg "kernel-governance" Fun.id kernel_governance
    @ Cli_arg.optional_arg
        "kernel-security-governance"
        Fun.id
        kernel_security_governance
    @ Cli_arg.optional_arg
        "minimum-base-fee-per-gas"
        Wei.to_string
        minimum_base_fee_per_gas
    @ ["--da-fee-per-byte"; Wei.to_string da_fee_per_byte]
    @ Cli_arg.optional_arg
        "delayed-inbox-timeout"
        string_of_int
        delayed_inbox_timeout
    @ Cli_arg.optional_arg
        "delayed-inbox-min-levels"
        string_of_int
        delayed_inbox_min_levels
    @ Cli_arg.optional_arg
        "sequencer-pool-address"
        Fun.id
        sequencer_pool_address
    @ Cli_arg.optional_arg
        "maximum-allowed-ticks"
        Int64.to_string
        maximum_allowed_ticks
    @ Cli_arg.optional_arg
        "maximum-gas-per-transaction"
        Int64.to_string
        maximum_gas_per_transaction
    @ [
        "--max-blueprint-lookahead-in-seconds";
        Int64.to_string max_blueprint_lookahead_in_seconds;
      ]
    @ Cli_arg.optional_switch "enable-fa-bridge" enable_fa_bridge
    @ Cli_arg.optional_switch "enable-dal" enable_dal
    @ Cli_arg.optional_arg
        "dal-slots"
        (fun l -> String.concat "," (List.map string_of_int l))
        dal_slots
    @ Cli_arg.optional_arg "bootstrap-balance" Wei.to_string bootstrap_balance
    @
    match bootstrap_accounts with
    | None -> []
    | Some bootstrap_accounts ->
        List.flatten
        @@ List.map
             (fun bootstrap_account ->
               ["--bootstrap-account"; bootstrap_account])
             bootstrap_accounts
  in
  let process = Process.spawn (Uses.path Constant.octez_evm_node) cmd in
  Runnable.{value = process; run = Process.check}

let preimages_dir evm_node =
  let rec from_node ~data_dir = function
    | Sandbox {preimage_dir; _}
    | Sequencer {preimage_dir; _}
    | Threshold_encryption_sequencer {preimage_dir; _} ->
        Option.value ~default:(data_dir // "wasm_2_0_0") preimage_dir
    | Rpc mode -> from_node ~data_dir mode
    | Observer {preimages_dir; _}
    | Threshold_encryption_observer {preimages_dir; _} ->
        preimages_dir
    | Proxy _ -> Test.fail "cannot start a RPC node from a proxy node"
  in
  from_node ~data_dir:(data_dir evm_node) evm_node.persistent_state.mode

let supports_threshold_encryption evm_node =
  let rec from_node = function
    | Sandbox _ | Sequencer _ | Observer _ -> false
    | Threshold_encryption_observer _ | Threshold_encryption_sequencer _ -> true
    | Rpc mode -> from_node mode
    | Proxy _ -> Test.fail "cannot start a RPC node from a proxy node"
  in
  from_node evm_node.persistent_state.mode

module Agent = struct
  (* Use for compatibility with `tezt-cloud`. *)
  let create ?(path = Uses.path Constant.octez_evm_node) ?name ?data_dir ?mode
      endpoint agent =
    let* path = Agent.copy agent ~source:path in
    let runner = Agent.runner agent in
    let rpc_port = Agent.next_available_port agent in
    create ?name ~path ~runner ?data_dir ~rpc_port ?mode endpoint |> Lwt.return

  let init ?patch_config ?name ?mode ?data_dir rollup_node agent =
    let* evm_node = create ?name ?mode ?data_dir rollup_node agent in
    let* () = Process.check @@ spawn_init_config evm_node in
    let* () =
      match patch_config with
      | Some patch_config -> Config_file.update evm_node patch_config
      | None -> unit
    in
    let* () = run evm_node in
    return evm_node

  let rpc_port evm_node = evm_node.persistent_state.rpc_port

  let name evm_node = evm_node.name
end
