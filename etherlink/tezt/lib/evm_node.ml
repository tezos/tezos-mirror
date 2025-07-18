(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
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

type history_mode = Archive | Rolling of int | Full of int

type tez_contract = {address : string; path : string; initial_storage : string}

type l2_setup = {
  l2_chain_id : int;
  l2_chain_family : string;
  world_state_path : string option;
  eth_bootstrap_accounts : string list option;
  tez_bootstrap_accounts : Account.key list option;
  tez_bootstrap_contracts : tez_contract list option;
  sequencer_pool_address : string option;
  minimum_base_fee_per_gas : Wei.t option;
  da_fee_per_byte : Wei.t option;
  maximum_gas_per_transaction : int64 option;
}

let eth_default_bootstrap_accounts =
  List.map
    (fun account -> account.Eth_account.address)
    (Array.to_list Eth_account.bootstrap_accounts)

let tez_default_bootstrap_accounts = Array.to_list Account.Bootstrap.keys

let default_l2_setup ~l2_chain_id =
  {
    l2_chain_id;
    l2_chain_family = "EVM";
    world_state_path = Some "/evm/world_state";
    eth_bootstrap_accounts = Some eth_default_bootstrap_accounts;
    tez_bootstrap_accounts = Some tez_default_bootstrap_accounts;
    tez_bootstrap_contracts = None;
    sequencer_pool_address = None;
    minimum_base_fee_per_gas = None;
    da_fee_per_byte = None;
    maximum_gas_per_transaction = None;
  }

type mode =
  | Observer of {
      initial_kernel : string;
      preimages_dir : string option;
      private_rpc_port : int option;
      rollup_node_endpoint : string;
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
      sequencer_sunset_sec : int option;
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
  | Tezlink_sandbox of {
      initial_kernel : string;
      funded_addresses : string list;
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
  | Proxy
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
    mutable last_finalized_level : int;
    mutable pending_blueprint_finalized :
      unit option Lwt.u list Per_level_map.t;
    mode : mode;
    mutable history : history_mode option;
    data_dir : string;
    config_file : string option;
    rpc_addr : string;
    rpc_port : int;
    websockets : bool;
    endpoint : string;
    runner : Runner.t option;
    restricted_rpcs : string option;
    spawn_rpc : int option;
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
  | Sequencer _ | Sandbox _ | Tezlink_sandbox _ -> true
  | Observer _ | Proxy | Rpc _ -> false

let is_observer t =
  match t.persistent_state.mode with
  | Sequencer _ | Sandbox _ | Tezlink_sandbox _ | Proxy | Rpc _ -> false
  | Observer _ -> true

let initial_kernel t =
  let rec from_mode = function
    | Sandbox {initial_kernel; _}
    | Tezlink_sandbox {initial_kernel; _}
    | Sequencer {initial_kernel; _}
    | Observer {initial_kernel; _} ->
        initial_kernel
    | Rpc mode -> from_mode mode
    | Proxy -> Test.fail "cannot start a RPC node from a proxy node"
  in
  from_mode t.persistent_state.mode

let can_apply_blueprint t =
  match t.persistent_state.mode with
  | Sequencer _ | Sandbox _ | Tezlink_sandbox _ | Observer _ -> true
  | Proxy | Rpc _ -> false

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

let trigger_blueprint_finalized evm_node level =
  let pending = evm_node.persistent_state.pending_blueprint_finalized in
  let pending_for_level = Per_level_map.find_opt level pending in
  evm_node.persistent_state.last_finalized_level <- level ;
  evm_node.persistent_state.pending_blueprint_finalized <-
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

let event_blueprint_finalized_name =
  "evm_events_follower_upstream_blueprint_applied.v0"

let event_blueprint_applied_name = "blueprint_application.v0"

let handle_is_ready_event (evm_node : t) {name; value = _; timestamp = _} =
  if name = event_ready_name then set_ready evm_node else ()

let handle_blueprint_injected_event (evm_node : t) {name; value; timestamp = _}
    =
  if name = event_blueprint_injected_name then
    trigger_blueprint_injected evm_node JSON.(value |> as_int)
  else ()

let handle_blueprint_finalized_event (evm_node : t) {name; value; timestamp = _}
    =
  if name = event_blueprint_finalized_name then
    trigger_blueprint_finalized evm_node JSON.(value |-> "level" |> as_int)
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

let wait_for_blueprint_finalized ?timeout evm_node level =
  match evm_node.status with
  | Running {session_state = {ready = true; _}; _}
    when is_sequencer evm_node || is_observer evm_node ->
      let current_level = evm_node.persistent_state.last_finalized_level in
      if level <= current_level then unit
      else
        let promise, resolver = Lwt.task () in
        evm_node.persistent_state.pending_blueprint_finalized <-
          Per_level_map.update
            level
            (fun pending -> Some (resolver :: Option.value ~default:[] pending))
            evm_node.persistent_state.pending_blueprint_finalized ;
        wait_for_event_listener
          ?timeout
          evm_node
          ~event:event_blueprint_finalized_name
          promise
  | Running {session_state = {ready = true; _}; _} ->
      failwith "EVM node is not a stateful node"
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      failwith "EVM node is not ready"

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

let wait_for_blueprint_invalid_applied evm_node =
  wait_for_event evm_node ~event:"blueprint_invalid_applied.v0" @@ fun _ ->
  Some ()

let wait_for_blueprint_injected_on_dal ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"blueprint_injection_on_DAL.v0"
  @@ JSON.(
       fun json ->
         let level = json |-> "level" |> as_int in
         let nb_chunks = json |-> "nb_chunks" |> as_int in
         Some (level, nb_chunks))

let wait_for_signal_signed ?timeout evm_node =
  let open JSON in
  let as_slot_index_and_published_level json =
    (json |-> "slot_index" |> as_int, json |-> "published_level" |> as_int)
  in
  wait_for_event ?timeout evm_node ~event:"signal_publisher_signal_signed.v0"
  @@ JSON.(
       fun json ->
         let smart_rollup_address =
           json |-> "smart_rollup_address" |> as_string
         in
         let signals = json |-> "signals" |> as_list in
         Some
           ( smart_rollup_address,
             List.map as_slot_index_and_published_level signals ))

let wait_for_rollup_node_follower_disabled ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"rollup_node_follower_disabled.v0"
  @@ Fun.const (Some ())

let wait_for_flush_delayed_inbox ?timeout ?level evm_node =
  wait_for_event ?timeout evm_node ~event:"flush_delayed_inbox.v0"
  @@ fun json ->
  let found_level = JSON.(json |-> "level" |> as_int) in
  match level with
  | Some level when level = found_level -> Some found_level
  | None -> Some found_level
  | Some _ -> None

let wait_for_blueprint_invalid ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"blueprint_invalid.v0"
  @@ Fun.const (Some ())

let wait_for_predownload_kernel ?timeout evm_node ~root_hash =
  wait_for_event ?timeout evm_node ~event:"predownload_kernel.v0" @@ fun json ->
  json |> JSON.as_string |> fun hash ->
  if root_hash = hash then Some () else None

let wait_for_predownload_kernel_failed ?timeout evm_node ~root_hash =
  wait_for_event ?timeout evm_node ~event:"predownload_kernel_failed.v0"
  @@ fun json ->
  let event_root_hash = JSON.(json |-> "version" |> as_string) in
  if root_hash = event_root_hash then Some () else None

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

let wait_for_spawn_rpc_ready ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"spawn_rpc_is_ready.v0"
  @@ Fun.const (Some ())

let wait_for_import_finished ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"import_finished.v0"
  @@ Fun.const (Some ())

let wait_for_finished_exporting_snapshot ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"finished_exporting_snapshot.v0"
  @@ fun json ->
  let filename = JSON.(json |> as_string) in
  Some filename

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

let wait_for_shutdown_event ?(can_terminate = false) evm_node =
  let shutdown_event = "shutting_down.v0" in
  Lwt.catch
    (fun () ->
      wait_for evm_node shutdown_event @@ fun json ->
      JSON.(json |> as_int |> Option.some |> Option.some))
    (function
      | Terminated_before_event {event; _}
        when event = shutdown_event && can_terminate ->
          Lwt.return_none
      | exn -> Lwt.reraise exn)

let wait_for_diverged evm_node =
  wait_for evm_node "evm_events_follower_diverged.v0" @@ fun json ->
  let open JSON in
  let level = json |-> "level" |> as_int in
  let expected_hash = json |-> "expected_hash" |> as_string in
  let found_hash = json |-> "found_hash" |> as_string in
  Some (level, expected_hash, found_hash)

let wait_for_reset evm_node =
  wait_for evm_node "evm_context_reset_at_level.v0" @@ fun _json -> Some ()

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

let wait_for_tx_queue_add_transaction ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"tx_queue_add_transaction.v0"
  @@ fun json -> JSON.(json |> as_string |> Option.some)

let wait_for_tx_queue_transaction_confirmed ?timeout ?hash evm_node =
  wait_for_event ?timeout evm_node ~event:"tx_queue_transaction_confirmed.v0"
  @@ fun json ->
  let found_hash = JSON.(json |> as_string) in
  match hash with
  | Some hash -> if found_hash = hash then Some found_hash else None
  | None -> Some found_hash

let wait_for_tx_queue_injecting_transaction ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"tx_queue_injecting_transaction.v0"
  @@ fun json -> JSON.(json |> as_int |> Option.some)

let wait_for_tx_queue_cleared ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"tx_queue_cleared.v0"
  @@ Fun.const (Some ())

let wait_for_block_producer_rejected_transaction ?timeout ?hash evm_node =
  wait_for_event
    ?timeout
    evm_node
    ~event:"block_producer_transaction_rejected.v0"
  @@ fun json ->
  let found_hash = JSON.(json |-> "tx_hash" |> as_string) in
  let reason = JSON.(json |-> "error" |> as_string) in
  match hash with
  | Some hash -> if found_hash = hash then Some reason else None
  | None -> Some reason

let wait_for_split ?level evm_node =
  wait_for_event evm_node ~event:"evm_context_gc_split.v0" @@ fun json ->
  let event_level = JSON.(json |-> "level" |> as_int) in
  match level with
  | Some level -> if event_level = level then Some event_level else None
  | None -> Some event_level

let wait_for_gc_finished ?gc_level ?head_level evm_node =
  wait_for_event evm_node ~event:"evm_context_gc_finished.v0" @@ fun json ->
  let event_gc_level = JSON.(json |-> "gc_level" |> as_int) in
  let event_head_level = JSON.(json |-> "head_level" |> as_int) in
  match (gc_level, head_level) with
  | Some gc_level, Some head_level ->
      if event_gc_level = gc_level && head_level = event_head_level then
        Some (event_gc_level, gc_level)
      else None
  | Some gc_level, None ->
      if event_gc_level = gc_level then Some (event_gc_level, gc_level)
      else None
  | None, Some head_level ->
      if event_gc_level = head_level then Some (event_gc_level, event_gc_level)
      else None
  | None, None -> Some (event_gc_level, event_gc_level)

type processed_l1_level = {l1_level : int; finalized_blueprint : int}

let wait_for_processed_l1_level ?timeout ?level evm_node =
  wait_for_event ?timeout evm_node ~event:"evm_context_processed_l1_level.v0"
  @@ fun json ->
  let l1_level = JSON.(json |-> "level" |> as_int) in
  let finalized_blueprint = JSON.(json |-> "finalized_blueprint" |> as_int) in
  let res = {l1_level; finalized_blueprint} in
  match level with
  | None -> Some res
  | Some level -> if level = l1_level then Some res else None

let wait_for_start_history_mode ?history_mode evm_node =
  wait_for_event evm_node ~event:"evm_context_start_history_mode.v0"
  @@ fun json ->
  let event_history_mode = JSON.as_string json in
  match history_mode with
  | Some history_mode ->
      if history_mode = event_history_mode then Some history_mode
      else
        Test.fail
          "Started with wrong history mode (expected: %s; current %s)"
          history_mode
          event_history_mode
  | None -> Some event_history_mode

let wait_for_blueprint_catchup ?timeout evm_node =
  wait_for_event ?timeout evm_node ~event:"blueprint_catchup.v0" @@ fun json ->
  let open JSON in
  let min = json |-> "min" |> as_int in
  let max = json |-> "max" |> as_int in
  Some (min, max)

let wait_for_blueprint_injection_failure ?timeout ?level evm_node =
  wait_for_event ?timeout evm_node ~event:"blueprint_injection_failure.v0"
  @@ fun json ->
  match level with
  | Some expected_level ->
      let open JSON in
      if json |-> "level" |> as_int = expected_level then Some () else None
  | None -> Some ()

let mode_with_new_private_rpc (mode : mode) =
  match mode with
  | Observer
      {
        initial_kernel;
        preimages_dir;
        private_rpc_port = Some _;
        rollup_node_endpoint;
      } ->
      Observer
        {
          initial_kernel;
          preimages_dir;
          private_rpc_port = Some (Port.fresh ());
          rollup_node_endpoint;
        }
  | Sequencer
      {
        initial_kernel;
        preimage_dir;
        private_rpc_port = Some _;
        time_between_blocks;
        sequencer;
        genesis_timestamp;
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
        sequencer_sunset_sec;
      } ->
      Sequencer
        {
          initial_kernel;
          preimage_dir;
          private_rpc_port = Some (Port.fresh ());
          time_between_blocks;
          sequencer;
          genesis_timestamp;
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
          sequencer_sunset_sec;
        }
  | Sandbox
      {
        initial_kernel;
        preimage_dir;
        private_rpc_port = Some _;
        time_between_blocks;
        genesis_timestamp;
        max_number_of_chunks;
        wallet_dir;
        tx_pool_timeout_limit;
        tx_pool_addr_limit;
        tx_pool_tx_per_addr_limit;
      } ->
      Sandbox
        {
          initial_kernel;
          preimage_dir;
          private_rpc_port = Some (Port.fresh ());
          time_between_blocks;
          genesis_timestamp;
          max_number_of_chunks;
          wallet_dir;
          tx_pool_timeout_limit;
          tx_pool_addr_limit;
          tx_pool_tx_per_addr_limit;
        }
  | _ -> mode

let create ?(path = Uses.path Constant.octez_evm_node) ?name ?runner
    ?(mode = Proxy) ?history ?data_dir ?config_file ?rpc_addr ?rpc_port
    ?restricted_rpcs ?spawn_rpc ?(websockets = false) endpoint =
  let arguments, rpc_addr, rpc_port =
    connection_arguments ?rpc_addr ?rpc_port ?runner ()
  in
  let new_name () =
    match mode with
    | Proxy -> "proxy_" ^ fresh_name ()
    | Sequencer _ -> "sequencer_" ^ fresh_name ()
    | Sandbox _ -> "sandbox_" ^ fresh_name ()
    | Tezlink_sandbox _ -> "tezlink_sandbox_" ^ fresh_name ()
    | Observer _ -> "observer_" ^ fresh_name ()
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
        last_finalized_level = 0;
        pending_blueprint_finalized = Per_level_map.empty;
        mode;
        history;
        data_dir;
        config_file;
        rpc_addr;
        rpc_port;
        websockets;
        endpoint;
        restricted_rpcs;
        runner;
        spawn_rpc;
      }
  in
  evm_node

let name evm_node = evm_node.name

let rpc_port evm_node = evm_node.persistent_state.rpc_port

let spawn_rpc evm_node = evm_node.persistent_state.spawn_rpc

let data_dir evm_node = evm_node.persistent_state.data_dir

let config_file evm_node = evm_node.persistent_state.config_file

let data_dir_arg evm_node = ["--data-dir"; evm_node.persistent_state.data_dir]

let config_file_arg evm_node =
  Cli_arg.optional_arg
    "config-file"
    Fun.id
    evm_node.persistent_state.config_file

(* assume a valid config for the given command and uses new latest run
   command format. *)
let run_args evm_node =
  let shared_args =
    config_file_arg evm_node @ data_dir_arg evm_node
    @ evm_node.persistent_state.arguments
  in
  let mode_args =
    match evm_node.persistent_state.mode with
    | Proxy -> ["run"; "proxy"]
    | Sequencer {initial_kernel; genesis_timestamp; wallet_dir; _} ->
        ["run"; "sequencer"; "--initial-kernel"; initial_kernel]
        @ Cli_arg.optional_arg
            "genesis-timestamp"
            (fun timestamp ->
              Client.time_of_timestamp timestamp |> Client.Time.to_notation)
            genesis_timestamp
        @ Cli_arg.optional_arg "wallet-dir" Fun.id wallet_dir
    | Sandbox {initial_kernel; genesis_timestamp; wallet_dir; _} ->
        ["run"; "sandbox"; "--kernel"; initial_kernel]
        @ Cli_arg.optional_arg
            "genesis-timestamp"
            (fun timestamp ->
              Client.time_of_timestamp timestamp |> Client.Time.to_notation)
            genesis_timestamp
        @ Cli_arg.optional_arg "wallet-dir" Fun.id wallet_dir
    | Tezlink_sandbox
        {initial_kernel; genesis_timestamp; wallet_dir; funded_addresses; _} ->
        ["run"; "tezlink"; "sandbox"; "--kernel"; initial_kernel]
        @ Cli_arg.optional_arg
            "genesis-timestamp"
            (fun timestamp ->
              Client.time_of_timestamp timestamp |> Client.Time.to_notation)
            genesis_timestamp
        @ Cli_arg.optional_arg "wallet-dir" Fun.id wallet_dir
        @ List.fold_left
            (fun acc pk -> Cli_arg.optional_arg "fund" Fun.id (Some pk) @ acc)
            []
            funded_addresses
    | Observer {initial_kernel; _} ->
        ["run"; "observer"; "--initial-kernel"; initial_kernel]
    | Rpc _ -> ["experimental"; "run"; "rpc"]
  in
  mode_args @ shared_args

let run ?(wait = true) ?(extra_arguments = []) evm_node =
  on_event evm_node (handle_is_ready_event evm_node) ;
  on_event evm_node (handle_blueprint_injected_event evm_node) ;
  on_event evm_node (handle_blueprint_applied_event evm_node) ;
  on_event evm_node (handle_blueprint_finalized_event evm_node) ;
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
    let pending_blueprint_finalized =
      evm_node.persistent_state.pending_blueprint_finalized
    in
    evm_node.persistent_state.pending_blueprint_finalized <- Per_level_map.empty ;
    Per_level_map.iter
      (fun _ pending_list ->
        List.iter (fun pending -> Lwt.wakeup_later pending None) pending_list)
      pending_blueprint_finalized ;
    unit
  in
  let env =
    match Sys.getenv_opt "RUST_LOG" with
    | Some _ -> None
    | None ->
        Some
          String_map.(
            singleton
              "RUST_LOG"
              "octez_evm_node_wasm_runtime::write_debug=trace")
  in
  let* () =
    run
      ?env
      ?runner:evm_node.persistent_state.runner
      ~event_level:`Debug
      evm_node
      {ready = false}
      (run_args evm_node @ extra_arguments)
      ~on_terminate
  in
  let* () =
    if wait then
      let* () = wait_for_ready evm_node
      and* () =
        match evm_node.persistent_state.spawn_rpc with
        | None -> unit
        | Some _ -> wait_for_spawn_rpc_ready evm_node
      in
      unit
    else unit
  in
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
    match evm_node.persistent_state.config_file with
    | Some config_file -> config_file
    | None -> Filename.concat evm_node.persistent_state.data_dir "config.json"

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

let spawn_init_config_minimal ~data_dir ?config_file
    ?(path = Uses.(path Constant.octez_evm_node)) ?(extra_arguments = []) () =
  Process.spawn ~name:"evm_node_init_config" path
  @@ ["init"; "config"; "--data-dir"; data_dir]
  @ Cli_arg.optional_arg "config-file" Fun.id config_file
  @ extra_arguments

let spawn_init_config ?(extra_arguments = []) evm_node =
  let shared_args =
    data_dir_arg evm_node @ config_file_arg evm_node
    @ evm_node.persistent_state.arguments
    @ Cli_arg.optional_arg
        "restricted-rpcs"
        Fun.id
        evm_node.persistent_state.restricted_rpcs
    @ Cli_arg.optional_arg
        "history"
        (function
          | Archive -> "archive"
          | Rolling n -> sf "rolling:%d" n
          | Full n -> sf "full:%d" n)
        evm_node.persistent_state.history
    @ Cli_arg.optional_switch "ws" evm_node.persistent_state.websockets
  in

  let time_between_blocks_fmt = function
    | Nothing -> "none"
    | Time_between_blocks f -> Format.sprintf "%.3f" f
  in
  let mode_args =
    match evm_node.persistent_state.mode with
    | Proxy -> ["--rollup-node-endpoint"; evm_node.persistent_state.endpoint]
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
          sequencer_sunset_sec;
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
        @ Cli_arg.optional_arg "sunset-sec" string_of_int sequencer_sunset_sec
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
    | Tezlink_sandbox
        {
          initial_kernel = _;
          funded_addresses = _;
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
    | Observer
        {
          preimages_dir;
          initial_kernel = _;
          rollup_node_endpoint;
          private_rpc_port;
        } ->
        [
          "--evm-node-endpoint";
          evm_node.persistent_state.endpoint;
          "--rollup-node-endpoint";
          rollup_node_endpoint;
        ]
        @ Cli_arg.optional_arg "preimages-dir" Fun.id preimages_dir
        @ Cli_arg.optional_arg "private-rpc-port" string_of_int private_rpc_port
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
      | Observer {private_rpc_port = Some private_rpc_port; _}
      | Sandbox {private_rpc_port = Some private_rpc_port; _}
      | Tezlink_sandbox {private_rpc_port = Some private_rpc_port; _} ->
          (host, private_rpc_port, "/private")
      | Sequencer {private_rpc_port = None; _}
      | Sandbox {private_rpc_port = None; _}
      | Tezlink_sandbox {private_rpc_port = None; _} ->
          Test.fail "Sequencer doesn't have a private RPC server"
      | Proxy -> Test.fail "Proxy doesn't have a private RPC server"
      | Observer _ -> Test.fail "Observer doesn't have a private RPC server"
      | Rpc _ -> Test.fail "Rpc node doesn't have a private RPC server"
    else (host, evm_node.persistent_state.rpc_port, "")
  in
  Format.sprintf "http://%s:%d%s" addr port path

let rpc_endpoint_record ?(local = false) (evm_node : t) =
  let host =
    if local then Constant.default_host
    else Runner.address evm_node.persistent_state.runner
  in
  let port = evm_node.persistent_state.rpc_port in
  Endpoint.make ~host ~scheme:"http" ~port ()

let endpoint = rpc_endpoint ?local:None

type rpc_server = Resto | Dream

let conditional_json_put ~name cond value_json json =
  if cond then
    JSON.put
      (name, JSON.annotate ~origin:"evm_node.config_patch" @@ value_json)
      json
  else json

let conditional_json_put_default ~name cond value_json ~default json =
  JSON.put
    ( name,
      JSON.annotate ~origin:"evm_node.config_patch"
      @@ if cond then value_json else default )
    json

let optional_json_put ~name v f json =
  match v with
  | None -> json
  | Some v ->
      let value_json = f v in
      JSON.put
        (name, JSON.annotate ~origin:"evm_node.config_patch" @@ value_json)
        json

type tx_queue_config =
  | Config of {max_size : int; max_lifespan : int; tx_per_addr_limit : int}
  | Enable of bool

let patch_config_with_experimental_feature
    ?(drop_duplicate_when_injection = false)
    ?(blueprints_publisher_order_enabled = false) ?(next_wasm_runtime = true)
    ?rpc_server
    ?(enable_tx_queue =
      Config
        {max_size = 1000; max_lifespan = 100_000; tx_per_addr_limit = 100_000})
    ?spawn_rpc ?periodic_snapshot_path ?l2_chains () =
  JSON.update "experimental_features" @@ fun json ->
  conditional_json_put
    drop_duplicate_when_injection
    ~name:"drop_duplicate_on_injection"
    (`Bool true)
    json
  |> conditional_json_put
       blueprints_publisher_order_enabled
       ~name:"blueprints_publisher_order_enabled"
       (`Bool true)
  |> conditional_json_put
       next_wasm_runtime
       ~name:"next_wasm_runtime"
       (`Bool true)
  |> optional_json_put ~name:"rpc_server" rpc_server (function
         | Resto -> `String "resto"
         | Dream -> `String "dream")
  |> fun json ->
  let value_json =
    JSON.annotate ~origin:"evm_node.config_patch"
    @@
    match enable_tx_queue with
    | Config {max_size; max_lifespan; tx_per_addr_limit} ->
        `O
          [
            ("max_size", `Float (Float.of_int max_size));
            ("max_lifespan", `Float (Float.of_int max_lifespan));
            ("tx_per_addr_limit", `String (string_of_int tx_per_addr_limit));
          ]
    | Enable b -> `Bool b
  in
  JSON.put ("enable_tx_queue", value_json) json
  |> optional_json_put spawn_rpc ~name:"spawn_rpc" (fun port ->
         `O [("protected_port", `Float (float_of_int port))])
  |> optional_json_put
       ~name:"periodic_snapshot_path"
       periodic_snapshot_path
       (fun path -> `String path)
  |> optional_json_put ~name:"l2_chains" l2_chains (fun l2_chains ->
         `A
           (List.map
              (fun {l2_chain_id; l2_chain_family; _} ->
                `O
                  [
                    ("chain_id", `String (string_of_int l2_chain_id));
                    ("chain_family", `String l2_chain_family);
                  ])
              l2_chains))

let patch_config_websockets_if_enabled ?max_message_length
    ?(monitor_heartbeat = true) ?rate_limit =
  JSON.update "websockets" @@ fun json ->
  if JSON.is_null json then json
  else
    optional_json_put
      max_message_length
      ~name:"max_message_length"
      (fun max -> `Float (float_of_int max))
      json
    (* Monitor websocket connections with frequent heartbeats and small timeout
       for the tests. *)
    |> conditional_json_put_default
         monitor_heartbeat
         ~name:"monitor_heartbeat"
         (`O [("ping_interval", `Float 0.5); ("ping_timeout", `Float 2.)])
         ~default:(`String "disabled")
    |> optional_json_put rate_limit ~name:"rate_limit" Fun.id

let patch_config_gc ?history_mode json =
  json
  |> optional_json_put ~name:"history" history_mode (function
         | Archive -> `String "archive"
         | Rolling retention -> `String (Format.sprintf "rolling:%d" retention)
         | Full retention -> `String (Format.sprintf "full:%d" retention))

let init ?patch_config ?name ?runner ?mode ?data_dir ?config_file ?rpc_addr
    ?rpc_port ?restricted_rpcs ?history_mode ?spawn_rpc ?websockets
    ?extra_arguments rollup_node =
  let evm_node =
    create
      ?name
      ?runner
      ?mode
      ?history:history_mode
      ?data_dir
      ?config_file
      ?rpc_addr
      ?rpc_port
      ?restricted_rpcs
      ?spawn_rpc
      ?websockets
      rollup_node
  in
  let* () = Process.check @@ spawn_init_config evm_node in
  let* () =
    match patch_config with
    | Some patch_config -> Config_file.update evm_node patch_config
    | None -> unit
  in
  let* () = run ?extra_arguments evm_node in
  return evm_node

let init_from_rollup_node_data_dir ?(omit_delayed_tx_events = false) evm_node
    rollup_node =
  let rollup_node_data_dir = Sc_rollup_node.data_dir rollup_node in
  let process =
    spawn_command
      evm_node
      (["init"; "from"; "rollup"; "node"; rollup_node_data_dir]
      @ data_dir_arg evm_node @ config_file_arg evm_node
      @ Cli_arg.optional_switch "omit-delayed-tx-events" omit_delayed_tx_events
      )
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
  let* json = Curl.post endpoint (batch_requests requests) |> Runnable.run in
  return (JSON.as_list json)

let open_websocket ?(private_ = false) evm_node =
  let kind = if private_ then "private" else "public" in
  Websocket.connect
    ~name:(String.concat "_" ["ws"; kind; evm_node.name])
    (endpoint ~private_ evm_node ^ "/ws")

let call_evm_websocket websocket request =
  Websocket.send_recv websocket (build_request request)

let batch_evm_websocket websocket requests =
  let* l =
    Lwt_list.map_s
      (fun r -> Websocket.send websocket (build_request r))
      requests
  in
  Lwt_list.map_s (fun () -> Websocket.recv websocket) l

let jsonrpc ?websocket ?private_ evm_node =
  match websocket with
  | None -> call_evm_rpc ?private_ evm_node
  | Some ws -> call_evm_websocket ws

let batch_jsonrpc ?websocket ?private_ evm_node =
  match websocket with
  | None -> batch_evm_rpc ?private_ evm_node
  | Some ws -> batch_evm_websocket ws

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

let debug_print_store_schemas ?(path = Uses.path Constant.octez_evm_node) ?hooks
    () =
  let args = ["debug"; "print"; "store"; "schemas"] in
  let process = Process.spawn ?hooks path @@ args in
  Process.check process

let man ?(path = Uses.path Constant.octez_evm_node) ?hooks () =
  let args = ["man"; "-v"; "3"] in
  let process = Process.spawn ?hooks path @@ args in
  Process.check process

let describe_config ?(path = Uses.path Constant.octez_evm_node) ?hooks () =
  let args = ["describe"; "config"] in
  let process = Process.spawn ?hooks path @@ args in
  Process.check process

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

let export_snapshot =
  let cpt = ref 0 in
  fun ?(compress_on_the_fly = false) evm_node ->
    incr cpt ;
    let dir = Tezt.Temp.dir "evm_snapshots" in
    let snapshot_file = (dir // "evm-snapshot-nb%r-%l.") ^ string_of_int !cpt in
    let args =
      [
        "snapshot";
        "export";
        "--data-dir";
        data_dir evm_node;
        "--snapshot-file";
        snapshot_file;
      ]
      @ Cli_arg.optional_switch "compress-on-the-fly" compress_on_the_fly
    in
    let process = spawn_command evm_node args in
    let parse process =
      let* output = Process.check_and_read_stdout process in
      match output =~* rex "Snapshot exported to ([^\n]*)" with
      | None -> Test.fail "Snapshot export failed"
      | Some filename -> return filename
    in
    Runnable.{value = process; run = parse}

let import_snapshot ?(force = false) evm_node ~snapshot_file =
  let args =
    ["snapshot"; "import"; snapshot_file; "--data-dir"; data_dir evm_node]
    @ Cli_arg.optional_switch "force" force
  in
  let process = spawn_command evm_node args in
  Runnable.{value = process; run = Process.check}

let snapshot_info ~snapshot_file =
  let cmd = ["snapshot"; "info"; snapshot_file] in
  let process = Process.spawn (Uses.path Constant.octez_evm_node) cmd in
  Runnable.{value = process; run = Process.check_and_read_stdout}

let wait_termination (evm_node : t) =
  match evm_node.status with
  | Not_running -> unit
  | Running {process; _} ->
      let* _status = Process.wait process in
      unit

let ten_years_in_seconds = 3600 * 24 * 365 * 10 |> Int64.of_int

let make_kernel_installer_config ?(l2_chain_ids = [])
    ?max_delayed_inbox_blueprint_length ?(mainnet_compat = false)
    ?(remove_whitelist = false) ?kernel_root_hash ?chain_id
    ?eth_bootstrap_balance ?eth_bootstrap_accounts ?sequencer ?delayed_bridge
    ?ticketer ?administrator ?sequencer_governance ?kernel_governance
    ?kernel_security_governance ?minimum_base_fee_per_gas
    ?(da_fee_per_byte = Wei.zero) ?delayed_inbox_timeout
    ?delayed_inbox_min_levels ?sequencer_pool_address ?maximum_allowed_ticks
    ?maximum_gas_per_transaction
    ?(max_blueprint_lookahead_in_seconds = ten_years_in_seconds)
    ?(set_account_code = []) ?(enable_fa_bridge = false) ?(enable_revm = false)
    ?(enable_dal = false) ?dal_slots ?(enable_fast_withdrawal = false)
    ?(enable_fast_fa_withdrawal = false) ?(enable_multichain = false)
    ?evm_version ~output () =
  let set_account_code =
    List.flatten
    @@ List.map
         (fun (address, code) ->
           ["--set-code"; Format.sprintf "%s,%s" address code])
         set_account_code
  in
  let l2_chain_ids =
    List.flatten
    @@ List.map
         (fun l2_chain_id -> ["--l2-chain-id"; string_of_int l2_chain_id])
         l2_chain_ids
  in
  let cmd =
    ["make"; "kernel"; "installer"; "config"; output]
    @ Cli_arg.optional_arg
        "max-delayed-inbox-blueprint-length"
        Int.to_string
        max_delayed_inbox_blueprint_length
    @ Cli_arg.optional_switch "mainnet-compat" mainnet_compat
    @ Cli_arg.optional_switch "remove-whitelist" remove_whitelist
    @ Cli_arg.optional_arg "kernel-root-hash" Fun.id kernel_root_hash
    @ Cli_arg.optional_arg "chain-id" string_of_int chain_id
    @ l2_chain_ids
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
    @ set_account_code
    @ Cli_arg.optional_arg
        "maximum-gas-per-transaction"
        Int64.to_string
        maximum_gas_per_transaction
    @ [
        "--max-blueprint-lookahead-in-seconds";
        Int64.to_string max_blueprint_lookahead_in_seconds;
      ]
    @ Cli_arg.optional_switch "enable-fa-bridge" enable_fa_bridge
    @ Cli_arg.optional_switch "enable-revm" enable_revm
    @ Cli_arg.optional_switch "enable-multichain" enable_multichain
    @ Cli_arg.optional_switch "enable-dal" enable_dal
    @ Cli_arg.optional_switch "enable-fast-withdrawal" enable_fast_withdrawal
    @ Cli_arg.optional_switch
        "enable-fast-fa-withdrawal"
        enable_fast_fa_withdrawal
    @ Cli_arg.optional_arg
        "dal-slots"
        (fun l -> String.concat "," (List.map string_of_int l))
        dal_slots
    @ Cli_arg.optional_arg
        "eth-bootstrap-balance"
        Wei.to_string
        eth_bootstrap_balance
    @ Cli_arg.optional_arg "evm-version" Evm_version.to_string evm_version
    @
    match eth_bootstrap_accounts with
    | None -> []
    | Some eth_bootstrap_accounts ->
        List.flatten
        @@ List.map
             (fun eth_bootstrap_account ->
               ["--eth-bootstrap-account"; eth_bootstrap_account])
             eth_bootstrap_accounts
  in
  let process = Process.spawn (Uses.path Constant.octez_evm_node) cmd in
  Runnable.{value = process; run = Process.check}

let make_l2_kernel_installer_config ?chain_id ?chain_family
    ?eth_bootstrap_balance ?tez_bootstrap_balance ?eth_bootstrap_accounts
    ?tez_bootstrap_accounts ?tez_bootstrap_contracts ?minimum_base_fee_per_gas
    ?(da_fee_per_byte = Wei.zero) ?sequencer_pool_address
    ?maximum_gas_per_transaction ?(set_account_code = []) ?world_state_path
    ~output () =
  let set_account_code =
    List.flatten
    @@ List.map
         (fun (address, code) ->
           ["--set-code"; Format.sprintf "%s,%s" address code])
         set_account_code
  in
  let cmd =
    ["make"; "l2"; "kernel"; "installer"; "config"; output]
    @ Cli_arg.optional_arg "l2-chain-id" string_of_int chain_id
    @ Cli_arg.optional_arg "l2-chain-family" Fun.id chain_family
    @ Cli_arg.optional_arg
        "minimum-base-fee-per-gas"
        Wei.to_string
        minimum_base_fee_per_gas
    @ Cli_arg.optional_arg "world-state-path" Fun.id world_state_path
    @ ["--da-fee-per-byte"; Wei.to_string da_fee_per_byte]
    @ Cli_arg.optional_arg
        "sequencer-pool-address"
        Fun.id
        sequencer_pool_address
    @ set_account_code
    @ Cli_arg.optional_arg
        "maximum-gas-per-transaction"
        Int64.to_string
        maximum_gas_per_transaction
    @ Cli_arg.optional_arg
        "eth-bootstrap-balance"
        Wei.to_string
        eth_bootstrap_balance
    @ Cli_arg.optional_arg
        "tez-bootstrap-balance"
        Tez.to_string
        tez_bootstrap_balance
    @ (match tez_bootstrap_accounts with
      | None -> []
      | Some tez_bootstrap_accounts ->
          List.flatten
          @@ List.map
               (fun tez_bootstrap_account ->
                 [
                   "--tez-bootstrap-account";
                   tez_bootstrap_account.Account.public_key;
                 ])
               tez_bootstrap_accounts)
    @ (match tez_bootstrap_contracts with
      | None -> []
      | Some contracts ->
          List.flatten
          @@ List.map
               (fun contract -> ["--tez-bootstrap-contract"; contract])
               contracts)
    @
    match eth_bootstrap_accounts with
    | None -> []
    | Some eth_bootstrap_accounts ->
        List.flatten
        @@ List.map
             (fun eth_bootstrap_account ->
               ["--eth-bootstrap-account"; eth_bootstrap_account])
             eth_bootstrap_accounts
  in
  let process = Process.spawn (Uses.path Constant.octez_evm_node) cmd in
  Runnable.{value = process; run = Process.check}

let preimages_dir evm_node =
  let rec from_node ~data_dir = function
    | Sandbox {preimage_dir; _}
    | Tezlink_sandbox {preimage_dir; _}
    | Sequencer {preimage_dir; _}
    | Observer {preimages_dir = preimage_dir; _} ->
        Option.value ~default:(data_dir // "wasm_2_0_0") preimage_dir
    | Rpc mode -> from_node ~data_dir mode
    | Proxy -> Test.fail "cannot start a RPC node from a proxy node"
  in
  from_node ~data_dir:(data_dir evm_node) evm_node.persistent_state.mode

let list_metrics ?hooks () =
  let cmd = ["list"; "metrics"] in
  let process = Process.spawn ?hooks (Uses.path Constant.octez_evm_node) cmd in
  Process.check process

let list_events ?hooks ?level ?(json = false) () =
  let cmd =
    ["list"; "events"]
    @ Cli_arg.optional_arg "level" Fun.id level
    @ Cli_arg.optional_switch "json" json
  in
  let process = Process.spawn ?hooks (Uses.path Constant.octez_evm_node) cmd in
  Process.check process

let switch_history_mode evm_node history =
  let hist =
    match history with
    | Archive -> "archive"
    | Rolling n -> sf "rolling:%d" n
    | Full n -> sf "full:%d" n
  in
  let args =
    ["switch"; "history"; "to"; hist; "--data-dir"; data_dir evm_node]
  in
  let run process =
    let* () = Process.check process in
    evm_node.persistent_state.history <- Some history ;
    unit
  in
  let process = spawn_command evm_node args in
  {Runnable.value = process; run}

let switch_sequencer_to_observer ~(old_sequencer : t) ~(new_sequencer : t) =
  let initial_kernel, preimages_dir, private_rpc_port =
    match mode old_sequencer with
    | Sequencer {initial_kernel; preimage_dir; private_rpc_port; _} ->
        (initial_kernel, preimage_dir, private_rpc_port)
    | _ -> invalid_arg "Evm_node is not a sequencer"
  in
  {
    old_sequencer with
    name = "observer_sequencer_" ^ fresh_name ();
    persistent_state =
      {
        old_sequencer.persistent_state with
        mode =
          Observer
            {
              initial_kernel;
              preimages_dir;
              private_rpc_port;
              rollup_node_endpoint = old_sequencer.persistent_state.endpoint;
            };
        endpoint = endpoint new_sequencer;
      };
  }
