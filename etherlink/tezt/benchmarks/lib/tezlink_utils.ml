(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Helpers for benchmarking the Michelson runtime on the Tezlink endpoint
    of an Etherlink EVM node. Mirrors the EVM-side helpers in
    [Benchmark_utils] but talks to the [/tezlink] sub-path via the regular
    [Client]. *)

let tezlink_foreign_endpoint evm_node =
  let evm_node_endpoint = Evm_node.rpc_endpoint_record evm_node in
  {evm_node_endpoint with path = "/tezlink"}

let tezlink_endpoint evm_node =
  Client.Foreign_endpoint (tezlink_foreign_endpoint evm_node)

let tezlink_client evm_node =
  Client.init ~endpoint:(tezlink_endpoint evm_node) ()

(** Counter of a Tezlink contract / implicit account, fetched via direct
    HTTP (no [octez-client] subprocess). *)
let get_counter ~tezlink_endpoint ~pkh () =
  let* json =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract_counter ~id:pkh ()
  in
  return JSON.(json |> as_int)

(** Write the source [code] of a Michelson contract to a fresh temp file and
    return its path. [Client.originate_contract] expects a file path on
    disk; the contract source is bundled in the benchmark binary via
    [ocaml-crunch], so we materialize it at runtime. *)
let materialize_contract ~name ~code =
  let path = Temp.file (sf "%s.tz" name) in
  let oc = open_out path in
  output_string oc code ;
  close_out oc ;
  path

(** Originate a Michelson contract from its source string.
    Returns the resulting [KT1...] address. *)
let originate_contract ~client ~endpoint ~src ~alias ~code ?(init = "Unit")
    ?(burn_cap = Tez.of_int 10) () =
  let prg = materialize_contract ~name:alias ~code in
  Client.originate_contract
    ~endpoint
    ~alias
    ~amount:Tez.zero
    ~src
    ~prg
    ~init
    ~burn_cap
    client

(** Sum [consumed_milligas] across all manager-operation contents and their
    internal results in the block at [level]. Uses the standard Tezos RPC
    [/chains/main/blocks/<level>/operations], exposed via the Tezlink proxy.
    Failed operations contribute the gas they consumed before failure. *)
let sum_block_consumed_milligas ~endpoint client level =
  let* operations =
    Client.RPC.call ~endpoint client
    @@ RPC.get_chain_block_operations ~block:(string_of_int level) ()
  in
  let open JSON in
  let consumed_milligas_of_result result =
    match result |-> "consumed_milligas" |> as_int_opt with
    | Some g -> g
    | None -> 0
  in
  let consumed_of_content content =
    let metadata = content |-> "metadata" in
    let main = metadata |-> "operation_result" |> consumed_milligas_of_result in
    let internals =
      match metadata |-> "internal_operation_results" |> as_list_opt with
      | None -> 0
      | Some l ->
          List.fold_left
            (fun acc internal ->
              acc + (internal |-> "result" |> consumed_milligas_of_result))
            0
            l
    in
    main + internals
  in
  let total =
    operations |> as_list
    |> List.fold_left
         (fun acc pass ->
           pass |> as_list
           |> List.fold_left
                (fun acc op ->
                  op |-> "contents" |> as_list
                  |> List.fold_left
                       (fun acc content -> acc + consumed_of_content content)
                       acc)
                acc)
         0
  in
  return total

(** Cached branch hash for direct injection. Refresh periodically with
    [refresh_branch]. *)
let get_branch ?chain ?(offset = 2) client =
  Operation_core.Manager.get_branch ?chain ~offset client

(** Forge, sign (in-process, via [Account.sign_bytes]) and inject one
    already-built manager operation [manager_ops] (a single content or a
    batch). The signed bytes are POSTed to [/injection/operation] directly
    via [RPC_core.call] (Cohttp), so this is exactly one HTTP RPC with no
    [octez-client] subprocess, no simulation and no preapply. *)
let inject_manager_op ~client ~tezlink_endpoint ~branch ~signer manager_ops =
  let* op =
    Operation_core.Manager.operation ~branch ~signer manager_ops client
  in
  let* signature = Operation_core.sign ~protocol:Protocol.Alpha op client in
  let* (`Hex signed) =
    Operation_core.hex ~protocol:Protocol.Alpha ~signature op client
  in
  let* (_ : JSON.t) =
    RPC_core.call tezlink_endpoint
    @@ RPC.post_injection_operation (Data (`String signed))
  in
  unit

(** Send [lanes] identical zero-amount calls to [receiver] from [signer].

    By default each of the [lanes] calls is injected as its own
    separately-signed single-content manager operation, sharing the same
    source and consecutive counters. This produces one signature and one
    signature check per call. The operations are injected sequentially to
    preserve counter order in the mempool.

    With [~individual_signatures:false] (driven by the [--batch] CLI flag)
    the same [lanes] contents are instead packaged as a single signed
    manager operation injected in exactly one HTTP RPC — one signature and
    one signature check for the whole batch. Either way the final source
    counter is identical, so the [confirm_counters] accounting is
    unaffected. None of the variants uses simulation, preapply, a per-call
    counter fetch, or an [octez-client] subprocess.

    The caller passes:
    - [~branch]: a recently fetched branch hash (reused across many
      operations; refresh every minute or so).
    - [~counter]: the counter for the first operation. Subsequent operations
      (or batch contents) take consecutive counters. The caller maintains
      the next counter locally.
    - [~gas_limit]: per-content gas, applied to every Transaction content.
    - [~fee]: per-content fee (defaults to 1000 mutez).
    - [~signer]: the signing [Account.key]; signing happens in-process
      via [Account.sign_bytes] (no subprocess).
    - [~tezlink_endpoint]: the [Endpoint.t] of the Tezlink RPC sub-path. *)
let batch_calls ~client ~tezlink_endpoint ~branch ~counter ~signer ~receiver
    ?entrypoint ?arg ?(fee = 1_000) ?(storage_limit = 0)
    ?(individual_signatures = not Benchmark_utils.parameters.batch) ~gas_limit
    ~lanes () =
  let make_call () =
    Operation_core.Manager.call ~dest:receiver ~amount:0 ?entrypoint ?arg ()
  in
  let make_manager_ops ~counter payloads =
    Operation_core.Manager.make_batch
      ~source:signer
      ~counter
      ~fee
      ~gas_limit
      ~storage_limit
      payloads
  in
  if individual_signatures then
    Lwt_list.iter_s
      (fun i ->
        let manager_ops =
          make_manager_ops ~counter:(counter + i) [make_call ()]
        in
        inject_manager_op ~client ~tezlink_endpoint ~branch ~signer manager_ops)
      (List.init lanes Fun.id)
  else
    let payloads = List.init lanes (fun _ -> make_call ()) in
    inject_manager_op
      ~client
      ~tezlink_endpoint
      ~branch
      ~signer
      (make_manager_ops ~counter payloads)

(** Native-tez transfer counterpart of [batch_calls]: sends [lanes]
    transfers of [amount] mutez from [signer] to the implicit account
    [dest]. Same counter / branch / signing discipline and same
    [~individual_signatures] ([--batch]) behaviour as [batch_calls]; the
    only difference is that each content is a plain [transfer] (no
    entrypoint, no Michelson argument) instead of a contract call. *)
let batch_transfers ~client ~tezlink_endpoint ~branch ~counter ~signer ~dest
    ?(fee = 1_000) ?(storage_limit = 0)
    ?(individual_signatures = not Benchmark_utils.parameters.batch) ~gas_limit
    ~amount ~lanes () =
  let make_transfer () = Operation_core.Manager.transfer ~dest ~amount () in
  let make_manager_ops ~counter payloads =
    Operation_core.Manager.make_batch
      ~source:signer
      ~counter
      ~fee
      ~gas_limit
      ~storage_limit
      payloads
  in
  if individual_signatures then
    Lwt_list.iter_s
      (fun i ->
        let manager_ops =
          make_manager_ops ~counter:(counter + i) [make_transfer ()]
        in
        inject_manager_op ~client ~tezlink_endpoint ~branch ~signer manager_ops)
      (List.init lanes Fun.id)
  else
    let payloads = List.init lanes (fun _ -> make_transfer ()) in
    inject_manager_op
      ~client
      ~tezlink_endpoint
      ~branch
      ~signer
      (make_manager_ops ~counter payloads)

(** Helper: fetch the current head level via the Tezlink endpoint. *)
let head_level ~client ~endpoint () =
  let* head =
    Client.RPC.call ~endpoint client
    @@ RPC.get_chain_block_helper_current_level ~block:"head" ()
  in
  return head.RPC.level

(** Run one real call to [receiver] with an explicit upper-bound gas limit
    (so the client's simulation does not reject it as exceeding the
    default), wait for the operation to be applied, then return the
    [consumed_milligas] reported in its receipt. *)
let estimate_consumed_milligas ~client ~endpoint ~sequencer ~giver ~receiver
    ?entrypoint ?arg ?(amount = Tez.zero) ?(gas_limit = 1_000_000_000)
    ?(max_wait_blocks = 30) ?(poll_interval_s = 0.5) ?(drive_block = false) () =
  let* pre_level = head_level ~client ~endpoint () in
  let* () =
    Client.transfer
      ~endpoint
      ~burn_cap:Tez.one
      ~fee_cap:(Tez.of_int 10)
      ~gas_limit
      ~amount
      ~giver
      ~receiver
      ?entrypoint
      ?arg
      client
  in
  (* In Manual block-production mode the caller passes [~drive_block:true]
     so we force a block to apply the simulated transfer. *)
  let* () =
    if drive_block then
      let* res = Rpc.produce_block sequencer in
      match res with
      | Ok _ -> unit
      | Error _ -> Test.fail "estimate: produce_block failed"
    else unit
  in
  let rec poll attempts last_seen =
    if attempts <= 0 then
      Test.fail
        "estimate: operation not applied within %d blocks (last head: %d)"
        max_wait_blocks
        last_seen ;
    let* head = head_level ~client ~endpoint () in
    let scan_from = last_seen + 1 in
    let scan_to = head in
    let rec scan lvl acc =
      if lvl > scan_to then return acc
      else
        let* milligas = sum_block_consumed_milligas ~endpoint client lvl in
        scan (lvl + 1) (acc + milligas)
    in
    let* total = scan scan_from 0 in
    if total > 0 then return total
    else
      let* () = Lwt_unix.sleep poll_interval_s in
      poll (attempts - 1) head
  in
  poll max_wait_blocks pre_level

(** [confirm_counters ~client ~endpoint ~expected ?timeout_s] polls each
    [(pkh, expected_counter, baseline)] entry until either every counter
    reaches its expected value, or [timeout_s] elapses. Logs how many
    operations were confirmed versus dropped in the same style as the EVM
    benchmark's end-of-iteration counters. *)
let confirm_counters ~tezlink_endpoint ?(timeout_s = 60.)
    ?(poll_interval_s = 0.5) ~expected () =
  let total_expected =
    List.fold_left (fun acc (_, e, b) -> acc + (e - b)) 0 expected
  in
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec poll () =
    let* checks =
      Lwt_list.map_s
        (fun (pkh, expected_counter, baseline) ->
          let* actual = get_counter ~tezlink_endpoint ~pkh () in
          return (pkh, expected_counter, actual, baseline))
        expected
    in
    let confirmed, dropped =
      List.fold_left
        (fun (c, d) (_, expected_counter, actual, baseline) ->
          (* Clamp per signer: a counter that already ran past its expected
             value must neither make [signer_dropped] negative nor inflate
             [confirmed] beyond this signer's share (which would let the
             [confirmed >= total_expected] early-exit mask real drops on the
             other signers). *)
          let signer_target = expected_counter - baseline in
          let signer_confirmed =
            max 0 (min signer_target (actual - baseline))
          in
          let signer_dropped = max 0 (expected_counter - actual) in
          (c + signer_confirmed, d + signer_dropped))
        (0, 0)
        checks
    in
    if confirmed >= total_expected then return (confirmed, 0)
    else if Unix.gettimeofday () >= deadline then return (confirmed, dropped)
    else
      let* () = Lwt_unix.sleep poll_interval_s in
      poll ()
  in
  let* confirmed, dropped = poll () in
  if dropped <> 0 then
    Log.report ~color:Log.Color.FG.red "%d operations DROPPED" dropped ;
  if confirmed <> 0 then Log.report "%d operations confirmed" confirmed ;
  return (confirmed, dropped)

(** Verify, for the levels recorded by the gasometer, that every manager
    operation has status [applied]. Fails the test if any non-applied
    status is found. Roughly the Michelson equivalent of the EVM
    benchmark's [check_success=true] receipt verification. *)
let check_levels_applied ~client ~endpoint levels =
  let open JSON in
  Lwt_list.iter_s
    (fun level ->
      let* operations =
        Client.RPC.call ~endpoint client
        @@ RPC.get_chain_block_operations ~block:(string_of_int level) ()
      in
      operations |> as_list
      |> List.iter (fun pass ->
             pass |> as_list
             |> List.iter (fun op ->
                    op |-> "contents" |> as_list
                    |> List.iter (fun content ->
                           let result =
                             content |-> "metadata" |-> "operation_result"
                           in
                           match result |-> "status" |> as_string_opt with
                           | Some "applied" | None -> ()
                           | Some status ->
                               Test.fail
                                 "Level %d: operation_result has status %s, \
                                  expected applied"
                                 level
                                 status))) ;
      unit)
    levels

(** Capacity summary for one gas metric (raw Michelson gas, or EVM-equivalent
    weighted total). Same shape as the EVM-side result, but reported twice —
    once per metric — by [monitor_michelson_gasometer]. *)
type capacity_summary = Benchmark_utils.monitor_result = {
  median : float;
  p90 : float;
  wall : float;
  tps : float;  (** Transactions per second over block application time. *)
  wall_tps : float;  (** Transactions per second over wall-clock time. *)
  gasometer : Benchmark_utils.gasometer;
}

type michelson_monitor_result = {
  michelson : capacity_summary;
  total : capacity_summary;
}

(** Install an [on_event] handler that records every [blueprint_application.v0]
    event into two fresh gasometers — one for the raw Michelson gas
    ([runtime_execution_gas.michelson]) and one for the EVM-equivalent
    weighted total ([execution_gas], with the Michelson part scaled by
    [michelson_to_evm_gas_multiplier]).

    A block is counted as non-trivial whenever the (scaled) total gas
    exceeds 100k. Only the total capacity is color-coded — the Michelson
    figure is in raw gas units (not scaled), so its absolute value isn't
    comparable to the EVM target. *)
let install_michelson_gasometer ~sequencer =
  let open Benchmark_utils in
  let michelson_gasometer = empty_gasometer () in
  let total_gasometer = empty_gasometer () in
  let visited_levels = ref [] in
  Evm_node.on_event sequencer (fun {name; value; timestamp; _} ->
      if name = "blueprint_application.v0" then (
        let open JSON in
        let level = value |-> "level" |> as_string |> int_of_string in
        let process_time =
          value |-> "process_time" |> as_float |> Ptime.Span.of_float_s
          |> Option.get
        in
        let michelson_gas =
          value |-> "runtime_execution_gas" |-> "michelson" |> as_string
          |> Z.of_string
        in
        let total_gas = value |-> "execution_gas" |> as_string |> Z.of_string in
        let txs = value |-> "txs_nb" |> as_int in
        let ignored = Z.lt total_gas (Z.of_int 100_000) in
        let michelson_capacity =
          capacity_mgas_sec ~gas:michelson_gas ~time:process_time
        in
        let total_capacity =
          capacity_mgas_sec ~gas:total_gas ~time:process_time
        in
        Log.info
          "Level %d: michelson %a gas / total %a gas in %a: michelson %a, \
           total %a"
          level
          Z.pp_print
          michelson_gas
          Z.pp_print
          total_gas
          Ptime.Span.pp
          process_time
          (fun fmt (i, c) ->
            if i then Format.pp_print_string fmt "(ignored)"
            else pp_capacity fmt c)
          (ignored, michelson_capacity)
          (fun fmt (i, c) ->
            if i then Format.pp_print_string fmt "(ignored)"
            else pp_capacity fmt c)
          (ignored, total_capacity) ;
        if ignored then warn_ignored_txs ~level ~txs
        else
          let michelson_info =
            {
              level;
              gas = michelson_gas;
              timestamp;
              gas_per_sec = michelson_capacity;
              txs;
            }
          in
          let total_info =
            {
              level;
              gas = total_gas;
              timestamp;
              gas_per_sec = total_capacity;
              txs;
            }
          in
          record_datapoint michelson_gasometer michelson_info ~process_time ;
          record_datapoint total_gasometer total_info ~process_time ;
          visited_levels := level :: !visited_levels ;
          let running_median g =
            let capacities =
              List.map
                (fun ({gas_per_sec; _} : gas_info) -> gas_per_sec)
                g.datapoints
            in
            get_capacity_percentile 50. capacities
          in
          let michelson_median = running_median michelson_gasometer in
          let total_median = running_median total_gasometer in
          Log.info
            ~prefix:"Current median michelson capacity"
            "%a"
            pp_capacity
            michelson_median ;
          Log.info
            ~color:(capacity_color ~bg:false total_median)
            ~prefix:"Current median total capacity"
            "%a"
            pp_capacity
            total_median)) ;
  (michelson_gasometer, total_gasometer, visited_levels)

(** Run [f], capturing every [blueprint_application.v0] event emitted by
    [sequencer] into a pair of capacity reports — one for raw Michelson gas
    and one for the EVM-equivalent weighted total.

    Mirrors the EVM-side [Benchmark_utils.monitor_gasometer]. *)
let monitor_michelson_gasometer ~sequencer f =
  let open Benchmark_utils in
  let michelson_gasometer, total_gasometer, visited_levels =
    install_michelson_gasometer ~sequencer
  in
  let start_time = Ptime_clock.now () in
  let* () = f () in
  let end_time = Ptime_clock.now () in
  let wall_time = Ptime.diff end_time start_time in
  let michelson = summarize_gasometer michelson_gasometer ~wall_time in
  let total = summarize_gasometer total_gasometer ~wall_time in
  Log.report
    "%d non-trivial block(s); michelson %a gas, total %a gas, %d transactions"
    (List.length michelson_gasometer.datapoints)
    Z.pp_print
    michelson_gasometer.total_gas
    Z.pp_print
    total_gasometer.total_gas
    total_gasometer.total_txs ;
  let log_michelson prefix c = Log.report ~prefix "%a" pp_capacity c in
  let log_total prefix c =
    let color = capacity_color ~bg:true c in
    Log.report ~color ~prefix "%a" pp_capacity c
  in
  log_michelson "Median michelson capacity" michelson.median ;
  log_total "Median total capacity" total.median ;
  log_michelson "90th percentile michelson capacity" michelson.p90 ;
  log_total "90th percentile total capacity" total.p90 ;
  log_michelson "Wall-clock michelson capacity" michelson.wall ;
  log_total "Wall-clock total capacity" total.wall ;
  Log.report
    ~prefix:"Transaction throughput"
    "%a application / %a wall clock"
    pp_tps
    total.tps
    pp_tps
    total.wall_tps ;
  return ({michelson; total}, List.rev !visited_levels)
