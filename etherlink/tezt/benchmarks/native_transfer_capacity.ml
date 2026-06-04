(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    EVM vs Michelson runtime capacity benchmark (native
                 token transfers)
   Invocation:   dune exec etherlink/tezt/benchmarks/main.exe -- \
                   --file native_transfer_capacity.ml [opts]
   Subject:      Compares the capacity of the two runtimes for *native*
                 token transfers — plain value transfers between
                 externally-owned accounts on the EVM runtime, and
                 native tez transfers between implicit accounts on the
                 Michelson runtime — and reports, for each runtime, both
                 the transaction throughput (TPS) and the gas throughput
                 (MGas/s).

                 No contracts are involved (this is the native-transfer
                 counterpart of [cross_runtime_capacity.ml], which
                 compares ERC-20 / FA token transfers).

                 The two runtimes are measured in two SEPARATE phases of
                 the SAME sandbox so that each TPS / MGas/s figure
                 reflects one runtime in isolation (the per-block
                 [txs_nb] event field is a single total and cannot be
                 split per runtime within a mixed block).

                 Both phases run on a single standalone EVM node in
                 sandbox mode with the Tezos (Michelson) runtime enabled.
                 No L1 node, no rollup node, no protocol activation.

                 Both runtimes apply the same number of transfers per
                 block: [-A]/[--accounts] sets the number of transfer
                 lanes (EVM senders and Michelson signers) and
                 [--batch-size] the per-lane transfers per block (packed
                 in one signed operation on the Michelson side, sent as
                 [lanes] sequential-nonce transactions per sender on
                 the EVM side), so each block carries [accounts ×
                 lanes] transfers on both runtimes. Both must be
                 large enough that every measured block exceeds the
                 gasometer's non-trivial threshold (100k gas).
*)

(* [Tezt_tezos.Account] (the L1 / Michelson account API) is shadowed by
   [Floodgate_lib.Account] (the EVM-side keypair API) below. Alias the
   former under [Tz_account] so we can still reach
   [generate_new_key]/[key]/[secret_key] for the Michelson lane signers. *)
module Tz_account = Tezt_tezos.Account
open Tezt_etherlink
open Etherlink_benchmark_lib
open Benchmark_utils
open Floodgate_lib

(* Amount moved per transfer, in the smallest unit of each runtime (wei
   on the EVM side, mutez on the Michelson side). Kept tiny — the
   capacity metric measures throughput, not token movement. *)
let transfer_value = 1

(* Spin up a sandbox running BOTH the EVM and the Michelson runtimes.
   Both bootstrap-account lists are passed to the kernel installer so
   that the EVM senders AND the Michelson lane signers are pre-funded
   and pre-revealed at genesis. Mirrors [cross_runtime_capacity.ml]. *)
let init_dual_sandbox ~tez_bootstrap_accounts ~eth_bootstrap_accounts =
  let* sequencer =
    Test_helpers.init_sequencer_sandbox
      ~with_runtimes:[Tezos]
      ~tez_bootstrap_accounts
      ~eth_bootstrap_accounts
      ~maximum_gas_per_transaction:1_000_000_000L
      ~michelson_hard_gas_limit_per_block:1_000_000_000
      ~minimum_base_fee_per_gas:Wei.one
      ~tx_queue_tx_per_addr_limit:100_000
      ()
  in
  let* client =
    Client.init ~endpoint:(Tezlink_utils.tezlink_endpoint sequencer) ()
  in
  return (sequencer, client)

(* Start the Floodgate-side machinery (Tx_queue + blueprint follower +
   tx_queue_beacon) so that the EVM-side helper [Benchmark_utils.call]
   works. Mirrors [cross_runtime_capacity.start_evm_pipeline]. Returns
   the [Network_info] handle plus cancellable promises for the two
   background loops. *)
let start_evm_pipeline ~sequencer ~accounts =
  let* floodgate_accounts = floodgate_accounts sequencer accounts in
  let endpoint = Evm_node.endpoint sequencer |> Uri.of_string in
  let*? infos =
    Network_info.fetch ~rpc_endpoint:endpoint ~base_fee_factor:100.
  in
  let max_size = 999_999 in
  let tx_per_addr_limit = Int64.of_int 999_999 in
  let max_transaction_batch_length = Some 300 in
  let max_lifespan_s = int_of_float parameters.timeout in
  let tx_queue_config : Evm_node_config.Configuration.tx_queue =
    {max_size; max_transaction_batch_length; max_lifespan_s; tx_per_addr_limit}
  in
  let*? () =
    Evm_node_lib_dev.Tx_queue.start
      ~config:tx_queue_config
      ~keep_alive:true
      ~timeout:parameters.timeout
      ~start_injector_worker:true
      ()
  in
  let* () = Floodgate_events.is_ready infos.chain_id infos.base_fee_per_gas in
  let follower =
    Floodgate.start_blueprint_follower
      ~relay_endpoint:endpoint
      ~rpc_endpoint:endpoint
      ()
  in
  let tx_queue_beacon =
    Evm_node_lib_dev.Tx_queue.tx_queue_beacon
      ~evm_node_endpoint:(Rpc endpoint)
      ~tick_interval:0.1
  in
  return (floodgate_accounts, infos, follower, tx_queue_beacon)

(* Estimate the gas limit of one native EVM value transfer to an
   externally-owned account (no calldata): ~21000 gas. *)
let evm_native_gas_limit ~infos ~rpc_node ~accounts =
  let sender = accounts.(0) in
  let dest = accounts.(1 mod Array.length accounts) in
  let rpc_endpoint = Evm_node.endpoint rpc_node |> Uri.of_string in
  let*? gas_limit =
    Network_info.get_gas_limit
      ~rpc_endpoint
      ~base_fee_per_gas:infos.Network_info.base_fee_per_gas
      ~from:(Account.address_et sender)
      ~to_:(Account.address_et dest)
      ~value:(Z.of_int transfer_value)
      ()
  in
  return gas_limit

(* One native EVM value transfer from [sender] to the EOA [dest]: no
   [~name], so [call] sends a plain transfer (empty calldata) rather
   than a contract call. *)
let evm_transfer_step ~infos ~rpc_node ~gas_limit ~nonce ~(sender : Account.t)
    ~value ~(dest : Account.t) =
  let* _ =
    call
      infos
      rpc_node
      (Account.address dest :> string)
      sender
      ~gas_limit
      ~nonce
      ~value:(Z.of_int value)
      []
      []
  in
  unit

(* Print the per-runtime capacities side by side. [tps] / [wall_tps] are
   the transaction throughput over block-application time and wall-clock
   time respectively; the MGas/s triples are median / p90 / wall-clock
   gas throughput. The Michelson rows show both the raw Michelson gas
   and the EVM-equivalent weighted gas (directly comparable to the EVM
   row's units). *)
let log_comparison ~(evm : monitor_result)
    ~(michelson : Tezlink_utils.michelson_monitor_result) =
  Log.report "==== Native token transfer capacity: EVM vs Michelson ====" ;
  Log.report
    "Runtime                TPS (app / wall)      MGas/s (median / p90 / wall)" ;
  Log.report
    "EVM                    %8.1f / %8.1f   %.3f / %.3f / %.3f"
    evm.tps
    evm.wall_tps
    evm.median
    evm.p90
    evm.wall ;
  Log.report
    "Michelson (raw gas)    %8.1f / %8.1f   %.3f / %.3f / %.3f"
    michelson.michelson.tps
    michelson.michelson.wall_tps
    michelson.michelson.median
    michelson.michelson.p90
    michelson.michelson.wall ;
  Log.report
    "Michelson (EVM-equiv.)            (same)        %.3f / %.3f / %.3f"
    michelson.total.median
    michelson.total.p90
    michelson.total.wall

let test_capacity () =
  Test.register
    ~__FILE__
    ~title:"Capacity of native transfers: EVM vs Michelson"
    ~tags:
      [
        "benchmark";
        "ci_disabled";
        "michelson";
        "tezlink";
        "evm";
        "capacity";
        "native";
        "transfer";
      ]
    ~uses_node:false
    ~uses:
      [
        Constant.octez_evm_node;
        Constant.octez_client;
        Constant.WASM.evm_kernel;
        Constant.smart_rollup_installer;
      ]
  @@ fun () ->
  (match parameters.time_between_blocks with
  | `Manual _ -> ()
  | `Auto _ ->
      Test.fail
        "native_transfer_capacity runs in Manual block-production mode only \
         (use -T manual:T)") ;
  let nb = Option.value parameters.accounts ~default:30 in
  let iterations = parameters.iterations in
  let lanes = parameters.lanes in

  (* Fresh Michelson lane signers — pre-funded + pre-revealed at genesis
     (same pattern as fa_transfer.ml). *)
  let tez_lanes =
    List.init nb (fun i ->
        Tz_account.generate_new_key
          ~algo:Tezos_crypto.Signature.Ed25519
          ~alias:(sf "nt_tez_lane_%d" i))
  in
  let lanes_arr = Array.of_list tez_lanes in
  let bootstrap1 = Constant.bootstrap1 in

  (* Fresh EVM senders — pre-funded at genesis. *)
  let eth_accounts = Eth_account.accounts nb in
  let eth_bootstrap_accounts =
    Array.to_list eth_accounts |> List.map (fun a -> a.Eth_account.address)
  in

  let* sequencer, client =
    init_dual_sandbox
      ~tez_bootstrap_accounts:(bootstrap1 :: tez_lanes)
      ~eth_bootstrap_accounts
  in
  let tezlink_endpoint = Tezlink_utils.tezlink_foreign_endpoint sequencer in
  let endpoint = Client.Foreign_endpoint tezlink_endpoint in

  (* Make sure at least one block past genesis exists. *)
  let* _ = Rpc.produce_block sequencer in

  (* Register every lane signer's secret key with the client (needed by
     the one-shot Michelson gas-estimate path; the hot path signs
     in-process). *)
  let* () =
    Lwt_list.iter_s
      (fun (s : Tz_account.key) ->
        Client.import_secret_key client s.secret_key ~alias:s.alias)
      tez_lanes
  in

  Log.report "Starting EVM pipeline (Tx_queue + blueprint follower)" ;
  let* evm_accounts, infos, follower, tx_queue_beacon =
    start_evm_pipeline ~sequencer ~accounts:eth_accounts
  in

  (* ===================== EVM phase ===================== *)
  let* evm_gas_limit =
    evm_native_gas_limit ~infos ~rpc_node:sequencer ~accounts:evm_accounts
  in
  Log.info "EVM native transfer gas limit: %a" Z.pp_print evm_gas_limit ;
  Log.report
    "[EVM] Running %d iterations × lanes=%d across %d sender(s): %d native \
     transfer(s) per block"
    iterations
    lanes
    nb
    (nb * lanes) ;
  let* evm_result =
    with_collect_host_function_metrics sequencer @@ fun () ->
    monitor_gasometer sequencer @@ fun () ->
    Lwt_list.iter_s
      (fun iteration ->
        Log.report "[EVM] Iteration %d" iteration ;
        (* Mirror the Michelson side: each sender issues [lanes]
           transfers per block (sequential nonces from its current
           nonce), so both runtimes apply [nb × lanes] native
           transfers per block. *)
        let step_f () =
          Lwt_list.iteri_p
            (fun idx (sender : Account.t) ->
              let dest = evm_accounts.((idx + 1) mod nb) in
              let base_nonce = sender.nonce in
              Lwt_list.iter_p
                (fun i ->
                  let nonce = Z.add base_nonce (Z.of_int i) in
                  evm_transfer_step
                    ~infos
                    ~rpc_node:sequencer
                    ~gas_limit:evm_gas_limit
                    ~nonce
                    ~sender
                    ~value:transfer_value
                    ~dest)
                (List.init lanes Fun.id))
            (Array.to_list evm_accounts)
        in
        wait_for_application sequencer step_f)
      (List.init iterations succ)
  in

  (* Tear down the EVM background loops before the Michelson phase, which
     drives block production directly via [Rpc.produce_block]. *)
  Lwt.cancel follower ;
  Lwt.cancel tx_queue_beacon ;
  let*? () = Evm_node_lib_dev.Tx_queue.shutdown () in

  (* ===================== Michelson phase ===================== *)
  (* One-shot gas estimate for a native tez transfer between two lanes. *)
  let (estimate_signer : Tz_account.key) = List.hd tez_lanes in
  let estimate_to = lanes_arr.(1 mod nb) in
  let* milligas =
    Tezlink_utils.estimate_consumed_milligas
      ~client
      ~endpoint
      ~sequencer
      ~giver:estimate_signer.alias
      ~receiver:estimate_to.Tz_account.public_key_hash
      ~amount:(Tez.of_mutez_int transfer_value)
      ~drive_block:true
      ()
  in
  let mich_gas = (milligas + 999) / 1000 in
  let mich_gas_limit = max (mich_gas + 100) (mich_gas * 105 / 100) in
  Log.info
    "Michelson native transfer cost: %d milligas (%d gas); using gas_limit=%d"
    milligas
    mich_gas
    mich_gas_limit ;
  Log.report
    "[Michelson] Running %d iterations × lanes=%d across %d lane(s): %d native \
     transfer(s) per block"
    iterations
    lanes
    nb
    (nb * lanes) ;

  (* Snapshot per-lane counters after the gas-estimate call (which
     advanced [estimate_signer]'s counter). *)
  let* baselines =
    Lwt_list.map_s
      (fun (signer : Tz_account.key) ->
        let* c =
          Tezlink_utils.get_counter
            ~tezlink_endpoint
            ~pkh:signer.public_key_hash
            ()
        in
        return (signer, c))
      tez_lanes
  in
  let expected =
    List.map
      (fun ((signer : Tz_account.key), baseline) ->
        (signer.public_key_hash, baseline + (iterations * lanes), baseline))
      baselines
  in

  let* initial_branch = Tezlink_utils.get_branch client in
  let branch_ref = ref initial_branch in

  let* michelson_result, _visited_levels =
    with_collect_host_function_metrics sequencer @@ fun () ->
    Tezlink_utils.monitor_michelson_gasometer ~sequencer @@ fun () ->
    Lwt_list.iter_s
      (fun iteration ->
        Log.report "[Michelson] Iteration %d" iteration ;
        let* () =
          if iteration > 1 && iteration mod 20 = 0 then (
            let* b = Tezlink_utils.get_branch client in
            branch_ref := b ;
            unit)
          else unit
        in
        (* Read each signer's actual on-chain counter right before injecting,
           instead of deriving it from the static baseline, so a dropped
           operation does not cascade into future-counter rejections for the
           rest of that signer's iterations. *)
        let* iteration_expected =
          Lwt_list.mapi_p
            (fun idx ((signer : Tz_account.key), _baseline) ->
              let dest = lanes_arr.((idx + 1) mod nb) in
              let* current =
                Tezlink_utils.get_counter
                  ~tezlink_endpoint
                  ~pkh:signer.public_key_hash
                  ()
              in
              let* () =
                Tezlink_utils.batch_transfers
                  ~client
                  ~tezlink_endpoint
                  ~branch:!branch_ref
                  ~counter:(current + 1)
                  ~signer
                  ~dest
                  ~gas_limit:mich_gas_limit
                  ~amount:transfer_value
                    (* Fee must cover per-gas base fee; sandbox + Tezos runtime
                       uses ~0.01 mutez/gas, so ~10k mutez per ~1M-gas call. *)
                  ~fee:((mich_gas_limit / 100) + 1_000)
                  ~lanes
                  ()
              in
              return (signer.public_key_hash, current + lanes, current))
            baselines
        in
        let* res = Rpc.produce_block sequencer in
        (match res with
        | Ok _ -> ()
        | Error _ -> Test.fail "produce_block failed at iteration %d" iteration) ;
        let* _ =
          Tezlink_utils.confirm_counters
            ~tezlink_endpoint
            ~expected:iteration_expected
            ()
        in
        unit)
      (List.init iterations succ)
  in
  let* _confirmed, _dropped =
    Tezlink_utils.confirm_counters ~tezlink_endpoint ~expected ()
  in

  log_comparison ~evm:evm_result ~michelson:michelson_result ;
  unit

let register () = test_capacity ()
