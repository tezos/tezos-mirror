(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Generic Michelson-runtime capacity benchmark.

    Factorises the procedure shared by all the single-contract Michelson
    capacity scenarios (stress_INT_nat, stress_DIPN, and the generic
    [michelson_call]). It works for any contract whose storage is [unit]
    and whose default entrypoint takes [unit]: such a contract can be
    called with a plain zero-amount, [Unit]-parameter transaction, which is
    exactly what [Tezlink_utils.batch_calls] injects.

    The flow is: originate the contract, estimate its per-call gas with one
    real call, then drive the standard stepped iteration loop ([lanes]
    signed operations per signer per block, batched or not depending on
    [--batch]) while a gasometer records per-block capacity. *)

open Benchmark_utils

(* Spin up a standalone EVM node in regular sandbox mode with the Tezos
   (Michelson) runtime enabled, and return both the node and a Client whose
   default endpoint is the node's [/tezlink] sub-path. *)
let init_michelson_sandbox ~tez_bootstrap_accounts =
  let* sequencer =
    Test_helpers.init_sequencer_sandbox
      ~tez_bootstrap_accounts
      ~with_runtimes:[Tezos]
        (* The stress contracts consume well above the L1 default (~1M gas);
           bump the per-tx cap so simulation and execution can run them. A
           lighter contract simply stays well under this generous cap. *)
      ~maximum_gas_per_transaction:1_000_000_000L
        (* Raise the Michelson per-block cap so that a single batch packing
           many full-gas calls survives the OCaml-side batch-sum check in
           tezlink_prevalidation.ml. The default 3M (= per-op cap) only fits
           one full call per block. *)
      ~michelson_hard_gas_limit_per_block:1_000_000_000
      ~minimum_base_fee_per_gas:Wei.zero
      ~tx_queue_tx_per_addr_limit:100_000
      ()
  in
  let* client =
    Client.init ~endpoint:(Tezlink_utils.tezlink_endpoint sequencer) ()
  in
  return (sequencer, client)

(** [run ~scenario_name ~code ()] is the test body shared by every
    single-contract Michelson capacity benchmark.

    - [scenario_name] is used as the origination alias and in log lines.
    - [code] is a thunk returning the contract's Michelson source (read
      lazily, at test time rather than registration time). The contract
      must have storage [unit] and a default entrypoint taking [unit]. *)
let run ~scenario_name ~code () =
  (* [Test_helpers.init_sequencer_sandbox] hardcodes
     [time_between_blocks = Nothing], so block production is Manual: the
     driver below drives every block via [Rpc.produce_block]. Auto mode is
     rejected up front rather than silently behaving as Manual. *)
  (match parameters.time_between_blocks with
  | `Manual _ -> ()
  | `Auto _ ->
      Test.fail
        "%s runs in Manual block-production mode only (use -T manual:T)"
        scenario_name) ;
  let nb_signers = Option.value parameters.accounts ~default:5 in
  let bootstrap1 = Constant.bootstrap1 in
  (* Generate one fresh signer per [--accounts]. Passing them to
     [init_sequencer_sandbox] via [~tez_bootstrap_accounts] makes them arrive
     pre-funded and pre-revealed at genesis, which sidesteps the 5-account cap
     of [Account.Bootstrap.keys] and lets [--accounts] scale freely. The lane
     signers sign in-process ([Account.sign_bytes] inside [batch_calls]), so —
     unlike [bootstrap1], which originates and runs the gas estimate via
     [octez-client] — they never need to be imported into the [Client]. *)
  let signers =
    List.init nb_signers (fun i ->
        Account.generate_new_key
          ~algo:Tezos_crypto.Signature.Ed25519
          ~alias:(sf "michelson_signer_%d" i))
  in

  let* sequencer, client =
    init_michelson_sandbox ~tez_bootstrap_accounts:(bootstrap1 :: signers)
  in
  let tezlink_endpoint = Tezlink_utils.tezlink_foreign_endpoint sequencer in
  let endpoint = Client.Foreign_endpoint tezlink_endpoint in

  (* Make sure at least one block past genesis exists. *)
  let* _ = Rpc.produce_block sequencer in

  Log.report "Originating %s contract" scenario_name ;
  let code = code () in
  let* contract =
    Tezlink_utils.originate_contract
      ~client
      ~endpoint
      ~src:bootstrap1.alias
      ~alias:scenario_name
      ~code
      ~init:"Unit"
      ()
  in
  Log.info "Originated %s at %s (forcing block to apply)" scenario_name contract ;
  let* res = Rpc.produce_block sequencer in
  (match res with
  | Ok _ -> ()
  | Error _ -> Test.fail "produce_block after origination failed") ;
  let* () = Lwt_unix.sleep 0.5 in

  Log.report "Estimating gas for one call to %s" contract ;
  let* milligas =
    Tezlink_utils.estimate_consumed_milligas
      ~client
      ~endpoint
      ~sequencer
      ~giver:bootstrap1.alias
      ~receiver:contract
      ~drive_block:true
      ()
  in
  let gas = (milligas + 999) / 1000 in
  let gas_limit = max (gas + 100) (gas * 105 / 100) in
  Log.info
    "Per-call cost: %d milligas (%d gas); using gas_limit=%d"
    milligas
    gas
    gas_limit ;

  let lanes = parameters.lanes in
  let iterations = parameters.iterations in
  Log.report
    "Running %d iterations × lanes=%d across %d signer(s)"
    iterations
    lanes
    (List.length signers) ;

  let* baselines =
    Lwt_list.map_s
      (fun (signer : Account.key) ->
        let* c =
          Tezlink_utils.get_counter
            ~tezlink_endpoint
            ~pkh:signer.public_key_hash
            ()
        in
        return (signer, c))
      signers
  in

  let expected =
    List.map
      (fun ((signer : Account.key), baseline) ->
        (signer.public_key_hash, baseline + (iterations * lanes), baseline))
      baselines
  in
  (* Snapshot a recent block hash to reuse as the operation [branch] across
     all batches. A branch is valid for many blocks; we refresh periodically
     inside the lane loop to be safe under long runs. *)
  let* initial_branch = Tezlink_utils.get_branch client in
  let branch_ref = ref initial_branch in

  (* Level of the first benchmark block, captured before the iteration loop
     so [check_levels_applied] can verify every block the loop produces,
     regardless of how much gas each one consumed. *)
  let* first_benchmark_level = Tezlink_utils.head_level ~client ~endpoint () in

  let* _result, _visited_levels =
    Tezlink_utils.monitor_michelson_gasometer ~sequencer @@ fun () ->
    (* Iterations are stepped: all signers inject their operations for
       iteration N in parallel, then we drive the block carrying them and
       move on to N+1. One block carries the operations from every signer. *)
    Lwt_list.iter_s
      (fun iteration ->
        Log.report "Iteration %d (%d signer(s))" iteration (List.length signers) ;
        (* Refresh the branch every 20 iterations. *)
        let* () =
          if iteration > 1 && iteration mod 20 = 0 then (
            let* b = Tezlink_utils.get_branch client in
            branch_ref := b ;
            unit)
          else unit
        in
        (* Read each signer's actual on-chain counter right before injecting,
           instead of deriving it from the static baseline. A dropped
           operation then does not cascade into future-counter rejections for
           the rest of that signer's iterations: the next iteration simply
           resumes from wherever the signer actually is. *)
        let* iteration_expected =
          Lwt_list.map_p
            (fun (signer : Account.key) ->
              let* current =
                Tezlink_utils.get_counter
                  ~tezlink_endpoint
                  ~pkh:signer.public_key_hash
                  ()
              in
              let* () =
                Tezlink_utils.batch_calls
                  ~client
                  ~tezlink_endpoint
                  ~branch:!branch_ref
                  ~counter:(current + 1)
                  ~signer
                  ~receiver:contract
                  ~gas_limit
                    (* Fee must cover per-gas base fee; sandbox + Tezos runtime
                       uses ~0.01 mutez/gas, so ~10k mutez per ~1M-gas call. *)
                  ~fee:((gas_limit / 100) + 1_000)
                  ~lanes
                  ()
              in
              return (signer.public_key_hash, current + lanes, current))
            signers
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
  (* Final confirmation that all expected counters were reached. *)
  let* _confirmed, _dropped =
    Tezlink_utils.confirm_counters ~tezlink_endpoint ~expected ()
  in
  if parameters.check_success then
    (* Verify every block the loop produced, from [first_benchmark_level + 1]
       up to the current head, rather than only the blocks the gasometer
       flagged as non-trivial — otherwise the check passes vacuously on a
       contract whose blocks stay under the gasometer's gas threshold. *)
    let* last_benchmark_level = Tezlink_utils.head_level ~client ~endpoint () in
    let levels =
      List.init (last_benchmark_level - first_benchmark_level) (fun i ->
          first_benchmark_level + 1 + i)
    in
    Tezlink_utils.check_levels_applied ~client ~endpoint levels
  else unit

(** [register_benchmark ~__FILE__ ~name ?extra_tags ~code ()] registers a
    tezt test running {!run} for the contract returned by the [code] thunk.

    [~__FILE__] is taken from the registering scenario file (not this
    module) so that the [--file <scenario>.ml] selector keeps working.
    [extra_tags] are appended to the common Michelson-capacity tag set.
    [title] overrides the default title (which is derived from [name]); use
    it to avoid clashing with another scenario that shares the same [name]
    (tezt requires unique titles). *)
let register_benchmark ~__FILE__ ~name ?title ?(extra_tags = []) ~code () =
  Test.register
    ~__FILE__
    ~title:
      (Option.value
         title
         ~default:(sf "Capacity of Michelson runtime (%s)" name))
    ~tags:
      (["benchmark"; "ci_disabled"; "michelson"; "tezlink"; "capacity"]
      @ extra_tags)
    ~uses_node:false
    ~uses:
      [
        Constant.octez_evm_node;
        Constant.octez_client;
        Constant.WASM.evm_kernel;
        Constant.smart_rollup_installer;
      ]
  @@ fun () -> run ~scenario_name:name ~code ()
