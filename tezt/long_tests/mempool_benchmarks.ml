(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component: Prevalidator
   Invocation: dune exec tezt/long_tests/main.exe -- --file mempool_benchmarks.ml
   Subject: Benchmarking the prevalidator classification time
   Prerequisite: To run this benchmark, it is needed to apply some patches on
   the mempool's code in order to force the check signature while reclassifying
   operations in the mempool. In addition to that, it is useful to introduce
   some environment variable, allowing tweaking some parameters:

   - TEZOS_DOMAINS: override the default number of domains used,
   - TEZOS_USE_NEW_CLASSIFY: force the use of classification,
   - TEZOS_OPERATION_BATCH_SIZE: tweak the size of the batches used while
     classifying pending operations in the mempool.

   To do so, one should apply the patch located at
   devtools/patchs/long_test_mempool_benchmark.patch.

   In addation to that, as the results relies on PPX profiling, it is needed to
   compile the node using the `TEZOS_PPX_PROFILER=t` flag.
*)

let team = Team.layer1

(** {2 Test parameters} *)
let manager_kinds = [`Transfer]

let manager_kind_to_string = function `Transfer -> "transfer"

let number_of_operation_from_manager_kind = function `Transfer -> 1_000

let protocols = Protocol.[Alpha]

let blocks_per_cycle = 4

let consensus_rights_delay = 1

(** We override the default [bake_for] command to wait on the next level
    incremented after the new block. If [wait_for_flush] is set we wait on a
    [flush] event from the mempool because the [set_head] event used by the
    default [bake_for] functions happens before a flush of the mempool. For
    mempool tests, we generally prefer to ensure that a [flush] did happen than
    a [set_head].

   Optionally, we can decide whether the block should be baked without taking
   the operations of the mempool.

   This function returns the level of the node after the bake. *)
let bake_for ?(keys = []) ?(wait_for_flush = false) ~empty ~protocol node
    ~benchmarked_nodes client ~benchmarked_clients =
  let flush_waiter =
    if wait_for_flush then Node.wait_for_request ~request:`Flush node else unit
  in
  let* () =
    Lwt_list.iter_s
      (fun node ->
        Client.attest_for ~endpoint:(Node node) ~key:keys ~force:true client)
      benchmarked_nodes
  in
  (* bake on node *)
  let* level = Client.level client in
  let* () =
    if empty then
      let empty_mempool_file = Client.empty_mempool_file () in
      Client.bake_for
        ~mempool:empty_mempool_file
        ~ignore_node_mempool:true
        ~protocol
        ~keys
        client
    else Client.bake_for ~keys client
  in
  let* () = flush_waiter in
  let* level = Node.wait_for_level node (succ level) in
  Log.info "Connect both nodes" ;
  (* Connect both nodes to force synchronisation and flush on private_node *)
  let* () =
    Lwt_list.iter_s
      (fun (benchmarked_node, client) ->
        let* () = Client.Admin.trust_address client ~peer:node in
        let* () =
          Client.Admin.connect_address
            ~endpoint:(Node benchmarked_node)
            ~peer:node
            client
        in
        Client.Admin.untrust_address client ~peer:node)
      (List.combine benchmarked_nodes benchmarked_clients)
  in
  Lwt.return level

(** Initialize a node with specific level for event sections, with a timeout for
    [operations_request_timeout] longer than the default one. *)
let init_node () =
  let node = Node.create Node.[Synchronisation_threshold 0; Connections 2] in
  let* () = Node.config_init node [] in
  let* () =
    Node.Config_file.update
      node
      (Node.Config_file.set_prevalidator ~operations_request_timeout:100.)
  in
  let* () =
    Node.run
      ~env:
        (String_map.of_list
           [("PROFILING", "*->debug"); ("PROFILING_BACKEND", "txt")])
      ~event_sections_levels:
        [("prevalidator", `Debug); ("validator", `Debug); ("block", `Debug)]
      node
      []
  in
  let* () = Node.wait_for_ready node in
  let client = Client.create_with_mode (Client (Some (Node node), None)) in
  return (node, client)

let init_node_client_with_protocol ?(sig_algo = "ed25519")
    number_of_additional_bootstrap protocol =
  let parameter_file = Protocol.parameter_file protocol in
  let* node, client = init_node () in
  (* Add additional_bootstrap_account to the client *)
  Account.write Constant.all_secret_keys ~base_dir:(Client.base_dir client) ;
  let* additional_bootstrap_account =
    Client.stresstest_gen_keys ~sig_algo number_of_additional_bootstrap client
  in
  let additional_bootstrap_accounts =
    List.map
      (fun x ->
        ( x,
          Some
            {
              Protocol.balance = Some 500_000_000_000_000;
              consensus_key = None;
              delegate = None;
            },
          true ))
      additional_bootstrap_account
  in
  let parameters =
    (* we update parameters for faster testing: no need to wait
       5 cycles for the consensus key to activate. *)
    [
      (["minimal_block_delay"], `String "3");
      (["consensus_threshold_size"], `Int 4_000);
      (["consensus_committee_size"], `Int 7_000);
      (["blocks_per_cycle"], `Int blocks_per_cycle);
      (["nonce_revelation_threshold"], `Int 2);
      (["consensus_rights_delay"], `Int consensus_rights_delay);
      (["cache_sampler_state_cycles"], `Int (consensus_rights_delay + 3));
      (["cache_stake_distribution_cycles"], `Int (consensus_rights_delay + 3));
    ]
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~additional_bootstrap_accounts
      ~base:(Either.left parameter_file)
      parameters
  in
  let* () =
    Client.activate_protocol_and_wait ~parameter_file ~protocol ~node client
  in
  let* _ = Node.wait_for_level node 1 in
  return (client, node, additional_bootstrap_account)

let craft_transfer ?(gas_limit = 3_000) ~source ~dest ~counter ~branch ~amount
    client =
  let op =
    Operation.Manager.make ~source ~counter ~gas_limit
    @@ Operation.Manager.transfer ~dest ~amount ()
  in
  Operation.Manager.operation ~branch ~signer:source [op] client

let craft_reveal ?(gas_limit = 10_000) ?proof ~source ~counter ~branch client =
  let op =
    Operation.Manager.make ~source ~counter ~gas_limit
    @@ Operation.Manager.reveal ?proof source ()
  in
  Operation.Manager.operation ~branch ~signer:source [op] client

let rec init_addrs node client protocol (ops : (Account.key * Account.key) list)
    =
  (* We need to drastically limit the number of ops here, as reveals are quite
     costly. We set it to 200 *)
  let now, next = Tezos_stdlib.TzList.rev_split_n 200 ops in
  let counter = 1 in
  let* branch =
    Client.RPC.call client @@ RPC.get_chain_block_hash ~block:"head" ()
  in
  let* ops =
    Lwt_list.map_s
      (fun (ba, k) ->
        craft_transfer
          ~source:ba
          ~dest:k
          ~counter
          ~branch
          ~amount:1_000_000
          client)
      now
  in
  let* _op_hashes =
    Operation.inject_operations
      ~use_tmp_file:true
      ~protocol
      ~force:false
      ops
      client
  in
  let* _ =
    bake_for
      ~wait_for_flush:true
      ~empty:false
      ~protocol
      node
      ~benchmarked_nodes:[]
      ~benchmarked_clients:[]
      client
  in
  if next = [] then unit else init_addrs node client protocol next

let rec reveal_addrs node client protocol (ops : (Account.key * int) list) =
  (* We need to drastically limit the number of ops here, as reveals are quite
     costly. We set it to 100 *)
  let now, next = Tezos_stdlib.TzList.rev_split_n 100 ops in
  let* branch =
    Client.RPC.call client @@ RPC.get_chain_block_hash ~block:"head" ()
  in
  let* ops =
    Lwt_list.map_s
      (fun (k, counter) ->
        (* A proof is generated only for BLS signatures. *)
        let proof = Operation.Manager.create_proof_of_possession ~signer:k in
        craft_reveal ?proof ~source:k ~counter ~branch client)
      now
  in
  let* _op_hashes =
    Operation.inject_operations
      ~use_tmp_file:true
      ~protocol
      ~force:false
      ops
      client
  in
  let* _ =
    bake_for
      ~wait_for_flush:true
      ~empty:false
      ~protocol
      node
      ~benchmarked_nodes:[]
      ~benchmarked_clients:[]
      client
  in
  if next = [] then unit else reveal_addrs node client protocol next

let bench_txs ?(count = 10) node client protocol
    (accounts : (Account.key * (Account.key * int)) list) =
  let* () =
    let* ops =
      let* branch =
        Client.RPC.call client @@ RPC.get_chain_block_hash ~block:"head" ()
      in
      Lwt_list.map_s
        (fun (x, (y, counter)) ->
          craft_transfer ~source:x ~dest:y ~counter ~branch ~amount:1 client)
        accounts
    in
    let* _op_hashes =
      Operation.inject_operations
        ~use_tmp_file:true
        ~protocol
        ~force:false
        ops
        client
    in
    let* () =
      repeat count (fun () ->
          let* (_ : int) =
            bake_for
              ~wait_for_flush:true
              ~empty:true
              ~protocol
              node
              ~benchmarked_nodes:[]
              ~benchmarked_clients:[]
              client
          in
          unit)
    in
    unit
  in
  unit

(* [mempool_bench_txs ?(sig_algo_bootstrap_accounts) ?(sig_algo_receiver)
   ?(nb_txs = 10) protocol ~tag:_] aims to benchmark the mempool by transferring
   [nb_txs] operations from addresses using [sig_algo_bootstrap_accounts]
   signatures, and then, using [sig_algo_receiver] signatures. The results are
   expected to be found in the tet artifacts. *)
let mempool_bench_txs ?(sig_algo_bootstrap_accounts = "ed25519")
    ?(sig_algo_receiver = "bls") ?(nb_txs = 10) protocol ~tag:_ =
  let* client, node, additional_bootstraps =
    init_node_client_with_protocol
      ~sig_algo:sig_algo_bootstrap_accounts
      nb_txs
      protocol
  in
  Log.info
    "Initialise node and client with %d additional bootstrap accounts with \
     protocol %s"
    nb_txs
    (Protocol.name protocol) ;
  let* keys =
    Client.stresstest_gen_keys
      ~alias_prefix:"account_"
      ~sig_algo:sig_algo_receiver
      (List.length additional_bootstraps)
      client
  in

  let* () =
    init_addrs node client protocol (List.combine additional_bootstraps keys)
  in
  (* Retrieve all the counters. Not efficient but easier to process. *)
  let* counters =
    Lwt_list.map_s (fun k -> Operation.get_next_counter ~source:k client) keys
  in

  Log.info "Let's reveal" ;
  let* () = reveal_addrs node client protocol (List.combine keys counters) in

  Log.info "------ STARTING PROFILERS" ;

  (* Start the profilers *)
  let* () =
    Lwt_list.iter_s
      (fun (node, parallelism) ->
        let* () = Node.terminate node in
        let* () =
          Node.run
            ~env:
              (String_map.of_list
                 ((match parallelism with
                  | None -> [("TEZOS_OPERATION_BATCH_SIZE", "100")]
                  | Some n ->
                      [
                        ("TEZOS_DOMAINS", string_of_int n);
                        ("TEZOS_USE_NEW_CLASSIFY", "true");
                        ("TEZOS_OPERATION_BATCH_SIZE", "100");
                      ])
                 @ [("PROFILING", "*->debug"); ("PROFILING_BACKEND", "txt")]))
            ~event_sections_levels:
              [
                ("prevalidator", `Debug);
                ("validator", `Debug);
                ("block", `Debug);
              ]
            node
            []
        in
        Node.wait_for_ready node)
      [(node, Some 4)]
  in

  Log.info "------ PROFILERS READY" ;
  Unix.sleep 2 ;

  Log.info
    "Benchmarking %s check signature: sending %d txs from %s to %s"
    sig_algo_bootstrap_accounts
    nb_txs
    sig_algo_bootstrap_accounts
    sig_algo_receiver ;
  let* () =
    bench_txs
      node
      client
      protocol
      (List.combine
         additional_bootstraps
         (List.combine keys (List.map (fun _ -> 2) keys)))
  in

  Log.info
    "Benchmarking %s check signature: sending %d txs from %s to %s"
    sig_algo_receiver
    nb_txs
    sig_algo_receiver
    sig_algo_bootstrap_accounts ;
  let* () =
    let* counters =
      Lwt_list.map_s (fun k -> Operation.get_next_counter ~source:k client) keys
    in
    bench_txs
      node
      client
      protocol
      (List.combine keys (List.combine additional_bootstraps counters))
  in

  unit

let bench_with_txs ~executors =
  Long_test.register
    ~__FILE__
    ~title:"Mempool benchmarks txs"
    ~tags:["prevalidator"; "benchmark"; "txs"]
    ~team
    ~executors
    ~timeout:(Hours 2)
  @@ fun () ->
  Lwt_list.iter_s
    (fun protocol ->
      let tag = Format.sprintf "%s" (Protocol.name protocol) in
      mempool_bench_txs protocol ~tag)
    protocols

let register ~executors () = bench_with_txs ~executors
