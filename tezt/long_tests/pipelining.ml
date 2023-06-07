(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component: Prevalidator, block validator, Baker
   Invocation: dune exec tezt/long_tests/main.exe -- --file pipelining.ml
   Subject: Benchmarking the prevalidator classification time the block
   validator time and the baker forging time.
*)

(** {2 Test parameters} *)
let manager_kinds = [`Transfer; `Origination; `Call]

let manager_kind_to_string = function
  | `Transfer -> "transfer"
  | `Origination -> "origination"
  | `Call -> "call"

(* These operations numbers are computed to consume the maximum number of gas
   unit in a block without exceeding the `hard_gas_limit_per_block` from the
   protocol (2.6M gas unit since Mumbai). *)
let number_of_operation_from_manager_kind = function
  | `Transfer -> 2500 (* x1040 gas units *)
  | `Origination -> 26 (* x100_000 gas units *)
  | `Call -> 3 (* x850_000 gas units *)

let protocols = Protocol.all

(** {2 Grafana panels} *)

let title = "Pipelining benchmarking"

let reclassify_title = "Mempool: flush time"

let reclassify_title_mean = "Mempool: flush time avg"

let classify_title = "Mempool: classification time"

let classify_title_mean = "Mempool: classification time avg"

let forging_title = "Block: forging time"

let forging_title_mean = "Block: forging time avg"

let validation_title = "Block: validation time"

let validation_title_mean = "Block: validation time avg"

let application_title = "Block: application time"

let application_title_mean = "Block: application time avg"

let total_title = "Block: validation + application time"

let total_title_mean = "Block: validation + application time avg"

let grafana_panels : Grafana.panel list =
  let tags =
    List.fold_left
      (fun acc p ->
        List.fold_left
          (fun acc manager_kind ->
            let tag =
              Format.sprintf
                "%s.%s"
                (Protocol.name p)
                (manager_kind_to_string manager_kind)
            in
            (tag, tag) :: acc)
          acc
          manager_kinds)
      []
      protocols
  in
  let graphs_per_tags measurement =
    Grafana.graphs_per_tags ~measurement ~field:"duration" ~test:title ~tags ()
  in
  [
    Row "Test: Pipelining";
    graphs_per_tags classify_title_mean;
    graphs_per_tags classify_title;
    graphs_per_tags reclassify_title_mean;
    graphs_per_tags reclassify_title;
    graphs_per_tags forging_title_mean;
    graphs_per_tags forging_title;
    graphs_per_tags validation_title_mean;
    graphs_per_tags validation_title;
    graphs_per_tags application_title_mean;
    graphs_per_tags application_title;
    graphs_per_tags total_title_mean;
    graphs_per_tags total_title;
  ]

(** [synchronize_mempool client node] calls the [request_operations] RPC from
    the [client] to retrieve mempool from its peers and waits for a [notify]
    event on the [node] (debug events must be enabled). *)
let synchronize_mempool client node =
  let mempool_notify_waiter = Node.wait_for_request ~request:`Notify node in
  let*? _ =
    RPC.Client.spawn client @@ RPC.post_chain_mempool_request_operations ()
  in
  mempool_notify_waiter

(** We override the default [bake_for] command to wait on the next level
    incremented after the new block. If [wait_for_flush] is set we wait on a
    [flush] event from the mempool because the [set_head] event used by the
    default [bake_for] functions happens before a flush of the mempool. For
    mempool tests, we generally prefer to ensure that a [flush] did happen than
    a [set_head].

   Optionally, we can decide whether the block should be baked without taking
   the operations of the mempool.

   This function returns the level of the node after the bake. *)
let bake_for ?(keys = []) ?(wait_for_flush = false) ~empty ~protocol node client
    =
  let flush_waiter =
    if wait_for_flush then Node.wait_for_request ~request:`Flush node else unit
  in
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
  Node.wait_for_level node (succ level)

(** Initialize a node with specific level for event sections, with a timeout for
    [operations_request_timeout] longer than the default one. *)
let init_node () =
  let node = Node.create Node.[Synchronisation_threshold 0; Connections 2] in
  let* () = Node.config_init node [] in
  Node.Config_file.update
    node
    (Node.Config_file.set_prevalidator ~operations_request_timeout:100.) ;
  let* () =
    Node.run
      ~event_sections_levels:
        [("prevalidator", `Debug); ("validator", `Debug); ("block", `Debug)]
      node
      []
  in
  let* () = Node.wait_for_ready node in
  return node

let init_node_client_with_protocol number_of_additional_bootstrap protocol =
  let parameter_file = Protocol.parameter_file protocol in
  let* node = init_node () in
  let client = Client.create_with_mode (Client (Some (Node node), None)) in
  (* Add additional_bootstrap_account to the client *)
  Account.write Constant.all_secret_keys ~base_dir:(Client.base_dir client) ;
  let* additional_bootstrap_account =
    Client.stresstest_gen_keys number_of_additional_bootstrap client
  in
  let additional_bootstrap_accounts =
    List.map (fun x -> (x, Some 500_000_000, true)) additional_bootstrap_account
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~additional_bootstrap_accounts
      ~base:(Either.left parameter_file)
      []
  in
  let* () =
    Client.activate_protocol_and_wait ~parameter_file ~protocol ~node client
  in
  return (client, node)

(** Originate the contract and bake a block to apply the origination *)
let originate_contract ~protocol client node =
  let* _, contract_hash =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~burn_cap:(Tez.of_int 99999)
      ~src:Constant.bootstrap1.alias
      client
      ["mini_scenarios"; "loop"]
      protocol
  in
  let* lvl = bake_for ~wait_for_flush:true ~empty:false ~protocol node client in
  return (contract_hash, lvl)

(** [forging_operation manager_kind ~source ~branch ~counter contract_hash
    client] create an {Operation_core.t} value that can be use by the
    [inject_operations] RPCs.

    - If [manager_kind] if [`Transfer] a transfer from [source] to
      [bootstrap1] is forged

    - If [manager_kind] if [`Call] a call to [contract_hash] is forged with an
      argument that make the operation consume approximately ~1M of gas unit.

    - If [manager_kind] if [`Origination] an origination of a big contract is
      forged. *)
let forging_operation manager_kind ~source ~branch ~counter contract_hash client
    =
  let operation =
    match manager_kind with
    | `Transfer ->
        Operation.Manager.make ~source ~counter
        @@ Operation.Manager.transfer
             ~dest:Account.Bootstrap.keys.(1)
             ~amount:1
             ()
    | `Call ->
        (* Magical constant that makes the contract consume around 850K gas unit *)
        let arg = `O [("int", `String "8206473")] in
        Operation.Manager.make ~source ~counter ~fee:850_000 ~gas_limit:850_000
        @@ Operation.Manager.call
             ~amount:0
             ~entrypoint:"default"
             ~arg
             ~dest:contract_hash
             ()
    | `Origination ->
        let contract =
          Format.asprintf
            {|
        [ { "prim": "parameter", "args": [ { "prim": "unit" } ] },
          { "prim": "storage", "args": [ { "prim": "unit" } ] },
          { "prim": "code",
            "args":
              [ [ { "prim": "UNPAIR" }, { "prim": "DROP" },
                  { "prim": "PUSH",
                    "args": [ { "prim": "string" }, { "string": "%s" } ] },
                  { "prim": "DROP" },
                  { "prim": "NIL", "args": [ { "prim": "operation" } ] },
                  { "prim": "PAIR" }, %a { "prim": "DUP" },
                  { "prim": "DROP" } ] ] } ]
|}
            source.alias
            (Format.pp_print_list (fun fmt s -> Format.fprintf fmt "%s" s))
            (List.init 4500 (fun _ ->
                 {| { "prim": "DUP" }, { "prim": "DROP" },|}))
        in
        let init_storage = Ezjsonm.from_string {|{ "prim": "Unit" }|} in
        let contract = Ezjsonm.from_string contract in
        Operation.Manager.make
          ~source
          ~counter
          ~gas_limit:100_000
          ~storage_limit:20_000
          ~fee:100_000
        @@ Operation.Manager.origination ~code:contract ~init_storage ()
  in
  Operation.Manager.operation ~branch ~signer:source [operation] client

let forging_n_operations bootstraps contract_hash manager_kind client =
  (* explicitly anchor the forged operations on the first block *)
  let* branch =
    RPC.Client.call client @@ RPC.get_chain_block_hash ~block:"0" ()
  in
  (* recover the counter of a bootstrap account, all other bootstrap accounts
     have the same counter *)
  let* counter =
    Operation.get_next_counter ~source:(List.hd bootstraps) client
  in
  Lwt_list.map_s
    (fun source ->
      forging_operation
        manager_kind
        ~source
        ~counter
        ~branch
        contract_hash
        client)
    bootstraps

(* Test *)
let operation_and_block_validation protocol manager_kind tag =
  Log.info
    "\nParameters of the test:\n  Protocol: %s\n  Operations: %s"
    (Protocol.name protocol)
    (manager_kind_to_string manager_kind) ;
  let number_of_operations =
    number_of_operation_from_manager_kind manager_kind
  in
  Log.info
    "Initialise node and client with %d bootstrap accounts with protocol %s"
    number_of_operations
    (Protocol.name protocol) ;
  let* client, node =
    init_node_client_with_protocol number_of_operations protocol
  in
  let additional_bootstraps = Client.additional_bootstraps client in
  let* contract_hash, lvl = originate_contract ~protocol client node in

  let color = Log.Color.FG.green in
  let tags = [(tag, tag)] in
  let measure_and_check_regression time f =
    Long_test.measure_and_check_regression ~tags time (fun () -> f)
  in
  let get_previous_stats time_mean time title =
    let* res =
      Long_test.get_previous_stats
        ~limit:20
        ~tags
        time
        "duration"
        Long_test.Stats.mean
    in
    match res with
    | None -> unit
    | Some (count, average) ->
        Log.info ~color "%s:%s, count:%d, average:%f" title tag count average ;
        measure_and_check_regression time_mean average
  in

  Log.info "Forging %d operations" number_of_operations ;
  let* ops =
    forging_n_operations additional_bootstraps contract_hash manager_kind client
  in

  Log.info "Injecting %d operations" number_of_operations ;
  let* op_hashes =
    Operation.inject_operations
      ~use_tmp_file:true
      ~protocol:Protocol.Alpha
      ~force:false
      ops
      client
  in
  let op_hashes =
    List.sort String.compare (List.map (fun (`OpHash op) -> op) op_hashes)
  in
  Check.(List.length op_hashes = number_of_operations)
    Check.int
    ~error_msg:"Expected %R forged operations. Got %L" ;

  Log.info
    "Init a second node and connect it with the first one and measure the time \
     to classified %d operations."
    number_of_operations ;
  let classification_time = ref 0. in
  let cpt_classify = ref 0 in
  let classified_list = ref [] in
  let* node2 = init_node () in
  let* client2 = Client.init ~endpoint:(Node node2) () in
  let classify_t, classify_u = Lwt.task () in
  let on_notify_event u cpt list time Node.{name; value; timestamp} =
    if name = "operation_reclassified.v0" then (
      list := JSON.encode value :: !list ;
      if !cpt = 0 then time := timestamp ;
      incr cpt ;
      if !cpt = number_of_operations then (
        time := timestamp -. !time ;
        Lwt.wakeup u ()))
  in
  Node.on_event
    node2
    (on_notify_event
       classify_u
       cpt_classify
       classified_list
       classification_time) ;
  let* () = Client.Admin.connect_address ~peer:node client2 in
  let* _ = Node.wait_for_level node2 lvl in
  let* () = synchronize_mempool client2 node in
  let* () = classify_t in
  let classified_list = List.sort String.compare !classified_list in
  if
    List.for_all2
      (fun op1 op2 -> String.equal op1 op2)
      classified_list
      op_hashes
  then
    Test.fail
      ~__LOC__
      "List of Classified operation is not equal to the list of injected \
       operation hashes" ;

  Log.info
    ~color
    "Classification time on %s: %f"
    (Node.name node2)
    !classification_time ;
  let* () = measure_and_check_regression classify_title !classification_time in

  Log.info
    "Measure the time that take a node to reclassify %d operations after a \
     flush."
    number_of_operations ;
  let reclassification_time = ref 0. in
  let cpt_reclassify = ref 0 in
  let reclassified_list = ref [] in
  let reclassify_t, reclassify_u = Lwt.task () in
  Node.on_event
    node2
    (on_notify_event
       reclassify_u
       cpt_reclassify
       reclassified_list
       reclassification_time) ;
  let* lvl =
    bake_for ~wait_for_flush:true ~empty:true ~protocol node2 client2
  in
  let* () = reclassify_t in
  let reclassified_list = List.sort String.compare !reclassified_list in
  if
    List.for_all2
      (fun op1 op2 -> String.equal op1 op2)
      reclassified_list
      op_hashes
  then
    Test.fail
      ~__LOC__
      "List of Reclassified operation is not equal to the list of injected \
       operation hashes" ;

  Log.info
    ~color
    "Reclassification time on %s: %f"
    (Node.name node2)
    !reclassification_time ;
  let* () =
    measure_and_check_regression reclassify_title !reclassification_time
  in

  Log.info "Ensure that the mempool contains %d operations" number_of_operations ;
  let* json =
    RPC.Client.call ~endpoint:(Node node2) client
    @@ RPC.get_chain_mempool_pending_operations ()
  in
  let json_list = JSON.(json |-> "applied" |> as_list) in
  Check.(List.length json_list = number_of_operations)
    Check.int
    ~error_msg:"Expected %R operations in block. Got %L" ;

  (* Setting up a event watchers on validation and application of blocks to
     measure the time needed to validate and apply a block *)
  let validation_time = ref 0. in
  let application_time = ref 0. in
  let on_bake_event u Node.{name; value = _; timestamp} =
    match name with
    | "prechecking_block.v0" -> validation_time := timestamp
    | "prechecked_block.v0" -> validation_time := timestamp -. !validation_time
    | "validating_block.v0" -> application_time := timestamp
    | "validation_success.v0" ->
        application_time := timestamp -. !application_time ;
        Lwt.wakeup u ()
    | _ -> ()
  in

  let rec get_timestamp_of_event expected_event io =
    try
      let* line = Lwt_io.read_line io in
      let res = JSON.parse ~origin:"event" line in
      let sink = JSON.(res |-> "fd-sink-item.v0") in
      let event, _ = JSON.(sink |-> "event" |> as_object |> List.hd) in
      if String.equal event expected_event then
        return JSON.(sink |-> "time_stamp" |> as_float)
      else get_timestamp_of_event expected_event io
    with End_of_file -> return 0.
  in

  Log.info
    "Bake a block on %s, with event watcher to measure the time needed to \
     forge the block."
    (Node.name node) ;
  let node_t, node_u = Lwt.task () in
  Node.on_event node2 (on_bake_event node_u) ;
  let data_dir = Node.data_dir node in
  let p =
    Client.spawn_command
      ~env:
        (String_map.singleton
           "TEZOS_EVENTS_CONFIG"
           "file-descriptor-stdout://?section-prefix=:debug")
      client
      [
        "--protocol";
        Protocol.hash protocol;
        "-m";
        "binary";
        "bake";
        "for";
        "--minimal-timestamp";
        "--context";
        data_dir // "context";
      ]
  in
  let io = Process.stdout p in
  let* () = Process.check p in
  let* prepare_timestamp =
    get_timestamp_of_event "prepare_forging_block.v0" io
  in
  let* injecting_timestamp = get_timestamp_of_event "injecting_block.v0" io in
  let forging_time = injecting_timestamp -. prepare_timestamp in
  Log.info ~color "Block forging time on node A : %f" forging_time ;
  let* () = measure_and_check_regression forging_title forging_time in

  let* () = node_t in

  let* lvl = Node.wait_for_level node2 (lvl + 1) in
  Log.info ~color "Block validation time on node B: %f" !validation_time ;
  let* () = measure_and_check_regression validation_title !validation_time in

  Log.info ~color "Block application time on node B: %f" !application_time ;
  let* () = measure_and_check_regression application_title !application_time in

  let total_time = !application_time +. !validation_time in
  Log.info ~color "Block validation + application time on node B: %f" total_time ;
  let* () = measure_and_check_regression total_title total_time in

  Log.info
    "Ensure that the block baked contains %d operations"
    number_of_operations ;
  let* block =
    RPC.Client.call client @@ RPC.get_chain_block ~block:(string_of_int lvl) ()
  in
  let ops =
    JSON.(List.nth (block |> get "operations" |> as_list) 3 |> as_list)
  in
  Check.(
    (number_of_operations = List.length ops)
      int
      ~error_msg:"expected %L operations in the block, got %R") ;

  let* () = Node.terminate node in
  let* () = Node.terminate node2 in

  let* () =
    get_previous_stats reclassify_title_mean reclassify_title "Reclassify"
  in
  let* () = get_previous_stats classify_title_mean classify_title "Classify" in
  let* () = get_previous_stats forging_title_mean forging_title "Forging" in
  let* () =
    get_previous_stats validation_title_mean validation_title "Validation"
  in
  let* () =
    get_previous_stats application_title_mean application_title "Application"
  in
  let* () = get_previous_stats total_title_mean total_title "Total" in
  Log.info "End of test: %s" tag ;
  unit

let operation_and_block_validation ~executors =
  Long_test.register
    ~__FILE__
    ~title
    ~tags:
      [
        "flush"; "classification"; "prevalidator"; "block"; "baker"; "pipelining";
      ]
    ~executors
    ~timeout:(Hours 2)
  @@ fun () ->
  Lwt_list.iter_s
    (fun protocol ->
      Lwt_list.iter_s
        (fun manager_kind ->
          let tag =
            Format.sprintf
              "%s.%s"
              (Protocol.name protocol)
              (manager_kind_to_string manager_kind)
          in
          operation_and_block_validation protocol manager_kind tag)
        manager_kinds)
    protocols

let register ~executors () = operation_and_block_validation ~executors
