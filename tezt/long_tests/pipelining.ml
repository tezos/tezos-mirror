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

let team = Team.layer1

(** {2 Test parameters} *)
let manager_kinds = [`Transfer; `Origination; `Call]

let manager_kind_to_string = function
  | `Transfer -> "transfer"
  | `Origination -> "origination"
  | `Call -> "call"

(* These operations numbers are computed to consume the maximum number of gas
   unit in a block without exceeding the `hard_gas_limit_per_block` from the
   protocol (1.73M gas unit since P). *)
let number_of_operation_from_manager_kind = function
  | `Transfer -> 456
  (* Gas cost for transfers has been increased by 2000 units.
     It was previously 1040 and it's now 3040.
     Since 1333 * (1040/3040) = 456, the number of expected operations is 456 *)
  | `Origination -> 13 (* x100_000 gas units *)
  | `Call -> 2 (* x650_000 gas units *)

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
    Client.RPC.spawn client @@ RPC.post_chain_mempool_request_operations ()
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
  let* () =
    Node.Config_file.update
      node
      (Node.Config_file.set_prevalidator ~operations_request_timeout:100.)
  in
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
    List.map
      (fun x ->
        ( x,
          Some
            {
              Protocol.balance = Some 500_000_000;
              consensus_key = None;
              delegate = None;
            },
          false ))
      additional_bootstrap_account
    (* Starting with Oxford, bootstrap delegates have part of their balance
       frozen. To avoid having account with none available balance we don't use
       revealed bootstrap account and we reveal them later on. *)
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
  return (client, node, additional_bootstrap_account)

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

(** [forging_operation ?contract_hash manager_kind ~source ~branch ~counter
    client] create an {Operation_core.t} value that can be use by the
    [inject_operations] RPCs.

    - If [manager_kind] is [`Reveal], a reveal of [source] is forged

    - If [manager_kind] is [`Transfer] a transfer from [source] to
      [bootstrap1] is forged

    - If [manager_kind] is [`Call] a call to [contract_hash] is forged with an
      argument that make the operation consume approximately ~1M of gas unit.

    - If [manager_kind] is [`Origination] an origination of a big contract is
      forged. *)
let forging_operation ?contract_hash manager_kind ~source ~branch ~counter
    client =
  let operation =
    match manager_kind with
    | `Reveal ->
        Operation.Manager.make ~source ~counter
        @@ Operation.Manager.reveal source ()
    | `Transfer ->
        Operation.Manager.make ~source ~counter
        @@ Operation.Manager.transfer
             ~dest:Account.Bootstrap.keys.(1)
             ~amount:1
             ()
    | `Call -> (
        (* Magical constant that makes the contract consume around 850K gas unit *)
        let arg = `O [("int", `String "6306473")] in
        match contract_hash with
        | None ->
            Test.fail "Contract hash should be given to craft Call operations"
        | Some contract_hash ->
            Operation.Manager.make
              ~source
              ~counter
              ~fee:650_000
              ~gas_limit:650_000
            @@ Operation.Manager.call
                 ~amount:0
                 ~entrypoint:"default"
                 ~arg
                 ~dest:contract_hash
                 ())
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

let forging_n_operations ?contract_hash bootstraps manager_kind client =
  (* explicitly anchor the forged operations on the first block *)
  let* branch =
    Client.RPC.call client @@ RPC.get_chain_block_hash ~block:"0" ()
  in
  (* recover the counter of a bootstrap account, all other bootstrap accounts
     have the same counter *)
  let* counter =
    Operation.get_next_counter ~source:(List.hd bootstraps) client
  in
  Lwt_list.map_s
    (fun source ->
      forging_operation
        ?contract_hash
        manager_kind
        ~source
        ~counter
        ~branch
        client)
    bootstraps

let revealing_additional_bootstrap_accounts additional_bootstraps
    number_of_operations protocol node client =
  let* ops = forging_n_operations additional_bootstraps `Reveal client in
  let* op_hashes =
    Operation.inject_operations
      ~use_tmp_file:true
      ~protocol:Protocol.Alpha
      ~force:false
      ops
      client
  in
  Check.(List.length op_hashes = number_of_operations)
    Check.int
    ~error_msg:"Expected %R forged operations. Got %L" ;

  let bake_and_get_ophs () =
    let* _lvl =
      bake_for ~wait_for_flush:true ~empty:false ~protocol node client
    in
    let* block_ophs =
      Client.RPC.call client
      @@ RPC.get_chain_block_operation_hashes_of_validation_pass 3
    in
    return block_ophs
  in

  (* Reveal operations cost more than simple transfers, we may need to bake 2
     blocks to reveal all additional accounts. *)
  let* block_ophs =
    let rec aux block_ophs_acc () =
      let* block_ophs = bake_and_get_ophs () in
      let block_ophs = List.rev_append block_ophs block_ophs_acc in
      let* mempool = Mempool.get_mempool client in
      if List.length mempool.validated <> 0 then aux block_ophs ()
      else return block_ophs
    in
    aux [] ()
  in
  Check.(List.length block_ophs = number_of_operations)
    Check.int
    ~error_msg:"Expected %R operations in the block. Got %L" ;

  let op_hashes = List.map (fun (`OpHash oph) -> oph) op_hashes in
  if
    not
      (List.for_all2
         String.equal
         (List.sort String.compare op_hashes)
         (List.sort String.compare block_ophs))
  then Test.fail ~__LOC__ "Block does not contains all reveal operation" ;
  unit

(* Test *)
let operation_and_block_validation protocol manager_kind tag =
  let color = Log.Color.FG.green in
  let tags = [(tag, tag)] in
  let measure time f = Long_test.measure ~tags time (fun () -> f) in
  let get_previous_stats time_mean time title =
    let* res =
      Long_test.get_previous_stats
        ~minimum_count:5
        ~limit:10
        ~tags
        time
        "duration"
        Long_test.Stats.mean
    in
    (match res with
    | None -> ()
    | Some (count, average) ->
        Log.info ~color "%s:%s, count:%d, average:%f" title tag count average ;
        measure time_mean average) ;
    unit
  in

  Log.info
    "\nParameters of the test:\n  Protocol: %s\n  Operations: %s"
    (Protocol.name protocol)
    (manager_kind_to_string manager_kind) ;
  let number_of_operations =
    number_of_operation_from_manager_kind manager_kind
  in
  Log.info
    "Initialise node and client with %d additional bootstrap accounts with \
     protocol %s"
    number_of_operations
    (Protocol.name protocol) ;
  let* client, node, additional_bootstraps =
    init_node_client_with_protocol number_of_operations protocol
  in

  Log.info "Revealing the %d additional bootstrap accounts" number_of_operations ;
  let* () =
    revealing_additional_bootstrap_accounts
      additional_bootstraps
      number_of_operations
      protocol
      node
      client
  in

  Log.info "Originate toy contract" ;
  let* contract_hash, lvl = originate_contract ~protocol client node in

  Log.info
    "Forging %d %s operations"
    number_of_operations
    (manager_kind_to_string manager_kind) ;
  let* ops =
    forging_n_operations
      ~contract_hash
      additional_bootstraps
      manager_kind
      client
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
    if name = "operation_classified.v0" then (
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
  measure classify_title !classification_time ;

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
  measure reclassify_title !reclassification_time ;

  Log.info
    "Ensure that the mempool contains %d validated operations"
    number_of_operations ;
  let* mempool = Mempool.get_mempool client in
  Check.(List.length mempool.validated = number_of_operations)
    Check.int
    ~error_msg:"Expected %R validated operations in the mempool. Got %L" ;

  (* Setting up a event watchers on validation and application of blocks to
     measure the time needed to validate and apply a block *)
  let validation_time = ref 0. in
  let application_time = ref 0. in
  let on_bake_event u Node.{name; value = _; timestamp} =
    match name with
    | "validating_block.v0" -> validation_time := timestamp
    | "validated_block.v0" -> validation_time := timestamp -. !validation_time
    | "applying_block.v0" -> application_time := timestamp
    | "validation_and_application_success.v0" ->
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
        data_dir;
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
  measure forging_title forging_time ;

  let* () = node_t in

  let* lvl = Node.wait_for_level node2 (lvl + 1) in
  Log.info ~color "Block validation time on node B: %f" !validation_time ;
  measure validation_title !validation_time ;

  Log.info ~color "Block application time on node B: %f" !application_time ;
  measure application_title !application_time ;

  let total_time = !application_time +. !validation_time in
  Log.info ~color "Block validation + application time on node B: %f" total_time ;
  measure total_title total_time ;

  Log.info
    "Ensure that the block baked contains %d operations"
    number_of_operations ;
  let* block =
    Client.RPC.call client @@ RPC.get_chain_block ~block:(string_of_int lvl) ()
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
    ~team
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
