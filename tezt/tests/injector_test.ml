(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Create a promise that resolves when the given operation ID is included *)
let wait_for_inclusion ?(timeout = 10.) injector operation_id =
  Injector.wait_for ~timeout injector "included.v0" (fun event ->
      let open JSON in
      let operations = event |-> "operations" |> as_list in
      let operation_ids = List.map as_string operations in
      if List.mem operation_id operation_ids then (
        let block = event |-> "block" |> as_string in
        let level = event |-> "level" |> as_int in
        Log.info
          "Operation %s included in block %s at level %d"
          operation_id
          block
          level ;
        Some ())
      else None)

(* Add a transaction and return both the operation ID and a promise that
 * resolves when the operation is included in a block. *)
let add_pending_transaction =
  (* Increase amount every time a transaction is added to the injector
   * queue so that transactions have different hashes.
   * TODO: https://gitlab.com/tezos/tezos/-/issues/6332
   * Fix injector so this workaround is no longer required. *)
  let transaction_amount = ref 100L in
  fun injector ->
    let* injector_operation_id =
      Injector.RPC.(
        call injector
        @@ add_pending_transaction
             !transaction_amount
             Account.Bootstrap.keys.(1).public_key_hash)
    in
    Log.info
      "Added pending transaction: amount = %Ld, id = %s"
      !transaction_amount
      injector_operation_id ;
    transaction_amount := Int64.succ !transaction_amount ;
    let wait_included = wait_for_inclusion injector injector_operation_id in
    return (injector_operation_id, wait_included)

(* Add a contract call and return both the operation ID and a promise that
 * resolves when the operation is included in a block. *)
let add_pending_contract_call =
  (* Increase the value passed in every contract call added to the injector
   * queue so that transactions have different hashes. *)
  let counter = ref 0L in
  fun injector contract ->
    let* injector_operation_id =
      Injector.RPC.(
        call injector
        @@ add_pending_transaction
             ~parameters:(String.empty, Int64.to_string !counter)
             0L
             contract)
    in
    Log.info
      "Added pending contract call: counter = %Ld, id = %s"
      !counter
      injector_operation_id ;
    counter := Int64.succ !counter ;
    (* Start waiting for the included event for this operation *)
    let wait_included = wait_for_inclusion injector injector_operation_id in
    return (injector_operation_id, wait_included)

(* Add one operation, bake, and return (id, wait_promise) *)
let add_one_transaction_and_bake client injector ?contract () =
  let* op_id, wait_included =
    match contract with
    | None -> add_pending_transaction injector
    | Some contract -> add_pending_contract_call injector contract
  in
  let* () = Client.bake_for client in
  return (op_id, wait_included)

(* Add n operations, baking after each one, return list of (id, wait_promise) *)
let rec add_transactions_with_baking client injector ?contract n_blocks acc =
  if n_blocks = 0 then return (List.rev acc)
  else
    let* op_tracked =
      add_one_transaction_and_bake client injector ?contract ()
    in
    add_transactions_with_baking
      client
      injector
      ?contract
      (n_blocks - 1)
      (op_tracked :: acc)

let test_injector : Protocol.t list -> unit =
  Protocol.register_test
    ~__FILE__
    ~title:"Injector daemon"
    ~tags:[]
    ~uses:(fun _ -> [Constant.octez_injector_server])
  @@ fun protocol ->
  let nodes_args = Node.[Synchronisation_threshold 0; Private_mode] in
  let* node, client =
    Client.init_with_protocol `Client ~protocol ~nodes_args ()
  in
  let injector = Injector.create node client in
  let* _config_file =
    Injector.init_config injector Account.Bootstrap.keys.(0)
  in
  let* () = Injector.run injector in
  let* () = Client.bake_for client in

  (* Originate a simple contract to make calls to *)
  let* _alias, contract =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap2.alias
      client
      ~burn_cap:Tez.one
      ~init:"3"
      ["attic"; "add1"]
      protocol
  in

  (* Bake 10 blocks: inject a transfer in each of the first 5, then
   * inject a contract call in each of the following 5.
   * Each operation has an associated promise that resolves when it's included. *)
  let* tracked_ops = add_transactions_with_baking client injector 5 [] in
  let* tracked_ops =
    add_transactions_with_baking client injector ~contract 5 tracked_ops
  in

  Log.info "Bake one more block to ensure all operations are included" ;
  let* () = Client.bake_for client in

  Log.info
    "Waiting for %d operations to be included..."
    (List.length tracked_ops) ;
  let all_wait_promises = List.map snd tracked_ops in
  let* () = Lwt.join all_wait_promises in
  Log.info "All operations successfully included!" ;
  unit

(* Michelson contract burning an amount of gas proportional to its nat
   parameter (number of loop iterations). *)
let gas_burner_script =
  {|parameter nat ;
storage unit ;
code { CAR ;
       PUSH nat 0 ;
       DUP 2 ; DUP 2 ; COMPARE ; LT ;
       LOOP { PUSH nat 1 ; ADD ;
              DUP 2 ; DUP 2 ; COMPARE ; LT } ;
       DROP 2 ;
       UNIT ; NIL operation ; PAIR }|}

(* Number of loop iterations for a call to [gas_burner_script] to consume
   about 60% of the sandbox block gas limit (1_040_000 gas, which is also the
   per-operation gas limit): a single call fits within the per-operation gas
   limit but any two calls exceed the block gas quota. *)
let gas_burner_iterations = 3_870_000

(* Test that a batch whose operations individually fail simulation with gas
   exhaustion is split by the injector (instead of the operations being
   repeatedly dropped) and that all operations are eventually included. *)
let test_injector_batch_split_on_gas_exhaustion : Protocol.t list -> unit =
  Protocol.register_test
    ~__FILE__
    ~title:"Injector batch splitting on gas exhaustion"
    ~tags:["injector"; "batch"; "split"; "gas"]
    ~uses:(fun _ -> [Constant.octez_injector_server])
  @@ fun protocol ->
  let nodes_args = Node.[Synchronisation_threshold 0; Private_mode] in
  let* node, client =
    Client.init_with_protocol `Client ~protocol ~nodes_args ()
  in
  let injector = Injector.create node client in
  let* _config_file =
    Injector.init_config injector Account.Bootstrap.keys.(0)
  in
  let* () = Injector.run injector in
  let* () = Client.bake_for client in
  (* Originate a contract which burns gas proportionally to its parameter *)
  let* contract =
    Client.originate_contract
      ~alias:"gas_burner"
      ~amount:Tez.zero
      ~src:Constant.bootstrap2.alias
      ~prg:gas_burner_script
      ~init:"Unit"
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for client in
  (* The event promises must be registered before the operations are
     queued. *)
  let wait_split =
    Injector.wait_for injector "batch_too_large_splitting.v0" (fun _ -> Some ())
  in
  let discarded =
    Injector.wait_for injector "discard_error_operation.v0" (fun _ -> Some ())
  in
  let nb_ops = 4 in
  (* Queue [nb_ops] gas-heavy contract calls without baking in between, so
     that they are all considered in a single batch. The loop counts differ so
     that the operations have different hashes. *)
  let* wait_included =
    Lwt_list.map_s
      (fun i ->
        let* op_id =
          Injector.RPC.(
            call injector
            @@ add_pending_transaction
                 ~parameters:
                   (String.empty, string_of_int (gas_burner_iterations + i))
                 0L
                 contract)
        in
        Log.info "Added pending gas-heavy contract call: id = %s" op_id ;
        return (wait_for_inclusion ~timeout:120. injector op_id))
      (Base.range 1 nb_ops)
  in
  (* Bake blocks until all the operations have been included. Because of the
     block gas quota, the injector can only get (roughly) one operation
     included per block. On each iteration we trigger an injection (the
     standalone injector server injects on demand, unlike the rollup node
     which injects on every new L1 head) so that the operations re-queued
     after a batch split are reconsidered. *)
  let is_resolved p = match Lwt.state p with Lwt.Sleep -> false | _ -> true in
  let rec bake_until_included attempts =
    if List.for_all is_resolved wait_included then unit
    else if attempts <= 0 then
      Test.fail "Operations were not included after baking many blocks"
    else
      let* () = Injector.RPC.(call injector @@ inject ()) in
      let* () = Lwt_unix.sleep 0.5 in
      let* () = Client.bake_for client in
      let* () = Lwt_unix.sleep 0.5 in
      bake_until_included (attempts - 1)
  in
  let* () = bake_until_included (6 * nb_ops) in
  let* () = Lwt.join wait_included in
  Log.info "All operations were included." ;
  (* The batch could not fit in a single block, so it must have been split at
     least once. Splitting happens before injection, hence before inclusion,
     so the event has necessarily been seen at this point. *)
  if not (is_resolved wait_split) then
    Test.fail "The batch was never split by the injector" ;
  (* No operation should have been discarded by the injector. The injector
     handles only the operations queued by this test, so any discard would
     necessarily be one of them. *)
  if is_resolved discarded then
    Test.fail "An operation was discarded by the injector" ;
  unit

(* Number of loop iterations for a call to [gas_burner_script] to exceed the
   per-operation gas limit (1_040_000): such a call can never be injected on
   its own. *)
let over_operation_quota_iterations = 7_000_000

(* Test that when an operation which individually exceeds the per-operation
   gas limit is batched with injectable operations, the injector still makes
   progress: the batch is split down to the failing operation (which can never
   be injected), and the injectable operations set aside by the split are
   re-queued and eventually injected instead of being lost. *)
let test_injector_split_isolates_failing_operation : Protocol.t list -> unit =
  Protocol.register_test
    ~__FILE__
    ~title:"Injector batch split isolates a failing operation"
    ~tags:["injector"; "batch"; "split"; "gas"; "fail"]
    ~uses:(fun _ -> [Constant.octez_injector_server])
  @@ fun protocol ->
  let nodes_args = Node.[Synchronisation_threshold 0; Private_mode] in
  let* node, client =
    Client.init_with_protocol `Client ~protocol ~nodes_args ()
  in
  let injector = Injector.create node client in
  let* _config_file =
    Injector.init_config injector Account.Bootstrap.keys.(0)
  in
  let* () = Injector.run injector in
  let* () = Client.bake_for client in
  let* contract =
    Client.originate_contract
      ~alias:"gas_burner"
      ~amount:Tez.zero
      ~src:Constant.bootstrap2.alias
      ~prg:gas_burner_script
      ~init:"Unit"
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for client in
  (* Queue first an operation that individually exceeds the per-operation gas
     limit (it can never be injected), then two cheap injectable ones. They
     are all considered in a single batch. *)
  let* failing_op_id =
    Injector.RPC.(
      call injector
      @@ add_pending_transaction
           ~parameters:
             (String.empty, string_of_int over_operation_quota_iterations)
           0L
           contract)
  in
  Log.info "Added over-quota operation: id = %s" failing_op_id ;
  let* wait_included =
    Lwt_list.map_s
      (fun i ->
        let* op_id =
          Injector.RPC.(
            call injector
            @@ add_pending_transaction
                 ~parameters:(String.empty, string_of_int (10 + i))
                 0L
                 contract)
        in
        Log.info "Added cheap operation: id = %s" op_id ;
        return (wait_for_inclusion ~timeout:120. injector op_id))
      (Base.range 1 2)
  in
  let is_resolved p = match Lwt.state p with Lwt.Sleep -> false | _ -> true in
  let rec bake_until_included attempts =
    if List.for_all is_resolved wait_included then unit
    else if attempts <= 0 then
      Test.fail
        "The injectable operations set aside by the split were not included"
    else
      let* () = Injector.RPC.(call injector @@ inject ()) in
      let* () = Lwt_unix.sleep 0.5 in
      let* () = Client.bake_for client in
      let* () = Lwt_unix.sleep 0.5 in
      bake_until_included (attempts - 1)
  in
  let* () = bake_until_included 30 in
  let* () = Lwt.join wait_included in
  Log.info
    "The injectable operations were included despite the failing operation." ;
  unit

let register ~protocols =
  test_injector protocols ;
  test_injector_batch_split_on_gas_exhaustion protocols ;
  test_injector_split_isolates_failing_operation protocols
