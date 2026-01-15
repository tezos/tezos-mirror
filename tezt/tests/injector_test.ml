(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Create a promise that resolves when the given operation ID is included *)
let wait_for_inclusion injector operation_id =
  Injector.wait_for ~timeout:10. injector "included.v0" (fun event ->
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

let register ~protocols = test_injector protocols
