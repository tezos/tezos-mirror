(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

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
    transaction_amount := Int64.succ !transaction_amount ;
    return injector_operation_id

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
    counter := Int64.succ !counter ;
    return injector_operation_id

(* For a list of pending injector operations, query their status with the
 * injector RPC, confirm the "included" status by checking the block in which
 * operations are reported to have been included, and return the list of
 * operations which are still pending. *)
let check_operation_status =
  (* Cache the last queried block to avoid repeat RPC calls when checking
   * multiple operations included in the same block. *)
  let last_queried_block = ref None in
  fun client injector ops ->
    let open Injector.RPC in
    let open JSON in
    let check_included op_hash level =
      let* ops_list =
        match !last_queried_block with
        | Some (last_level, ops_list) when level = last_level -> return ops_list
        | _ ->
            let* ops =
              Client.RPC.call client
              @@ RPC.get_chain_block_operations ~block:(string_of_int level) ()
            in
            let ops_list = ops |=> 3 |> as_list in
            last_queried_block := Some (level, ops_list) ;
            return ops_list
      in
      (* TODO: this checks that the batch this operation is part of
       * has been included, could check the individual operation
       * in the batch too. *)
      assert (
        List.exists (fun op -> op |-> "hash" |> as_string = op_hash) ops_list) ;
      return ()
    in

    let check_status op_id =
      let* status = Injector.RPC.(call injector @@ operation_status op_id) in
      match status with
      | Some Pending -> return [op_id]
      | Some (Injected _info) ->
          (* Here, we could check that the operation is indeed in the mempool,
           * but it would be a flaky test because by the time we query the mempool
           * the operation might have already been injected. *)
          return [op_id]
      | Some (Included info) ->
          let* () = check_included info.included_oph info.level in
          return []
      | None -> Test.fail "Uninjected operation no longer in injector queue"
    in
    let* () = Client.bake_for client in
    let* remaining_ops = Lwt_list.map_p check_status ops in
    return (List.flatten remaining_ops)

let add_one_transaction_and_bake client injector ?contract ops =
  let* inj_operation_hash =
    match contract with
    | None -> add_pending_transaction injector
    | Some contract -> add_pending_contract_call injector contract
  in
  let* ops = check_operation_status client injector ops in
  return (inj_operation_hash :: ops)

let rec add_one_transaction_per_block client injector ?contract n_blocks ops =
  if n_blocks = 0 then return ops
  else
    let* ops = add_one_transaction_and_bake client injector ?contract ops in
    add_one_transaction_per_block client injector ?contract (n_blocks - 1) ops

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
   * The list of pending operations is threaded throughout in order to
   * check their status after baking every block, removing those which
   * have been injected.
   * Lastly, check that all transactions have been included. *)
  let* ops = add_one_transaction_per_block client injector 5 [] in
  let* ops = add_one_transaction_per_block client injector ~contract 5 ops in
  let* ops = check_operation_status client injector ops in
  let* () = Client.bake_for client in
  let* ops = check_operation_status client injector ops in
  assert (ops = []) ;
  unit

let register ~protocols = test_injector protocols
