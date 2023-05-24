(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 G.B. Fefe <g.b.fefe@protonmail.com>                    *)
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
   Component:    Baker
   Invocation:   dune exec tezt/tests/main.exe -- --file consensus_key.ml
   Subject:      Test the operation `Update_consensus_key` and its effects
                 on the baker.
*)

let hooks = Tezos_regression.hooks

let blocks_per_cycle = 4

let preserved_cycles = 1

module Helpers = struct
  let level_type : RPC.level Check.typ =
    Check.convert
      (fun RPC.
             {level; level_position; cycle; cycle_position; expected_commitment} ->
        (level, level_position, cycle, cycle_position, expected_commitment))
      Check.(tuple5 int int int int bool)

  let get_current_level client =
    RPC.Client.call client @@ RPC.get_chain_block_helper_current_level ()

  let check_current_level client expected_level =
    let* level = get_current_level client in
    Check.((level = expected_level) level_type)
      ~error_msg:"expected current_period = %R, got %L" ;
    unit

  let bake_and_wait_block node client =
    let level = Node.get_level node in
    let* () =
      Client.bake_for ~context_path:(Node.data_dir node // "context") client
    in
    let* _i = Node.wait_for_level node (level + 1) in
    Lwt.return_unit

  let bake_n_cycles ?keys n client =
    let rec loop n =
      if n = 0 then unit
      else
        let* () = Client.bake_for_and_wait ?keys client in
        loop (n - 1)
    in
    let* current_level = get_current_level client in
    let current_level = current_level.level in
    let nb_baked_blocks_in_cycle = current_level mod blocks_per_cycle in
    let nb_blocks_to_bake = (n * blocks_per_cycle) - nb_baked_blocks_in_cycle in
    Log.info
      "Bake past %d cycles (from level %d to %d)"
      n
      current_level
      (current_level + nb_blocks_to_bake) ;
    loop nb_blocks_to_bake
end

open Helpers

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4243
   Instead of [~expect_failure:true], the helpers should take a
   function that identifies which error should happen, to avoid the
   tests succeeding for wrong reasons. *)

let test_update_consensus_key =
  Protocol.register_test
    ~__FILE__
    ~title:"update consensus key"
    ~tags:["consensus_key"]
  @@ fun protocol ->
  let manual_staking = Protocol.(protocol > Nairobi) in
  let parameters =
    (* we update paramaters for faster testing: no need to wait
       5 cycles for the consensus key to activate. *)
    [
      (["blocks_per_cycle"], `Int blocks_per_cycle);
      (["nonce_revelation_threshold"], `Int 2);
      (["preserved_cycles"], `Int preserved_cycles);
    ]
  in
  let* parameter_file =
    Protocol.write_parameter_file ~base:(Right (protocol, None)) parameters
  in
  let* _, client =
    Client.init_with_protocol ~parameter_file ~protocol `Client ()
  in
  let* key_a = Client.gen_and_show_keys client in
  let* key_b = Client.gen_and_show_keys client in
  let* key_c = Client.gen_and_show_keys client in
  let* key_bls = Client.gen_and_show_keys ~sig_alg:"bls" client in
  let* destination = Client.gen_and_show_keys client in

  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 1_000_000)
      ~giver:Constant.bootstrap1.alias
      ~receiver:key_b.alias
      client
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 1_000_000)
      ~giver:Constant.bootstrap2.alias
      ~receiver:key_c.alias
      client
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 1)
      ~giver:Constant.bootstrap4.alias
      ~receiver:destination.alias
      client
  in
  let* () = Client.bake_for_and_wait client in

  Log.info
    "Invalid update: changing the consensus key of an unregistered delegate" ;
  let* () =
    Client.update_consensus_key
      ~expect_failure:true
      ~src:key_b.alias
      ~pk:Constant.bootstrap1.alias
      client
  in
  Log.info "Invalid update: changing the consensus key to its actual value" ;
  let* () =
    Client.update_consensus_key
      ~expect_failure:true
      ~src:Constant.bootstrap1.alias
      ~pk:Constant.bootstrap1.alias
      client
  in
  Log.info
    "Invalid update: changing the consensus key to an active consensus key \
     (bootstrap)" ;
  let* () =
    Client.update_consensus_key
      ~expect_failure:true
      ~src:Constant.bootstrap2.alias
      ~pk:Constant.bootstrap1.alias
      client
  in
  Log.info "Invalid update: changing the consensus key to a BLS key." ;
  let* () =
    Client.update_consensus_key
      ~expect_failure:true
      ~src:Constant.bootstrap1.alias
      ~pk:key_bls.alias
      client
  in

  Log.info "Trying a valid consensus key update." ;
  let* () =
    Client.update_consensus_key
      ~src:Constant.bootstrap1.alias
      ~pk:key_a.alias
      client
  in
  let* () = Client.bake_for_and_wait client in

  Log.info
    "Invalid update: changing the consensus key to an active consensus key \
     (set)" ;
  let* () =
    Client.update_consensus_key
      ~expect_failure:true
      ~src:Constant.bootstrap2.alias
      ~pk:key_a.alias
      client
  in

  Log.info "Register a delegate with a consensus key." ;
  let* () = Client.register_key ~consensus:key_c.alias key_b.alias client in
  let* () = Client.bake_for_and_wait client in

  let* () =
    if manual_staking then (
      Log.info "Add stake for `key_b` so that `key_c` can bake later on." ;
      Client.transfer
        ~entrypoint:"stake"
        ~burn_cap:Tez.one
        ~amount:(Tez.of_int 500_000)
        ~giver:key_b.alias
        ~receiver:key_b.alias
        client)
    else return ()
  in

  Log.info "Bake until the end of the next cycle with bootstrap1..." ;
  let* () =
    bake_n_cycles preserved_cycles ~keys:[Constant.bootstrap1.alias] client
  in

  Log.info "Bootstrap1 should not be able to bake anymore..." ;
  let* () =
    Client.bake_for
      ~expect_failure:true
      ~keys:[Constant.bootstrap1.alias]
      client
  in

  let* () =
    if manual_staking then (
      Log.info "`key_c` cannot bake yet." ;
      Client.bake_for ~expect_failure:true ~keys:[key_c.alias] client)
    else return ()
  in

  Log.info "... while `key_a` is able to bake." ;
  let* () = Client.bake_for_and_wait ~keys:[key_a.alias] client in

  Log.info "Bake until the end of the next cycle, again." ;
  let* () =
    bake_n_cycles preserved_cycles ~keys:[Constant.bootstrap2.alias] client
  in

  Log.info "`key_c` is now able to bake as well." ;
  let* () = Client.bake_for_and_wait ~keys:[key_c.alias] client in

  Log.info "Switch back to the initial consensus key." ;
  let* () =
    Client.update_consensus_key
      ~src:Constant.bootstrap1.alias
      ~pk:Constant.bootstrap1.alias
      client
  in
  let* () =
    Client.update_consensus_key ~src:key_b.alias ~pk:key_b.alias client
  in

  Log.info "Bake until the end of the next cycle..." ;
  let* () = bake_n_cycles (preserved_cycles + 1) ~keys:[key_a.alias] client in

  Log.info "We are not able to bake with `key_a` nor `key_c` anymore..." ;
  let* () = Client.bake_for ~expect_failure:true ~keys:[key_a.alias] client in
  let* () = Client.bake_for ~expect_failure:true ~keys:[key_c.alias] client in

  Log.info "... but are able to bake again with `bootstrap1` and `key_b`..." ;
  let* () = Client.bake_for_and_wait ~keys:[Constant.bootstrap1.alias] client in
  let* () = Client.bake_for_and_wait ~keys:[key_b.alias] client in

  Log.info
    "Update the consensus key of bootstrap3 and bootstrap4 to `key_a` and \
     `key_c`, respectively..." ;
  let* () =
    Client.update_consensus_key
      ~src:Constant.bootstrap3.alias
      ~pk:key_a.alias
      client
  in
  let* () =
    Client.update_consensus_key
      ~src:Constant.bootstrap4.alias
      ~pk:key_c.alias
      client
  in

  Log.info "Bake until the end of the next cycle..." ;
  let* () =
    bake_n_cycles
      (preserved_cycles + 1)
      ~keys:[Constant.bootstrap1.alias]
      client
  in

  Log.info "Invalid drain: unregistered delegate." ;
  let* () =
    Client.drain_delegate
      ~expect_failure:true
      ~delegate:destination.alias
      ~consensus_key:destination.alias
      ~destination:destination.alias
      client
  in

  Log.info "Invalid drain: bootstrap2 is has no custom consensus key." ;
  let* () =
    Client.drain_delegate
      ~expect_failure:true
      ~delegate:Constant.bootstrap2.alias
      ~consensus_key:Constant.bootstrap2.alias
      ~destination:destination.alias
      client
  in

  Log.info "Invalid drain: bootstrap2 is not the consensus key for bootstrap1." ;
  let* () =
    Client.drain_delegate
      ~expect_failure:true
      ~delegate:Constant.bootstrap1.alias
      ~consensus_key:Constant.bootstrap2.alias
      ~destination:destination.alias
      client
  in

  Log.info "Invalid drain: cannot drain to itself." ;
  let* () =
    Client.drain_delegate
      ~expect_failure:true
      ~delegate:Constant.bootstrap4.alias
      ~consensus_key:key_c.alias
      ~destination:Constant.bootstrap4.alias
      client
  in

  Log.info "Invalid drain: there is nothing drain to itself." ;
  let* () =
    let delegate = Constant.bootstrap4.alias in
    let* balance = Client.get_balance_for ~account:delegate client in
    let* () =
      Client.transfer
        ~fee:Tez.zero
        ~amount:balance
        ~giver:delegate
        ~receiver:key_c.alias
        client
    in
    let* () = Client.bake_for_and_wait client in
    Client.drain_delegate
      ~expect_failure:true
      ~delegate:Constant.bootstrap4.alias
      ~consensus_key:key_c.alias
      ~destination:Constant.bootstrap4.alias
      client
  in

  Log.info "Inject a valid drain..." ;
  let* () =
    Client.drain_delegate
      ~delegate:Constant.bootstrap4.alias
      ~consensus_key:key_c.alias
      ~destination:destination.alias
      client
  in
  Log.info
    "Check that after a drain, the mempool rejects a manager operation from \
     the same manager..." ;
  let* () =
    Client.transfer
      ~expect_failure:true
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 1)
      ~giver:Constant.bootstrap4.alias
      ~receiver:Constant.bootstrap5.alias
      client
  in
  Log.info "Bake and check the effects of the valid drain..." ;
  let* old_balance = Client.get_balance_for ~account:destination.alias client in
  let* () = Client.bake_for_and_wait ~keys:[Constant.bootstrap1.alias] client in
  let* new_balance4 =
    Client.get_balance_for ~account:Constant.bootstrap4.alias client
  in
  Check.(new_balance4 = Tez.zero)
    Tez.typ
    ~error_msg:"Drained account should be empty but its balance is %L." ;
  let* new_balance = Client.get_balance_for ~account:destination.alias client in
  Check.(old_balance < new_balance)
    Tez.typ
    ~error_msg:"Destination account of the drain has not been credited." ;

  Log.info
    "Check that a drain replaces a manager operation from the same delegate..." ;
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 1)
      ~giver:Constant.bootstrap3.alias
      ~receiver:Constant.bootstrap5.alias
      client
  in
  let* () =
    Client.drain_delegate
      ~delegate:Constant.bootstrap3.alias
      ~consensus_key:key_a.alias
      ~destination:destination.alias
      client
  in
  let* () =
    let* json =
      RPC.get_chain_mempool_pending_operations () |> RPC.Client.call client
    in
    let replaced_op = JSON.(json |-> "outdated" |> geti 0) in
    let replaced_op_kind =
      JSON.(replaced_op |-> "contents" |> geti 0 |-> "kind" |> as_string)
    in
    Check.((replaced_op_kind = "transaction") string)
      ~error_msg:
        "Expected the replaced transaction to be in the outdated pool, but \
         instead found %L." ;
    let replaced_op_err =
      JSON.(replaced_op |-> "error" |> geti 0 |-> "id" |> as_string)
    in
    Check.((replaced_op_err = "prevalidation.operation_replacement") string)
      ~error_msg:
        "The replaced transaction has an unexpected error (expected %R, got \
         %L)." ;
    Lwt.return_unit
  in
  let* old_balance = Client.get_balance_for ~account:destination.alias client in
  let* old_balance5 =
    Client.get_balance_for ~account:Constant.bootstrap5.alias client
  in
  let* () = Client.bake_for_and_wait ~keys:[Constant.bootstrap1.alias] client in
  let* new_balance3 =
    Client.get_balance_for ~account:Constant.bootstrap3.alias client
  in
  Check.(new_balance3 = Tez.zero)
    Tez.typ
    ~error_msg:"Drained account should be empty but its balance is %L." ;
  let* new_balance = Client.get_balance_for ~account:destination.alias client in
  Check.(old_balance < new_balance)
    Tez.typ
    ~error_msg:"Destination account of the drain has not been credited." ;
  let* new_balance5 =
    Client.get_balance_for ~account:Constant.bootstrap5.alias client
  in
  Check.(old_balance5 = new_balance5)
    Tez.typ
    ~error_msg:"Destination of the transaction has been credited." ;

  unit

let update_consensus_key ?(expect_failure = false)
    ?(baker = Constant.bootstrap1.alias) ~(src : Account.key)
    ~(consensus_key : Account.key) client =
  Log.info "Set consensus_key of %s to %s" src.alias consensus_key.alias ;
  let* () =
    Client.update_consensus_key
      ~hooks
      ~expect_failure
      ~src:src.alias
      ~pk:consensus_key.alias
      client
  in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_delegate src.public_key_hash
  in
  let* () = Client.bake_for_and_wait ~keys:[baker] client in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_delegate src.public_key_hash
  in
  let* () = bake_n_cycles (preserved_cycles + 1) client in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_delegate src.public_key_hash
  in
  unit

let test_consensus_key_update ?(expect_failure = false)
    ?(baker = Constant.bootstrap1.alias) ~(src : Account.key)
    ~(consensus_key : Account.key) client =
  (* Update the consensus key and go past [preserved_cycles + 1] *)
  let* () =
    update_consensus_key ~expect_failure ~baker ~src ~consensus_key client
  in
  (* Check that one can bake with the updated consensus key *)
  let* () = Client.bake_for_and_wait ~keys:[consensus_key.alias] client in
  unit

let drain_delegate ~delegate ~consensus_key ~destination
    ?(expect_failure = false) ?(baker = Constant.bootstrap1.alias) client =
  let* () =
    Client.drain_delegate
      ~hooks
      ~expect_failure
      ~delegate
      ~consensus_key
      ~destination
      client
  in
  Client.bake_for_and_wait ~keys:[baker] client

type consensus_key = {
  active_consensus_key : string;
  (* Pending consensus keys per cycle *)
  pending_consensus_keys : (int * string) list;
}

let consensus_key_typ : consensus_key Check.typ =
  Check.(
    convert
      (fun {active_consensus_key; pending_consensus_keys} ->
        (active_consensus_key, pending_consensus_keys))
      (tuple2 string (list (tuple2 int string))))

let get_consensus_key client (delegate : Account.key) : consensus_key Lwt.t =
  let* delegate_json =
    RPC.Client.call client
    @@ RPC.get_chain_block_context_delegate delegate.public_key_hash
  in
  return
    JSON.
      {
        active_consensus_key =
          delegate_json |-> "active_consensus_key" |> as_string;
        pending_consensus_keys =
          delegate_json |-> "pending_consensus_keys" |> as_list
          |> List.map (fun pending_key ->
                 ( pending_key |-> "cycle" |> as_int,
                   pending_key |-> "pkh" |> as_string ));
      }

let check_consensus_key ~__LOC__ delegate ?(expected_active = delegate)
    ?(expected_pending = []) client =
  let* consensus_key = get_consensus_key client delegate in
  let expected_consensus_key =
    {
      active_consensus_key = expected_active.public_key_hash;
      pending_consensus_keys =
        List.map
          (fun (cycle, (account : Account.key)) ->
            (cycle, account.public_key_hash))
          expected_pending;
    }
  in
  Check.(
    (consensus_key = expected_consensus_key)
      consensus_key_typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  unit

let register_key_as_delegate ?(expect_failure = false)
    ?(baker = Constant.bootstrap1.alias) ~(owner : Account.key)
    ~(consensus_key : Account.key) client =
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_contract ~id:consensus_key.public_key_hash ()
  in
  let* () =
    Client.register_key
      ~hooks
      ~expect_failure
      ~consensus:consensus_key.alias
      owner.alias
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[baker] client in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_delegate owner.public_key_hash
  in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_contract ~id:consensus_key.public_key_hash ()
  in
  (* Wait for consensus key to be active *)
  let* () = bake_n_cycles (preserved_cycles + 1) client in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_delegate owner.public_key_hash
  in
  unit

let transfer ?hooks ?(expect_failure = false)
    ?(baker = Constant.bootstrap1.alias) ~source ~destination ~amount client =
  let* () =
    Client.transfer
      ?hooks
      ~expect_failure
      ~burn_cap:Tez.one
      ~giver:source
      ~receiver:destination
      ~amount
      client
  in
  Client.bake_for_and_wait ~keys:[baker] client

let register ?(regression = true) title test =
  Protocol.(if regression then register_regression_test else register_test)
    ~__FILE__
    ~title
    ~tags:["consensus_key"]
  @@ fun protocol ->
  let parameters =
    (* we update paramaters for faster testing: no need to wait
       5 cycles for the consensus key to activate. *)
    [
      (["blocks_per_cycle"], `Int blocks_per_cycle);
      (["nonce_revelation_threshold"], `Int 2);
      (["preserved_cycles"], `Int preserved_cycles);
    ]
  in
  let* parameter_file =
    Protocol.write_parameter_file ~base:(Right (protocol, None)) parameters
  in
  let* _, client =
    Client.init_with_protocol ~parameter_file ~protocol `Client ()
  in
  let baker_0 = Constant.bootstrap1 in
  let baker_1 = Constant.bootstrap2 in
  let* account_0 = Client.gen_and_show_keys ~alias:"dummy_account_0" client in
  let* account_1 = Client.gen_and_show_keys ~alias:"dummy_account_1" client in
  test client baker_0 baker_1 account_0 account_1

let test_register_delegate_with_consensus_key ?(expect_failure = false)
    ?(baker = Constant.bootstrap1.alias) ~(new_delegate : Account.key)
    ~(new_consensus_key : Account.key) client =
  let* () =
    transfer
      ~source:baker
      ~destination:new_delegate.alias
      ~amount:(Tez.of_int 1_000_000)
      ~baker
      client
  in
  let* () =
    register_key_as_delegate
      ~expect_failure
      ~owner:new_delegate
      ~consensus_key:new_consensus_key
      ~baker
      client
  in
  (* Check that one can bake with the consensus key *)
  let* () = Client.bake_for_and_wait ~keys:[new_consensus_key.alias] client in
  unit

(* Like [register_key_as_delegate] this function registers [owner] as a delegate,
   sets its consensus key to [consensus_key] and bakes until the
   consensus_key takes effect. Unlike [register_key_as_delegate], this
   does not store a regression trace. Instead, it [Check]s that the new
   consensus key is as expected. *)
let register_key_as_delegate_no_reg ?(baker = Constant.bootstrap1.alias)
    ~(owner : Account.key) ~(consensus_key : Account.key) client =
  let* () =
    Client.register_key ~consensus:consensus_key.alias owner.alias client
  in
  let* level_information = Helpers.get_current_level client in
  let* () = Client.bake_for_and_wait ~keys:[baker] client in
  let* () =
    check_consensus_key
      ~__LOC__
      owner
      ~expected_pending:
        [(level_information.cycle + preserved_cycles + 1, consensus_key)]
      client
  in
  (* Wait for consensus key to be active *)
  let* () = bake_n_cycles (preserved_cycles + 1) client in
  check_consensus_key ~__LOC__ owner ~expected_active:consensus_key client

(* Like [update_consensus_key] this function updates the consensus key
   of [src] to [consensus_key] and bakes until the new [consensus_key]
   takes effect. Unlike [update_consensus_key] this does does not
   store a regression trace. Instead, it [Check]s that the new
   consensus key is as expected. *)
let update_consensus_key_no_reg ?(baker = Constant.bootstrap1.alias)
    ~(src : Account.key) ~(consensus_key : Account.key) ~expected_active client
    =
  let* () =
    Client.update_consensus_key ~src:src.alias ~pk:consensus_key.alias client
  in
  let* level_information = Helpers.get_current_level client in
  let* () = Client.bake_for_and_wait ~keys:[baker] client in
  let* () =
    check_consensus_key
      ~__LOC__
      src
      ~expected_active
      ~expected_pending:
        [(level_information.cycle + preserved_cycles + 1, consensus_key)]
      client
  in
  let* () = bake_n_cycles (preserved_cycles + 1) client in
  check_consensus_key
    ~__LOC__
    consensus_key
    ~expected_active:consensus_key
    client

let test_revert_to_unique_consensus_key ?(baker = Constant.bootstrap1.alias)
    ~(new_delegate : Account.key) ~(new_consensus_key : Account.key) client =
  (* Set a new consensus key *)
  Log.info "Transfer 1_000_000 tez from baker to new_delegate" ;
  let* () =
    transfer
      ~source:baker
      ~destination:new_delegate.alias
      ~amount:(Tez.of_int 1_000_000)
      ~baker
      client
  in
  let* () =
    Log.info "Register as delegate with consensus key" ;
    register_key_as_delegate_no_reg
      ~owner:new_delegate
      ~consensus_key:new_consensus_key
      ~baker
      client
  in
  let* () =
    Log.info "Check that the new consensus key can bake" ;
    Client.bake_for_and_wait ~keys:[new_consensus_key.alias] client
  in
  let* () =
    Log.info "Revert the consensus key" ;
    update_consensus_key_no_reg
      ~src:new_delegate
      ~consensus_key:new_delegate
      ~expected_active:new_consensus_key
      client
  in
  let* () =
    Log.info "Check that the new delegate can bake" ;
    Client.bake_for_and_wait ~keys:[new_delegate.alias] client
  in
  unit

let test_drain_delegate_1 ?(baker = Constant.bootstrap1.alias)
    ~(delegate : Account.key) ~(consensus : Account.key)
    ~(destination : Account.key) client =
  let* () =
    update_consensus_key ~baker ~src:delegate ~consensus_key:consensus client
  in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_delegate delegate.public_key_hash
  in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_contract_balance
         ~id:delegate.public_key_hash
         ()
  in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_contract_balance
         ~id:consensus.public_key_hash
         ()
  in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_contract_balance
         ~id:destination.public_key_hash
         ()
  in
  (* Goto beginning of cycle to ensure a baking slot is available *)
  let* () =
    drain_delegate
      ~delegate:delegate.alias
      ~consensus_key:consensus.alias
      ~destination:destination.alias
      ~baker:consensus.alias
        (* In case delegate = baker, use the consensus key updated above *)
      client
  in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_delegate delegate.public_key_hash
  in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_contract_balance
         ~id:delegate.public_key_hash
         ()
  in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_contract_balance
         ~id:consensus.public_key_hash
         ()
  in
  let* _ =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_contract_balance
         ~id:destination.public_key_hash
         ()
  in
  unit

let register ~protocols =
  let () = test_update_consensus_key protocols in
  let () =
    register
      "Test set consensus key - baker is not delegate"
      (fun client baker_0 baker_1 account_0 _account_1 ->
        let baker_0_consensus_key = account_0 in
        let* () =
          test_consensus_key_update
            ~src:baker_0
            ~consensus_key:baker_0_consensus_key
            ~baker:baker_1.alias
            client
        in
        unit)
      protocols
  in
  let () =
    register
      "Test set consensus key - baker is delegate"
      (fun client baker_0 _baker_1 account_0 _account_1 ->
        let baker_0_consensus_key = account_0 in
        let* () =
          test_consensus_key_update
            ~src:baker_0
            ~consensus_key:baker_0_consensus_key
            ~baker:baker_0.alias
            client
        in
        unit)
      protocols
  in
  let () =
    register
      "Test register with consensus key"
      (fun client baker_0 _baker_1 account_0 account_1 ->
        let new_delegate = account_0 in
        let new_consensus_key = account_1 in
        test_register_delegate_with_consensus_key
          ~baker:baker_0.alias
          ~new_delegate
          ~new_consensus_key
          client)
      protocols
  in
  let () =
    register
      "Test revert to unique consensus key"
      ~regression:false
      (fun client baker_0 _baker_1 account_0 account_1 ->
        let new_delegate = account_0 in
        let new_consensus_key = account_1 in
        test_revert_to_unique_consensus_key
          ~baker:baker_0.alias
          ~new_delegate
          ~new_consensus_key
          client)
      protocols
  in
  let () =
    register
      "Test drain delegate with (baker = delegate & consensus = destination)"
      (fun client baker_0 _baker_1 account_0 _account_1 ->
        let delegate = baker_0 in
        let consensus = account_0 in
        let destination = account_0 in
        test_drain_delegate_1
          ~baker:baker_0.alias
          ~delegate
          ~consensus
          ~destination
          client)
      protocols
  in
  let () =
    register
      "Test drain delegate with (baker = delegate & consensus <> destination)"
      (fun client baker_0 _baker_1 account_0 account_1 ->
        let delegate = baker_0 in
        let consensus = account_0 in
        let destination = account_1 in
        test_drain_delegate_1
          ~baker:baker_0.alias
          ~delegate
          ~consensus
          ~destination
          client)
      protocols
  in
  let () =
    register
      "Test drain delegate with (baker <> delegate & consensus = destination)"
      (fun client baker_0 baker_1 account_0 _account_1 ->
        let delegate = baker_0 in
        let consensus = account_0 in
        let destination = account_0 in
        test_drain_delegate_1
          ~baker:baker_1.alias
          ~delegate
          ~consensus
          ~destination
          client)
      protocols
  in
  let () =
    register
      "Test drain delegate with (baker <> delegate & consensus <> destination)"
      (fun client baker_0 baker_1 account_0 account_1 ->
        let delegate = baker_0 in
        let consensus = account_0 in
        let destination = account_1 in
        test_drain_delegate_1
          ~baker:baker_1.alias
          ~delegate
          ~consensus
          ~destination
          client)
      protocols
  in
  ()
