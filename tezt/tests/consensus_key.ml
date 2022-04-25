(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 G.B. Fefe <g.b.fefe@protonmail.com>                    *)
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

module Helpers = struct
  type level = {
    level : int;
    level_position : int;
    cycle : int;
    cycle_position : int;
    expected_commitment : bool;
  }

  let level_type : level Check.typ =
    Check.convert
      (fun {level; level_position; cycle; cycle_position; expected_commitment} ->
        (level, level_position, cycle, cycle_position, expected_commitment))
      Check.(tuple5 int int int int bool)

  let decode_level json =
    let level = JSON.(json |-> "level" |> as_int) in
    let level_position = JSON.(json |-> "level_position" |> as_int) in
    let cycle = JSON.(json |-> "cycle" |> as_int) in
    let cycle_position = JSON.(json |-> "cycle_position" |> as_int) in
    let expected_commitment =
      JSON.(json |-> "expected_commitment" |> as_bool)
    in
    {level; level_position; cycle; cycle_position; expected_commitment}

  let get_current_level client =
    let* json = RPC.get_current_level client in
    return (decode_level json)

  let check_current_level client expected_level =
    let* level = get_current_level client in
    Check.((level = expected_level) level_type)
      ~error_msg:"expected current_period = %R, got %L" ;
    unit

  let bake_and_wait_block node client =
    let* level_json = RPC.get_current_level client in
    let level = JSON.(level_json |-> "level" |> as_int) in
    let* () =
      Client.bake_for ~context_path:(Node.data_dir node // "context") client
    in
    let* _i = Node.wait_for_level node (level + 1) in
    Lwt.return_unit
end

open Helpers

let test_update_consensus_key =
  Protocol.register_test
    ~__FILE__
    ~title:"update consensus key"
    ~tags:["consensus_key"]
  @@ fun protocol ->
  let parameters =
    (* we update paramaters for faster testing: no need to wait
       5 cycles for the consensus key to activate. *)
    [
      (["blocks_per_cycle"], Some "4");
      (["nonce_revelation_threshold"], Some "2");
      (["preserved_cycles"], Some "1");
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

  Log.info "Bake until the end of the next cycle..." ;
  let* () = repeat 5 (fun () -> Client.bake_for_and_wait client) in
  let* () =
    check_current_level
      client
      {
        level = 8;
        level_position = 7;
        cycle = 1;
        cycle_position = 3;
        expected_commitment = true;
      }
  in

  Log.info "Bootstrap1 should not be able to bake anymore..." ;
  let* () =
    Client.bake_for
      ~expect_failure:true
      ~keys:[Constant.bootstrap1.alias]
      client
  in

  Log.info "... while `key_a` and `key_c` are able to bake." ;
  let* () = Client.bake_for_and_wait ~keys:[key_a.alias] client in
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
  let* () =
    repeat 6 (fun () -> Client.bake_for_and_wait ~keys:[key_a.alias] client)
  in
  let* () =
    check_current_level
      client
      {
        level = 16;
        level_position = 15;
        cycle = 3;
        cycle_position = 3;
        expected_commitment = true;
      }
  in

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
    repeat 6 (fun () ->
        Client.bake_for_and_wait ~keys:[Constant.bootstrap1.alias] client)
  in
  let* () =
    check_current_level
      client
      {
        level = 24;
        level_position = 23;
        cycle = 5;
        cycle_position = 3;
        expected_commitment = true;
      }
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

  Log.info "Drain bootstrap4 to key_c..." ;
  let* old_balance = Client.get_balance_for ~account:destination.alias client in
  let* old_balance5 =
    Client.get_balance_for ~account:Constant.bootstrap5.alias client
  in
  let* () =
    Client.drain_delegate
      ~delegate:Constant.bootstrap4.alias
      ~consensus_key:key_c.alias
      ~destination:destination.alias
      client
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 1)
      ~giver:Constant.bootstrap4.alias
      ~receiver:Constant.bootstrap5.alias
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[Constant.bootstrap1.alias] client in
  Log.info
    "Check that other manager operations are not included after a drain..." ;
  let* () =
    let* json =
      RPC.get_chain_mempool_pending_operations () |> RPC.Client.call client
    in
    let delayed_op_kind =
      JSON.(
        json |-> "branch_delayed" |> geti 0 |> geti 1 |-> "contents" |> geti 0
        |-> "kind" |> encode)
    in
    Check.((delayed_op_kind = "\"transaction\"") string)
      ~error_msg:
        "The transaction is not in the branch_delayed pool (expected %R, got \
         %L)" ;
    Lwt.return_unit
  in
  Log.info "The manager account has been drained..." ;
  let* b = Client.get_balance_for ~account:Constant.bootstrap4.alias client in
  Check.((Tez.to_mutez b = 0) int) ~error_msg:"Manager balance is not empty" ;
  let* new_balance = Client.get_balance_for ~account:destination.alias client in
  Check.((Tez.to_mutez old_balance < Tez.to_mutez new_balance) int)
    ~error_msg:"Destination account has not been credited" ;
  let* new_balance5 =
    Client.get_balance_for ~account:Constant.bootstrap5.alias client
  in
  Check.((Tez.to_mutez new_balance5 = Tez.to_mutez old_balance5) int)
    ~error_msg:"Manager operation was included" ;

  Log.info
    "Check that a drain conflicts with (ie is not included after) a manager \
     operation of the same delegate..." ;
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
      ~expect_failure:true
      ~delegate:Constant.bootstrap3.alias
      ~consensus_key:key_a.alias
      ~destination:destination.alias
      client
  in
  let* old_balance3 =
    Client.get_balance_for ~account:Constant.bootstrap3.alias client
  in
  let* old_balance5 =
    Client.get_balance_for ~account:Constant.bootstrap5.alias client
  in
  let* old_balance = Client.get_balance_for ~account:destination.alias client in

  let* () = Client.bake_for_and_wait ~keys:[Constant.bootstrap1.alias] client in

  let* new_balance5 =
    Client.get_balance_for ~account:Constant.bootstrap5.alias client
  in
  let* new_balance = Client.get_balance_for ~account:destination.alias client in
  Check.((Tez.to_mutez old_balance = Tez.to_mutez new_balance) int)
    ~error_msg:"Drain operation was included (destination balance changed)" ;
  Check.((Tez.to_mutez old_balance3 > 0) int)
    ~error_msg:
      "Drain operation was included (delegate balance changed: old %L versus \
       new %R)" ;
  Check.((Tez.to_mutez old_balance5 < Tez.to_mutez new_balance5) int)
    ~error_msg:"Destination account has not been credited" ;

  unit

let register ~protocols = test_update_consensus_key protocols
