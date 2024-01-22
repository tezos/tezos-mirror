(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Tenderbake
   Invocation:   dune exec tezt/tests/main.exe -- --file tenderbake.ml
   Subject:      Basic test for Tenderbake and related newly added API components
*)

(* ------------------------------------------------------------------------- *)
(* Typedefs *)

let transfer_data =
  (Constant.bootstrap1.alias, Tez.one, Constant.bootstrap2.alias)

let baker = Constant.bootstrap5.alias

let default_overrides =
  [(* ensure that blocks must be attested *) (["consensus_threshold"], `Int 6)]

let init ?(overrides = default_overrides) protocol =
  let* sandbox_node = Node.init [Synchronisation_threshold 0; Private_mode] in
  let sandbox_endpoint = Client.Node sandbox_node in
  let* sandbox_client = Client.init ~endpoint:sandbox_endpoint () in
  let* parameter_file =
    let base = Either.Right (protocol, None) in
    Protocol.write_parameter_file ~base overrides
  in
  let* () =
    (* activate in the past - let timestamp_delay be the default value of 1 year *)
    Client.activate_protocol ~protocol sandbox_client ~parameter_file
  in
  Log.info "Activated protocol." ;
  return
  @@ ( Tezos_crypto.Hashed.Protocol_hash.of_b58check_exn (Protocol.hash protocol),
       sandbox_endpoint,
       sandbox_client,
       sandbox_node )

let bootstrap_accounts = List.tl Constant.all_secret_keys

let attesters =
  [
    Constant.bootstrap1.alias;
    Constant.bootstrap2.alias;
    Constant.bootstrap3.alias;
    Constant.bootstrap4.alias;
    Constant.bootstrap5.alias;
  ]

let attest endpoint protocol client attesters =
  Client.attest_for ~endpoint ~protocol ~key:attesters ~force:true client

let preattest endpoint protocol client preattesters =
  Client.preattest_for ~endpoint ~protocol ~key:preattesters ~force:true client

let test_bake_two =
  Protocol.register_test
    ~__FILE__
    ~title:"Tenderbake transfer - baking 2"
    ~tags:["baking"; "tenderbake"]
  @@ fun protocol ->
  let* _proto_hash, endpoint, client, _node = init protocol in
  let end_idx = List.length bootstrap_accounts in
  let rec loop i =
    if i = end_idx then Lwt.return_unit
    else
      let baker =
        [
          (List.nth bootstrap_accounts i).alias;
          (List.nth bootstrap_accounts ((i + 3) mod end_idx)).alias;
        ]
      in
      let amount = Tez.of_int (i + 1) in
      let giver = (List.nth bootstrap_accounts ((i + 1) mod end_idx)).alias in
      let receiver =
        (List.nth bootstrap_accounts ((i + 2) mod end_idx)).alias
      in
      Log.info "Phase %d" i ;
      let* () = Client.transfer ~amount ~giver ~receiver client in
      let* () =
        Client.bake_for_and_wait ~endpoint ~protocol ~keys:baker client
      in
      loop (i + 1)
  in
  loop 0

let test_low_level_commands =
  Protocol.register_test
    ~__FILE__
    ~title:"Tenderbake low level commands"
    ~tags:["propose"; "attest"; "preattest"; "tenderbake"; "low_level"]
  @@ fun protocol ->
  let* _proto_hash, endpoint, client, _node = init protocol in
  Log.info "Doing a propose -> preattest -> attest cycle" ;
  let proposer = attesters in
  let preattesters = attesters in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 3)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      client
  in
  let* () = Client.propose_for client ~protocol ~endpoint ~key:proposer in
  let* () = preattest endpoint protocol client preattesters in
  let* () = attest endpoint protocol client attesters in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 2)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      client
  in
  let* () = Client.propose_for client ~protocol ~endpoint ~key:proposer in
  let* () = preattest endpoint protocol client preattesters in
  let* () = attest endpoint protocol client attesters in
  let* () = Client.propose_for client ~protocol ~endpoint ~key:proposer in
  Lwt.return_unit

let find_account public_key_hash' =
  match
    Array.find_opt
      (fun Account.{public_key_hash; _} ->
        String.equal public_key_hash public_key_hash')
      Account.Bootstrap.keys
  with
  | Some v -> v
  | None ->
      Test.fail
        "Could not find a bootstrap account with public_key_hash %s"
        public_key_hash'

let baker_at_round0 ?level client =
  let* json =
    Client.RPC.call client @@ RPC.get_chain_block_helper_baking_rights ?level ()
  in
  match JSON.(json |=> 0 |-> "delegate" |> as_string_opt) with
  | Some delegate_id -> return (find_account delegate_id)
  | None ->
      Test.fail
        "Could not find the baker at round 0 at level %d"
        (Option.value ~default:0 level)

let recipient_initial_balance = Tez.of_int 4_000_000

let find_bootstrap_account_not_in unwanted =
  match
    Array.find_opt
      (fun acct -> not @@ List.mem acct unwanted)
      Account.Bootstrap.keys
  with
  | Some v -> v
  | None ->
      Test.fail
        "Could not find a bootstrap account not in {%s}"
        (String.concat ", "
        @@ List.map (fun Account.{alias; _} -> alias) unwanted)

(* Run one node, and bake, preattest, and attest using the
   client commands. *)
let test_manual_bake =
  let minimal_block_delay = 1 in
  Protocol.register_test
    ~__FILE__
    ~title:"Tenderbake manual bake"
    ~tags:
      ["propose"; "attest"; "preattest"; "tenderbake"; "low_level"; "manual"]
  @@ fun protocol ->
  let* _proto_hash, _endpoint, client, node =
    init
      ~overrides:
        [
          (["minimal_block_delay"], `String_of_int minimal_block_delay);
          (["delay_increment_per_round"], `String_of_int 1);
          (["consensus_threshold"], `Int 45);
          (["consensus_committee_size"], `Int 67);
        ]
      protocol
  in

  Log.info "Choose players" ;
  (* we will make a transfer to someone who is not baking, to simplify
     the computation of the expected balance *)
  let* level2_baker = baker_at_round0 client in
  let* level3_baker = baker_at_round0 ~level:3 client in
  (* recipient should also be different from bootstrap1 because
     bootstrap1 makes the transfer and we don't want a self-transfer *)
  let recipient =
    find_bootstrap_account_not_in
      [Account.Bootstrap.keys.(0); level2_baker; level3_baker]
  in
  Log.info
    "Chose players level2_baker = %s, level3_baker = %s, and recipient = %s "
    level2_baker.alias
    level3_baker.alias
    recipient.alias ;

  Log.info "Bake" ;
  let* () =
    Client.propose_for client ~minimal_timestamp:true ~key:[level2_baker.alias]
  in

  (* Test balance of the receipient takes into account deposits taken at level 2 *)
  Log.info "Deposit" ;
  let* balance = Client.get_balance_for client ~account:recipient.alias in
  let* deposit =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_delegate_frozen_deposits
         recipient.public_key_hash
  in
  Check.(balance = Tez.(recipient_initial_balance - deposit))
    Tez.typ
    ~__LOC__
    ~error_msg:"Expected balance %R, got %L" ;

  Log.info "Wait for level 2" ;
  let* (_ : int) = Node.wait_for_level node 2 in

  Log.info "Preattest" ;
  let attesters =
    [
      Constant.bootstrap1.alias;
      Constant.bootstrap2.alias;
      Constant.bootstrap3.alias;
      Constant.bootstrap4.alias;
    ]
  in
  let* () = Client.preattest_for ~protocol ~key:attesters ~force:true client in

  let giver = "bootstrap1" in
  Log.info "Transfer from giver %s to recipient %s" giver recipient.alias ;
  let transfer_amount = Tez.of_int 500 in
  let fee = Tez.of_mutez_int 100_000 in
  let process =
    Client.spawn_transfer
      ~amount:transfer_amount
      ~giver:"bootstrap1"
      ~receiver:recipient.alias
      ~fee
      client
  in
  let* client_output = Process.check_and_read_stdout process in
  let* operation_hash =
    match client_output =~* rex "Operation hash is '?(o\\w{50})'" with
    | None ->
        Test.fail
          "Cannot extract operation hash from client_output: %s"
          client_output
    | Some hash -> return hash
  in
  Log.info "Transfer injected with operation hash %s" operation_hash ;

  Log.info "Attest" ;
  let* () = Client.attest_for ~protocol ~key:attesters ~force:true client in

  Log.info "Test that %s is pending" operation_hash ;
  let* pending_ops =
    Client.RPC.call client @@ RPC.get_chain_mempool_pending_operations ()
  in
  let op_hashes =
    JSON.(
      pending_ops |-> "applied" |> as_list
      |> List.map (fun op -> op |-> "hash" |> as_string))
  in
  Check.(list_mem string)
    ~__LOC__
    operation_hash
    op_hashes
    ~error_msg:"Expected to find operation_hash %L in pending operations %R" ;

  Log.info "Bake again" ;
  Log.info "Sleep a bit" ;
  let* () = Lwt_unix.sleep (2.0 *. float_of_int minimal_block_delay) in
  let* () =
    Client.propose_for client ~minimal_timestamp:true ~key:[level3_baker.alias]
  in

  Log.info "Wait for level 3" ;
  let* (_ : int) = Node.wait_for_level node 3 in

  Log.info "Test balance" ;
  let* balance = Client.get_balance_for client ~account:recipient.alias in
  let* deposit =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_delegate_frozen_deposits
         recipient.public_key_hash
  in
  Check.(balance = Tez.(recipient_initial_balance + transfer_amount - deposit))
    Tez.typ
    ~__LOC__
    ~error_msg:"Expected balance of recipient to be %R, got %L" ;

  Log.info "Test bake fails" ;
  let bake_should_fail = Client.spawn_bake_for ~keys:["bootstrap1"] client in
  let* std_err =
    Process.check_and_read_stderr ~expect_failure:true bake_should_fail
  in
  if std_err =~! rex "Delegates do not have enough voting power." then
    Test.fail ~__LOC__ "Baking should have failed." ;

  unit

let test_manual_bake_null_threshold =
  let minimal_block_delay = 1 in
  Protocol.register_test
    ~__FILE__
    ~title:"Tenderbake manual bake null threshold"
    ~tags:
      ["propose"; "attest"; "preattest"; "tenderbake"; "low_level"; "manual"]
  @@ fun protocol ->
  let* _proto_hash, _endpoint, client, node =
    init
      ~overrides:
        [
          (["minimal_block_delay"], `String_of_int minimal_block_delay);
          (["delay_increment_per_round"], `String_of_int 1);
          (["consensus_threshold"], `Int 0);
          (["consensus_committee_size"], `Int 67);
        ]
      protocol
  in

  Log.info "Choose players" ;
  (* we will make a transfer to someone who is not baking, to simplify
     the computation of the expected balance *)
  let* level2_baker = baker_at_round0 client in
  let* level3_baker = baker_at_round0 ~level:3 client in
  (* recepient should also be different from bootstrap1 because
     bootstrap1 makes the transfer and we don't want a self-transfer *)
  let recipient =
    find_bootstrap_account_not_in
      [Account.Bootstrap.keys.(0); level2_baker; level3_baker]
  in
  Log.info
    "Chose players level2_baker = %s, level3_baker = %s, and recipient = %s "
    level2_baker.alias
    level3_baker.alias
    recipient.alias ;

  Log.info "Bake" ;
  let* () =
    Client.propose_for client ~minimal_timestamp:true ~key:[level2_baker.alias]
  in

  Log.info "Wait for level 2" ;
  let* (_ : int) = Node.wait_for_level node 2 in

  let giver = "bootstrap1" in
  Log.info "Transfer from giver %s to recipient %s" giver recipient.alias ;
  let transfer_amount = Tez.of_int 500 in
  let* () =
    Client.transfer
      ~amount:transfer_amount
      ~giver
      ~receiver:recipient.alias
      client
  in

  Log.info "Bake again" ;
  let* () =
    Client.propose_for client ~minimal_timestamp:true ~key:[level3_baker.alias]
  in

  Log.info "Wait for level 3" ;
  let* (_ : int) = Node.wait_for_level node 3 in

  Log.info "Test balance" ;
  let* balance = Client.get_balance_for client ~account:recipient.alias in
  let* deposit =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_delegate_frozen_deposits
         recipient.public_key_hash
  in
  Check.(balance = Tez.(recipient_initial_balance + transfer_amount - deposit))
    Tez.typ
    ~__LOC__
    ~error_msg:"Expected balance of recipient to be %R, got %L" ;

  unit

let test_repropose =
  Protocol.register_test
    ~__FILE__
    ~title:"Tenderbake low level repropose"
    ~tags:
      ["propose"; "attest"; "preattest"; "tenderbake"; "low_level"; "repropose"]
  @@ fun protocol ->
  let* _proto_hash, endpoint, client, _node = init protocol in
  Log.info "Doing a propose -> preattest -> attest cycle" ;
  let proposer = attesters in
  let preattesters = attesters in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 3)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      client
  in
  let* () = Client.propose_for client ~protocol ~endpoint ~key:proposer in
  let* () = preattest endpoint protocol client preattesters in
  let* () = attest endpoint protocol client attesters in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 2)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      client
  in
  let* () = Client.propose_for client ~protocol ~endpoint ~key:proposer in
  let sleeping_time = 5. in
  Log.debug "Waiting %.0fs so that the previous round ends" sleeping_time ;
  let* () = Lwt_unix.sleep sleeping_time in
  let* () = Client.propose_for client ~protocol ~endpoint ~key:proposer in
  Lwt.return_unit

let register ~protocols =
  test_bake_two protocols ;
  test_low_level_commands protocols ;
  test_manual_bake protocols ;
  test_manual_bake_null_threshold protocols ;
  test_repropose protocols
