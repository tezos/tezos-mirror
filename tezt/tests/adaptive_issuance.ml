(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Adaptive Issuance
   Invocation:   dune exec tezt/tests/main.exe -- --file adaptive_issuance.ml
   Subject:      Basic test for Adaptive Issuance and related newly added API components
*)

(* ------------------------------------------------------------------------- *)
let blocks_per_cycle = 4

let nonce_revelation_threshold = 2

let bootstrap_accounts = Constant.all_secret_keys

module Helpers = struct
  let level_type : RPC.level Check.typ =
    Check.convert
      (fun RPC.
             {level; level_position; cycle; cycle_position; expected_commitment}
         -> (level, level_position, cycle, cycle_position, expected_commitment))
      Check.(tuple5 int int int int bool)

  let get_current_level client =
    Client.RPC.call client @@ RPC.get_chain_block_helper_current_level ()

  let check_current_level client expected_level =
    let* level = get_current_level client in
    Check.((level = expected_level) level_type)
      ~error_msg:"expected current_period = %R, got %L" ;
    unit

  let bake ?ai_vote
      ?(keys = List.map (fun x -> x.Account.alias) bootstrap_accounts) ~endpoint
      ~protocol client =
    Client.bake_for
      ~endpoint
      ~minimal_timestamp:true
      ~protocol
      ~keys
      client
      ?ai_vote

  let bake_n_cycles bake ?keys n client =
    let* current_level = get_current_level client in
    let current_level = current_level.level in
    let nb_baked_blocks_in_cycle = current_level mod blocks_per_cycle in
    let nb_blocks_to_bake = (n * blocks_per_cycle) - nb_baked_blocks_in_cycle in
    Log.info
      "Bake %d cycle(s) (from level %d to %d)"
      n
      current_level
      (current_level + nb_blocks_to_bake) ;
    repeat nb_blocks_to_bake (fun () -> bake ?keys client)
end

let log_step counter msg =
  let color = Log.Color.(bold ++ FG.blue) in
  let prefix = "step" ^ string_of_int counter in
  Log.info ~color ~prefix msg

(* Matches events where the message is of the form:
     "double baking evidence injected <operation_hash>".
     For example:

      "event": {
        "double_baking_denounced.v0": {
          "hash": "onkfjSun49iRrGtuN9FwtiCqDAEgzPKzg1BSa7BSHnaAkButUxx",
          "bytes": "..."
        }
      }
*)
let wait_for_denunciation accuser =
  let filter json = JSON.(json |-> "hash" |> as_string_opt) in
  Accuser.wait_for accuser "double_baking_denounced.v0" filter

(* Matches events which contain an injection request.
   For example:

   "event": {
     "node_prevalidator.v0": [
       "2020-09-11T12:32:05.353-00:00",
       {
         "event": {
           "request": {
             "request": "inject",
             "operation": {
               "branch": "BM3J62AvjnjJKfinoq1op2uw5Hdn3YGMQmusnLdrfCd1yrpftG2",
               "data": "030000...00000"
             }
           },
           "status": {
             "pushed": "2020-09-11T12:32:05.343-00:00",
             "treated": 4.5947e-05,
             "completed": 0.009614550999999999
           }
         },
         "level": "info"
       }
     ]
   }
*)
let wait_for_denunciation_injection node client accuser =
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when s = "inject" -> Some s
    | Some _ | None -> None
  in
  let denunciation_event = wait_for_denunciation accuser in
  let* _ = Node.wait_for node "request_completed_info.v0" filter in
  let* oph = denunciation_event in
  let* mempool = Mempool.get_mempool client in
  if List.mem oph mempool.validated then return oph
  else Test.fail "the denunciation operation was rejected by the mempool"

let default_overrides =
  [
    (* Shorter cycles *)
    (["blocks_per_cycle"], `Int blocks_per_cycle);
    (["nonce_revelation_threshold"], `Int nonce_revelation_threshold);
  ]

let launch_ema_threshold client =
  let* json =
    Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
  in
  Lwt.return
  @@ JSON.(json |-> "adaptive_issuance_launch_ema_threshold" |> as_int)

let init ?(overrides = default_overrides) protocol =
  let* sandbox_node = Node.init [Synchronisation_threshold 0; Private_mode] in
  let sandbox_endpoint = Client.Node sandbox_node in
  let* sandbox_client = Client.init ~endpoint:sandbox_endpoint () in
  let* parameter_file =
    let base = Either.Right (protocol, None) in
    Protocol.write_parameter_file ~base overrides
  in
  let* () = Client.activate_protocol ~protocol sandbox_client ~parameter_file in
  Log.info "Activated protocol." ;
  return
  @@ ( Tezos_crypto.Hashed.Protocol_hash.of_b58check_exn (Protocol.hash protocol),
       sandbox_endpoint,
       sandbox_client,
       sandbox_node )

let activate_ai protocol sandbox_client sandbox_endpoint =
  let* ema_threshold = launch_ema_threshold sandbox_client in
  Log.info "EMA threshold: %d" ema_threshold ;
  (* Feature vote has not passed so AI should not activate and <launch_cycle> should be null *)
  let* launch_cycle =
    Client.RPC.call sandbox_client
    @@ RPC.get_chain_block_context_adaptive_issuance_launch_cycle ()
  in
  Assert.is_none ~loc:__LOC__ ~pp:Format.pp_print_int launch_cycle ;
  (* Make delegate vote for AI activation*)
  let bake ?keys client =
    Helpers.bake ~ai_vote:On ~endpoint:sandbox_endpoint ~protocol ?keys client
  in
  (* The vote should have passed during the first cycle *)
  let* () =
    Helpers.bake_n_cycles
      bake
      1
      ~keys:[Constant.bootstrap2.alias]
      sandbox_client
  in

  (* Now AI should activate and <launch_cycle> should be set *)
  let* launch_cycle =
    Client.RPC.call sandbox_client
    @@ RPC.get_chain_block_context_adaptive_issuance_launch_cycle ()
  in
  Log.info "AI will activate in cycle %d" (Option.get launch_cycle) ;

  (* Wait for <launch_cycle> to have AI fully activated *)
  let* current_level = Helpers.get_current_level sandbox_client in

  Helpers.bake_n_cycles
    bake
    (Option.get launch_cycle - current_level.cycle)
    sandbox_client

let get_hash_of operation =
  let* stdout = Process.check_and_read_stdout operation in
  Log.info "%s" stdout ;
  match stdout =~* rex "Operation hash is '?(o\\w{50})'" with
  | None ->
      Test.fail "Cannot extract operation hash from client_output: %s" stdout
  | Some hash -> return hash

let bake_n ~endpoint ~protocol client i =
  repeat i (fun () ->
      Client.bake_for_and_wait
        ~endpoint
        ~protocol
        ~minimal_timestamp:true
        ~keys:[Constant.bootstrap2.alias]
        client
        ~ai_vote:On)

type bu_check = {
  pred : Operation_receipt.Balance_updates.t -> bool;
  change : int;
  msg : string;
}

(* some values might be slightly different (+-[margin] mutez) because of roundings and
   randomness in baking rights that may affect the overall rewards coming from
   previous blocks, to avoid flakiness we test the "rounded range" of those
   values *)
let check_with_roundings ?(margin = 1) got expected =
  got >= expected - margin && got <= expected + margin

let assert_with_roundings ~__LOC__ ?margin got expected =
  if not (check_with_roundings ?margin got expected) then
    Test.fail
      "@[<v 2>%s: Asserted equality (up to rounding) failed. got %d, expected \
       %d.@]@."
      __LOC__
      got
      expected

let check_balance_updates balance_updates (predicates : bu_check list) =
  List.iter
    (fun {pred; change; msg} ->
      let pre_filtered = List.filter pred balance_updates in
      if pre_filtered = [] then Test.fail "Inconsistent balance update: %s" msg
      else if
        not
          (List.exists
             (fun x ->
               check_with_roundings
                 x.Operation_receipt.Balance_updates.change
                 change)
             pre_filtered)
      then
        Test.fail
          "@[<v 2>Inconsistent balance update, could it be a regression.@. \
           Expected:@ @[%s, change amount: %d@]@.Got:@ @[%s@]@]"
          msg
          change
          (List.fold_left
             (fun acc x ->
               acc ^ "\nor\n " ^ Operation_receipt.Balance_updates.to_string x)
             ""
             pre_filtered))
    predicates

let check_balance_updates_for operation_hash predicates client =
  let* receipt = Operation_receipt.get_result_for operation_hash client in
  let* balance_updates =
    Operation_receipt.Balance_updates.from_result receipt
  in
  check_balance_updates balance_updates predicates ;
  Lwt.return_unit

(* This scenario tests the overall staking mechanism introduced with adaptive issuance:
   - staking with a delegate
   - staking with stakers
   - unstaking
   - receiving staking rewards
   - slashing (because of double bake)
*)
let test_staking =
  Protocol.register_test
    ~__FILE__
    ~title:
      "Staking - test staking with delegate and staker in a simple scenario"
    ~tags:
      [
        "adaptive_issuance";
        "staking";
        "double";
        "baking";
        "accuser";
        "node";
        "rewards";
        "slashing";
      ]
    ~uses:(fun _protocol -> [Constant.octez_accuser])
  @@ fun protocol ->
  let overrides =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7576 use a
       default value for [tolerated_inactivity_period] *)
    (["tolerated_inactivity_period"], `Int 3)
    :: (["issuance_weights"; "dal_rewards_weight"], `Int 0)
    :: default_overrides
  in

  let* _proto_hash, endpoint, client_1, node_1 = init ~overrides protocol in

  log_step 1 "Prepare second node for double baking" ;
  Log.info "Starting second node" ;
  let* node_2 =
    Node.init
      [
        Synchronisation_threshold 0;
        Private_mode;
        Node.(History_mode default_full);
      ]
  in
  let* () = Node.wait_for_ready node_2 in
  let* client_2 = Client.init ~endpoint:(Node node_2) () in

  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in

  let stake_amount = Tez.of_int 600 in
  let delegate = "bootstrap2" in

  log_step 2 "Create two stakers accounts" ;
  let* staker0 = Client.gen_and_show_keys client_1 in
  let* staker1 = Client.gen_and_show_keys client_1 in

  let transfer_to_staker0 =
    Client.spawn_transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 1000000)
      ~giver:Constant.bootstrap1.alias
      ~receiver:staker0.alias
      client_1
  in
  let transfer_to_staker1 =
    Client.spawn_transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 1000000)
      ~giver:Constant.bootstrap3.alias
      ~receiver:staker1.alias
      client_1
  in
  let* () = bake_n ~endpoint ~protocol client_1 1 in
  let* () = transfer_to_staker0 |> Process.check in
  let* () = transfer_to_staker1 |> Process.check in
  let* () = bake_n ~endpoint ~protocol client_1 1 in

  (* check staker0 cannot stake *)
  (* staker need a delegate to stake *)
  log_step 3 "Check staker0 cannot (un)stake as its delegate is not set" ;
  let stake = Client.spawn_stake Tez.one ~staker:staker0.alias client_1 in
  let* () =
    Process.check_error
      ~msg:(rex ".*Stake operations are only allowed when delegate is set.")
      stake
  in
  let unstake = Client.spawn_unstake Tez.one ~staker:staker0.alias client_1 in
  let* () =
    Process.check_error
      ~msg:(rex ".*Stake operations are only allowed when delegate is set.")
      unstake
  in
  log_step 4 "Set delegate for stakers" ;
  let* () =
    Client.set_delegate ~src:staker0.alias ~delegate:"bootstrap2" client_1
  in
  let* () =
    Client.set_delegate ~src:staker1.alias ~delegate:"bootstrap2" client_1
  in
  let* () = bake_n ~endpoint ~protocol client_1 1 in

  (* delegate must accept staking *)
  log_step
    5
    "Check staker0 cannot stake as its delegate does not accept staking" ;
  let stake = Client.spawn_stake Tez.one ~staker:staker0.alias client_1 in
  let* () =
    Process.check_error
      ~msg:
        (rex
           "The delegate currently does not accept staking operations from \
            sources other than itself: its `limit_of_staking_over_baking` \
            parameter is set to 0.")
      stake
  in

  log_step 6 "Changing delegate parameters to accept staking" ;

  (* set bootstrap2 parameters to accept stakers *)
  let set_delegate_parameters =
    Client.spawn_set_delegate_parameters
      ~delegate:"bootstrap2"
      ~limit:"5"
      ~edge:"0.5"
      client_1
  in

  log_step
    7
    "Check balances while waiting for parameters to be taken into account" ;
  let* () = bake_n ~endpoint ~protocol client_1 2 in
  let* () = set_delegate_parameters |> Process.check in

  let* balance = Client.get_balance_for ~account:delegate client_1 in

  let stake_error_balance_too_low =
    Client.spawn_stake balance ~staker:delegate client_1
  in
  let* () =
    Process.check_error
      ~msg:(rex "Balance of contract .* too low (.*) to spend .*")
      stake_error_balance_too_low
  in

  let stake_error_negative =
    Client.spawn_stake (Tez.of_int (-1)) ~staker:delegate client_1
  in
  let* () = Process.check_error stake_error_negative in

  let stake_error_negative =
    Client.spawn_stake (Tez.of_int 0) ~staker:delegate client_1
  in
  let* () =
    Process.check_error
      ~msg:
        (rex
           "Transactions of 0ꜩ towards a contract without code are forbidden \
            (.*).")
      stake_error_negative
  in

  let stake = Client.spawn_stake stake_amount ~staker:delegate client_1 in
  let* () = bake_n ~endpoint ~protocol client_1 2 in
  let* operation_hash = get_hash_of stake in
  Log.info "Stake operation hash: %s" operation_hash ;

  log_step 8 "Check balance updates and unstake requests" ;
  let* () =
    check_balance_updates_for
      operation_hash
      [
        {
          pred = (fun opr -> opr.kind = "freezer");
          change = 600000000;
          msg = "Frozen balance deposit of 600tez";
        };
      ]
      client_1
  in
  let unstake =
    Client.spawn_unstake (Tez.of_int 200) ~staker:delegate client_1
  in
  let* () = bake_n ~endpoint ~protocol client_1 2 in
  let* operation_hash = get_hash_of unstake in
  Log.info "Unstake operation hash: %s" operation_hash ;
  let* () =
    check_balance_updates_for
      operation_hash
      [
        {
          pred =
            (fun opr ->
              opr.kind = "freezer"
              && opr.category = Some "deposits"
              && opr.staker
                 = Some
                     (Baker_own_stake
                        {baker = Constant.bootstrap2.public_key_hash}));
          change = -200000000;
          msg = "Frozen deposits decreased by 200tez";
        };
        {
          pred =
            (fun opr ->
              opr.kind = "freezer"
              && opr.category = Some "unstaked_deposits"
              && opr.staker
                 = Some
                     (Delegate
                        {
                          delegate = Constant.bootstrap2.public_key_hash;
                          contract = Some Constant.bootstrap2.public_key_hash;
                        }));
          change = 200000000;
          msg = "Unstaked frozen increased by 200tez";
        };
      ]
      client_1
  in
  let* unstake_requests =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_unstake_requests
         Constant.bootstrap2.public_key_hash
  in
  let finalizable = JSON.(unstake_requests |-> "finalizable" |> as_list) in
  let unfinalizable =
    JSON.(unstake_requests |-> "unfinalizable" |-> "requests" |> as_list)
  in
  assert (List.length finalizable == 0) ;
  assert (List.length unfinalizable > 0) ;

  let* unstaked_frozen_balance =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_unstaked_frozen_balance
         Constant.bootstrap2.public_key_hash
  in
  assert (unstaked_frozen_balance = 200000000) ;

  let* unstaked_finalizable_balance =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_unstaked_finalizable_balance
         Constant.bootstrap2.public_key_hash
  in
  assert (unstaked_finalizable_balance = 0) ;
  let stake = Client.spawn_stake (Tez.of_int 600) ~staker:delegate client_1 in
  let* () = bake_n ~endpoint ~protocol client_1 2 in
  let* operation_hash = get_hash_of stake in
  let* () =
    check_balance_updates_for
      operation_hash
      [
        {
          pred =
            (fun opr ->
              opr.kind = "freezer"
              && opr.category = Some "unstaked_deposits"
              && opr.staker
                 = Some
                     (Delegate
                        {
                          delegate = Constant.bootstrap2.public_key_hash;
                          contract = Some Constant.bootstrap2.public_key_hash;
                        }));
          change = -200000000;
          msg = "Pending unstaked deposit decreased by 200tez";
        };
        {
          pred =
            (fun opr ->
              opr.kind = "freezer"
              && opr.staker
                 = Some
                     (Baker_own_stake
                        {baker = Constant.bootstrap2.public_key_hash}));
          change = 400000000;
          msg = " Frozen balance deposit of 400tez";
        };
      ]
      client_1
  in

  log_step 9 "Check set_deposits_limit is not allowed after AI activation" ;
  let set_deposits_limit =
    Client.spawn_set_deposits_limit
      ~src:Constant.bootstrap1.alias
      ~limit:"0"
      client_1
  in

  (* lets bake 2 more blocks and delegate should accept staking *)
  let* () = bake_n ~endpoint ~protocol client_1 2 in

  (* set_deposits_limit fails after AI activation *)
  let* () = Process.check ~expect_failure:true set_deposits_limit in

  let* numerator =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_staking_numerator
         Constant.bootstrap2.public_key_hash
  in
  let* denominator =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_staking_numerator
         Constant.bootstrap2.public_key_hash
  in
  Log.info "Numerator/denominator before: %d/%d " numerator denominator ;
  let bake = Helpers.bake ~ai_vote:Pass ~endpoint ~protocol in
  let* () = Helpers.bake_n_cycles bake 1 client_1 in

  let stake0 =
    Client.spawn_stake (Tez.of_int 2000) ~staker:staker0.alias client_1
  in
  let stake1 =
    Client.spawn_stake (Tez.of_int 1000) ~staker:staker1.alias client_1
  in
  let* () = bake_n ~endpoint ~protocol client_1 1 in
  let* () = Process.check ~expect_failure:false stake0 in
  let* () = Process.check ~expect_failure:false stake1 in

  let* () = Helpers.bake_n_cycles bake 3 client_1 in

  let* denominator =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_delegate_staking_denominator
         Constant.bootstrap2.public_key_hash
  in

  let* numerator0 =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_staking_numerator
         staker0.public_key_hash
  in
  Log.info
    "Numerator/denominator after for %s: %d/%d "
    staker0.alias
    numerator0
    (JSON.as_int denominator) ;

  let* numerator1 =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_staking_numerator
         staker1.public_key_hash
  in

  assert (numerator0 + numerator1 = JSON.as_int denominator) ;

  Log.info
    "Numerator/denominator after for %s: %d/%d "
    staker1.alias
    numerator1
    (JSON.as_int denominator) ;

  log_step 10 "Unstake with staker 0 and bootstrap2" ;
  let unstake0 =
    Client.spawn_unstake (Tez.of_int 1000) ~staker:staker0.alias client_1
  in
  let unstake_baker =
    Client.spawn_unstake
      (Tez.of_int 1000)
      ~staker:Constant.bootstrap2.alias
      client_1
  in
  let* () = bake_n ~endpoint ~protocol client_1 2 in
  let* () = Process.check ~expect_failure:false unstake0 in
  let* () = Process.check ~expect_failure:false unstake_baker in

  log_step 11 "Check reward increase with each blocks" ;
  let check_and_return_balances ?check contract =
    let open Account in
    let* balance = Client.get_balance_for ~account:contract.alias client_1 in

    let* staked_balance =
      Client.RPC.call client_1
      @@ RPC.get_chain_block_context_contract_staked_balance
           contract.public_key_hash
    in
    Log.info
      "Balance of %s: spendable : %s, staked_balance : %d"
      contract.alias
      (Tez.to_string balance)
      staked_balance ;
    (match check with
    | Some (pred_balance, pred_staked_balance) ->
        assert (pred_balance <= balance) ;
        assert (pred_staked_balance < staked_balance)
    | None -> ()) ;
    Lwt.return (balance, staked_balance)
  in
  let* balances0 = check_and_return_balances staker0 in
  let* balances1 = check_and_return_balances staker1 in
  let* balances_dlgt = check_and_return_balances Constant.bootstrap2 in
  let balances0 = ref balances0 in
  let balances1 = ref balances1 in
  let balances_dlgt = ref balances_dlgt in
  let* () = bake_n ~endpoint ~protocol client_1 1 in

  let* () =
    repeat 7 (fun () ->
        let* () = bake_n ~endpoint ~protocol client_1 1 in
        let* b0 = check_and_return_balances ~check:!balances0 staker0 in
        let* b1 = check_and_return_balances ~check:!balances1 staker1 in
        let* b_dlgt =
          check_and_return_balances ~check:!balances_dlgt Constant.bootstrap2
        in
        balances0 := b0 ;
        balances1 := b1 ;
        balances_dlgt := b_dlgt ;
        let* bu = Operation_receipt.get_block_metadata client_1 in
        let* bu = Operation_receipt.Balance_updates.from_result [bu] in
        let amount_baker_share, amount_delegation, amount_edge, amount_stakers =
          (1194, 7514, 6, 5)
        in
        (* check rewards *)
        check_balance_updates
          bu
          [
            {
              pred =
                (fun opr ->
                  opr.kind = "minted" && opr.category = Some "baking rewards");
              change = -amount_baker_share;
              msg = "Minting baker share";
            };
            {
              pred =
                (fun opr ->
                  opr.kind = "freezer"
                  && opr.category = Some "deposits"
                  && opr.staker
                     = Some
                         (Baker_own_stake
                            {baker = Constant.bootstrap2.public_key_hash}));
              change = amount_baker_share;
              msg = "Baker's frozen deposits increased by baker share";
            };
            {
              pred =
                (fun opr ->
                  opr.kind = "minted"
                  && opr.category = Some "baking rewards"
                  && opr.change = -amount_delegation);
              change = -amount_delegation;
              msg = "Minting from staking rights";
            };
            {
              pred =
                (fun opr ->
                  opr.kind = "contract"
                  && opr.contract = Some Constant.bootstrap2.public_key_hash);
              change = amount_delegation;
              msg = "Delegate's spendable balance increased";
            };
            {
              pred =
                (fun opr ->
                  opr.kind = "minted"
                  && opr.category = Some "baking rewards"
                  && opr.change = -amount_edge);
              change = -amount_edge;
              msg = "Baker's edge on staker rewards";
            };
            {
              pred =
                (fun opr ->
                  opr.kind = "freezer"
                  && opr.category = Some "deposits"
                  && opr.staker
                     = Some
                         (Baker_edge
                            {baker = Constant.bootstrap2.public_key_hash}));
              change = amount_edge;
              msg =
                "Baker's frozen deposits increased by its edge on staker \
                 rewards";
            };
            {
              pred =
                (fun opr ->
                  opr.kind = "minted"
                  && opr.category = Some "baking rewards"
                  && opr.change = -amount_stakers);
              change = -amount_stakers;
              msg = "Minting staker rewards";
            };
            {
              pred =
                (fun opr ->
                  opr.kind = "freezer"
                  && opr.category = Some "deposits"
                  && opr.staker
                     = Some
                         (Delegate
                            {
                              delegate = Constant.bootstrap2.public_key_hash;
                              contract = None;
                            }));
              change = amount_stakers;
              msg = "Delegates frozen deposits increased by staker rewards";
            };
          ] ;

        Lwt.return_unit)
  in

  (* unstake all *)
  log_step 12 "Unstake all with staker 0" ;
  let unstake0 =
    Client.spawn_unstake (Tez.of_int 500000) ~staker:staker0.alias client_1
  in

  let* _ = Helpers.bake_n_cycles bake 2 client_1 in

  let* () = Process.check ~expect_failure:false unstake0 in

  let is_operation_in_operations ops oph =
    let open JSON in
    let ops_list = ops |=> 2 |> as_list in
    List.exists (fun e -> e |-> "hash" |> as_string = oph) ops_list
  in

  (* Steps 13 to 20 are largely duplicated in [remote_tests/double_baking.ml]
     and [double_bake.ml]. Any modification to this test should be reported there
     too. *)
  log_step 13 "Start setup for double baking" ;
  let* current_level = Helpers.get_current_level client_1 in

  let common_ancestor = current_level.level in
  let base_branch_size = 1 in
  let node_2_branch_size = base_branch_size + 1 in
  let node_1_branch_size = node_2_branch_size + 1 in
  let node_3_first_catch_up_level = common_ancestor + node_2_branch_size in
  let node_3_second_catch_up_level = common_ancestor + node_1_branch_size in
  let node_3_final_level = node_3_second_catch_up_level + 1 in

  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  let* _ = Node.wait_for_level node_1 common_ancestor
  and* _ = Node.wait_for_level node_2 common_ancestor in
  let* () = Node.terminate node_2 in

  log_step
    14
    "Bake %d blocks on Node 1 and terminate Node 1."
    node_1_branch_size ;
  (* Craft a branch from [common_ancestor] of size
     [node_1_branch_size], baked by bootstrap1. *)
  let* () =
    (* Base branch is baked by bootstrap1. *)
    repeat base_branch_size (fun () ->
        Client.bake_for_and_wait
          ~keys:[Constant.bootstrap2.public_key_hash]
          client_1)
  in

  (* Two other bake to make this branch longer than Node 2's one. *)
  let* () =
    repeat 2 (fun () ->
        Client.bake_for_and_wait
          ~keys:[Constant.bootstrap2.public_key_hash]
          client_1)
  in

  let* _ = Node.wait_for_level node_1 (common_ancestor + node_1_branch_size) in
  let* () = Node.terminate node_1 in

  log_step 15 "Run Node 2 and bake %d blocks" node_2_branch_size ;
  let* () = Node.run node_2 [Synchronisation_threshold 0; Private_mode] in
  let* () = Node.wait_for_ready node_2 in

  (* Craft a branch from [common_ancestor] of size
     [node_2_branch_size], the first block is baked by bootstrap2. *)
  let* () =
    (* Base branch is baked by bootstrap2. *)
    repeat base_branch_size (fun () ->
        Client.bake_for_and_wait
          ~keys:[Constant.bootstrap1.public_key_hash]
          client_2)
  in

  (* The second block is baked by bootstrap2 to simulate a
     double bake. *)
  let* () =
    Client.bake_for_and_wait
      ~keys:[Constant.bootstrap2.public_key_hash]
      client_2
  in

  let* _ = Node.wait_for_level node_2 (common_ancestor + node_2_branch_size) in

  (* Bootstrap2 has not yet been denounced, check it is not forbidden *)
  let* is_forbidden =
    Client.RPC.call client_2
    @@ RPC.get_chain_block_context_delegate_is_forbidden
         Constant.bootstrap2.public_key_hash
  in
  assert (not is_forbidden) ;
  log_step 16 "Run Node 3, bake one block and wait for the accuser to be ready." ;
  let* node_3 = Node.init [Synchronisation_threshold 0; Private_mode] in
  let* client_3 = Client.init ~endpoint:(Node node_3) () in
  (* Need to know the protocol for the agnostic accuser to start. *)
  let* () = Client.activate_protocol ~protocol client_3 in
  let* accuser_3 = Accuser.init node_3 in
  let denunciation_injection =
    wait_for_denunciation_injection node_3 client_3 accuser_3
  in

  log_step 17 "Connect Node 3 with Node 2 and wait for Node 3 to catch up." ;
  let* () = Client.Admin.trust_address client_2 ~peer:node_3
  and* () = Client.Admin.trust_address client_3 ~peer:node_2 in
  let* () = Client.Admin.connect_address client_2 ~peer:node_3 in
  let* _ = Node.wait_for_level node_3 node_3_first_catch_up_level in

  log_step 18 "Run and connect Node 1 to Node 3. Wait for Node 3 to catch up." ;
  let* () = Node.run node_1 [Synchronisation_threshold 0; Private_mode] in
  let* () = Node.wait_for_ready node_1 in
  let* () = Client.Admin.trust_address client_1 ~peer:node_3
  and* () = Client.Admin.trust_address client_3 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_3 in
  let* _ = Node.wait_for_level node_3 node_3_second_catch_up_level in
  (* Ensure that the denunciation is in node_3's mempool. *)
  let* denunciation_oph = denunciation_injection in

  log_step 19 "Bake a block on Node 3 and wait for everybody to catch up." ;
  let* () =
    Client.bake_for_and_wait
      ~keys:[Constant.bootstrap1.public_key_hash]
      client_3
  in
  let* _ = Node.wait_for_level node_1 node_3_final_level
  and* _ = Node.wait_for_level node_2 node_3_final_level
  and* _ = Node.wait_for_level node_3 node_3_final_level in

  log_step 20 "Check denunciation is in the last block." ;
  (* Getting the operations of the current head. *)
  let* ops = Client.RPC.call client_1 @@ RPC.get_chain_block_operations () in
  let* () = Accuser.terminate accuser_3 in
  let* () =
    if is_operation_in_operations ops denunciation_oph then unit
    else Test.fail "Double baking evidence was not found"
  in

  (* Bootstrap2 has been denounced, check it is not forbidden *)
  let* is_forbidden =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_delegate_is_forbidden
         Constant.bootstrap2.public_key_hash
  in
  assert is_forbidden ;

  (* Bake a cycle to wait for the slashing *)
  let* () =
    Helpers.bake_n_cycles
      bake
      ~keys:[Constant.bootstrap1.public_key_hash]
      1
      client_1
  in

  let* bu = Operation_receipt.get_block_metadata client_1 in
  let* bu = Operation_receipt.Balance_updates.from_result [bu] in

  (* check slashed and rewarded amounts *)
  let global_limit_of_staking_over_baking = 9 in
  (* It's critical that the rewarded amount cannot exceed the amount
     slashed from the baker's own deposits; otherwise, the baker may
     actually gain tez by purposefully double signing and denuncing
     itself. Therefore, the rewarded part is set to 1 /
     (global_limit_of_staking_over_baking + 2) of the slashed
     amount. *)
  let reward_denominator = global_limit_of_staking_over_baking + 2 in

  (* slashed stakers (including baker) unstake deposit *)
  let amount_slashed_from_unstake_stakers_deposits =
    if Protocol.(number protocol > number S023) then
      (* From T on, when activating the protocol from Genesis, the
         initialization of consensus rights for the first cycles is
         done with AI already in effect, so delegation already counts
         less than staking. This slightly skews the balances in the
         whole test. *)
      50_000_004
    else 50_000_003
  in
  let amount_rewarded_from_unstake_stakers_deposits =
    amount_slashed_from_unstake_stakers_deposits / reward_denominator
  in
  let amount_burned_from_unstake_stakers_deposits =
    amount_slashed_from_unstake_stakers_deposits
    - amount_rewarded_from_unstake_stakers_deposits
  in

  (* slashed stake *)
  let amount_slashed_from_stakers_deposits =
    if Protocol.(number protocol > number S023) then 50_248_756 else 50_248_756
  in
  let amount_rewarded_from_stakers_deposits =
    amount_slashed_from_stakers_deposits / reward_denominator
  in
  let amount_burned_from_stakers_deposits =
    amount_slashed_from_stakers_deposits - amount_rewarded_from_stakers_deposits
  in

  (* slashing baker (bootstrap2) stake*)
  let amount_slashed_from_baker_deposits =
    if Protocol.(number protocol > number S023) then 10_049_785_808
    else 10_049_764_732
  in

  let amount_rewarded_from_baker_deposits =
    amount_slashed_from_baker_deposits / reward_denominator
  in
  let amount_burned_from_baker_deposits =
    amount_slashed_from_baker_deposits - amount_rewarded_from_baker_deposits
  in

  (* total amounts *)
  let total_amount_rewarded =
    amount_rewarded_from_unstake_stakers_deposits
    + amount_rewarded_from_stakers_deposits
    + amount_rewarded_from_baker_deposits
  in
  let total_amount_burned =
    amount_burned_from_unstake_stakers_deposits
    + amount_burned_from_stakers_deposits + amount_burned_from_baker_deposits
  in

  let check_opr ~kind ~category ~change ~staker ~msg ~delayed_operation_hash =
    let open Operation_receipt.Balance_updates in
    {
      pred =
        (fun opr ->
          opr.kind = kind && opr.category = category && opr.staker = staker
          &&
          match delayed_operation_hash with
          | None -> true
          | Some oph -> opr.delayed_operation_hash = Some oph);
      change;
      msg;
    }
  in

  check_balance_updates
    bu
    [
      check_opr
        ~kind:"freezer"
        ~category:(Some "unstaked_deposits")
        ~change:(-amount_burned_from_unstake_stakers_deposits)
        ~staker:
          (Some
             (Delegate
                {
                  delegate = Constant.bootstrap2.public_key_hash;
                  contract = None;
                }))
        ~delayed_operation_hash:(Some denunciation_oph)
        ~msg:"Slashed from unstake(+baker) deposits";
      check_opr
        ~kind:"freezer"
        ~category:(Some "deposits")
        ~change:(-amount_burned_from_baker_deposits)
        ~staker:
          (Some (Baker_own_stake {baker = Constant.bootstrap2.public_key_hash}))
        ~msg:"Slashed from baker deposits"
        ~delayed_operation_hash:None;
      check_opr
        ~kind:"freezer"
        ~category:(Some "deposits")
        ~change:(-amount_burned_from_stakers_deposits)
        ~staker:
          (Some
             (Delegate
                {
                  delegate = Constant.bootstrap2.public_key_hash;
                  contract = None;
                }))
        ~delayed_operation_hash:None
        ~msg:"Slashed from stakers deposits";
      check_opr
        ~kind:"burned"
        ~category:(Some "punishments")
        ~change:total_amount_burned
        ~staker:None
        ~delayed_operation_hash:None
        ~msg:"Punishment for double baking";
      check_opr
        ~kind:"freezer"
        ~category:(Some "unstaked_deposits")
        ~change:(-amount_rewarded_from_unstake_stakers_deposits)
        ~staker:
          (Some
             (Delegate
                {
                  delegate = Constant.bootstrap2.public_key_hash;
                  contract = None;
                }))
        ~delayed_operation_hash:(Some denunciation_oph)
        ~msg:"Reward from unstake stakers(+baker) deposits";
      check_opr
        ~kind:"freezer"
        ~category:(Some "deposits")
        ~change:(-amount_rewarded_from_baker_deposits)
        ~staker:
          (Some (Baker_own_stake {baker = Constant.bootstrap2.public_key_hash}))
        ~delayed_operation_hash:(Some denunciation_oph)
        ~msg:"Reward from baker deposits";
      check_opr
        ~kind:"freezer"
        ~category:(Some "deposits")
        ~change:(-amount_rewarded_from_stakers_deposits)
        ~staker:
          (Some
             (Delegate
                {
                  delegate = Constant.bootstrap2.public_key_hash;
                  contract = None;
                }))
        ~delayed_operation_hash:(Some denunciation_oph)
        ~msg:"Reward from stakers deposits";
      {
        pred =
          (fun opr ->
            opr.kind = "contract"
            && opr.origin = "delayed_operation"
            && opr.contract = Some Constant.bootstrap1.public_key_hash
            && opr.delayed_operation_hash = Some denunciation_oph);
        msg = "Reward for denunciator";
        change = total_amount_rewarded;
      };
    ] ;

  log_step 21 "Test finalize_unstake" ;
  let* balance = Client.get_balance_for ~account:staker0.alias client_1 in
  Log.info
    "Balance of %s before unstake: spendable : %s"
    Constant.bootstrap2.alias
    (Tez.to_string balance) ;

  let* unstake_requests =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_unstake_requests
         staker0.public_key_hash
  in
  let finalizable = JSON.(unstake_requests |-> "finalizable" |> as_list) in
  let unfinalizable =
    JSON.(unstake_requests |-> "unfinalizable" |-> "requests" |> as_list)
  in

  assert (List.length finalizable == 1) ;
  assert (List.length unfinalizable == 1) ;

  Log.info "Unstaked frozen balance: %d" unstaked_frozen_balance ;
  let* unstaked_finalizable_balance =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_unstaked_finalizable_balance
         staker0.public_key_hash
  in
  Log.info "Unstaked finalizable balance: %d" unstaked_finalizable_balance ;
  assert (check_with_roundings unstaked_finalizable_balance 1000000000) ;

  let finalize_unstake =
    Client.spawn_finalize_unstake ~staker:staker0.public_key_hash client_1
  in
  (* bake with bootstrap1 as bootstrap2 should have been forbidden after slashing *)
  let* () =
    repeat 2 (fun () ->
        Client.bake_for_and_wait
          ~endpoint
          ~protocol
          ~minimal_timestamp:true
          ~keys:[Constant.bootstrap1.alias]
          client_1
          ~ai_vote:On)
  in
  let* finalise_unstake_hash = get_hash_of finalize_unstake in

  let* () =
    check_balance_updates_for
      finalise_unstake_hash
      [
        {
          pred =
            (fun opr ->
              opr.kind = "freezer"
              && opr.category = Some "unstaked_deposits"
              && opr.staker
                 = Some
                     (Delegate
                        {
                          delegate = Constant.bootstrap2.public_key_hash;
                          contract = Some staker0.public_key_hash;
                        })
              && opr.delayed_operation_hash = None);
          change = -unstaked_finalizable_balance;
          msg = "Retrieved from staker0 unstaked_deposits";
        };
        {
          pred =
            (fun opr ->
              opr.kind = "contract"
              && opr.contract = Some staker0.public_key_hash
              && check_with_roundings opr.change unstaked_finalizable_balance);
          change = unstaked_finalizable_balance;
          msg = "Added to staker0 spendable balance";
        };
      ]
      client_1
  in

  let* unstake_requests =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_unstake_requests
         staker0.public_key_hash
  in

  let finalizable = JSON.(unstake_requests |-> "finalizable" |> as_list) in
  let unfinalizable =
    JSON.(unstake_requests |-> "unfinalizable" |-> "requests" |> as_list)
  in

  assert (List.length finalizable == 0) ;
  assert (List.length unfinalizable == 0) ;

  let* balance2 = Client.get_balance_for ~account:staker0.alias client_1 in
  Log.info
    "Balance of %s after unstake: spendable : %s"
    staker0.alias
    (Tez.to_string balance2) ;

  Log.info "Balance change = %s" (Tez.to_string Tez.(balance2 - balance)) ;
  (* spendable balance should have increased from the finalized unstaked tokens
     (minus the fees associated to the finalize_unstake call) *)
  assert (Tez.(balance2 - balance > of_mutez_int 999999000)) ;

  let* unstaked_finalizable_balance =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_unstaked_finalizable_balance
         staker0.public_key_hash
  in
  Log.info "Unstaked finalizable balance: %d" unstaked_finalizable_balance ;
  assert (unstaked_finalizable_balance = 0) ;
  let* () =
    repeat 2 (fun () ->
        Client.bake_for_and_wait
          ~endpoint
          ~protocol
          ~minimal_timestamp:true
          ~keys:[Constant.bootstrap1.alias]
          client_1
          ~ai_vote:On)
  in

  let* () =
    Client.set_delegate ~src:staker1.alias ~delegate:"bootstrap3" client_1
  in
  let* () =
    repeat 2 (fun () ->
        Client.bake_for_and_wait
          ~endpoint
          ~protocol
          ~minimal_timestamp:true
          ~keys:[Constant.bootstrap1.alias]
          client_1
          ~ai_vote:On)
  in
  unit

let check_rpc_error client ?msg rpc =
  let*? process = Client.RPC.spawn client @@ rpc in
  Process.check_error ?msg process

let check_rpc_ok client rpc =
  let*? process = Client.RPC.spawn client @@ rpc in
  Process.check process

let test_fix_delegated_balance =
  Protocol.register_test
    ~__FILE__
    ~title:"Test protocol fix for delegated balance rpc"
    ~tags:["rpc"; "delegated_balance"]
  @@ fun protocol ->
  let* _proto_hash, endpoint, client, _node = init protocol in
  let stake_amount = Tez.of_int 6_000 in
  let delegate = "bootstrap1" in
  log_step 1 "Preparing delegator accounts" ;
  let* delegator = Client.gen_and_show_keys client in
  let* delegator2 = Client.gen_and_show_keys client in

  let transfer_to dlgtor amount =
    Client.spawn_transfer
      ~burn_cap:Tez.one
      ~amount
      ~giver:Constant.bootstrap3.alias
      ~receiver:dlgtor.Account.alias
      client
  in

  let* () = bake_n ~endpoint ~protocol client 1 in
  let* () = transfer_to delegator (Tez.of_int 1_000_000) |> Process.check in
  let* () = bake_n ~endpoint ~protocol client 1 in
  (* Let's make sure that delegator2 delegates less than 6_000 tez *)
  let* () = transfer_to delegator2 (Tez.of_int 1_000) |> Process.check in
  let* () = bake_n ~endpoint ~protocol client 1 in

  log_step 2 "Preparing delegate account" ;
  let* () = Client.set_delegate ~src:delegator.alias ~delegate client in
  let* () = bake_n ~endpoint ~protocol client 1 in
  let set_delegate_parameters =
    Client.spawn_set_delegate_parameters ~delegate ~limit:"5" ~edge:"0.5" client
  in
  let* () = bake_n ~endpoint ~protocol client 2 in
  let* () = set_delegate_parameters |> Process.check in

  log_step 3 "Delegator delegates to delegate" ;
  let* delegated = Client.get_delegate ~src:delegator.alias client in
  (match delegated with
  | None -> Test.fail "No delegate found"
  | Some d -> Log.info "Delegated: %s" d) ;

  let bake = Helpers.bake ~ai_vote:Pass ~endpoint ~protocol in
  log_step 4 "Wait for delegate to accept staking" ;
  let* _ = Helpers.bake_n_cycles bake 3 client in

  log_step 5 "Delegator stakes" ;
  let stake = Client.spawn_stake stake_amount ~staker:delegator.alias client in
  let* () = bake_n ~endpoint ~protocol client 2 in
  let* () = Process.check ~expect_failure:false stake in

  log_step 6 "Delegator delegates to itself" ;
  let* () =
    Client.set_delegate ~src:delegator.alias ~delegate:delegator.alias client
  in
  let* () = bake_n ~endpoint ~protocol client 2 in

  let* () =
    Client.set_delegate ~src:delegator2.alias ~delegate:delegator.alias client
  in
  let* () = bake_n ~endpoint ~protocol client 2 in

  let* delegated = Client.get_delegate ~src:delegator.alias client in
  (match delegated with
  | None -> Test.fail "No delegate found"
  | Some d -> Log.info "Delegated: %s" d) ;

  log_step 7 "RPC calls should not fail" ;
  let* () =
    check_rpc_ok
      client
      (RPC.get_chain_block_context_delegate_delegated_balance
         delegator.public_key_hash)
  in
  let* () =
    check_rpc_ok
      client
      (RPC.get_chain_block_context_delegate delegator.public_key_hash)
  in
  Lwt.return_unit

(* This scenario tests set_delegate_parameters and update_delegate_parameters UX
*)
let test_delegate_parameter_UX =
  Protocol.register_test
    ~__FILE__
    ~title:
      "Staking - test set_delegate_parameters and update_delegate_parameters UX"
    ~tags:["staking"; "node"; "client"]
  @@ fun protocol ->
  let* _proto_hash, endpoint, client, _node_1 = init protocol in
  let bake = Helpers.bake ~ai_vote:Pass ~endpoint ~protocol in
  let current_cycle () =
    let* level =
      Client.RPC.call client @@ RPC.get_chain_block_helper_current_level ()
    in
    return level.cycle
  in

  let* delegate_parameters_activation_delay =
    let* json =
      Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
    in
    Lwt.return
    @@ JSON.(json |-> "delegate_parameters_activation_delay" |> as_int)
  in
  let delegate = "bootstrap2" in
  let* delegate_key = Client.show_address ~alias:delegate client in
  log_step 1 "set delegate parameters" ;
  let limit_int = 10 in
  let edge_pct = 2 in
  let* () =
    Client.set_delegate_parameters
      ~delegate
      ~limit:(string_of_int limit_int)
      ~edge:(string_of_int edge_pct ^ "%")
      client
  in
  let* () = bake client in
  let* operation_cycle = current_cycle () in
  let* () = Helpers.bake_n_cycles bake 1 client in
  let check_parameters ~operation_cycle ~limit ~edge_pct =
    let* json =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_delegate_pending_staking_parameters
           Account.(delegate_key.public_key_hash)
    in
    let cycle = operation_cycle + 1 + delegate_parameters_activation_delay in
    let parameter_json =
      try
        List.find
          (fun json -> JSON.(json |-> "cycle" |> as_int) = cycle)
          JSON.(as_list json)
      with Not_found ->
        Log.error "cycle %d not found in %s@." cycle (JSON.encode json) ;
        failwith "NOT_found"
    in
    let expected_limit = limit * 1_000_000 in
    let expected_edge = edge_pct * 1_000_000_000 / 100 in
    if
      JSON.(
        parameter_json |-> "parameters"
        |-> "limit_of_staking_over_baking_millionth" |> as_int)
      = expected_limit
      && JSON.(
           parameter_json |-> "parameters"
           |-> "edge_of_baking_over_staking_billionth" |> as_int)
         = expected_edge
    then return ()
    else (
      Log.error
        "expected: limit %d, edge %d at cycle %d. got %s"
        expected_limit
        expected_edge
        cycle
        (JSON.encode json) ;
      failwith "wrong delegate parameter")
  in
  let* () = check_parameters ~operation_cycle ~limit:limit_int ~edge_pct in
  let* () =
    Client.set_delegate_parameters ~delegate ~limit:"10" ~edge:"3%" client
  in
  let* () = bake client in
  let* operation_cycle = current_cycle () in
  let* () = check_parameters ~operation_cycle ~limit:10 ~edge_pct:3 in
  let updater =
    Client.spawn_update_delegate_parameters
      ~delegate
      ~limit:"10"
      ~edge:"3%"
      client
  in
  let* () = Process.check ~expect_failure:true updater in
  let updater =
    Client.spawn_update_delegate_parameters ~delegate ~edge:"3%" client
  in
  let* () = Process.check ~expect_failure:true updater in
  let updater =
    Client.spawn_update_delegate_parameters ~delegate ~limit:"10" client
  in
  let* () = Process.check ~expect_failure:true updater in
  let* () = Client.update_delegate_parameters ~delegate ~edge:"4%" client in
  let* () = bake client in
  let* operation_cycle = current_cycle () in
  let* () = check_parameters ~operation_cycle ~limit:10 ~edge_pct:4 in
  let* () = Client.update_delegate_parameters ~delegate ~limit:"9" client in
  let* () = bake client in
  let* operation_cycle = current_cycle () in
  let* () = check_parameters ~operation_cycle ~limit:9 ~edge_pct:4 in
  unit

let register ~protocols =
  test_staking protocols ;
  test_fix_delegated_balance protocols ;
  test_delegate_parameter_UX protocols
