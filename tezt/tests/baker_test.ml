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
   Component:    Baker
   Invocation:   dune exec tezt/tests/main.exe -- --file baker_test.ml
   Subject:      Run the baker while performing a lot of transfers
*)

let team = Tag.layer1

let hooks = Tezos_regression.hooks

let check_node_version_check_bypass_test =
  Protocol.register_test
    ~__FILE__
    ~title:"baker node version check bypass test"
    ~tags:[team; "node"; "baker"]
    ~supports:Protocol.(From_protocol 021)
    ~uses:(fun protocol -> [Protocol.baker protocol])
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let baker =
    Baker.create ~protocol ~node_version_check_bypass:true node client
  in
  let check_bypassed_event_promise =
    Baker.wait_for baker "node_version_check_bypass.v0" (fun _ -> Some ())
  in
  let* () = Baker.run baker in
  let* () = check_bypassed_event_promise in
  unit

let baker_reward_test =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Baker rewards"
    ~tags:[team; "baker"; "rewards"]
    ~uses:(fun protocol -> [Protocol.baker protocol])
    (fun protocol ->
      let* parameter_file =
        Protocol.write_parameter_file
          ~base:(Either.Right (protocol, Some Constants_mainnet))
          []
      in
      let* node, client =
        Client.init_with_protocol
          `Client
          ~protocol
          ~timestamp:Now
          ~parameter_file
          ()
      in
      let level_2_promise = Node.wait_for_level node 2 in
      let* baker = Baker.init ~protocol node client in
      Log.info "Wait for new head." ;
      Baker.log_events baker ;
      let* _ = level_2_promise in
      let* _ =
        Client.RPC.call ~hooks client @@ RPC.get_chain_block_metadata ()
      in
      unit)

let baker_test ?force_apply protocol ~keys =
  let* parameter_file =
    Protocol.write_parameter_file
      ~bootstrap_accounts:(List.map (fun k -> (k, None)) keys)
      ~base:(Right (protocol, None))
      []
  in
  let* node, client =
    Client.init_with_protocol
      ~keys:(Constant.activator :: keys)
      `Client
      ~protocol
      ~timestamp:Now
      ~parameter_file
      ()
  in
  let level_2_promise = Node.wait_for_level node 2 in
  let level_3_promise = Node.wait_for_level node 3 in
  let* baker = Baker.init ?force_apply ~protocol node client in
  Log.info "Wait for new head." ;
  Baker.log_events baker ;
  let* _ = level_2_promise in
  Log.info "New head arrive level 2" ;
  let* _ = level_3_promise in
  Log.info "New head arrive level 3" ;
  Lwt.return client

let baker_simple_test =
  Protocol.register_test
    ~__FILE__
    ~title:"baker test"
    ~tags:[team; "node"; "baker"]
    ~uses:(fun protocol -> [Protocol.baker protocol])
  @@ fun protocol ->
  let* _ =
    baker_test protocol ~keys:(Account.Bootstrap.keys |> Array.to_list)
  in
  unit

let baker_stresstest =
  Protocol.register_test
    ~__FILE__
    ~title:"baker stresstest"
    ~tags:[team; "node"; "baker"; "stresstest"]
    ~uses:(fun protocol -> [Protocol.baker protocol])
  @@ fun protocol ->
  let* node, client =
    Client.init_with_protocol `Client ~protocol () ~timestamp:Now
  in
  let* _ = Baker.init ~protocol node client in
  let* _ = Node.wait_for_level node 3 in
  (* Use a large tps, to have failing operations too *)
  let* () = Client.stresstest ~tps:25 ~transfers:100 client in
  Lwt.return_unit

(* Force the baker to apply operations after validating them *)
let baker_stresstest_apply =
  Protocol.register_test
    ~__FILE__
    ~title:"baker stresstest with forced application"
    ~tags:[team; "node"; "baker"; "stresstest"; "apply"]
    ~uses:(fun protocol -> [Protocol.baker protocol])
  @@ fun protocol ->
  let* node, client =
    Client.init_with_protocol `Client ~protocol () ~timestamp:Now
  in
  let* _ = Baker.init ~force_apply:true ~protocol node client in
  let* _ = Node.wait_for_level node 3 in
  (* Use a large tps, to have failing operations too *)
  let* () = Client.stresstest ~tps:25 ~transfers:100 client in
  unit

let baker_bls_test =
  Protocol.register_test
    ~__FILE__
    ~title:"No BLS baker test"
    ~tags:[team; "node"; "baker"; "bls"]
  @@ fun protocol ->
  let* client0 = Client.init_mockup ~protocol () in
  Log.info "Generate BLS keys for client" ;
  let* keys =
    Lwt_list.map_s
      (fun i ->
        Client.gen_and_show_keys
          ~alias:(sf "bootstrap_bls_%d" i)
          ~sig_alg:"bls"
          client0)
      (Base.range 1 5)
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~bootstrap_accounts:(List.map (fun k -> (k, None)) keys)
      ~base:(Right (protocol, None))
      []
  in
  let* _node, client =
    Client.init_with_node ~keys:(Constant.activator :: keys) `Client ()
  in
  let activate_process =
    Client.spawn_activate_protocol
      ~protocol
      ~timestamp:Now
      ~parameter_file
      client
  in
  let msg =
    rex "The delegate tz4.*\\w is forbidden as it is a BLS public key hash"
  in
  Process.check_error activate_process ~exit_code:1 ~msg

let baker_remote_test =
  Protocol.register_test
    ~__FILE__
    ~title:"Baker in RPC-only mode"
    ~tags:[team; "baker"; "remote"]
    ~uses:(fun protocol -> [Protocol.baker protocol])
  @@ fun protocol ->
  let* node, client =
    Client.init_with_protocol `Client ~protocol () ~timestamp:Now
  in
  let* _ = Baker.init ~remote_mode:true ~protocol node client in
  let* _ = Node.wait_for_level node 3 in
  unit

let baker_check_consensus_branch =
  Protocol.register_test
    ~__FILE__
    ~title:"Baker check branch in consensus operations"
    ~tags:[team; "baker"; "grandparent"; "parent"]
    ~uses:(fun protocol -> [Protocol.baker protocol])
  @@ fun protocol ->
  Log.info "Init client and node with protocol %s" (Protocol.name protocol) ;
  let* node, client =
    Client.init_with_protocol `Client ~protocol () ~timestamp:Now
  in

  let target_level = 5 in
  Log.info "Start a baker and bake until level %d" target_level ;
  let* baker = Baker.init ~protocol node client in
  let* _ = Node.wait_for_level node target_level in
  let* () = Baker.kill baker in

  Log.info "Retrieve mempool" ;
  let* mempool =
    Client.RPC.call client
    @@ RPC.get_chain_mempool_pending_operations ~outdated:false ()
  in

  let branch_s, branch_lvl =
    if Protocol.number protocol >= Protocol.number ParisC then ("grandparent", 2)
    else ("parent", 1)
  in
  Log.info "Check that consensus operations are branched on %s block" branch_s ;
  let ops = JSON.(mempool |-> "validated" |> as_list) in
  assert (not ([] = ops)) ;
  Tezos_base__TzPervasives.List.iter_s
    (fun op ->
      let branch = JSON.(op |-> "branch" |> as_string) in
      let content = JSON.(op |-> "contents" |> as_list |> List.hd) in
      let level = JSON.(content |-> "level" |> as_int) in

      let* target_branch =
        Client.RPC.call client
        @@ RPC.get_chain_block_header
             ~block:(string_of_int (level - branch_lvl))
             ()
      in
      let target_branch = JSON.(target_branch |-> "hash" |> as_string) in

      Tezt.Check.(
        (target_branch = branch)
          string
          ~error_msg:"Consensus operation should be branch on block %L, got %R") ;
      unit)
    ops

let register ~protocols =
  check_node_version_check_bypass_test protocols ;
  baker_simple_test protocols ;
  baker_reward_test protocols ;
  baker_stresstest protocols ;
  baker_stresstest_apply protocols ;
  baker_bls_test protocols ;
  baker_remote_test protocols ;
  baker_check_consensus_branch protocols
