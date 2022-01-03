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
   Component:    Client
   Invocation:   dune exec tezt/tests/main.exe -- --file stresstest_command.ml
   Subject:      Test the [stresstest] client command
*)

(** Waits for [n] injection request events. *)
let wait_for_n_injections ?(log = false) n node =
  if log then Log.info "Waiting for %d injections." n ;
  let seen = ref 0 in
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when s = "inject" ->
        if log then Log.info "Injection %d witnessed." !seen ;
        incr seen ;
        if !seen >= n then Some () else None
    | Some _ | None -> None
  in
  Node.wait_for node "request_completed_notice.v0" filter

(** Tests the various possible formats to provide sources to the
    [stresstest] command: alias, public key hash, or explicit key. *)
let test_stresstest_sources_format =
  Protocol.register_test
    ~__FILE__
    ~title:"stresstest sources format"
    ~tags:["client"; "stresstest"; "sources"]
  @@ fun protocol ->
  let transfers = 30 in
  let* (node, client) =
    Client.init_with_protocol
      ~nodes_args:
        [
          Synchronisation_threshold 0;
          Connections 0;
          Disable_operations_precheck;
          (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2085
             Stresstest command uses counters to inject a lot of operations with limited
             number of bootstrap accounts. With precheck these operations are mostly
             rejected because we don't apply the effect of operations in the
             prevalidation context in mempool mode anymore. So, only the operation with
             the correct counter is considered as Applied (without incrementing the
             counter in the context). Once the issue is fixed, the
             [Disable_operations_precheck] flag above can be removed. *)
        ]
      ~additional_bootstrap_account_count:1
      `Client
      ~protocol
      ()
  in
  let* bootstrap6 = Client.show_address ~alias:"bootstrap6" client in
  let waiter = wait_for_n_injections transfers node in
  let* () =
    Constant.(
      Client.stresstest
        ~source_aliases:[bootstrap1.alias; bootstrap2.alias; bootstrap6.alias]
        ~source_pkhs:[bootstrap3.public_key_hash]
        ~source_accounts:[bootstrap1; bootstrap4]
        ~transfers
        client)
  in
  let* () = waiter in
  let* mempool_ops = RPC.get_mempool_pending_operations client in
  let actual_sources =
    let open JSON in
    mempool_ops |-> "applied" |> as_list
    |> List.fold_left
         (fun sources op ->
           String_set.add
             (op |-> "contents" |=> 0 |-> "source" |> as_string)
             sources)
         String_set.empty
  in
  let expected_sources =
    List.map
      (fun bootstrap -> bootstrap.Account.public_key_hash)
      Constant.[bootstrap1; bootstrap2; bootstrap3; bootstrap4; bootstrap6]
    |> String_set.of_list
  in
  let pp_sources fmt sources =
    Format.(
      pp_print_seq
        ~pp_sep:(fun fmt () -> fprintf fmt ", ")
        pp_print_string
        fmt
        (String_set.to_seq sources))
  in
  Check.(
    (actual_sources = expected_sources)
      (equalable pp_sources String_set.equal)
      ~error_msg:"Set of sources is %L; expected %R.") ;
  unit

let check_n_applied_operations ?(log = false) ~expected client =
  let* mempool_ops = RPC.get_mempool_pending_operations client in
  let n_applied = JSON.(mempool_ops |-> "applied" |> as_list |> List.length) in
  if not (Int.equal n_applied expected) then (
    let name = Client.name client in
    Log.warn "Mempool of %s: %s" name (JSON.encode mempool_ops) ;
    Test.fail
      "Expected %d applied operations in %s's mempool, got %d."
      expected
      name
      n_applied) ;
  if log then
    Log.info
      "Found %d applied operations in %s's mempool, as expected."
      n_applied
      (Client.name client) ;
  unit

(** Waits for either [waiter] to resolve or [timeout] seconds.
    If [sleep_at_resolution] is set, then also sleeps this amount
    of seconds upon resolution of [waiter]. *)
let wait_or_timeout ?(timeout = 30.) ?sleep_at_resolution (waiter : unit Lwt.t)
    =
  Lwt.pick
    [
      (let* () = waiter in
       match sleep_at_resolution with
       | None -> unit
       | Some delay -> Lwt_unix.sleep delay);
      Lwt_unix.sleep timeout;
    ]

(** Initializes a single node and runs the [stresstest] command.
    Waits for it to inject the requested number of transfers
    (argument [transfers]), and checks that the node's mempool
    contains this many applied operations. *)
let test_stresstest_applied =
  Protocol.register_test
    ~__FILE__
    ~title:"stresstest applied"
    ~tags:["client"; "stresstest"; "applied"]
  @@ fun protocol ->
  let transfers = 30 in
  let* (node, client) =
    Client.init_with_protocol
      ~nodes_args:
        [
          Synchronisation_threshold 0;
          Connections 0;
          Disable_operations_precheck;
          (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2085
             Stresstest command uses counters to inject a lot of operations with limited
             number of bootstrap accounts. With precheck these operations are mostly
             rejected because we don't apply the effect of operations in the
             prevalidation context in mempool mode anymore. So, only the operation with
             the correct counter is considered as Applied (without incrementing the
             counter in the context). Once the issue is fixed, the
             [Disable_operations_precheck] flag above can be removed. *)
        ]
      `Client
      ~protocol
      ()
  in
  let waiter = wait_for_n_injections transfers node in
  let* () = Client.stresstest ~transfers client in
  let* () = wait_or_timeout waiter in
  check_n_applied_operations ~log:true ~expected:transfers client

(** Similar to {!test_stresstest_applied}, but instead of the
    default five bootstrap accounts, provide the command with
    fresh accounts generated during the node's initialization. *)
let test_stresstest_applied_new_bootstraps =
  Protocol.register_test
    ~__FILE__
    ~title:"stresstest applied new bootstraps"
    ~tags:["client"; "stresstest"; "applied"]
  @@ fun protocol ->
  let transfers = 30 in
  let n_new_accounts = 10 in
  let* (node, client) =
    Client.init_with_protocol
      ~nodes_args:
        [
          Synchronisation_threshold 0;
          Connections 0;
          Disable_operations_precheck;
          (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2085
             Stresstest command uses counters to inject a lot of operations with limited
             number of bootstrap accounts. With precheck these operations are mostly
             rejected because we don't apply the effect of operations in the
             prevalidation context in mempool mode anymore. So, only the operation with
             the correct counter is considered as Applied (without incrementing the
             counter in the context). Once the issue is fixed, the
             [Disable_operations_precheck] flag above can be removed. *)
        ]
      ~additional_bootstrap_account_count:n_new_accounts
      `Client
      ~protocol
      ()
  in
  let bootstrap_nums =
    Constant.(
      range
        (default_bootstrap_count + 1)
        (default_bootstrap_count + n_new_accounts))
  in
  let source_aliases = List.map (sf "bootstrap%d") bootstrap_nums in
  let waiter = wait_for_n_injections transfers node in
  let* () = Client.stresstest ~source_aliases ~transfers client in
  let* () = wait_or_timeout waiter in
  check_n_applied_operations ~log:true ~expected:transfers client

(** Does nothing, but means that we have spawned a [process] that we
    don't expect to terminate. *)
let non_terminating_process (_process : Process.t) = ()

(** Tests the [stresstest] client command with the
    [--single-op-per-pkh-per-block] flag. We do not provide a
    [transfers] argument, so the command never stops trying to
    craft transfers. But as we do not bake any block, it runs out
    of distinct accounts to use. Therefore, we expect it to inject
    exactly [target_transfers] operations, all of which should be
    applied in the node's mempool. *)
let test_stresstest_applied_1op =
  Protocol.register_test
    ~__FILE__
    ~title:"stresstest applied 1op"
    ~tags:["client"; "stresstest"; "stresstest_1op"; "applied"]
  @@ fun protocol ->
  let target_transfers = 10 in
  let n_accounts = target_transfers in
  let* (node, client) =
    Client.init_with_protocol
      ~nodes_args:[Synchronisation_threshold 0; Connections 0]
      ~additional_bootstrap_account_count:
        (n_accounts - Constant.default_bootstrap_count)
      `Client
      ~protocol
      ()
  in
  let bootstrap_nums = range 1 n_accounts in
  let source_aliases = List.map (sf "bootstrap%d") bootstrap_nums in
  let waiter = wait_for_n_injections target_transfers node in
  non_terminating_process
  @@ Client.spawn_stresstest
       ~source_aliases
       ~single_op_per_pkh_per_block:true
       client ;
  (* When [waiter] resolves, wait a little longer to check that the command
     has not issued more operations than expected. *)
  let* () =
    wait_or_timeout
      ~timeout:(Int.to_float target_transfers)
      ~sleep_at_resolution:5.
      waiter
  in
  check_n_applied_operations ~log:true ~expected:target_transfers client

(** Initializes [n_nodes] nodes with mode [Client] and with disjoint
    sets of [accounts_per_nodes] accounts each. The first node activates
    the [protocol]. Each other node is connected exclusively with the
    first node. For each node, returns the node together with the
    associated client and keys. The return value takes the form:
    [((first_node, first_client, first_accounts),
    list_of_other_nodes_clients_accounts)]. *)
let init_nodes_star ~protocol ~n_nodes ~accounts_per_node
    ?(additional_node_args = []) () =
  let* (first_node, first_client) =
    Client.init_with_protocol
      ~nodes_args:
        (Node.[Synchronisation_threshold 0; Connections (n_nodes - 1)]
        @ additional_node_args)
      ~additional_bootstrap_account_count:
        ((n_nodes * accounts_per_node) - Constant.default_bootstrap_count)
      `Client
      ~protocol
      ()
  in
  let accounts_of_num_range num_range =
    let aliases = List.map (sf "bootstrap%d") num_range in
    Lwt_list.map_p
      (fun alias -> Client.show_address ~alias first_client)
      aliases
  in
  let* first_node_accounts =
    accounts_of_num_range (range 1 accounts_per_node)
  in
  let* other_nodes_clients_accounts =
    Lwt_list.map_p
      (fun i ->
        let* accounts =
          accounts_of_num_range
          @@ range
               (((i + 1) * accounts_per_node) + 1)
               ((i + 2) * accounts_per_node)
        in
        let* (node, client) =
          Client.init_with_node
            ~nodes_args:
              (Node.[Synchronisation_threshold 0; Connections 1]
              @ additional_node_args)
            ~keys:accounts
            `Client
            ()
        in
        let* () = Client.Admin.connect_address first_client ~peer:node in
        let* _ = Node.wait_for_level node 1 in
        return (node, client, accounts))
      (List.init (n_nodes - 1) Fun.id)
  in
  return
    ( (first_node, first_client, first_node_accounts),
      other_nodes_clients_accounts )

(** Runs the [stresstest] command on multiple connected nodes
    (see {!init_nodes_star}). Each node calls it with a distinct
    set of source accounts and the same target number of transfers.
    Waits for this number of injections in the first node, then
    checks that all transfers from all nodes are applied in its
    mempool. *)
let test_stresstest_applied_multiple_nodes =
  Protocol.register_test
    ~__FILE__
    ~title:"stresstest applied multiple nodes"
    ~tags:["client"; "stresstest"; "applied"]
  @@ fun protocol ->
  let n_nodes = 3 in
  let transfers_per_node = 10 in
  let accounts_per_node = 5 in
  let* (((first_node, first_client, _) as first_node_client_accounts), others) =
    init_nodes_star
      ~protocol
      ~n_nodes
      ~accounts_per_node
      ~additional_node_args:
        [
          Disable_operations_precheck;
          (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2085
             Stresstest command uses counters to inject a lot of operations with limited
             number of bootstrap accounts. With precheck these operations are mostly
             rejected because we don't apply the effect of operations in the
             prevalidation context in mempool mode anymore. So, only the operation with
             the correct counter is considered as Applied (without incrementing the
             counter in the context). Once the issue is fixed, the
             [Disable_operations_precheck] flag above can be removed. *)
        ]
      ()
  in
  let nodes_clients_accounts = first_node_client_accounts :: others in
  let waiter = wait_for_n_injections transfers_per_node first_node in
  List.iter
    (fun (_, client, source_accounts) ->
      non_terminating_process
      @@ Client.spawn_stresstest
           ~source_accounts
           ~transfers:transfers_per_node
           client)
    nodes_clients_accounts ;
  let* () =
    wait_or_timeout
      ~timeout:(Int.to_float transfers_per_node)
      ~sleep_at_resolution:2.
      waiter
  in
  check_n_applied_operations
    ~log:true
    ~expected:(n_nodes * transfers_per_node)
    first_client

(** Similar to {!test_stresstest_applied_multiple_nodes} but with
    the [--single-op-per-pkh-per-block] flag. As in
    {!test_stresstest_applied_1op}, we do not provide a [transfers]
    argument to the command, but instead the expected number of
    operations is tied to the number of provided accounts. *)
let test_stresstest_applied_multiple_nodes_1op =
  Protocol.register_test
    ~__FILE__
    ~title:"stresstest applied multiple nodes 1op"
    ~tags:["client"; "stresstest"; "stresstest_1op"; "applied"]
  @@ fun protocol ->
  let n_nodes = 3 in
  let target_transfers_per_node = 4 in
  let accounts_per_node = target_transfers_per_node in
  let* (((first_node, first_client, _) as first_node_client_accounts), others) =
    init_nodes_star ~protocol ~n_nodes ~accounts_per_node ()
  in
  let nodes_clients_accounts = first_node_client_accounts :: others in
  let waiter = wait_for_n_injections target_transfers_per_node first_node in
  List.iter
    (fun (_, client, source_accounts) ->
      non_terminating_process
      @@ Client.spawn_stresstest
           ~source_accounts
           ~transfers:(10 * target_transfers_per_node)
           ~single_op_per_pkh_per_block:true
           client)
    nodes_clients_accounts ;
  (* When [waiter] resolves, wait a little longer for operations from
     other nodes to arrive, and also to check that the command
     has not issued more operations than expected. *)
  let* () =
    wait_or_timeout
      ~timeout:(Int.to_float target_transfers_per_node)
      ~sleep_at_resolution:5.
      waiter
  in
  check_n_applied_operations
    ~log:true
    ~expected:(n_nodes * target_transfers_per_node)
    first_client

let register ~protocols =
  test_stresstest_sources_format ~protocols ;
  test_stresstest_applied ~protocols ;
  test_stresstest_applied_new_bootstraps ~protocols ;
  test_stresstest_applied_1op ~protocols ;
  test_stresstest_applied_multiple_nodes ~protocols ;
  test_stresstest_applied_multiple_nodes_1op ~protocols
