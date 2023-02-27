(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
   Component:    [stresstest] client command
   Invocation:   dune exec tezt/tests/main.exe -- --file stresstest_command.ml
   Subject:      Test the [stresstest] client command. Even though this
                 command is only used for testing purposes, its development
                 is both tricky and ongoing. Moreover, it can be complex to
                 debug when called in various contexts. That is why these
                 tests are nice to have. *)

(** Wait for [n] injection request events. *)
let wait_for_n_injections n node =
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when s = "inject" -> Some ()
    | Some _ | None -> None
  in
  let* _ =
    Node.wait_for node "request_completed_info.v0" (Daemon.n_events n filter)
  in
  unit

(** Do nothing, but emphasize that we have spawned a [process] that we
    do not expect to terminate. *)
let non_terminating_process (_process : Process.t) = ()

(** Check that the mempool contains [n] applied operations. Also
    return these applied operations (which will usually be transactions
    injected by the transfer command). *)
let check_n_applied_operations_in_mempool n client =
  let* mempool_ops =
    RPC.Client.call client @@ RPC.get_chain_mempool_pending_operations ()
  in
  let applied_ops = JSON.(mempool_ops |-> "applied" |> as_list) in
  Check.(
    (List.length applied_ops = n)
      int
      ~error_msg:"Found %L applied operations in the mempool; expected %R.") ;
  return applied_ops

(** Check that the head block contains [n] manager operations. Also
    return these manager operations (which will usually be transactions
    injected by the transfer command). *)
let check_n_manager_operations_in_head n client =
  let* head = RPC.Client.call client @@ RPC.get_chain_block () in
  let manager_ops = JSON.(head |-> "operations" |=> 3 |> as_list) in
  Check.(
    (List.length manager_ops = n)
      int
      ~error_msg:"Found %L manager operations in the head block; expected %R.") ;
  return manager_ops

(** Run the [stresstest] command in an isolated node with sources
    provided in various formats; then check the number and the sources
    of the operations in the mempool and in subsequently baked blocks.

    More precisely, this test checks that:

    - After letting the [stresstest] command run for a while on a
      given head, the mempool contains exactly one applied operation by
      each source account provided to the command. The same goes for
      manager operations included in the next baked block. (Note that
      this is true in a minimalist context where no operation injection
      happens aside from [stresstest], and there are few enough source
      accounts that all the new operations can fit in a block.)

    - This holds regardless of whether a source account has been
      provided in the form of an alias, a public key hash, or an explicit
      key - or even provided redundantly in several of these
      categories. *)
let test_stresstest_sources_format =
  Protocol.register_test
    ~__FILE__
    ~title:"stresstest sources format"
    ~tags:["stresstest"; "isolated_node"; "sources"]
  @@ fun protocol ->
  let n_bootstraps_to_use = 10 in
  let n_bootstraps_total = 2 * n_bootstraps_to_use
  and source_aliases_cutoff = 3
  and source_pkhs_cutoff = 6 in
  (* [n_bootstraps_to_use] is the number of bootstraps that we will
     provide to the [stresstest] command overall. We initialize the
     protocol with twice as many bootstraps ([n_bootstraps_total]),
     then select every other one of those to obtain the list of
     [bootstraps_to_use].

     Among the list of [bootstraps_to_use], elements of index 0 to
     [source_aliases_cutoff] (both included) will be given to the
     command as an alias alone; elements [source_aliases_cutoff] to
     [source_pkhs_cutoff] as a public key hash; elements
     [source_pkhs_cutoff] to [n_bootstraps_to_use - 1], and also
     element 0, as a full explicit key. Overlaps are intentional to
     test that the command tolerates them. The various bounds should
     be at least two units apart such that each category has at least
     one bootstrap that belongs exclusively to it.

     Below is a representation of the format distribution of sources
     provided to the [stresstest] command.

     [--------------------] All bootstraps
       | | | | | | | | | |
      [- - - - - - - - - -] Bootstraps to use
       | | | | | | | | | |
      [- - - -]             Aliases
       |     | | | | | | |
            [- - - -]       Pkhs
       |           | | | |
      [-           - - - -] Accounts
  *)
  let additional_bootstrap_account_count =
    max 0 (n_bootstraps_total - (Account.Bootstrap.keys |> Array.length))
  in
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:[Synchronisation_threshold 0; Connections 0]
      ~additional_bootstrap_account_count
      `Client
      ~protocol
      ()
  in
  (* Prepare sources for stresstest *)
  let* () = Base.repeat 2 (fun () -> Client.bake_for_and_wait client) in
  let* bootstraps_to_use =
    (* Note that bootstrap indices start at 1; we keep the ones with
       an even index. *)
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2836
       Calling Client.show_address in parallel is flaky but should not. *)
    Lwt_list.map_s
      (fun i -> Client.show_address ~alias:(sf "bootstrap%d" (2 * i)) client)
      (range 1 n_bootstraps_to_use)
  in
  let sublist_bounds_included first last l =
    (* Keep elements between positions [first] and [last] of a list,
       both included; the position of the first element is 0. The list
       must have at least [last + 1] elements (@raise Invalid_argument
       otherwise). *)
    drop first l |> take (last - first + 1)
  in
  let source_aliases =
    sublist_bounds_included 0 source_aliases_cutoff bootstraps_to_use
    |> List.map (fun account -> account.Account.alias)
  in
  let source_pkhs =
    sublist_bounds_included
      source_aliases_cutoff
      source_pkhs_cutoff
      bootstraps_to_use
    |> List.map (fun account -> account.Account.public_key_hash)
  in
  let source_accounts =
    List.hd bootstraps_to_use
    :: sublist_bounds_included
         source_pkhs_cutoff
         (n_bootstraps_to_use - 1)
         bootstraps_to_use
  in
  (* Helpers to check that operations (from the mempool or the last
     block) have the right sources. *)
  let expected_pkhs : String_set.t =
    List.map (fun account -> account.Account.public_key_hash) bootstraps_to_use
    |> String_set.of_list
  in
  let pp_pkhs fmt (pkhs : String_set.t) =
    Format.(
      pp_print_seq
        ~pp_sep:(fun fmt () -> fprintf fmt ", ")
        pp_print_string
        fmt
        (String_set.to_seq pkhs))
  in
  let check_pkhs (ops : JSON.t list) =
    let actual_pkhs =
      ops
      |> List.map (fun op ->
             JSON.(op |-> "contents" |=> 0 |-> "source" |> as_string))
      |> String_set.of_list
    in
    Check.(
      (actual_pkhs = expected_pkhs)
        (equalable pp_pkhs String_set.equal)
        ~error_msg:"Set of sources is %L; expected %R.")
  in
  (* Main loop:
     - Launch the stresstest command if it is the first iteration;
       otherwise bake a block and inspect the new block's manager
       operations (check that they number [n_bootstraps_to_use] and the
       set of their sources is [expected_pkhs]).
     - Wait for [n_bootstraps_to_use] injections.
     - Inspect the mempool's applied operations (check that they
       number [n_bootstraps_to_use] and the set of their sources is
       [expected_pkhs]). *)
  let rec loop ~first_iteration ~repeat =
    let waiter = wait_for_n_injections n_bootstraps_to_use node in
    let* () =
      if first_iteration then (
        (* Bake some blocks to reach required level for stresstest command. *)
        let* () = Base.repeat 2 (fun () -> Client.bake_for_and_wait client) in
        non_terminating_process
        @@ Client.spawn_stresstest
             ~source_aliases
             ~source_pkhs
             ~source_accounts
             ~fresh_probability:0.
             (* Prevent the command from randomly creating fresh
                accounts, as this would allow more operations than
                expected in the mempool / in the baked block. *)
             client ;
        unit)
      else
        let* () = Client.bake_for_and_wait client in
        let* ops =
          check_n_manager_operations_in_head n_bootstraps_to_use client
        in
        check_pkhs ops ;
        unit
    in
    let* () =
      let time_start_actually_waiting = Unix.gettimeofday () in
      let* () = waiter in
      Log.info
        "Waiter injection: %.2fs"
        (Unix.gettimeofday () -. time_start_actually_waiting) ;
      unit
    in
    (* Sleep to ensure that we do not validate more than
       [n_bootstraps_to_use] operations. Note that the [waiter] above
       takes about 2s to resolve. The chosen delay of 5s is longer
       than that, but not too long, because we will have to wait for
       it [repeat] times. *)
    let* () = Lwt_unix.sleep 5. in
    let* ops =
      check_n_applied_operations_in_mempool n_bootstraps_to_use client
    in
    check_pkhs ops ;
    if repeat <= 1 then unit
    else loop ~first_iteration:false ~repeat:(repeat - 1)
  in
  loop ~first_iteration:true ~repeat:3

(** Run the [stresstest] command in an isolated node with an explicit
    parameter [transfers], that makes it stop after injecting this
    number of transfers. Then check that the mempool contains this many
    applied operations, and bake a block and check that it also
    contains the same number of manager operations. *)
let test_stresstest_n_transfers =
  Protocol.register_test
    ~__FILE__
    ~title:"stresstest explicit transfers argument"
    ~tags:["stresstest"; "isolated_node"; "n_transfers"]
  @@ fun protocol ->
  let n_transfers = 10 in
  let n_bootstraps = 2 * n_transfers in
  let additional_bootstrap_account_count =
    max 0 (n_bootstraps - (Account.Bootstrap.keys |> Array.length))
  in
  let* _node, client =
    Client.init_with_protocol
      ~nodes_args:[Synchronisation_threshold 0; Connections 0]
      ~additional_bootstrap_account_count
      `Client
      ~protocol
      ()
  in
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  let source_aliases =
    List.map (fun i -> sf "bootstrap%d" i) (range 1 n_bootstraps)
  in
  (* Bake some blocks to reach required level for stresstest command. *)
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  let* () = Client.stresstest ~transfers:n_transfers ~source_aliases client in
  let* _ = check_n_applied_operations_in_mempool n_transfers client in
  let* () = Client.bake_for_and_wait client in
  let* _ = check_n_manager_operations_in_head n_transfers client in
  unit

(** Wait for [n] "arrived" request events.

    Note that event level of section [prevalidator] should be [Debug]
    for these events to exist. *)
let wait_for_n_arrivals n node =
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when s = "arrived" -> Some ()
    | Some _ | None -> None
  in
  Node.wait_for node "request_completed_debug.v0" (Daemon.n_events n filter)

(** Run the [stresstest] command on multiple connected nodes, and
    check the number of operations in the mempool of one of these nodes,
    as well as in subsequently baked blocks.

    [n_nodes] nodes are initialized with mode [Client] and with
    disjoint sets of [n_bootstraps_per_node] accounts each. A central
    node is connected to all the other nodes, which are each connected
    exclusively to the central node. This central node activates the
    [protocol], then every node runs the [stresstest] command with its
    own accounts.

    We check that after the initial command calls and after each
    subsequent baking, the mempool of the central node eventually
    contains [n_nodes * n_bootstraps_per_node] applied operations. We
    also check that the baked blocks contain the same number of manager
    operations. *)
let test_stresstest_multiple_nodes =
  Protocol.register_test
    ~__FILE__
    ~title:"stresstest multiple nodes"
    ~tags:["stresstest"; "multiple_nodes"]
  @@ fun protocol ->
  let n_nodes = 4 in
  let n_bootstraps_per_node = 5 in
  let n_bootstraps_total = n_nodes * n_bootstraps_per_node in
  let additional_bootstrap_account_count =
    max 0 (n_bootstraps_total - (Account.Bootstrap.keys |> Array.length))
  in
  let* central_node, central_client =
    Client.init_with_protocol
      ~nodes_args:Node.[Synchronisation_threshold 0; Connections (n_nodes - 1)]
      ~event_sections_levels:[("prevalidator", `Debug)]
        (* for "arrived" request events *)
      ~additional_bootstrap_account_count
      `Client
      ~protocol
      ()
  in
  (* Bake some blocks to reach required level for stresstest command. *)
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait central_client) in
  let* _ = Node.wait_for_level central_node 3 in
  let get_accounts_from_num_range (num_range : int list) =
    let aliases = List.map (sf "bootstrap%d") num_range in
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2836
       Calling Client.show_address in parallel is flaky but should not. *)
    Lwt_list.map_s
      (fun alias -> Client.show_address ~alias central_client)
      aliases
  in
  let* central_node_accounts =
    get_accounts_from_num_range (range 1 n_bootstraps_per_node)
  in
  let* other_nodes_clients_accounts =
    Lwt_list.map_p
      (fun i ->
        let* accounts =
          get_accounts_from_num_range
          @@ range
               (((i + 1) * n_bootstraps_per_node) + 1)
               ((i + 2) * n_bootstraps_per_node)
        in
        let* node, client =
          Client.init_with_node
            ~nodes_args:Node.[Synchronisation_threshold 0; Connections 1]
            ~keys:accounts
            `Client
            ()
        in
        let* () = Client.Admin.connect_address central_client ~peer:node in
        let* _ = Node.wait_for_level node 3 in
        return (node, client, accounts))
      (List.init (n_nodes - 1) Fun.id)
  in
  let nodes_clients_accounts =
    (central_node, central_client, central_node_accounts)
    :: other_nodes_clients_accounts
  in
  let n_expected_arrivals = (n_nodes - 1) * n_bootstraps_per_node in
  let setup_waiter () =
    let waiter_injection =
      wait_for_n_injections n_bootstraps_per_node central_node
    and waiter_arrival = wait_for_n_arrivals n_expected_arrivals central_node in
    fun (time_start_actually_waiting : float) ->
      let* () =
        let* _ = waiter_injection in
        Log.info
          "Waiter injection: %.2fs"
          (Unix.gettimeofday () -. time_start_actually_waiting) ;
        unit
      and* () =
        let* _ = waiter_arrival in
        Log.info
          "Waiter arrival: %.2fs"
          (Unix.gettimeofday () -. time_start_actually_waiting) ;
        unit
      in
      unit
  in
  let rec loop ~first_iteration ~repeat =
    let waiter = setup_waiter () in
    let* () =
      if first_iteration then (
        List.iter
          (fun (_, client, source_accounts) ->
            non_terminating_process
            @@ Client.spawn_stresstest
                 ~source_accounts
                 ~fresh_probability:0.
                 (* Prevent the command from randomly creating fresh
                    accounts, as this would allow more operations than
                    expected in the mempool / in the baked block. *)
                 client)
          nodes_clients_accounts ;
        unit)
      else
        let* () = Client.bake_for_and_wait central_client in
        let* _ =
          check_n_manager_operations_in_head n_bootstraps_total central_client
        in
        unit
    in
    let* () = waiter (Unix.gettimeofday ()) in
    (* Sleep to ensure that we do not inject/receive more operations
       than expected. Note that we spend at most around 1s waiting for
       the [waiter] right above. The chosen delay of 5s is longer than
       that, but not too long, considering that we will have to wait
       for it [repeat] times. *)
    let* () = Lwt_unix.sleep 5. in
    let* _ =
      check_n_applied_operations_in_mempool n_bootstraps_total central_client
    in
    if repeat <= 1 then unit
    else loop ~first_iteration:false ~repeat:(repeat - 1)
  in
  loop ~first_iteration:true ~repeat:3

let register ~protocols =
  test_stresstest_sources_format protocols ;
  test_stresstest_n_transfers protocols ;
  test_stresstest_multiple_nodes protocols
