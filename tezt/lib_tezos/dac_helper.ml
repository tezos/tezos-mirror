(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
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

let hooks = Tezos_regression.hooks

module Scenarios = struct
  type full = {
    protocol : Protocol.t;
    node : Node.t;
    client : Client.t;
    key : string;
    sc_rollup_address : string;
    sc_rollup_node : Sc_rollup_node.t;
    coordinator_node : Dac_node.t;
    committee_members : Account.aggregate_key list;
    committee_members_nodes : Dac_node.t list;
    observer_nodes : Dac_node.t list;
    rollup_nodes : Sc_rollup_node.t list;
  }
end

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/3173
   The functions below are duplicated from sc_rollup.ml.
   They should be moved to a common submodule. *)
let make_int_parameter name = function
  | None -> []
  | Some value -> [(name, `Int value)]

let test ~__FILE__ ?(tags = []) ?supports title f =
  let tags = "dac" :: tags in
  Protocol.register_test ~__FILE__ ~title ~tags ?supports f

let regression_test ~__FILE__ ?(tags = []) title f =
  let tags = "dac" :: tags in
  Protocol.register_regression_test ~__FILE__ ~title ~tags f

(* Some initialization functions to start needed nodes. *)

let setup_node ?(additional_bootstrap_accounts = 5) ~parameters ~protocol
    ?(event_sections_levels = []) ?(node_arguments = []) () =
  (* Temporary setup to initialise the node. *)
  let base = Either.right (protocol, None) in
  let* parameter_file = Protocol.write_parameter_file ~base parameters in
  let* _client = Client.init_mockup ~parameter_file ~protocol () in
  let nodes_args =
    Node.
      [
        Synchronisation_threshold 0; History_mode (Full None); No_bootstrap_peers;
      ]
  in
  let node = Node.create nodes_args in
  let* () = Node.config_init node [] in
  let* () = Node.run node ~event_sections_levels node_arguments in
  let* () = Node.wait_for_ready node in
  let* client = Client.init ~endpoint:(Node node) () in
  let* additional_account_keys =
    Client.stresstest_gen_keys additional_bootstrap_accounts client
  in
  let additional_bootstrap_accounts =
    List.map (fun x -> (x, None, false)) additional_account_keys
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~additional_bootstrap_accounts
      ~base
      parameters
  in
  let* () =
    Client.activate_protocol_and_wait ~parameter_file ~protocol client
  in
  return (node, client)

let with_layer1 ?additional_bootstrap_accounts ?commitment_period
    ?challenge_window ?event_sections_levels ?node_arguments f ~protocol =
  let parameters =
    make_int_parameter
      ["smart_rollup_commitment_period_in_blocks"]
      commitment_period
    @ make_int_parameter
        ["smart_rollup_challenge_window_in_blocks"]
        challenge_window
    @ [(["smart_rollup_arith_pvm_enable"], `Bool true)]
  in
  let* node, client =
    setup_node
      ?additional_bootstrap_accounts
      ?event_sections_levels
      ?node_arguments
      ~parameters
      ~protocol
      ()
  in
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  f node client bootstrap1_key

let with_legacy_dac_node ?name ?sc_rollup_node ?(pvm_name = "arith")
    ?(wait_ready = true) ?committee_member_address ~threshold ~committee_size
    tezos_node tezos_client f =
  let range i = List.init i Fun.id in
  let reveal_data_dir =
    Option.map
      (fun sc_rollup_node ->
        Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
      sc_rollup_node
  in
  let* committee_members =
    List.fold_left
      (fun keys i ->
        let* keys in
        let* key =
          Client.bls_gen_and_show_keys
            ~alias:(Format.sprintf "dac-member-%d" i)
            tezos_client
        in
        return (key :: keys))
      (return [])
      (range committee_size)
  in
  let dac_node =
    Dac_node.create_legacy
      ?name
      ~node:tezos_node
      ~client:tezos_client
      ?reveal_data_dir
      ~threshold
      ?committee_member_address
      ~committee_members:
        (List.map
           (fun (dc : Account.aggregate_key) -> dc.aggregate_public_key_hash)
           committee_members)
      ()
  in
  let* _dir = Dac_node.init_config dac_node in
  let* () = Dac_node.run dac_node ~wait_ready in
  f dac_node committee_members

let with_coordinator_node ?name ?sc_rollup_node ?(pvm_name = "arith")
    ?(wait_ready = true) ?(custom_committee_members = []) ~committee_size
    tezos_node tezos_client f =
  let range i = List.init i Fun.id in
  let reveal_data_dir =
    Option.map
      (fun sc_rollup_node ->
        Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
      sc_rollup_node
  in
  let* committee_members =
    List.fold_left
      (fun keys i ->
        let* keys in
        let* key =
          Client.bls_gen_and_show_keys
            ~alias:(Format.sprintf "committee-member-%d" i)
            tezos_client
        in
        return (key :: keys))
      (return [])
      (range committee_size)
  in
  let* () =
    Lwt_list.iter_s
      (fun (aggregate_key : Account.aggregate_key) ->
        Client.bls_import_secret_key aggregate_key tezos_client)
      custom_committee_members
  in
  let committee_members =
    List.append committee_members custom_committee_members
  in
  let dac_node =
    Dac_node.create_coordinator
      ?name
      ~node:tezos_node
      ~client:tezos_client
      ?reveal_data_dir
      ~committee_members:
        (List.map
           (fun (dc : Account.aggregate_key) -> dc.aggregate_public_key_hash)
           committee_members)
      ()
  in
  let* _dir = Dac_node.init_config dac_node in
  let* () = Dac_node.run dac_node ~wait_ready in
  f dac_node committee_members

let with_committee_member ?name ?sc_rollup_node ?(pvm_name = "arith")
    ?(wait_ready = true) ~committee_member tezos_node coordinator_node
    tezos_client f =
  let reveal_data_dir =
    Option.map
      (fun sc_rollup_node ->
        Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
      sc_rollup_node
  in
  let Account.{public_key_hash; _} = committee_member in
  let dac_node =
    Dac_node.create_committee_member
      ?name
      ~node:tezos_node
      ~client:tezos_client
      ?reveal_data_dir
      ~coordinator_rpc_host:(Dac_node.rpc_host coordinator_node)
      ~coordinator_rpc_port:(Dac_node.rpc_port coordinator_node)
      ~address:public_key_hash
      ()
  in
  let* _dir = Dac_node.init_config dac_node in
  let* () = Dac_node.run dac_node ~wait_ready in
  f dac_node committee_member

let with_observer ?name ?sc_rollup_node ?(pvm_name = "arith")
    ?(wait_ready = true) tezos_node coordinator_node tezos_client f =
  let reveal_data_dir =
    Option.map
      (fun sc_rollup_node ->
        Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
      sc_rollup_node
  in
  let dac_node =
    Dac_node.create_observer
      ?name
      ~node:tezos_node
      ~client:tezos_client
      ?reveal_data_dir
      ~coordinator_rpc_host:(Dac_node.rpc_host coordinator_node)
      ~coordinator_rpc_port:(Dac_node.rpc_port coordinator_node)
      ()
  in
  let* _dir = Dac_node.init_config dac_node in
  let* () = Dac_node.run dac_node ~wait_ready in
  f dac_node

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4706
   Keep pvm name value in Sc_rollup.t. *)
let with_fresh_rollup ?(pvm_name = "arith") ~protocol tezos_node tezos_client
    bootstrap1_key f =
  let sc_rollup_node =
    Sc_rollup_node.create
      ~protocol
      Operator
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~default_operator:bootstrap1_key
  in
  let* rollup_address =
    Client.Sc_rollup.originate
      ~hooks
      ~burn_cap:Tez.(of_int 9999999)
      ~alias:"rollup"
      ~src:bootstrap1_key
      ~kind:pvm_name
      ~boot_sector:""
      ~parameters_ty:"string"
      tezos_client
  in
  let* () = Client.bake_for_and_wait tezos_client in
  f rollup_address sc_rollup_node

let scenario_with_full_dac_infrastructure ?(tags = ["dac"; "full"])
    ?(pvm_name = "arith") ?(custom_committee_members = []) ?commitment_period
    ?challenge_window ?event_sections_levels ?node_arguments ~__FILE__
    ~committee_size ~observers variant scenario =
  let description = "Testing Full DAC infrastructure" in
  test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1
        ?commitment_period
        ?challenge_window
        ?event_sections_levels
        ?node_arguments
        ~protocol
      @@ fun node client key ->
      with_fresh_rollup ~protocol ~pvm_name node client key
      @@ fun sc_rollup_address sc_rollup_node ->
      with_coordinator_node
        node
        client
        ~name:"coordinator"
        ~pvm_name
        ~custom_committee_members
        ~committee_size
      @@ fun coordinator_node committee_members ->
      let committee_members_nodes =
        List.mapi
          (fun i Account.{aggregate_public_key_hash; _} ->
            Dac_node.create_committee_member
              ~name:("committee-member-" ^ Int.to_string i)
              ~node
              ~client
              ~coordinator_rpc_host:(Dac_node.rpc_host coordinator_node)
              ~coordinator_rpc_port:(Dac_node.rpc_port coordinator_node)
              ~address:aggregate_public_key_hash
              ())
          committee_members
      in
      let rollup_nodes, observer_nodes =
        List.init observers Fun.id
        |> List.map (fun i ->
               let rollup_node_i =
                 Sc_rollup_node.create
                   ~name:("observer-" ^ Int.to_string i ^ "-rollup-node")
                   ~protocol
                   Operator
                   node
                   ~base_dir:(Client.base_dir client)
                   ~default_operator:key
               in
               let reveal_data_dir =
                 Filename.concat
                   (Sc_rollup_node.data_dir rollup_node_i)
                   pvm_name
               in
               let dac_node_i =
                 Dac_node.create_observer
                   ~name:("observer-" ^ Int.to_string i)
                   ~node
                   ~client
                   ~reveal_data_dir
                   ~coordinator_rpc_host:(Dac_node.rpc_host coordinator_node)
                   ~coordinator_rpc_port:(Dac_node.rpc_port coordinator_node)
                   ()
               in
               (rollup_node_i, dac_node_i))
        |> List.split
      in
      scenario
        Scenarios.
          {
            protocol;
            node;
            client;
            key;
            coordinator_node;
            committee_members;
            committee_members_nodes;
            observer_nodes;
            rollup_nodes;
            sc_rollup_address;
            sc_rollup_node;
          })

(* Wrapper scenario functions that should be re-used as much as possible when
   writing tests. *)
let scenario_with_layer1_node ?(tags = ["dac"; "layer1"]) ?commitment_period
    ?challenge_window ?event_sections_levels ?node_arguments ~__FILE__ variant
    scenario =
  let description = "Testing DAC L1 integration" in
  test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1
        ?commitment_period
        ?challenge_window
        ?event_sections_levels
        ?node_arguments
        ~protocol
      @@ fun node client key -> scenario protocol node client key)

let scenario_with_layer1_and_legacy_dac_nodes
    ?(tags = ["dac"; "layer1"; "legacy"]) ?commitment_period ?challenge_window
    ~__FILE__ ~threshold ~committee_size variant scenario =
  let description = "Testing DAC node" in
  test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1 ?commitment_period ?challenge_window ~protocol
      @@ fun node client _key ->
      with_legacy_dac_node ~threshold ~committee_size node client
      @@ fun dac_node committee_members ->
      scenario protocol node client dac_node threshold committee_members)

let scenario_with_layer1_legacy_and_rollup_nodes
    ?(tags = ["dac"; "dac_node"; "legacy"]) ?(pvm_name = "arith")
    ?commitment_period ?challenge_window ?committee_member_address ~__FILE__
    ~threshold ~committee_size variant scenario =
  let description = "Testing DAC rollup and node with L1" in
  regression_test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1 ?commitment_period ?challenge_window ~protocol
      @@ fun node client key ->
      with_fresh_rollup
        node
        client
        key
        ~protocol
        ~pvm_name
        (fun sc_rollup_address sc_rollup_node ->
          with_legacy_dac_node
            node
            ~sc_rollup_node
            ~pvm_name
            ~threshold
            ~committee_size
            ?committee_member_address
            client
          @@ fun dac_node committee_members ->
          scenario
            protocol
            dac_node
            sc_rollup_node
            sc_rollup_address
            node
            client
            pvm_name
            threshold
            committee_members))
