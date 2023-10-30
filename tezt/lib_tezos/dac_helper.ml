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

let test ~__FILE__ ?(tags = []) ?uses ?supports title f =
  let tags = "dac" :: tags in
  Protocol.register_test ~__FILE__ ~title ~tags ?uses ?supports f

let regression_test ~__FILE__ ?(tags = []) ?uses ?supports title f =
  let tags = "dac" :: tags in
  Protocol.register_regression_test ~__FILE__ ~title ~tags ?uses ?supports f

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

let with_coordinator_node ?name ?sc_rollup_node ?(pvm_name = "arith")
    ?(wait_ready = true) ?(allow_v1_api = false) ~committee_members tezos_node
    tezos_client f =
  let reveal_data_dir =
    Option.map
      (fun sc_rollup_node ->
        Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
      sc_rollup_node
  in
  let dac_node =
    Dac_node.create_coordinator
      ?name
      ~node:tezos_node
      ~client:tezos_client
      ?reveal_data_dir
      ~allow_v1_api
      ~committee_members:
        (List.map
           (fun (dc : Account.aggregate_key) -> dc.aggregate_public_key)
           committee_members)
      ()
  in
  let* _dir = Dac_node.init_config dac_node in
  let* () = Dac_node.run dac_node ~wait_ready in
  f dac_node committee_members

let with_committee_member ?name ?sc_rollup_node ?(pvm_name = "arith")
    ?(wait_ready = true) ?(allow_v1_api = false) ~committee_member tezos_node
    coordinator_node tezos_client f =
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
      ~allow_v1_api
      ~address:public_key_hash
      ()
  in
  let* _dir = Dac_node.init_config dac_node in
  let* () = Dac_node.run dac_node ~wait_ready in
  f dac_node committee_member

let with_observer ?name ?sc_rollup_node ?(pvm_name = "arith")
    ?(wait_ready = true) ?(allow_v1_api = false) ~committee_member_rpcs
    tezos_node coordinator_node tezos_client f =
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
      ~allow_v1_api
      ~committee_member_rpcs
      ()
  in
  let* _dir = Dac_node.init_config dac_node in
  let* () = Dac_node.run dac_node ~wait_ready in
  f dac_node

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4706
   Keep pvm name value in Sc_rollup.t. *)
let with_fresh_rollup ?(pvm_name = "arith") ?hooks tezos_node tezos_client
    bootstrap1_key f =
  let sc_rollup_node =
    Sc_rollup_node.create
      Operator
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~default_operator:bootstrap1_key
  in
  let* rollup_address =
    Client.Sc_rollup.originate
      ?hooks
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

let scenario_with_full_dac_infrastructure ?supports ?(tags = ["dac"; "full"])
    ?(pvm_name = "arith") ?(custom_committee_members = []) ?commitment_period
    ?challenge_window ?event_sections_levels ?node_arguments
    ?(allow_v1_api = false) ?(allow_regression = false) ~__FILE__
    ~committee_size ~observers variant scenario =
  let description = "Testing Full DAC infrastructure" in
  (if allow_regression then regression_test else test)
    ?supports
    ~__FILE__
    ~tags
    ~uses:(fun _protocol -> [Constant.octez_dac_node])
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1
        ?commitment_period
        ?challenge_window
        ?event_sections_levels
        ?node_arguments
        ~protocol
      @@ fun node client key ->
      with_fresh_rollup ~pvm_name node client key
      @@ fun sc_rollup_address sc_rollup_node ->
      let range i = List.init i Fun.id in
      let* committee_members =
        List.fold_left
          (fun keys i ->
            let* keys in
            let* key =
              Client.bls_gen_and_show_keys
                ~alias:(Format.sprintf "committee-member-%d" i)
                client
            in
            return (key :: keys))
          (return [])
          (range committee_size)
      in
      let* () =
        Lwt_list.iter_s
          (fun (aggregate_key : Account.aggregate_key) ->
            Client.bls_import_secret_key aggregate_key client)
          custom_committee_members
      in
      let committee_members =
        List.append committee_members custom_committee_members
      in
      (* Use a fresh tezos client for the coordinator. This is needed to make
         sure that the coordinator does not have access to the secret keys of
         the coordinator stored in the wallet directory of [client]. *)
      let* coordinator_wallet_client = Client.init () in
      with_coordinator_node
        node
        coordinator_wallet_client
        ~name:"coordinator"
        ~pvm_name
        ~committee_members
        ~allow_v1_api
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
              ~allow_v1_api
              ())
          committee_members
      in
      let committee_member_rpcs =
        List.map
          (fun committee_member_node ->
            ( Dac_node.rpc_host committee_member_node,
              Dac_node.rpc_port committee_member_node ))
          committee_members_nodes
      in
      let rollup_nodes, observer_nodes =
        List.init observers Fun.id
        |> List.map (fun i ->
               let rollup_node_i =
                 Sc_rollup_node.create
                   ~name:("observer-" ^ Int.to_string i ^ "-rollup-node")
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
                   ~committee_member_rpcs
                   ~allow_v1_api
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
let scenario_with_layer1_node ?(tags = ["dac"; "layer1"]) ?uses
    ?commitment_period ?challenge_window ?event_sections_levels ?node_arguments
    ~__FILE__ variant scenario =
  let description = "Testing DAC L1 integration" in
  test
    ~__FILE__
    ~tags
    ?uses
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1
        ?commitment_period
        ?challenge_window
        ?event_sections_levels
        ?node_arguments
        ~protocol
      @@ fun node client key -> scenario protocol node client key)

module Call_endpoint = struct
  module V0 = struct
    let get_preimage dac_node page_hash =
      Dac_node.RPC.call dac_node (Dac_rpc.V0.get_preimage page_hash)

    let put_dac_member_signature dac_node ~hex_root_hash ~dac_member_pkh
        ~signature =
      Dac_node.RPC.call
        dac_node
        (Dac_rpc.V0.put_dac_member_signature
           ~hex_root_hash
           ~dac_member_pkh
           ~signature)

    let get_missing_page dac_node ~hex_root_hash =
      Dac_node.RPC.call dac_node (Dac_rpc.V0.get_missing_page ~hex_root_hash)

    let get_certificate dac_node ~hex_root_hash =
      Dac_node.RPC.call dac_node (Dac_rpc.V0.get_certificate ~hex_root_hash)

    module Coordinator = struct
      let post_preimage dac_node ~payload =
        Dac_node.RPC.call
          dac_node
          (Dac_rpc.V0.Coordinator.post_preimage ~payload)
    end
  end

  module V1 = struct
    let get_pages dac_node page_hash =
      Dac_node.RPC.call dac_node (Dac_rpc.V1.get_pages page_hash)
  end

  let get_health_live dac_node =
    Dac_node.RPC.call dac_node Dac_rpc.get_health_live

  let get_health_ready dac_node =
    Dac_node.RPC.call dac_node Dac_rpc.get_health_ready
end
