(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Scenarios_helpers
open Tezos
open Yes_crypto

let generate_nginx_config ~port ~default_endpoint
    (producers : (int * string) Seq.t) =
  let open Nginx_reverse_proxy in
  [
    Context_block
      ( "server",
        [
          Directive (sf "listen 0.0.0.0:%d" port);
          (* When fetching data about a slot the RPC are of the
              form (GET /levels/<level>/slots/<slot_index>/...); we
              direct the request to a DAL node subscribed to the
              topics of the queried slot index. *)
          Context_block
            ( "location /levels/",
              producers
              |> Seq.map (fun (slot_index, endpoint) ->
                     Context_block
                       ( sf "location ~ ^/levels/[0-9]+?/slots/%d/" slot_index,
                         [Directive (sf "proxy_pass %s" endpoint)] ))
              |> List.of_seq );
          (* When posting a DAL slot, the format of the RPC is POST
              /slots/?slot_index=<slot_index>. In this case too we
              redirect the request to a DAL node subscribed to the
              topics of the queried slot index. *)
          Context_block
            ( "location /slots",
              producers
              |> Seq.map (fun (slot_index, endpoint) ->
                     Context_block
                       ( sf "if ($query_string ~ \"slot_index=%d\")" slot_index,
                         [Directive (sf "proxy_pass %s" endpoint)] ))
              |> List.of_seq );
          (* Other queries can be answered by any DAL node. *)
          Context_block
            ("location /", [Directive (sf "proxy_pass %s" default_endpoint)]);
        ] );
  ]

let init_dal_reverse_proxy_observers ~external_rpc ~network ~snapshot
    ~ppx_profiling_verbosity ~ppx_profiling_backends ~memtrace ~simulate_network
    ~name_of ~default_endpoint ~node_p2p_endpoint ~dal_node_p2p_endpoint
    ~dal_slots ~index ~next_agent ~otel ~cloud =
  if dal_slots = [] then failwith "Expected at least a DAL slot." ;
  let* dal_slots_and_nodes =
    dal_slots
    |> Lwt_list.map_p (fun slot_index ->
           let name = name_of slot_index in
           let* agent = next_agent ~name in
           let env, with_yes_crypto = may_set_yes_crypto_env simulate_network in
           let* node =
             Node_helpers.init
               ?env
               ~name
               ~arguments:
                 [
                   Peer node_p2p_endpoint;
                   refutation_game_minimal_rolling_history_mode;
                 ]
               ~rpc_external:external_rpc
               ~with_yes_crypto
               ~snapshot
               ~ppx_profiling_verbosity
               ~ppx_profiling_backends
               network
               cloud
               agent
           in
           let* dal_node = Dal_node.Agent.create ~name ~node cloud agent in
           let* () =
             Dal_node.init_config
               ~expected_pow:(Network.expected_pow network)
               ~operator_profiles:[slot_index]
               ~peers:(Option.to_list dal_node_p2p_endpoint)
               dal_node
           in
           let* () =
             Dal_node.Agent.run
               ~prometheus:Tezt_cloud_cli.prometheus
               ~event_level:`Notice
               ?otel
               ~memtrace
               ~ppx_profiling_verbosity
               ~ppx_profiling_backends
               dal_node
           in
           return (slot_index, Dal_node.rpc_endpoint dal_node))
  in
  let default_endpoint =
    match default_endpoint with
    | Some e -> e
    | None ->
        let _, first_observer_endpoint = List.hd dal_slots_and_nodes in
        first_observer_endpoint
  in
  let name = Agent_kind.(name_of Reverse_proxy) in
  let* agent = next_agent ~name in
  let port = Agent.next_available_port agent in
  let* () =
    Nginx_reverse_proxy.init
      ~agent
      ~site:(sf "reverse_proxy_%d" index)
      (generate_nginx_config
         ~default_endpoint
         ~port
         (List.to_seq dal_slots_and_nodes))
  in
  (* In order to pass the reverse proxy to the various Tezt helpers we
     need to pretend to be a DAL node. The simplest way to do so is to
     call Dal_node.Agent.create_from_endpoint with the appropriate
     rpc_port and never call Dal_node.run on the result.

     Since the DAL node never runs, it does not call it's L1 endpoint. *)
  let l1_node_endpoint = Endpoint.make ~host:"" ~scheme:"" ~port:0 () in
  let* dal_node =
    Dal_node.Agent.create_from_endpoint
      ~name:(Format.sprintf "reverse-proxy-dal-node-%d" index)
      ~rpc_port:port
      cloud
      agent
      ~l1_node_endpoint
  in
  return dal_node
