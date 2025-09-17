(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Agent_kind
open Scenarios_helpers
open Tezos
open Yes_crypto

(** This module allows to configure NginX as a reverse proxy in
      front of a collection of DAL nodes. This allows to balance the
      load on the DAL nodes based on which DAL slot index is used
      while complying with the interface of the rollup node, which
      expects a single DAL node endpoint.

      The NginX reverse-proxy configuration is added as an NginX site
      (in directory /etc/nginx/sites-available/ which is symlinked in
      /etc/nginx/sites-enabled/). This module makes no assumption
      about the existence of other NginX sites and should be invisible
      to them except that:

      - it disables the "default" NginX site (the file
        /etc/nginx/sites-enabled/default is removed if it exists),
      - it overrides the "reverse_proxy" NgninX site if there is some.
  *)

(* An NginX configuration file is made of directives inside
   potentially nested context blocks. *)
type config = Directive of string | Context_block of string * config list

let rec pp_config out = function
  | Directive s -> Format.fprintf out "%s;" s
  | Context_block (head, l) ->
      Format.fprintf
        out
        "@[<v 2>%s {@;%a@]@;}"
        head
        (Format.pp_print_list
           ~pp_sep:(fun out () -> Format.fprintf out "@;")
           pp_config)
        l

let generate_nginx_config ~port out ~default_endpoint
    (producers : (int * string) Seq.t) =
  Format.fprintf
    out
    "@[<v 0>%a@]@."
    pp_config
    (Context_block
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
         ] ))

let init_reverse_proxy cloud ~next_agent ~default_endpoint ~index
    (proxified_dal_nodes : (int * string) Seq.t) =
  (* A NGINX reverse proxy which balances load between producer DAL
     nodes based on the requested slot index. *)
  let name = name_of Reverse_proxy in
  let* agent = next_agent ~name in
  let runner = Agent.runner agent in
  let port = Agent.next_available_port agent in
  let () = toplog "Launching reverse proxy" in
  let () = toplog "Generating nginx reverse proxy config" in
  let config_filename = Format.sprintf "nginx_reverse_proxy_config_%d" index in
  let out_chan = Stdlib.open_out config_filename in
  let config_ppf = Format.formatter_of_out_channel out_chan in
  Format.fprintf
    config_ppf
    "%a"
    (generate_nginx_config ~default_endpoint ~port)
    proxified_dal_nodes ;
  close_out out_chan ;

  (* Upload the configuration file. *)
  let* (_ : string) =
    Agent.copy
      ~destination:
        (Format.sprintf "/etc/nginx/sites-available/reverse_proxy_%d" index)
      ~source:config_filename
      agent
  in

  (* Disable the default configuration *)
  let* () =
    Process.spawn ?runner "rm" ["-f"; "/etc/nginx/sites-enabled/default"]
    |> Process.check
  in

  (* Enable the reverse proxy configuration *)
  let* () =
    Process.spawn
      ?runner
      "ln"
      [
        "-s";
        Format.sprintf "/etc/nginx/sites-available/reverse_proxy_%d" index;
        Format.sprintf "/etc/nginx/sites-enabled/reverse_proxy_%d" index;
      ]
    |> Process.check
  in

  (* Check the NginX configuration *)
  let* () = Process.spawn ?runner "nginx" ["-t"] |> Process.check in

  (* Start the NginX service *)
  let* () =
    (* If the service can be stopped (i.e. the command doesn't fail), we
       probably are in a SysV system. Nginx will run as a service natively.
       Otherwise, we need to start it manually. *)
    let* service_cmd =
      Process.spawn ?runner "service" ["nginx"; "stop"] |> Process.wait
    in
    match Process.validate_status service_cmd with
    | Ok () ->
        Process.spawn ?runner "service" ["nginx"; "start"] |> Process.check
    | Error _ ->
        (* Runs the process as a daemon, and don't bind the process, otherwise
           Tezt will wait for it to finish.
        *)
        let _process = Process.spawn ?runner "nginx" ["-g"; "daemon on;"] in
        unit
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
  init_reverse_proxy
    cloud
    ~next_agent
    ~default_endpoint
    ~index
    (List.to_seq dal_slots_and_nodes)
