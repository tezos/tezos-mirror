(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let toplog (fmt : ('a, Format.formatter, unit, unit) format4) : 'a =
  Log.info ~prefix:"TOP" ~color:Log.Color.FG.green fmt

let init_teztale cloud agent =
  let () = toplog "Initialize Teztale server" in
  let* teztale =
    Tezos.Teztale.run_server
      ?artifact_dir:
        (if Tezt_cloud_cli.teztale_artifacts then Tezt_cloud_cli.artifacts_dir
         else None)
      cloud
      agent
  in
  let* () = Tezos.Teztale.wait_server teztale in
  let () = toplog "Teztale server is ready" in
  let* () =
    let domain = Agent.point agent |> Option.fold ~none:"localhost" ~some:fst in
    let port = teztale.server.conf.interface.port in
    let url = sf "http://%s:%d" domain port in
    let () = toplog "Teztale server URL is '%s'" url in
    Cloud.add_service cloud ~name:"teztale" ~url
  in
  Lwt.return teztale

let add_prometheus_source ?dal_node ?sc_rollup_node ?evm_node ?node cloud agent
    name =
  let agent_name = Agent.name agent in
  let target port app_name =
    Cloud.{agent; port; app_name = Format.asprintf "%s:%s" agent_name app_name}
  in
  let node_metric_target =
    Option.map
      (fun node -> target (Node.metrics_port node) (Node.name node))
      node
  in
  let dal_node_metric_target =
    Option.map
      (fun dal_node ->
        target (Dal_node.metrics_port dal_node) (Dal_node.name dal_node))
      dal_node
  in
  let sc_rollup_node_metric_target =
    Option.map
      (fun sc_rollup_node ->
        target
          (Sc_rollup_node.metrics sc_rollup_node |> snd)
          (Sc_rollup_node.name sc_rollup_node))
      sc_rollup_node
  in
  let evm_node_metric_target =
    Option.map
      (fun evm_node ->
        target
          (* Note: Evm node uses the same port for rpc and for metrics *)
          (Tezos.Evm_node.rpc_port evm_node)
          (Tezt_etherlink.Evm_node.name evm_node))
      evm_node
  in
  let targets =
    List.filter_map
      Fun.id
      [
        node_metric_target;
        dal_node_metric_target;
        sc_rollup_node_metric_target;
        evm_node_metric_target;
      ]
  in
  Cloud.add_prometheus_source cloud ~name targets

let init_explorus cloud node =
  Cloud.add_service
    cloud
    ~name:"Explorus"
    ~url:(sf "http://explorus.io?network=%s" (Node.rpc_endpoint node))

(* Some DAL nodes (those in operator mode) refuse to start unless they are
   connected to an Octez node keeping enough history to play refutation
   games. *)
let refutation_game_minimal_rolling_history_mode =
  Node.(History_mode (Rolling (Some 79)))

let default_page_size = 3967

let default_slot_size = 32 * default_page_size
