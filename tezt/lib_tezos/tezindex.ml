(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {process : Process.t; rpc_port : int; base_dir : string}

let run ?runner ?(path = Uses.path Constant.octez_tezindex) ?name ~node
    ?rpc_port ?(watched_addresses = []) () =
  let rpc_port = match rpc_port with Some p -> p | None -> Port.fresh () in
  let base_dir = Temp.dir ?runner "tezindex" in
  let rpc_addr = sf "localhost:%d" rpc_port in
  let node_endpoint = sf "http://localhost:%d" (Node.rpc_port node) in
  let name = match name with Some n -> n | None -> "octez-tezindex" in
  let watched_args =
    List.concat_map (fun addr -> ["--watched-address"; addr]) watched_addresses
  in
  let process =
    Process.spawn
      ~name
      ?runner
      path
      ([
         "--base-dir";
         base_dir;
         "--endpoint";
         node_endpoint;
         "run";
         "--rpc-addr";
         rpc_addr;
       ]
      @ watched_args)
  in
  {process; rpc_port; base_dir}

let rpc_url t path = sf "http://localhost:%d%s" t.rpc_port path

let wait_for_ready ?(attempts = 30) t =
  let url = rpc_url t "/health" in
  let rec loop remaining =
    if remaining <= 0 then
      Test.fail "Tezindex health endpoint did not respond in time" ;
    let process = Process.spawn ~log_output:false "curl" ["-sf"; url] in
    let* status = Process.wait process in
    match status with
    | Unix.WEXITED 0 -> unit
    | _ ->
        let* () = Lwt_unix.sleep 1.0 in
        loop (remaining - 1)
  in
  loop attempts

let get_v1_rewards_split t ~baker ~cycle =
  let url = rpc_url t (sf "/v1/rewards/split/%s/%d" baker cycle) in
  let* json = Curl.get url |> Runnable.run in
  Lwt.return json
