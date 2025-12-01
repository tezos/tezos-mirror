(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Agent_kind
open Yes_crypto
open Tezos
open Scenarios_helpers

type t = {tps : int; seed : int}

type stresstester = {
  node : Node.t;
  client : Client.t;
  accounts : Account.key list;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {tps; seed} -> (tps, Some seed))
    (fun (tps, seed_opt) ->
      {tps; seed = Option.value ~default:(Random.int 1073741823) seed_opt})
    (obj2 (req "tps" int31) (opt "seed" int31))

let to_string {tps; seed} = Format.sprintf "TPS : %d / SEED : %d" tps seed

let typ =
  Clap.typ
    ~name:"stresstest"
    ~dummy:{tps = 0; seed = 0}
    ~parse:(fun str ->
      match str |> String.split_on_char '/' with
      | [n] ->
          Some
            {tps = int_of_string n; seed = Random.int 1073741823 (* 2^30 -1 *)}
      | [n; seed] -> Some {tps = int_of_string n; seed = int_of_string seed}
      | _ -> None)
    ~show:to_string

(** Max number of TPS one stresstest node is expected to reach *)
let stresstest_max_tps_per_node = 50

(** Max number of pkh handled by one stresstest node *)
let stresstest_max_pkh_per_node = 100

let div_ceil a b = (a + b - 1) / b

(** For a given [network], returns the number of stesstest node to spawn
    in order to achieve [tps] tps. *)
let nb_stresstester network tps =
  let n1 = div_ceil tps stresstest_max_tps_per_node in
  let n2 =
    div_ceil (tps * Network.block_time network) stresstest_max_pkh_per_node
  in
  max 1 (max n1 n2)

let init_stresstester ?seed ~data_dir ~network ~external_rpc ~simulate_network
    ~snapshot ~node_p2p_endpoint ~ppx_profiling_verbosity
    ~ppx_profiling_backends accounts cloud i agent =
  let name = name_of_daemon (Stresstester i) in
  let data_dir = data_dir |> Option.map (fun data_dir -> data_dir // name) in
  let env, with_yes_crypto = may_set_yes_crypto_env simulate_network in
  let () = toplog "Init stresstester %s: init L1 node" name in
  let* node =
    Node_helpers.init
      ?env
      ?data_dir
      ~arguments:Node.[Peer node_p2p_endpoint]
      ~name
      ~rpc_external:external_rpc
      network
      ~with_yes_crypto
      ~snapshot
      ~ppx_profiling_verbosity
      ~ppx_profiling_backends
      cloud
      agent
  in
  let () = toplog "Init stresstester %s: create client" name in
  let endpoint = Client.Node node in
  let* client = Client.Agent.create ~endpoint agent in
  let () = toplog "Init stresstester %s: import key" name in
  let* () = Client.forget_all_keys client in
  let* () =
    Lwt_list.iter_s
      (fun account ->
        Client.import_secret_key
          client
          ~endpoint
          account.Account.secret_key
          ~alias:account.Account.alias)
      accounts
  in
  let () = toplog "Init stresstester %s: reveal account" name in
  let* () =
    Lwt_list.iter_s
      (fun account ->
        let*? process =
          Client.reveal client ~endpoint ~src:account.Account.alias
        in
        let* _ = Process.wait process in
        return ())
      accounts
  in
  let () = toplog "Launch stresstesting %s" name in
  let _process =
    Client.spawn_stresstest
      ?seed
      ~source_accounts:accounts
      ~tps:(List.length accounts / Network.block_time network)
      client
  in
  return {node; client; accounts}

let init_stresstesters ?seed ~network ~external_rpc ~simulate_network ~snapshot
    ~data_dir ~node_p2p_endpoint ~ppx_profiling_verbosity
    ~ppx_profiling_backends ~accounts cloud next_agent =
  let* stresstest_agents =
    Lwt_list.mapi_s
      (fun i _account ->
        let name = name_of (Stresstest i) in
        next_agent ~name)
      accounts
  in
  Lwt_list.mapi_p
    (fun i (agent, accounts) ->
      init_stresstester
        ?seed
        ~data_dir
        ~network
        ~external_rpc
        ~simulate_network
        ~snapshot
        ~node_p2p_endpoint
        ~ppx_profiling_verbosity
        ~ppx_profiling_backends
        accounts
        cloud
        i
        agent)
    (List.combine stresstest_agents accounts)

let create_stresstest_accounts ?stresstest_config network client =
  match stresstest_config with
  | None -> return []
  | Some {tps; _} ->
      let nb_agents = nb_stresstester network tps in
      (* This integer division might induce that the number of effective TPS is slightly lower than the targeted one. *)
      let nb_per_machine = tps * Network.block_time network / nb_agents in
      Lwt_list.map_s
        (fun i ->
          Client.stresstest_gen_keys
            ~alias_prefix:("stresstest-" ^ string_of_int i)
            nb_per_machine
            client)
        (List.init nb_agents (fun i -> i))
