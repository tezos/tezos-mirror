(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Stresstest parameters
    - [tps]: targeted number of transactions per second
    - [seed]: seed used for stresstest traffic generation
 *)
type t = {tps : int; seed : int}

val encoding : t Data_encoding.t

val typ : t Clap.typ

type stresstester = {
  node : Node.t;
  client : Client.t;
  accounts : Account.key list;
}

val nb_stresstester : Network.t -> int -> int

val init_stresstesters :
  ?seed:int ->
  network:Network.t ->
  external_rpc:bool ->
  simulate_network:Network_simulation.t ->
  snapshot:Snapshot_helpers.t ->
  data_dir:string option ->
  node_p2p_endpoint:string ->
  ppx_profiling_verbosity:string option ->
  ppx_profiling_backends:string list ->
  accounts:Account.key list list ->
  Cloud.t ->
  (name:string -> Agent.t Lwt.t) ->
  stresstester list Lwt.t

val create_stresstest_accounts :
  ?stresstest_config:t -> Network.t -> Client.t -> Account.key list list Lwt.t
