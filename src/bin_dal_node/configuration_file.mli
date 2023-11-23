(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type neighbor = {addr : string; port : int}

type t = {
  data_dir : string;  (** The path to the DAL node data directory. *)
  rpc_addr : P2p_point.Id.t;
      (** The TCP address the DAL node's RPC server listens to. *)
  neighbors : neighbor list;
      (** List of neighbors to monitor and download shards from via RPCs. *)
  listen_addr : P2p_point.Id.t;  (** The TCP address bound by the DAL node. *)
  public_addr : P2p_point.Id.t;
      (** The TCP address at which this instance can be reached. *)
  peers : string list;
      (** The list of P2P peers to connect to at startup, in addition to the
          list given by L1 node's configuration parameter
          dal_config.bootstrap_peers. *)
  expected_pow : float;
      (** The expected PoW difficulty level for the peers' identity. *)
  network_name : string;
      (** A string that identifies the network's name. E.g. dal-sandbox. *)
  endpoint : Uri.t;  (** The endpoint of a Tezos L1 node. *)
  metrics_addr : P2p_point.Id.t;
      (** The TCP address of the node's server used to export metrics. *)
  profiles : Types.profiles;
      (** The profiles determining the topics of interest. *)
}

(** [default] is the default configuration. *)
val default : t

(** [store_path config] returns a path for the store *)
val store_path : t -> string

(** [save config] writes config file in [config.data_dir] *)
val save : t -> unit tzresult Lwt.t

val load : data_dir:string -> (t, Error_monad.tztrace) result Lwt.t

(** [identity_file t] returns the absolute path to the "identity.json"
    file of the DAL node, based on the configuration [t]. *)
val identity_file : t -> string

(** [peers_file data_dir] returns the absolute path to the
    "peers.json" file of the DAL node, based on the configuration
    [t]. *)
val peers_file : t -> string
