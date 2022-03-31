(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type net_config

(* NOTE: Some fields are not documented.
 *       Please help complete it if you can. *)

(** This record represents a collection of command line arguments given when
    starting the node. *)
type t = {
  disable_config_validation : bool;
  data_dir : string option;
      (** a directory where the node's configuration and state should be
          stored *)
  config_file : string;  (** the config.json file to use *)
  network : net_config option;  (** the P2P network to connect to *)
  connections : int option;
  max_download_speed : int option;
  max_upload_speed : int option;
  binary_chunks_size : int option;
  peer_table_size : int option;
  expected_pow : float option;
  peers : string list;
  no_bootstrap_peers : bool;
  listen_addr : string option;
      (** address to listen to connections from peers *)
  advertised_net_port : int option;
      (** port advertised for other peers to connect to *)
  discovery_addr : string option;
  rpc_listen_addrs : string list;
      (** a list of addresses to listen to RPC requests on *)
  private_mode : bool;
      (** enables the private mode, see
          https://tezos.gitlab.io/user/node-configuration.html#private-node *)
  disable_mempool : bool;
  disable_mempool_precheck : bool;
      (** If [disable_mempool_precheck] is [true] operations are executed by
          the protocol before behing propagated. This flag is intended to be
          used for testing and debugging. *)
  enable_testchain : bool;
  cors_origins : string list;
  cors_headers : string list;
  rpc_tls : Node_config_file.tls option;
  log_output : Lwt_log_sink_unix.Output.t option;
  bootstrap_threshold : int option;
  history_mode : History_mode.t option;
  synchronisation_threshold : int option;
  latency : int option;
  allow_all_rpc : P2p_point.Id.addr_port_id list;
      (** a list of RPC listening addresses for which a full
          access should be granted *)
  operation_metadata_size_limit : int option option;
      (** maximum operation metadata size allowed to be stored on disk *)
}

module Term : sig
  val args : t Cmdliner.Term.t

  val data_dir : string option Cmdliner.Term.t

  val config_file : string option Cmdliner.Term.t
end

val read_config_file : t -> Node_config_file.t tzresult Lwt.t

val read_data_dir : t -> string tzresult Lwt.t

val read_and_patch_config_file :
  ?may_override_network:bool ->
  ?ignore_bootstrap_peers:bool ->
  t ->
  Node_config_file.t tzresult Lwt.t

module Manpage : sig
  val misc_section : string

  val args : Cmdliner.Manpage.block list

  val bugs : Cmdliner.Manpage.block list
end
