(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                        *)
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

(** [host_and_port] holds server configuration details of another dac node
    required to instantiate [Dac_node_client.(#cctxt)]. *)
type host_and_port = {host : string; port : int}

(** Coordinator specific configuration. *)
module Coordinator : sig
  (** The type of a coordinator specific configuration mode. *)
  type t = {
    committee_members : Tezos_crypto.Aggregate_signature.public_key list;
  }

  (** [committee_members_addresses t] retrieves the addresses of the committee
     members from the coordinator configuration [t].*)
  val committee_members_addresses :
    t -> Tezos_crypto.Aggregate_signature.public_key_hash list
end

(** Committee_member specific configuration. *)
module Committee_member : sig
  (** The type of a Committee_member specific configuration mode. *)
  type t = {
    coordinator_rpc_address : Uri.t;
    address : Tezos_crypto.Aggregate_signature.public_key_hash;
  }
end

(** Observer specific configuration. *)
module Observer : sig
  (** The type of an Observer specific configuration mode. *)
  type t = {
    coordinator_rpc_address : string;
    coordinator_rpc_port : int;
    committee_rpc_addresses : (string * int) list;
    timeout : int;
  }

  (** Default timeout for fetching a missing page from Committee members. *)
  val default_timeout : int
end

(** Legacy specific configuration. *)
module Legacy : sig
  (** The type of a legacy-specific configuration mode. *)
  type t = {
    threshold : int;
    committee_members_addresses :
      Tezos_crypto.Aggregate_signature.public_key_hash list;
    dac_cctxt_config : host_and_port option;
    committee_member_address_opt :
      Tezos_crypto.Aggregate_signature.public_key_hash option;
  }

  (** [committee_members_addresses t] retrieves the addresses of the committee
     members from the legacy configuration [t].*)
  val committee_members_addresses :
    t -> Tezos_crypto.Aggregate_signature.public_key_hash list

  (** [threshold t] retrieves the Data Availability Committee threshold from
     the legacy configuration [t]. *)
  val threshold : t -> int

  (** [host_and_port t] retrieves the host and port of the node that serves
     as a coordinator for the DAC, if any is specified in the legacy node
     condiguration. *)
  val dac_cctxt_config : t -> host_and_port option

  val committee_member_address_opt :
    t -> Tezos_crypto.Aggregate_signature.public_key_hash option
end

(** Mode specific fragment of a configuration. *)
type mode = private
  | Coordinator of Coordinator.t
  | Committee_member of Committee_member.t
  | Observer of Observer.t
  | Legacy of Legacy.t

type t = private {
  data_dir : string;  (** The path to the DAC node data directory. *)
  rpc_address : string;  (** The address the DAC node listens to. *)
  rpc_port : int;  (** The port the DAC node listens to. *)
  reveal_data_dir : string;
      (** The directory where the DAC node saves pages. *)
  mode : mode;
      (** Configuration parameters specific to the operating mode of the
          DAC. *)
  allow_v1_api : bool;  (** Feature flag for registering [V1] API endpoints.*)
}

(** [mode_name t] returns a string representation of the operating mode
    for the configuration [t]. *)
val mode_name : t -> string

(** [make_coordinator committee_members] creates a new coordinator
    configuration mode using the given [committee_members] public keys.
*)
val make_coordinator : Tezos_crypto.Aggregate_signature.public_key list -> mode

(** [make_committee_member ~coordinator_rpc_address committee_member_address] 
    creates a new committee member configuration with [committee_member_address]
    as the signer and [coordinator_rpc_address] as the coordinator. *)
val make_committee_member :
  coordinator_rpc_address:Uri.t ->
  Tezos_crypto.Aggregate_signature.public_key_hash ->
  mode

(** [make_observer committee_endpoints coordinator_rpc_address coordinator_rpc_port]
    creates a new observer configuration that sets the Data Availabiity Committee 
    endpoints to [committee_endpoints] and Coordinator endpoint to
    [(coordinator_rpc_address * coordinator_rpc_port)] as the coordinator. *)
val make_observer :
  committee_rpc_addresses:(string * int) list ->
  ?timeout:int ->
  string ->
  int ->
  mode

(** [make_legacy ?coordinator_host_and_port threshold
    committee_members_addresses]
    creates a new legacy configuration mode with the specified input
    parameters. If [host_and_port] is specified, then it will use as the
    address of the coordinator node for the Data Availability Committee. *)
val make_legacy :
  ?coordinator_host_and_port:host_and_port ->
  int ->
  Tezos_crypto.Aggregate_signature.public_key_hash trace ->
  Tezos_crypto.Aggregate_signature.public_key_hash option ->
  mode

(** [make ~data_dir ~reveal_data_dir ~allow_v1_api rpc_address rpc_port mode]
    creates a configuration value from the specified parameters. *)
val make :
  data_dir:string ->
  reveal_data_dir:string ->
  allow_v1_api:bool ->
  string ->
  int ->
  mode ->
  t

(** [filename config] gets the path to config file *)
val filename : t -> string

(** [data_dir_path config subpath] builds a subpath relatively to the
    [config] *)
val data_dir_path : t -> string -> string

(* [reveal_data_dir config] returns the reveal data directory of
   [config]. *)
val reveal_data_dir : t -> string

(** [default_data_dir] is the data directory that the DAC node
    will use, when one is not specified in the configuration:
    currently set to [${HOME}/.tezos-dac-node]. *)
val default_data_dir : string

(** [default_reveal_data_dir] is the directory that the DAC node
    will use to store pages on disk. Currently set to
    [${HOME}/.tezos_rollup_node/wasm_2_0_0]. *)
val default_reveal_data_dir : string

(** [default_rpc_address] is the default address of the RPC server
    of the DAC node: currently set to "127.0.0.1" *)
val default_rpc_address : string

(** [default_rpc_port] is the default port of the RPC server
    of the DAC node: currently set to 10832. *)
val default_rpc_port : int

(** [save config] writes config file in [config.data_dir] *)
val save : t -> unit tzresult Lwt.t

(** [load ~data_dir] config tries to load the configuration of the DAC node
    from [data_dir]. *)
val load : data_dir:string -> (t, Error_monad.tztrace) result Lwt.t

(** [mode config] returns the mode specific configuration of [config]. *)
val mode : t -> mode
