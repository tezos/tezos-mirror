(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

(** Configuration type for coordinators. *)
type coordinator = {
  threshold : int;
      (** The number of signatures required from DAC members to consider a
      message valid. *)
  dac_members_addresses : Tezos_crypto.Aggregate_signature.public_key_hash list;
      (** The list of tz4 addresses denoting the dac members. *)
}

(** Configuration type for dac members. *)
type dac_member = {
  coordinator_rpc_address : string;  (** RPC address of the coordinator. *)
  coordinator_rpc_port : int;  (** RPC port of the coordinator. *)
  address : Tezos_crypto.Aggregate_signature.public_key_hash;
      (** Tz4 address of the DAC member. *)
}

(** Configuation type for observers. *)
type observer = {
  coordinator_rpc_address : string;  (** RPC address of the coordinator. *)
  coordinator_rpc_port : int;  (** RPC port of the coordinator. *)
}

(** Configuration type for legacy mode. *)
type legacy = {
  threshold : int;
      (** The number of signatures required from DAC members to consider a
      message valid. *)
  dac_members_addresses : Tezos_crypto.Aggregate_signature.public_key_hash list;
      (** The list of tz4 addresses denoting the dac members. *)
}

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4707.
   Remove legacy mode once other DAC operating modes are fully functional. *)
type mode =
  | Coordinator of coordinator
  | Dac_member of dac_member
  | Observer of observer
  | Legacy of legacy

type t = {
  data_dir : string;  (** The path to the DAC node data directory. *)
  rpc_address : string;  (** The address the DAC node listens to. *)
  rpc_port : int;  (** The port the DAC node listens to. *)
  reveal_data_dir : string;
      (** The directory where the DAC node saves pages. *)
  mode : mode;
      (** Configuration parameters specific to the operating mode of the
          DAC. *)
}

(** [filename config] gets the path to config file *)
val filename : t -> string

(** [data_dir_path config subpath] builds a subpath relatively to the
    [config] *)
val data_dir_path : t -> string -> string

(** [default_data_dir] is the data directory that the DAC node
    will use, when one is not specified in the configuration:
    currently set to "${HOME}/.tezos-dac-node." *)
val default_data_dir : string

(** [default_reveal_data_dir] is the directory that the DAC node
    will use to store pages on disk. Currently set to
    "${HOME}/.tezos_rollup_node/wasm_2_0_0". *)
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
