(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2458
   Provide a default configuration
*)

(** Mode for the rollup node *)
type mode =
  | Observer  (** Only follows the chain and reconstructs L2 blocks *)
  | Accuser  (** Follows the chain and rejects bad commitments *)
  | Batcher  (** Accept transactions in its queue and batches them on the L1 *)
  | Maintenance
      (** Follows the chain and injects commitments (and rejects bad ones) *)
  | Operator  (** Equivalent to maintenance + batcher  *)
  | Custom
      (** This mode allows to tweak which operations are injected by selecting the
          signers *)

type 'a purposed = {
  operator : 'a;
  submit_batch : 'a;
  finalize_commitment : 'a;
  remove_commitment : 'a;
  rejection : 'a;
  dispatch_withdrawals : 'a;
}

type signers = Signature.public_key_hash option purposed

type cost_caps = {
  fee_cap : Protocol.Alpha_context.Tez.t;
  burn_cap : Protocol.Alpha_context.Tez.t;
}

type caps = cost_caps purposed

type t = {
  data_dir : string;
  rollup_id : Protocol.Alpha_context.Tx_rollup.t;
  rollup_genesis : Block_hash.t option;
  rpc_addr : P2p_point.Id.t;
  reconnection_delay : float;
  mode : mode;
  signers : signers;
  allow_deposit : bool;
  l2_blocks_cache_size : int;
  caps : caps;
  batch_burn_limit : Protocol.Alpha_context.Tez.t option;
}

(** [default_data_dir] is the default value for [data_dir]. *)
val default_data_dir : Protocol.Alpha_context.Tx_rollup.t -> string

(** [default_rpc_addr] is the default value for [rpc_addr]. *)
val default_rpc_addr : P2p_point.Id.t

(** [default_reconnection_delay] is the default value for [reconnection-delay] *)
val default_reconnection_delay : float

(** [default_l2_blocks_cache_size] is the default number of L2 blocks that are
    cached by the rollup node *)
val default_l2_blocks_cache_size : int

(** The default fees/burn caps *)
val default_cost_caps : cost_caps

(** The default fees/burn caps for operations of the injector *)
val default_caps : caps

val modes : mode list

val string_of_mode : mode -> string

val mode_of_string : string -> mode tzresult

(** [check_mode config] ensures the signers correspond to the chosen mode. *)
val check_mode : t -> unit tzresult

(** [save configuration] overwrites [configuration] file and returns the filename. *)
val save : t -> string tzresult Lwt.t

(** [load ~data_dir] loads a configuration stored in [data_dir]. *)
val load : data_dir:string -> t tzresult Lwt.t

(** [encoding] encodes a configuration. *)
val encoding : t Data_encoding.t
