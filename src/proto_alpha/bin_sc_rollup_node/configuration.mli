(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** Mode for the rollup node *)
type mode =
  | Observer  (** Only follows the chain and reconstructs inboxes *)
  | Batcher  (** Accept transactions in its queue and batches them on the L1 *)
  | Maintenance  (** Follows the chain and publishes commitments *)
  | Operator  (** Equivalent to maintenance + batcher  *)
  | Custom
      (** This mode allows to tweak which operations are injected by selecting
          the signers *)

(** Purposes for operators, indicating the kind of operations that they sign. *)
type purpose = Publish | Add_messages | Cement | Timeout | Refute

module Operator_purpose_map : Map.S with type key = purpose

type operators = Signature.Public_key_hash.t Operator_purpose_map.t

type fee_parameters = Injection.fee_parameter Operator_purpose_map.t

type t = {
  data_dir : string;
  sc_rollup_address : Protocol.Alpha_context.Sc_rollup.t;
  sc_rollup_node_operators : operators;
  rpc_addr : string;
  rpc_port : int;
  reconnection_delay : float;
  fee_parameters : fee_parameters;
  mode : mode;
  loser_mode : Loser_mode.t;
  (*DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3718
    Decide whether we want to handle connections to multiple
    Dal nodes for different slot indexes.
  *)
  dal_node_addr : string;
  dal_node_port : int;
}

(** [make_purpose_map ~default purposes] constructs a purpose map from a list of
    bindings [purposes], with a potential [default] value. *)
val make_purpose_map :
  default:'a option -> (purpose * 'a) trace -> 'a Operator_purpose_map.t

(** [purpose_of_string s] parses a purpose from the given string [s]. *)
val purpose_of_string : string -> purpose option

(** [string_of_purpose p] returns a string representation of purpose [p]. *)
val string_of_purpose : purpose -> string

(** List of possible purposes for operator specialization. *)
val purposes : purpose list

(** [default_data_dir] is the default value for [data_dir]. *)
val default_data_dir : string

(** [default_storage_dir] returns the default value of the storage dir
    given a [data_dir]. *)
val default_storage_dir : string -> string

(** [default_context_dir] returns the default value of the directory
    for persisting the context given a [data_dir]. *)
val default_context_dir : string -> string

(** [default_rpc_addr] is the default value for [rpc_addr]. *)
val default_rpc_addr : string

(** [default_rpc_port] is the default value for [rpc_port]. *)
val default_rpc_port : int

(** [default_reconnection_delay] is the default value for [reconnection_delay]. *)
val default_reconnection_delay : float

(** [default_fee_parameter ?purpose ()] is the default fee parameter to inject
    operation on L1. If [purpose] is provided, it returns the default fee
    parameter for the specific purpose. *)
val default_fee_parameter : ?purpose:purpose -> unit -> Injection.fee_parameter

(** [default_fee_parameters] is the default fee parameters configuration build
    with {!default_fee_parameter} for all purposes. *)
val default_fee_parameters : fee_parameters

(** [default_dal_node_addr] is the default value for [dal_node_addr]. *)
val default_dal_node_addr : string

(** [default_dal_node_port] is the default value for [dal_node_port]. *)
val default_dal_node_port : int

(** This is the list of available modes. *)
val modes : mode list

(** [string_of_mode mode] returns a string representation of the mode [mode]. *)
val string_of_mode : mode -> string

(** [mode_of_string s] returns the mode represented by string [s] if it exists. *)
val mode_of_string : string -> mode tzresult

(** [description_of_mode m] returns a textual description of the mode [m]. *)
val description_of_mode : mode -> string

(** [filename configuration] returns the [configuration] filename. *)
val filename : t -> string

(** [check_mode config] ensures the operators correspond to the chosen mode and
    removes the extra ones. *)
val check_mode : t -> t tzresult

(** [save configuration] overwrites [configuration] file. *)
val save : t -> unit tzresult Lwt.t

(** [load ~data_dir] loads a configuration stored in [data_dir]. *)
val load : data_dir:string -> t tzresult Lwt.t
