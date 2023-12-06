(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
(** {1 Operations_source abstraction} *)
module Operations_source : sig
  type t =
    | Local of {filename : string}
        (** local mempool resource located in [filename] *)
    | Remote of {uri : Uri.t; http_headers : (string * string) list option}
        (** remote resource located a [uri], with additional [http_headers]
            parameters *)

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end

type fees_config = {
  minimal_fees : Protocol.Alpha_context.Tez.t;
  minimal_nanotez_per_gas_unit : Q.t;
  minimal_nanotez_per_byte : Q.t;
}

type validation_config =
  | Local of {context_path : string}
  | Node
  | ContextIndex of Abstract_context_index.t

type nonce_config = Deterministic | Random

type state_recorder_config = Filesystem | Memory

type liquidity_baking_config = {
  vote_file : string option;
  liquidity_baking_vote :
    Protocol.Alpha_context.Liquidity_baking.liquidity_baking_toggle_vote;
}

type t = {
  fees : fees_config;
  nonce : nonce_config;
  validation : validation_config;
  retries_on_failure : int;
  user_activated_upgrades : (int32 * Protocol_hash.t) list;
  liquidity_baking : liquidity_baking_config;
  force_apply : bool;
  force : bool;
  state_recorder : state_recorder_config;
  extra_operations : Operations_source.t option;
  dal_node_endpoint : Uri.t option;
  pre_emptive_forge_time : Time.System.Span.t;
}

val default_fees_config : fees_config

val default_validation_config : validation_config

val default_nonce_config : nonce_config

val default_retries_on_failure_config : int

val default_user_activated_upgrades : (int32 * Protocol_hash.t) list

val default_liquidity_baking_config : liquidity_baking_config

val default_force_apply : bool

val default_force : bool

val default_state_recorder_config : state_recorder_config

val default_extra_operations : Operations_source.t option

val default_pre_emptive_forge_time : Time.System.Span.t

val default_config : t

val make :
  ?minimal_fees:Protocol.Alpha_context.Tez.t ->
  ?minimal_nanotez_per_gas_unit:Q.t ->
  ?minimal_nanotez_per_byte:Q.t ->
  ?nonce:nonce_config ->
  ?context_path:string ->
  ?retries_on_failure:int ->
  ?user_activated_upgrades:(int32 * Protocol_hash.t) list ->
  ?liquidity_baking:liquidity_baking_config ->
  ?force_apply:bool ->
  ?force:bool ->
  ?state_recorder:state_recorder_config ->
  ?extra_operations:Operations_source.t ->
  ?dal_node_endpoint:Uri.t ->
  ?pre_emptive_forge_time:Time.System.Span.t ->
  unit ->
  t

val fees_config_encoding : fees_config Data_encoding.t

val validation_config_encoding : validation_config Data_encoding.t

val nonce_config_encoding : nonce_config Data_encoding.t

val retries_on_failure_config_encoding : int Data_encoding.t

val user_activate_upgrades_config_encoding :
  (int32 * Protocol_hash.t) list Data_encoding.t

val liquidity_baking_toggle_vote_config_encoding :
  Protocol.Alpha_context.Liquidity_baking.liquidity_baking_toggle_vote
  Data_encoding.t

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit
