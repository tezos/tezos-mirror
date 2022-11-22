(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol
open Alpha_context
open Protocol_client_context

val tez_sym : string

val init_arg : (string, full) Tezos_clic.arg

val fee_arg : (Tez.t option, full) Tezos_clic.arg

val default_fee_arg : (Tez.t option, full) Tezos_clic.arg

val counter_arg : (Z.t option, full) Tezos_clic.arg

val gas_limit_arg : (Gas.Arith.integral option, full) Tezos_clic.arg

val default_gas_limit_arg : (Gas.Arith.integral option, full) Tezos_clic.arg

val run_gas_limit_arg : (Gas.Arith.integral option, full) Tezos_clic.arg

val storage_limit_arg : (Z.t option, full) Tezos_clic.arg

val default_storage_limit_arg : (Z.t option, full) Tezos_clic.arg

val arg_arg : (string option, full) Tezos_clic.arg

val default_arg_arg : (string option, full) Tezos_clic.arg

val source_arg : (string option, full) Tezos_clic.arg

val entrypoint_arg : (string option, full) Tezos_clic.arg

val default_entrypoint_arg : (string option, full) Tezos_clic.arg

val delegate_arg :
  (Tezos_crypto.Signature.V0.Public_key_hash.t option, full) Tezos_clic.arg

val max_priority_arg : (int option, full) Tezos_clic.arg

val minimal_fees_arg : (Tez.tez, full) Tezos_clic.arg

val minimal_nanotez_per_gas_unit_arg : (Q.t, full) Tezos_clic.arg

val minimal_nanotez_per_byte_arg : (Q.t, full) Tezos_clic.arg

val force_low_fee_arg : (bool, full) Tezos_clic.arg

val fee_cap_arg : (Tez.t, full) Tezos_clic.arg

val burn_cap_arg : (Tez.t, full) Tezos_clic.arg

val force_switch : (bool, full) Tezos_clic.arg

val no_endorse_switch : (bool, full) Tezos_clic.arg

val minimal_timestamp_switch : (bool, full) Tezos_clic.arg

val preserved_levels_arg : (int, full) Tezos_clic.arg

val no_print_source_flag : (bool, full) Tezos_clic.arg

val no_confirmation : (bool, full) Tezos_clic.arg

val tez_arg :
  default:string ->
  parameter:string ->
  doc:string ->
  (Tez.t, full) Tezos_clic.arg

val tez_param :
  name:string ->
  desc:string ->
  ('a, full) Tezos_clic.params ->
  (Tez.t -> 'a, full) Tezos_clic.params

val global_constant_param :
  name:string ->
  desc:string ->
  ('a, full) Tezos_clic.params ->
  (string -> 'a, full) Tezos_clic.params

val signature_parameter :
  (Tezos_crypto.Signature.V0.t, full) Tezos_clic.parameter

module Daemon : sig
  val baking_switch : (bool, full) Tezos_clic.arg

  val endorsement_switch : (bool, full) Tezos_clic.arg

  val denunciation_switch : (bool, full) Tezos_clic.arg
end

val int_parameter : (int, full) Tezos_clic.parameter

val uri_parameter : (Uri.t, full) Tezos_clic.parameter

val string_parameter : (string, full) Tezos_clic.parameter

val bytes_of_prefixed_string : string -> Bytes.t tzresult Lwt.t

val bytes_parameter : (Bytes.t, full) Tezos_clic.parameter

val data_parameter : (Michelson_v1_parser.parsed, full) Tezos_clic.parameter

val unparsing_mode_arg :
  default:string -> (Script_ir_translator.unparsing_mode, full) Tezos_clic.arg

val enforce_indentation_flag : (bool, full) Tezos_clic.arg

val display_names_flag : (bool, full) Tezos_clic.arg

val level_arg : (Script_int.n Script_int.num option, full) Tezos_clic.arg

val now_arg : (Script_timestamp.t option, full) Tezos_clic.arg
