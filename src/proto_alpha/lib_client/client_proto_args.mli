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

val entrypoint_parameter : (Entrypoint.t, full) Clic.parameter

val init_arg : (string, full) Clic.arg

val fee_arg : (Tez.t option, full) Clic.arg

val default_fee_arg : (Tez.t option, full) Clic.arg

val counter_arg : (Z.t option, full) Clic.arg

val gas_limit_arg : (Gas.Arith.integral option, full) Clic.arg

val default_gas_limit_arg : (Gas.Arith.integral option, full) Clic.arg

val run_gas_limit_arg : (Gas.Arith.integral option, full) Clic.arg

val unlimited_gas_arg : (bool, full) Clic.arg

val storage_limit_arg : (Z.t option, full) Clic.arg

val default_storage_limit_arg : (Z.t option, full) Clic.arg

val arg_arg : (string option, full) Clic.arg

val default_arg_arg : (string option, full) Clic.arg

val source_arg : (string option, full) Clic.arg

val entrypoint_arg : (Entrypoint.t option, full) Clic.arg

val default_entrypoint_arg : (Entrypoint.t option, full) Clic.arg

val delegate_arg : (Signature.Public_key_hash.t option, full) Clic.arg

val max_priority_arg : (int option, full) Clic.arg

val minimal_fees_arg : (Tez.tez, full) Clic.arg

val minimal_nanotez_per_gas_unit_arg : (Q.t, full) Clic.arg

val minimal_nanotez_per_byte_arg : (Q.t, full) Clic.arg

val force_low_fee_arg : (bool, full) Clic.arg

val fee_cap_arg : (Tez.t, full) Clic.arg

val burn_cap_arg : (Tez.t, full) Clic.arg

val replace_by_fees_arg : (bool, full) Clic.arg

val successor_level_arg : (bool, full) Clic.arg

val force_switch : (bool, full) Clic.arg

val no_endorse_switch : (bool, full) Clic.arg

val minimal_timestamp_switch : (bool, full) Clic.arg

val preserved_levels_arg : (int, full) Clic.arg

val no_print_source_flag : (bool, full) Clic.arg

val no_confirmation : (bool, full) Clic.arg

val tez_arg :
  default:string -> parameter:string -> doc:string -> (Tez.t, full) Clic.arg

val tez_opt_arg :
  parameter:string -> doc:string -> (Tez.t option, full) Clic.arg

val tez_param :
  name:string ->
  desc:string ->
  ('a, full) Clic.params ->
  (Tez.t -> 'a, full) Clic.params

val non_negative_z_parameter : (Z.t, full) Clic.parameter

val non_negative_z_param :
  name:string ->
  desc:string ->
  ('a, full) Clic.params ->
  (Z.t -> 'a, full) Clic.params

val global_constant_param :
  name:string ->
  desc:string ->
  ('a, full) Clic.params ->
  (string -> 'a, full) Clic.params

val signature_parameter : (Signature.t, full) Clic.parameter

module Daemon : sig
  val baking_switch : (bool, full) Clic.arg

  val endorsement_switch : (bool, full) Clic.arg

  val denunciation_switch : (bool, full) Clic.arg
end

val int_parameter : (int, full) Clic.parameter

val uri_parameter : (Uri.t, full) Clic.parameter

val string_parameter : (string, full) Clic.parameter

val bytes_of_prefixed_string : string -> Bytes.t tzresult Lwt.t

val bytes_parameter : (Bytes.t, full) Clic.parameter

val file_or_text :
  from_text:(string -> 'a tzresult Lwt.t) ->
  read_file:(string -> string tzresult Lwt.t) ->
  string ->
  'a tzresult Lwt.t

val file_or_text_parameter :
  from_text:(string -> 'a tzresult Lwt.t) -> unit -> ('a, full) Clic.parameter

val json_parameter : (Data_encoding.Json.t, full) Clic.parameter

val data_parameter : (Michelson_v1_parser.parsed, full) Clic.parameter

val unparsing_mode_arg :
  default:string -> (Script_ir_translator.unparsing_mode, full) Clic.arg

val enforce_indentation_flag : (bool, full) Clic.arg

val display_names_flag : (bool, full) Clic.arg

val level_arg : (Script_int.n Script_int.num option, full) Clic.arg

val now_arg : (Script_timestamp.t option, full) Clic.arg

module Tx_rollup : sig
  val tx_rollup_address_param :
    ?name:string ->
    usage:string ->
    ('a, full) Clic.params ->
    (Alpha_context.Tx_rollup.t -> 'a, full) Clic.params

  val level_param :
    ?name:string ->
    usage:string ->
    ('a, full) Clic.params ->
    (Tx_rollup_level.t -> 'a, full) Clic.params

  val context_hash_param :
    ?name:string ->
    usage:string ->
    ('a, full) Clic.params ->
    (Context_hash.t -> 'a, full) Clic.params

  val message_result_path_param :
    ?name:string ->
    usage:string ->
    ('a, full) Clic.params ->
    (Tx_rollup_commitment.Merkle.path -> 'a, full) Clic.params

  val tickets_dispatch_info_param :
    ?name:string ->
    usage:string ->
    ('a, full) Clic.params ->
    (Tx_rollup_reveal.t -> 'a, full) Clic.params

  val message_result_hash_param :
    ?name:string ->
    usage:string ->
    ('a, full) Clic.params ->
    (Tx_rollup_message_result_hash.t -> 'a, full) Clic.params

  val withdraw_list_hash_param :
    ?name:string ->
    usage:string ->
    ('a, full) Clic.params ->
    (Tx_rollup_withdraw_list_hash.t -> 'a, full) Clic.params

  val commitment_hash_param :
    ?name:string ->
    usage:string ->
    ('a, full) Clic.params ->
    (Tx_rollup_commitment_hash.t -> 'a, full) Clic.params

  val commitment_hash_arg :
    ?long:string ->
    ?placeholder:string ->
    usage:string ->
    unit ->
    (Tx_rollup_commitment_hash.t option, full) Clic.arg

  val message_param :
    ?name:string ->
    usage:string ->
    ('a, full) Clic.params ->
    (Tx_rollup_message.t -> 'a, full) Clic.params

  val message_path_param :
    ?name:string ->
    usage:string ->
    ('a, full) Clic.params ->
    (Tx_rollup_inbox.Merkle.path -> 'a, full) Clic.params

  val proof_param :
    ?name:string ->
    usage:string ->
    ('a, full) Clic.params ->
    (Tx_rollup_l2_proof.t -> 'a, full) Clic.params

  val inbox_root_hash_param :
    ?name:string ->
    usage:string ->
    ('a, full) Clic.params ->
    (Tx_rollup_inbox.Merkle.root -> 'a, full) Clic.params
end
