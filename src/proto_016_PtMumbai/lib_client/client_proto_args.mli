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

val entrypoint_parameter : (Entrypoint.t, full) Tezos_clic.parameter

val init_arg : (string, full) Tezos_clic.arg

val fee_arg : (Tez.t option, full) Tezos_clic.arg

val default_fee_arg : (Tez.t option, full) Tezos_clic.arg

val counter_arg : (Manager_counter.t option, full) Tezos_clic.arg

val gas_limit_arg : (Gas.Arith.integral option, full) Tezos_clic.arg

val default_gas_limit_arg : (Gas.Arith.integral option, full) Tezos_clic.arg

val run_gas_limit_arg : (Gas.Arith.integral option, full) Tezos_clic.arg

val unlimited_gas_arg : (bool, full) Tezos_clic.arg

val storage_limit_arg : (Z.t option, full) Tezos_clic.arg

val default_storage_limit_arg : (Z.t option, full) Tezos_clic.arg

val arg_arg : (string option, full) Tezos_clic.arg

val default_arg_arg : (string option, full) Tezos_clic.arg

val source_arg : (string option, full) Tezos_clic.arg

val entrypoint_arg : (Entrypoint.t option, full) Tezos_clic.arg

val default_entrypoint_arg : (Entrypoint.t option, full) Tezos_clic.arg

val delegate_arg :
  (Tezos_crypto.Signature.Public_key_hash.t option, full) Tezos_clic.arg

val max_priority_arg : (int option, full) Tezos_clic.arg

val minimal_fees_arg : (Tez.tez, full) Tezos_clic.arg

val minimal_nanotez_per_gas_unit_arg : (Q.t, full) Tezos_clic.arg

val minimal_nanotez_per_byte_arg : (Q.t, full) Tezos_clic.arg

val replace_by_fees_arg : (bool, full) Tezos_clic.arg

val successor_level_arg : (bool, full) Tezos_clic.arg

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

val tez_opt_arg :
  parameter:string -> doc:string -> (Tez.t option, full) Tezos_clic.arg

val tez_param :
  name:string ->
  desc:string ->
  ('a, full) Tezos_clic.params ->
  (Tez.t -> 'a, full) Tezos_clic.params

val non_negative_z_parameter : (Z.t, full) Tezos_clic.parameter

val non_negative_z_param :
  name:string ->
  desc:string ->
  ('a, full) Tezos_clic.params ->
  (Z.t -> 'a, full) Tezos_clic.params

val non_negative_parameter : (int, full) Tezos_clic.parameter

val global_constant_param :
  name:string ->
  desc:string ->
  ('a, full) Tezos_clic.params ->
  (string -> 'a, full) Tezos_clic.params

val signature_parameter : (Tezos_crypto.Signature.t, full) Tezos_clic.parameter

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

val file_or_text :
  from_text:(string -> 'a tzresult Lwt.t) ->
  read_file:(string -> string tzresult Lwt.t) ->
  string ->
  'a tzresult Lwt.t

val file_or_text_parameter :
  from_text:(string -> 'a tzresult Lwt.t) ->
  unit ->
  ('a, full) Tezos_clic.parameter

val json_parameter : (Data_encoding.Json.t, full) Tezos_clic.parameter

val data_parameter : (Michelson_v1_parser.parsed, full) Tezos_clic.parameter

val raw_level_parameter : (Raw_level.t, full) Tezos_clic.parameter

val unparsing_mode_arg :
  default:string -> (Script_ir_unparser.unparsing_mode, full) Tezos_clic.arg

val enforce_indentation_flag : (bool, full) Tezos_clic.arg

val display_names_flag : (bool, full) Tezos_clic.arg

val level_arg : (Script_int.n Script_int.num option, full) Tezos_clic.arg

val now_arg : (Script_timestamp.t option, full) Tezos_clic.arg

module Tx_rollup : sig
  val tx_rollup_address_param :
    ?name:string ->
    usage:string ->
    ('a, full) Tezos_clic.params ->
    (Alpha_context.Tx_rollup.t -> 'a, full) Tezos_clic.params

  val level_param :
    ?name:string ->
    usage:string ->
    ('a, full) Tezos_clic.params ->
    (Tx_rollup_level.t -> 'a, full) Tezos_clic.params

  val context_hash_param :
    ?name:string ->
    usage:string ->
    ('a, full) Tezos_clic.params ->
    (Context_hash.t -> 'a, full) Tezos_clic.params

  val message_result_path_param :
    ?name:string ->
    usage:string ->
    ('a, full) Tezos_clic.params ->
    (Tx_rollup_commitment.Merkle.path -> 'a, full) Tezos_clic.params

  val tickets_dispatch_info_param :
    ?name:string ->
    usage:string ->
    ('a, full) Tezos_clic.params ->
    (Tx_rollup_reveal.t -> 'a, full) Tezos_clic.params

  val message_result_hash_param :
    ?name:string ->
    usage:string ->
    ('a, full) Tezos_clic.params ->
    (Tx_rollup_message_result_hash.t -> 'a, full) Tezos_clic.params

  val withdraw_list_hash_param :
    ?name:string ->
    usage:string ->
    ('a, full) Tezos_clic.params ->
    (Tx_rollup_withdraw_list_hash.t -> 'a, full) Tezos_clic.params

  val commitment_hash_param :
    ?name:string ->
    usage:string ->
    ('a, full) Tezos_clic.params ->
    (Tx_rollup_commitment_hash.t -> 'a, full) Tezos_clic.params

  val commitment_hash_arg :
    ?long:string ->
    ?placeholder:string ->
    usage:string ->
    unit ->
    (Tx_rollup_commitment_hash.t option, full) Tezos_clic.arg

  val message_param :
    ?name:string ->
    usage:string ->
    ('a, full) Tezos_clic.params ->
    (Tx_rollup_message.t -> 'a, full) Tezos_clic.params

  val message_path_param :
    ?name:string ->
    usage:string ->
    ('a, full) Tezos_clic.params ->
    (Tx_rollup_inbox.Merkle.path -> 'a, full) Tezos_clic.params

  val proof_param :
    ?name:string ->
    usage:string ->
    ('a, full) Tezos_clic.params ->
    (Tx_rollup_l2_proof.t -> 'a, full) Tezos_clic.params

  val inbox_root_hash_param :
    ?name:string ->
    usage:string ->
    ('a, full) Tezos_clic.params ->
    (Tx_rollup_inbox.Merkle.root -> 'a, full) Tezos_clic.params
end

module Sc_rollup_params : sig
  val sc_rollup_address_parameter : (Sc_rollup.t, full) Tezos_clic.parameter

  val sc_rollup_address_param :
    ?name:string ->
    ?desc:string ->
    ('a, full) Tezos_clic.params ->
    (Sc_rollup.t -> 'a, full) Tezos_clic.params

  val rollup_kind_parameter : (Sc_rollup.Kind.t, full) Tezos_clic.parameter

  val boot_sector_parameter :
    (Sc_rollup.PVM.t -> string tzresult Lwt.t, full) Tezos_clic.parameter

  val messages_parameter :
    ([`Bin of string | `Json of Data_encoding.json], full) Tezos_clic.parameter

  val commitment_hash_parameter :
    (Sc_rollup.Commitment.Hash.t, full) Tezos_clic.parameter

  val unchecked_payload_parameter : (string, full) Tezos_clic.parameter

  val compressed_state_parameter :
    (Sc_rollup.State_hash.t, full) Tezos_clic.parameter

  val number_of_ticks_parameter :
    (Sc_rollup.Number_of_ticks.t, full) Tezos_clic.parameter
end

module Zk_rollup_params : sig
  val address_parameter : (Zk_rollup.t, full) Tezos_clic.parameter

  val plonk_public_parameters_parameter :
    (Plonk.Main_protocol.verifier_public_parameters, full) Tezos_clic.parameter

  val update_parameter : (Zk_rollup.Update.t, full) Tezos_clic.parameter

  val operations_parameter :
    ( (Zk_rollup.Operation.t * Zk_rollup.Ticket.t option) list,
      full )
    Tezos_clic.parameter

  val state_parameter : (Zk_rollup.State.t, full) Tezos_clic.parameter

  val circuits_info_parameter :
    ( [`Fee | `Private | `Public] Zk_rollup.Account.SMap.t,
      full )
    Tezos_clic.parameter
end

val fee_parameter_args : (Injection.fee_parameter, full) Tezos_clic.arg
