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

val safety_guard_arg : (Gas.Arith.integral option, full) Tezos_clic.arg

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

val delegate_arg : (Signature.Public_key_hash.t option, full) Tezos_clic.arg

val max_priority_arg : (int option, full) Tezos_clic.arg

val minimal_fees_arg : (Tez.t, full) Tezos_clic.arg

val minimal_nanotez_per_gas_unit_arg : (Q.t, full) Tezos_clic.arg

val minimal_nanotez_per_byte_arg : (Q.t, full) Tezos_clic.arg

val replace_by_fees_arg : (bool, full) Tezos_clic.arg

val successor_level_arg : (bool, full) Tezos_clic.arg

val force_switch : (bool, full) Tezos_clic.arg

val minimal_timestamp_switch : (bool, full) Tezos_clic.arg

val preserved_levels_arg : (int, full) Tezos_clic.arg

val no_print_source_flag : (bool, full) Tezos_clic.arg

val no_confirmation : (bool, full) Tezos_clic.arg

val timelock_locked_value_arg : (string option, full) Tezos_clic.arg

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

val everything_or_tez_param :
  name:string ->
  desc:string ->
  ('a, full) Tezos_clic.params ->
  (Tez.t -> 'a, full) Tezos_clic.params

val non_negative_z_parameter :
  unit -> (Z.t, #Client_context.io) Tezos_clic.parameter

val non_negative_z_param :
  name:string ->
  desc:string ->
  ('a, (#Client_context.io as 'b)) Tezos_clic.params ->
  (Z.t -> 'a, 'b) Tezos_clic.params

val non_negative_parameter :
  unit -> (int, #Client_context.io) Tezos_clic.parameter

val non_negative_param :
  name:string ->
  desc:string ->
  ('a, (#Client_context.io as 'b)) Tezos_clic.params ->
  (int -> 'a, 'b) Tezos_clic.params

val positive_int_parameter :
  unit -> (int, #Client_context.io) Tezos_clic.parameter

val positive_int_param :
  name:string ->
  desc:string ->
  ('a, (#Client_context.io as 'b)) Tezos_clic.params ->
  (int -> 'a, 'b) Tezos_clic.params

val global_constant_param :
  name:string ->
  desc:string ->
  ('a, full) Tezos_clic.params ->
  (string -> 'a, full) Tezos_clic.params

val signature_parameter : (Signature.t, full) Tezos_clic.parameter

val int_parameter : (int, full) Tezos_clic.parameter

val z_parameter : (Z.t, full) Tezos_clic.parameter

val uri_parameter : (Uri.t, full) Tezos_clic.parameter

val string_parameter : (string, full) Tezos_clic.parameter

val bytes_of_prefixed_string : full -> string -> Bytes.t tzresult Lwt.t

val bytes_parameter : (Bytes.t, full) Tezos_clic.parameter

type 'a file_or_text = File of {path : string; content : 'a} | Text of 'a

val content_of_file_or_text : 'a file_or_text -> 'a

val file_or_text :
  from_text:(string -> 'a tzresult Lwt.t) ->
  read_file:(string -> string tzresult Lwt.t) ->
  string ->
  'a file_or_text tzresult Lwt.t

val file_or_text_with_origin_parameter :
  from_text:(full -> string -> 'a tzresult Lwt.t) ->
  unit ->
  ('a file_or_text, full) Tezos_clic.parameter

val file_or_text_parameter :
  from_text:(full -> string -> 'a tzresult Lwt.t) ->
  unit ->
  ('a, full) Tezos_clic.parameter

val json_with_origin_parameter :
  (Data_encoding.Json.t file_or_text, full) Tezos_clic.parameter

val safe_decode_json :
  full ->
  name:string ->
  ?pp_error:(Data_encoding.json -> Format.formatter -> exn -> unit) ->
  'a Data_encoding.t ->
  Data_encoding.json ->
  'a tzresult Lwt.t

val json_encoded_with_origin_parameter :
  name:string ->
  ?pp_error:(Data_encoding.json -> Format.formatter -> exn -> unit) ->
  'a Data_encoding.t ->
  ('a file_or_text, full) Tezos_clic.parameter

val json_encoded_parameter :
  name:string ->
  ?pp_error:(Data_encoding.json -> Format.formatter -> exn -> unit) ->
  'a Data_encoding.t ->
  ('a, full) Tezos_clic.parameter

val json_encoded_param :
  name:string ->
  desc:string ->
  ?pp_error:(Data_encoding.json -> Format.formatter -> exn -> unit) ->
  'b Data_encoding.t ->
  ('a, full) Tezos_clic.params ->
  ('b -> 'a, full) Tezos_clic.params

val json_parameter : (Data_encoding.Json.t, full) Tezos_clic.parameter

val data_parameter : (Michelson_v1_parser.parsed, full) Tezos_clic.parameter

val raw_level_parameter :
  unit -> (Raw_level.t, #Client_context.io) Tezos_clic.parameter

val raw_level_param :
  name:string ->
  desc:string ->
  ('a, (#Client_context.io as 'b)) Tezos_clic.params ->
  (Raw_level.t -> 'a, 'b) Tezos_clic.params

val micheline_parameter :
  (string Michelson_v1_parser.parser_result, full) Tezos_clic.parameter

val unparsing_mode_arg :
  default:string -> (Script_ir_unparser.unparsing_mode, full) Tezos_clic.arg

val enforce_indentation_flag : (bool, full) Tezos_clic.arg

val display_names_flag : (bool, full) Tezos_clic.arg

val level_arg : (Script_int.n Script_int.num option, full) Tezos_clic.arg

val now_arg : (Script_timestamp.t option, full) Tezos_clic.arg

val limit_of_staking_over_baking_millionth_arg :
  (int option, full) Tezos_clic.arg

val edge_of_baking_over_staking_billionth_arg :
  (int option, full) Tezos_clic.arg

val other_contracts_arg :
  (RPC.Scripts.S.other_contract_description list option, full) Tezos_clic.arg

val extra_big_maps_arg :
  (RPC.Scripts.S.extra_big_map_description list option, full) Tezos_clic.arg

module Sc_rollup_params : sig
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

  val whitelist : (Sc_rollup.Whitelist.t, full) Tezos_clic.parameter
end

val whitelist_arg : (Sc_rollup.Whitelist.t option, full) Tezos_clic.arg

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

module Dal : sig
  val commitment_parameter :
    (Dal_slot_repr.Commitment.t, full) Tezos_clic.parameter

  val commitment_proof_parameter :
    (Dal_slot_repr.Commitment_proof.t, full) Tezos_clic.parameter
end

val fee_parameter_args : (Injection.fee_parameter, full) Tezos_clic.arg
