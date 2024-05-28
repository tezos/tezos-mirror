(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** This module contains constants and utility functions for gas metering
    functions used when handling SC rollups operations in context. *)

module Constants : sig
  val cost_serialize_state_hash : Gas_limit_repr.cost

  val cost_serialize_commitment_hash : Gas_limit_repr.cost

  val cost_serialize_commitment : Gas_limit_repr.cost

  val cost_serialize_nonce : Gas_limit_repr.cost
end

(** [is_valid_parameters_ty_cost ty] returns the cost of checking whether a type
    is a valid sc rollup parameter. *)
val is_valid_parameters_ty_cost :
  ty_size:'a Saturation_repr.t -> Saturation_repr.may_saturate Saturation_repr.t

(** [cost_serialize_internal_inbox_message internal_inbox_message] is the cost
    of the serialization of an internal inbox message. It's equal to the cost of
    serializing the script expression, with {!Script_repr.force_bytes_cost} plus
    a fixed amount for the serialized addresses.

    It traverses the payload expression to find the precise cost. It is safe to
    use {!Script_repr.force_bytes_cost} because the payload of an internal inbox
    message is bounded.
*)
val cost_serialize_internal_inbox_message :
  Sc_rollup_inbox_message_repr.internal_inbox_message -> Gas_limit_repr.cost

(** [cost_deserialize_output_proof ~bytes_len] is the cost of the
    deserialization of an output proof. *)
val cost_deserialize_output_proof : bytes_len:int -> Gas_limit_repr.cost

(** [cost_serialize_external_inbox_message ~bytes_len] is the cost of the
    serialization of an external inbox message of length [bytes_len]. It is
    equal to the estimated cost of encoding a byte multiplied by [bytes_len]. *)
val cost_serialize_external_inbox_message : bytes_len:int -> Gas_limit_repr.cost

(** [cost_hash_bytes ~bytes_len] is the cost of hashing [bytes_len] bytes. *)
val cost_hash_bytes : bytes_len:int -> Gas_limit_repr.cost

(** [cost_check_dissection ~number_of_states ~tick_size ~hash_size] is the cost
    of checking that a dissection with a given [number_of_states] used in a
    refutation game is well-formed. This includes the comparison of a linear
    number of ticks as well as the verification of two hashes of given
    [hash_size]. *)
val cost_check_dissection :
  number_of_states:int -> tick_size:int -> hash_size:int -> Gas_limit_repr.cost

(** [cost_verify_output_proof ~bytes_len] is the cost of verifying an output
     proof of length [bytes_len]. *)
val cost_verify_output_proof : bytes_len:int -> Gas_limit_repr.cost

(** [cost_add_message ~new_cell_index ~msg_len] returns the cost of adding a
    message of length [msg_len] to a sc-rollup inbox. This function is used
    internally in the [Sc_rollup_storage] module and covers the function
    {!Sc_rollup_inbox_merkelized_payload_hashes_repr.add_payload} *)
val cost_add_message : current_index:Z.t -> msg_len:int -> Gas_limit_repr.cost

(** [cost_install_boot_sector_in_wasm_pvm ~boot_sector_size_in_bytes]
    returns the cost of installing a boot sector in an empty WASM PVM
    state. This function is used in the implementation of the
    origination. *)
val cost_install_boot_sector_in_wasm_pvm :
  boot_sector_size_in_bytes:int -> Gas_limit_repr.cost
