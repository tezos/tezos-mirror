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

(** High-level operations over smart contract rollups. *)
open Alpha_context

type error +=
  | (* Permanent *) Sc_rollup_invalid_parameters_type
  | (* Permanent *) Sc_rollup_invalid_last_cemented_commitment
  | (* Permanent *) Sc_rollup_invalid_output_proof
  | (* Permanent *) Sc_rollup_invalid_outbox_level

(** Result of calling the {!execute_outbox_message} function. *)
type execute_outbox_message_result = {
  paid_storage_size_diff : Z.t;
  ticket_receipt : Ticket_receipt.t;
  operations : Script_typed_ir.packed_internal_operation list;
  whitelist_update : Sc_rollup.Whitelist.update option;
}

type origination_result = {
  address : Sc_rollup.Address.t;
  size : Z.t;
  genesis_commitment_hash : Sc_rollup.Commitment.Hash.t;
}

(** [originate ?whitelist context ~kind ~boot_sector ~parameters_ty] adds a new rollup
    running in a given [kind] initialized with a [boot_sector] and to accept
    smart contract calls of type [parameters_ty]. *)
val originate :
  ?whitelist:Sc_rollup.Whitelist.t ->
  context ->
  kind:Sc_rollup.Kind.t ->
  boot_sector:string ->
  parameters_ty:Script_repr.lazy_expr ->
  (origination_result * context) tzresult Lwt.t

(** [execute_outbox_message ctxt rollup ~cemented_commitment
      ~output_proof] validates the given outbox message and prepares a
      set of resulting operations. *)
val execute_outbox_message :
  context ->
  Sc_rollup.t ->
  cemented_commitment:Sc_rollup.Commitment.Hash.t ->
  output_proof:string ->
  (execute_outbox_message_result * context) tzresult Lwt.t

(** [validate_untyped_parameters_ty ctxt script] parses the type and check
    that the entrypoints are well-formed. *)
val validate_untyped_parameters_ty : context -> Script.expr -> context tzresult

(** A module used for testing purposes only. *)
module Internal_for_tests : sig
  (** Same as {!execute_outbox_message} but allows overriding the extraction
      and validation of output proofs. *)
  val execute_outbox_message :
    context ->
    validate_and_decode_output_proof:
      (context ->
      cemented_commitment:Sc_rollup.Commitment.Hash.t ->
      Sc_rollup.t ->
      output_proof:string ->
      (Sc_rollup.output * context) tzresult Lwt.t) ->
    Sc_rollup.t ->
    cemented_commitment:Sc_rollup.Commitment.Hash.t ->
    output_proof:string ->
    (execute_outbox_message_result * context) tzresult Lwt.t
end
