(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type error +=
  | (* `Branch *)
      Counter_in_the_future of Contract_repr.contract * Z.t * Z.t
  | (* `Temporary *)
      Non_existing_contract of Contract_repr.contract
  | (* `Permanent *)
      Inconsistent_public_key of
      Signature.Public_key.t * Signature.Public_key.t
  | (* `Permanent *) Failure of string

(** [allocated ctxt contract] returns [true] if and only if the
   contract is stored in [Storage.Contract.Balance]. *)
val allocated : Raw_context.t -> Contract_repr.t -> bool tzresult Lwt.t

(** [exists ctxt contract] returns [true] if and only if either the
   contract is originated or it is (implicit and) "allocated". *)
val exists : Raw_context.t -> Contract_repr.t -> bool tzresult Lwt.t

(** [must_exist ctxt contract] fails with the [Non_existing_contract] error if
    [exists ctxt contract] returns [false]. Even though this function is
    gas-free, it is always called in a context where some gas consumption is
    guaranteed whenever necessary. The first context is that of a transfer
    operation, and in that case the base cost of a manager operation
    ([Micheclson_v1_gas.Cost_of.manager_operation]) is consumed. The second
    context is that of an activation operation, and in that case no gas needs to
    be consumed since that operation is not a manager operation. *)
val must_exist : Raw_context.t -> Contract_repr.t -> unit tzresult Lwt.t

(** [must_be_allocated ctxt contract] fails when the contract is not
   allocated. It fails with [Non_existing_contract] if the contract is
   originated, and it fails with [Empty_implicit_contract] if the
   contract is implicit. *)
val must_be_allocated : Raw_context.t -> Contract_repr.t -> unit tzresult Lwt.t

val list : Raw_context.t -> Contract_repr.t list Lwt.t

val check_counter_increment :
  Raw_context.t -> Signature.Public_key_hash.t -> Z.t -> unit tzresult Lwt.t

val increment_counter :
  Raw_context.t -> Signature.Public_key_hash.t -> Raw_context.t tzresult Lwt.t

val get_balance : Raw_context.t -> Contract_repr.t -> Tez_repr.t tzresult Lwt.t

val get_balance_carbonated :
  Raw_context.t ->
  Contract_repr.t ->
  (Raw_context.t * Tez_repr.t) tzresult Lwt.t

val get_counter :
  Raw_context.t -> Signature.Public_key_hash.t -> Z.t tzresult Lwt.t

val get_script_code :
  Raw_context.t ->
  Contract_repr.t ->
  (Raw_context.t * Script_repr.lazy_expr option) tzresult Lwt.t

val get_script :
  Raw_context.t ->
  Contract_repr.t ->
  (Raw_context.t * Script_repr.t option) tzresult Lwt.t

val get_storage :
  Raw_context.t ->
  Contract_repr.t ->
  (Raw_context.t * Script_repr.expr option) tzresult Lwt.t

module Legacy_big_map_diff : sig
  type item = private
    | Update of {
        big_map : Z.t;
        diff_key : Script_repr.expr;
        diff_key_hash : Script_expr_hash.t;
        diff_value : Script_repr.expr option;
      }
    | Clear of Z.t
    | Copy of {src : Z.t; dst : Z.t}
    | Alloc of {
        big_map : Z.t;
        key_type : Script_repr.expr;
        value_type : Script_repr.expr;
      }

  type t = item list

  val encoding : t Data_encoding.t

  val to_lazy_storage_diff : t -> Lazy_storage_diff.diffs

  val of_lazy_storage_diff : Lazy_storage_diff.diffs -> t
end

val update_script_storage :
  Raw_context.t ->
  Contract_repr.t ->
  Script_repr.expr ->
  Lazy_storage_diff.diffs option ->
  Raw_context.t tzresult Lwt.t

val credit_only_call_from_token :
  Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

val spend_only_call_from_token :
  Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

(** [raw_originate ctxt ~prepaid_bootstrap_storage contract ~script]
    originates the [contract] parameter. The [storage] space allocated by this
    origination is considered to be free of charge or to have been already paid
    for by the user, if and only if [prepaid_bootstrap_storage] is [true]. In 
    particular, the amount of space allocated by this origination will be part
    of the consumed space to pay for returned by the next call to
    [Fees_storage.record_paid_storage_space ctxt contract], if and only if
    [prepaid_bootstrap_storage] is [false]. *)
val raw_originate :
  Raw_context.t ->
  prepaid_bootstrap_storage:bool ->
  Contract_repr.t ->
  script:Script_repr.t * Lazy_storage_diff.diffs option ->
  Raw_context.t tzresult Lwt.t

val fresh_contract_from_current_nonce :
  Raw_context.t -> (Raw_context.t * Contract_repr.t) tzresult

val originated_from_current_nonce :
  since:Raw_context.t ->
  until:Raw_context.t ->
  Contract_repr.t list tzresult Lwt.t

val init : Raw_context.t -> Raw_context.t tzresult Lwt.t

val used_storage_space : Raw_context.t -> Contract_repr.t -> Z.t tzresult Lwt.t

val paid_storage_space : Raw_context.t -> Contract_repr.t -> Z.t tzresult Lwt.t

val set_paid_storage_space_and_return_fees_to_pay :
  Raw_context.t ->
  Contract_repr.t ->
  Z.t ->
  (Z.t * Raw_context.t) tzresult Lwt.t

(** Increases the balance of a contract. Calling this function directly may
    break important invariants. Consider calling [credit] instead. *)
val increase_balance_only_call_from_token :
  Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

(** Decreases the balance of a contract. Calling this function directly may
    break important invariants. Consider calling [spend] instead. *)
val decrease_balance_only_call_from_token :
  Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t
