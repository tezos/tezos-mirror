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

(** Low-level handlers of raw contexts for base operations on
    contracts. *)

type error +=
  | (* `Temporary *)
      Balance_too_low of Contract_repr.t * Tez_repr.t * Tez_repr.t
  | (* `Temporary *)
      Counter_in_the_past of {
      contract : Contract_repr.t;
      expected : Manager_counter_repr.t;
      found : Manager_counter_repr.t;
    }
  | (* `Branch *)
      Counter_in_the_future of {
      contract : Contract_repr.t;
      expected : Manager_counter_repr.t;
      found : Manager_counter_repr.t;
    }
  | (* `Temporary *)
      Non_existing_contract of Contract_repr.t
  | (* `Permanent *)
      Inconsistent_public_key of
      Signature.Public_key.t * Signature.Public_key.t
  | (* `Permanent *) Failure of string
  | (* `Branch *)
      Empty_implicit_contract of Signature.Public_key_hash.t
  | (* `Branch *)
      Empty_implicit_delegated_contract of
      Signature.Public_key_hash.t

(** [allocated ctxt contract] returns [true] if and only if the
   contract is stored in {!Storage.Contract.Spendable_balance}. *)
val allocated : Raw_context.t -> Contract_repr.t -> bool Lwt.t

(** [exists ctxt contract] returns [true] if and only if either the
   contract is implicit or it is (originated and) {!allocated}. *)
val exists : Raw_context.t -> Contract_repr.t -> bool Lwt.t

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
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Manager_counter_repr.t ->
  unit tzresult Lwt.t

val increment_counter :
  Raw_context.t -> Signature.Public_key_hash.t -> Raw_context.t tzresult Lwt.t

(** [get_balance ctxt contract] returns the balance of spendable tez owned by
    [contract] given raw context [ctxt]. This does not include the contract's
    frozen balances. *)
val get_balance : Raw_context.t -> Contract_repr.t -> Tez_repr.t tzresult Lwt.t

val get_balance_carbonated :
  Raw_context.t ->
  Contract_repr.t ->
  (Raw_context.t * Tez_repr.t) tzresult Lwt.t

(** Return the balance of spendable tez owned by the Implicit contract
    of the given [public_key_hash].

    @return [Error Empty_implicit_contract] if the contract is not
    allocated in {!Storage.Contract.Spendable_balance}.

    This function is a fusion of {!must_be_allocated} and
    {!get_balance} for Implicit contracts exclusively. *)
val check_allocated_and_get_balance :
  Raw_context.t -> Signature.public_key_hash -> Tez_repr.t tzresult Lwt.t

val get_counter :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Manager_counter_repr.t tzresult Lwt.t

val get_script_code :
  Raw_context.t ->
  Contract_hash.t ->
  (Raw_context.t * Script_repr.lazy_expr option) tzresult Lwt.t

val get_script :
  Raw_context.t ->
  Contract_hash.t ->
  (Raw_context.t * Script_repr.t option) tzresult Lwt.t

val get_storage :
  Raw_context.t ->
  Contract_hash.t ->
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
  Contract_hash.t ->
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
  Contract_hash.t ->
  script:Script_repr.t * Lazy_storage_diff.diffs option ->
  Raw_context.t tzresult Lwt.t

val fresh_contract_from_current_nonce :
  Raw_context.t -> (Raw_context.t * Contract_hash.t) tzresult

val originated_from_current_nonce :
  since:Raw_context.t ->
  until:Raw_context.t ->
  Contract_hash.t list tzresult Lwt.t

val init : Raw_context.t -> Raw_context.t tzresult Lwt.t

val used_storage_space : Raw_context.t -> Contract_repr.t -> Z.t tzresult Lwt.t

val paid_storage_space : Raw_context.t -> Contract_repr.t -> Z.t tzresult Lwt.t

val set_paid_storage_space_and_return_fees_to_pay :
  Raw_context.t ->
  Contract_repr.t ->
  Z.t ->
  (Z.t * Raw_context.t) tzresult Lwt.t

(** Enable a payer to increase the paid storage of a contract by some amount. *)
val increase_paid_storage :
  Raw_context.t ->
  Contract_hash.t ->
  amount_in_bytes:Z.t ->
  Raw_context.t tzresult Lwt.t

(** Increases the balance of a contract. Calling this function directly may
    break important invariants. Consider calling [credit] instead. *)
val increase_balance_only_call_from_token :
  Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

(** Decreases the balance of a contract. Calling this function directly may
    break important invariants. Consider calling [spend] instead. *)
val decrease_balance_only_call_from_token :
  Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

(** [get_balance_and_frozen_bonds ctxt contract] returns the sum of the
    (spendable) balance and the frozen bonds associated to [contract]. *)
val get_balance_and_frozen_bonds :
  Raw_context.t -> Contract_repr.t -> Tez_repr.t tzresult Lwt.t

(** This error is raised when [spend_bond_only_call_from_token] is called with
    an amount that is not equal to the deposit associated to the given contract
    and bond id. *)
type error +=
  | (* `Permanent *)
      Frozen_bonds_must_be_spent_at_once of
      Contract_repr.t * Bond_id_repr.t

(** [bond_allocated ctxt contract bond_id] returns a new context because of an
    access to carbonated data, and [true] if there is a bond associated to
    [contract] and [bond_id], or [false] otherwise. *)
val bond_allocated :
  Raw_context.t ->
  Contract_repr.t ->
  Bond_id_repr.t ->
  (Raw_context.t * bool) tzresult Lwt.t

(** [find_bond ctxt contract bond_id] returns a new context because of an access
    to carbonated data, and the bond associated to [(contract, bond_id)] if
    there is one, or [None] otherwise. *)
val find_bond :
  Raw_context.t ->
  Contract_repr.t ->
  Bond_id_repr.t ->
  (Raw_context.t * Tez_repr.t option) tzresult Lwt.t

(** [spend_bond ctxt contract bond_id amount] withdraws the given [amount] from
    the value of the bond associated to [contract] and [bond_id].

    The argument [amount] is required to be strictly positive.

    @raise a [Storage_Error Missing_key] error when there is no bond associated
    to [contract] and [bond_id].

    @raise a [Frozen_bonds_must_be_spent_at_once (contract, bond_id)]
    error when the amount is different from the bond associated to [contract]
    and [bond_id]. *)
val spend_bond_only_call_from_token :
  Raw_context.t ->
  Contract_repr.t ->
  Bond_id_repr.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [credit_bond ctxt contract bond_id amount] adds the given [amount] to the
    bond associated to [contract] and [bond_id]. If no bond exists, one whose
    value is [amount] is created.

    The argument [amount] is required to be strictly positive.

    @raise a [Addition_overflow] error when
    [(find ctxt contract bond_id) + amount > Int64.max_int]. *)
val credit_bond_only_call_from_token :
  Raw_context.t ->
  Contract_repr.t ->
  Bond_id_repr.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [has_frozen_bonds ctxt contract] returns [true] if there are frozen bonds
    associated to [contract], and returns [false] otherwise. *)
val has_frozen_bonds : Raw_context.t -> Contract_repr.t -> bool tzresult Lwt.t

(** [get_frozen_bonds ctxt contract] returns the total amount of bonds associated
    to [contract]. *)
val get_frozen_bonds :
  Raw_context.t -> Contract_repr.t -> Tez_repr.t tzresult Lwt.t

(** [fold_on_bond_ids ctxt contract order init f] folds [f] on all bond
    identifiers associated to [contract]. *)
val fold_on_bond_ids :
  Raw_context.t ->
  Contract_repr.t ->
  order:[`Sorted | `Undefined] ->
  init:'a ->
  f:(Bond_id_repr.t -> 'a -> 'a Lwt.t) ->
  'a Lwt.t

(** [ensure_deallocated_if_empty ctxt contract] de-allocates [contract] if its
    full balance is zero, and it does not delegate. *)
val ensure_deallocated_if_empty :
  Raw_context.t -> Contract_repr.t -> Raw_context.t tzresult Lwt.t

(** [simulate_spending ctxt ~balance ~amount source] removes [amount]
    from [balance] as if it were the balance of the implicit contract
    associated with [source]. It returns the resulting [new_balance],
    and a boolean [still_allocated] that indicates whether this
    contract would still exist.

    [still_allocated] is always [true] when [new_balance] is
    positive. When [new_balance] is zero, it depends on the contract's
    delegated status and frozen bonds (cf {!spend_only_call_from_token}
    and {!ensure_deallocated_if_empty}).

    Note that this function does not retrieve the actual balance of
    the contract, nor does it update or delete it. Indeed, its purpose
    is to simulate the spending of fees when validating operations,
    without actually spending them.

    @return [Error Balance_too_low] if [balance] is smaller than
    [amount].

    @return [Error Empty_implicit_delegated_contract] if [new_balance]
    would be zero and the contract has a delegate that is not the
    contract's own manager. *)
val simulate_spending :
  Raw_context.t ->
  balance:Tez_repr.t ->
  amount:Tez_repr.t ->
  Signature.public_key_hash ->
  (Tez_repr.t * bool) tzresult Lwt.t
