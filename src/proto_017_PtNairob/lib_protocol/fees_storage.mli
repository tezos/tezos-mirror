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

type error += Cannot_pay_storage_fee (* `Temporary *)

type error += Negative_storage_input (* `Temporary *)

type error += Operation_quota_exceeded (* `Temporary *)

type error += Storage_limit_too_high (* `Permanent *)

(** [record_global_constant_storage_space ctxt size] records
    paid storage space for registering a new global constant.
    Cost is <size> in bytes + 65 additional bytes for the key
    hash of the expression. Returns new context and the cost.
*)
val record_global_constant_storage_space :
  Raw_context.t -> Z.t -> Raw_context.t * Z.t

(** [record_paid_storage_space ctxt contract] updates the amount of
    storage consumed by the [contract]. This total size is considered
    as accounted for as far as future payment is concerned.

    Returns a new context, the total space consumed by the [contract],
    and the additional (and unpaid) space consumed since the last call
    of this function on this [contract]. *)
val record_paid_storage_space :
  Raw_context.t -> Contract_hash.t -> (Raw_context.t * Z.t * Z.t) tzresult Lwt.t

(** [check_storage_limit ctxt ~storage_limit] raises the [Storage_limit_too_high]
     error iff [storage_limit] is negative or greater the constant
     [hard_storage_limit_per_operation]. *)
val check_storage_limit : Raw_context.t -> storage_limit:Z.t -> unit tzresult

(** [burn_storage_fees ctxt ~storage_limit ~payer consumed] takes funds from the
    [payer] to pay the cost of the [consumed] storage. This function has an
    optional parameter [~origin] that allows to set the origin of returned
    balance updates (by default the parameter is set to [Block_application]).
    Returns an updated context, an updated storage limit equal to
    [storage_limit - consumed], and the relevant balance updates.
    Raises the [Operation_quota_exceeded] error if [storage_limit < consumed].
    Raises the [Cannot_pay_storage_fee] error if the funds from the [payer] are
    not sufficient to pay the storage fees. *)
val burn_storage_fees :
  ?origin:Receipt_repr.update_origin ->
  Raw_context.t ->
  storage_limit:Z.t ->
  payer:Token.giver ->
  Z.t ->
  (Raw_context.t * Z.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** [burn_storage_increase_fees ctxt ~payer amount_in_bytes] takes funds from the
    [payer] to pay the cost of the [amount_in_bytes] storage. This function has an
    optional parameter [~origin] that allows to set the origin of returned
    balance updates (by default the parameter is set to [Block_application]).
    Returns an updated context and the relevant balance updates.
    Raises the [Negative_storage_input] error if the amount_in_bytes is null or negative.
    Raises the [Cannot_pay_storage_fee] error if the funds from the [payer] are
    not sufficient to pay the storage fees. *)
val burn_storage_increase_fees :
  ?origin:Receipt_repr.update_origin ->
  Raw_context.t ->
  payer:Token.giver ->
  Z.t ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** Calls [burn_storage_fees] with the parameter [consumed] mapped to the
    constant [origination_size]. *)
val burn_origination_fees :
  ?origin:Receipt_repr.update_origin ->
  Raw_context.t ->
  storage_limit:Z.t ->
  payer:Token.giver ->
  (Raw_context.t * Z.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** Calls [burn_storage_fees] with the parameter [consumed] mapped to the
    constant [tx_rollup_origination_size]. *)
val burn_tx_rollup_origination_fees :
  ?origin:Receipt_repr.update_origin ->
  Raw_context.t ->
  storage_limit:Z.t ->
  payer:Token.giver ->
  (Raw_context.t * Z.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** [burn_sc_rollup_origination_fees ~origin ctxt ~storage_limit ~payer consumed]
    burns the storage fees for smart contract rollup creation fees. *)
val burn_sc_rollup_origination_fees :
  ?origin:Receipt_repr.update_origin ->
  Raw_context.t ->
  storage_limit:Z.t ->
  payer:Token.giver ->
  Z.t ->
  (Raw_context.t * Z.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** [burn_zk_rollup_origination_fees ~origin ctxt ~storage_limit ~payer consumed]
    burns the storage fees for ZK rollup origination fees. *)
val burn_zk_rollup_origination_fees :
  ?origin:Receipt_repr.update_origin ->
  Raw_context.t ->
  storage_limit:Z.t ->
  payer:Token.giver ->
  Z.t ->
  (Raw_context.t * Z.t * Receipt_repr.balance_updates) tzresult Lwt.t
