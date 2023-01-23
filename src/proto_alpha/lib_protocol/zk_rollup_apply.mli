(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module handles all the validation/application of any operation
    related to the ZK Rollup.
    All of the functions defined in this module require that the ZKRU
    feature flag is enabled.
*)

(** In the ZK Rollup, L2 operations are validated in two steps:
    {ol
      {li The Protocol does the first pass of (light) validation and
          appends the L2 operation to a pending list.}
      {li The ZKRU Operator does the second pass of validation for a prefix
          of the pending list and posts a proof on chain of the validity of
          each of them.
          Based on this proof, the Protocol is going to remove the prefix
          from the pending list, and apply their effect on the ZKRU L2 state
          and on the L1 balances.}
    }

    The first step of validation is split into two cases, depending on
    the type of L2 operation that is being submitted:
    {ul
      {li If the application of said L2 operation results in a transfer
          of a ticket from L1 to L2 (i.e. it is a ZKRU {i deposit}), the
          L2 operation has to be submitted through a call to the ZKRU
          [%deposit] entrypoint from a smart contract.
          This constraint is imposed by the fact that implicit accounts
          cannot transfer tickets.
          Then, the validation of these L2 operations will be performed
          when applying the internal Tezos operation emitted by the call
          to the ZKRU's deposit entrypoint. This is implemented by the
          [transaction_to_zk_rollup] function in this module.
      }
      {li If its application results in a ticket transfer from L2 to L1
          (i.e. it is a ZKRU {i withdrawal}) or it has no transfer between
          layers, the L2 operation has to be submitted through a
          [Zk_rollup_publish] external Tezos operation.
          The checks for these L2 operations will be perform upon application
          of said external Tezos operation, whose logic is implemented by the
          [publish] function in this module.
      }
    }

    Although L2 operations are mostly opaque, they expose a header that is
    transparent to the Protocol (see {!Zk_rollup_operation_repr.t}).
    In this header there's a field for the [price] of an L2 operation, which
    will expose its kind. Concretely, the [price] encodes the net ticket
    transfer from L1 to L2 caused by an L2 operation. Then, deposits have
    a positive price, withdrawals a negative one, and pure L2 operations
    must have a price of zero.

    An L2 operation's price also encodes which ticket is being transferred,
    by storing the ticket's hash (see {!Ticket_hash_repr}). These hashes are
    used as token identifiers inside the ZKRU. In both cases, the L2 operations
    with a non-zero price (i.e. deposits and withdrawals) will be submitted
    alongside the values describing the ticket being transferred
    (see {!Zk_rollup_ticket_repr}). These values have to be consistent with
    the token identifier used in the L2 operation's price.

    NB: if ticket transfers by implicit accounts was supported, these two cases
    could be unified into the application of the [Zk_rollup_publish] operation.
*)

open Alpha_context

(** These errors are only to be matched in tests. *)
type error +=
  | Zk_rollup_feature_disabled
        (** Emitted when trying to apply a ZK Rollup operation while the ZKRU
            feature flag is not active. *)
  | Zk_rollup_negative_nb_ops
        (** Emitted when originating a ZK Rollup with a negative [nb_ops]. *)

(** [assert_feature_enabled ctxt] asserts that the ZK Rollup feature flag
    is activated.

    May fail with:
    {ul
      {li [Zk_rollup_feature_disabled] if the ZKRU feature flag is not
        activated.}
    }
*)
val assert_feature_enabled : t -> unit tzresult

(** [originate ~ctxt_before_op ~ctxt ~public_parameters ~transcript
               ~circuits_info ~init_state ~nb_ops]
    applies the origination operation for a ZK rollup.
    See {!Zk_rollup_storage.originate}.

    May fail with:
    {ul
      {li [Zk_rollup_feature_disabled] if the ZKRU feature flag is not
        activated.}
      {li [Zk_rollup_negative_nb_ops] if [nb_ops] is negative.}
    }
*)
val originate :
  ctxt_before_op:t ->
  ctxt:t ->
  public_parameters:Plonk.public_parameters ->
  circuits_info:[`Public | `Private | `Fee] Zk_rollup.Account.SMap.t ->
  init_state:Zk_rollup.State.t ->
  nb_ops:int ->
  (t
  * Kind.zk_rollup_origination Apply_results.successful_manager_operation_result
  * Script_typed_ir.packed_internal_operation list)
  tzresult
  Lwt.t

(** [publish ~ctxt_before_op ~ctxt ~zk_rollup ~l2_ops]
    applies a publish operation to [zk_rollup] by adding [l2_ops] to its
    pending list.

    All L2 operations in [l2_ops] must claim a non-positive [price]
    (see {!Zk_rollup_operation_repr}). In other words, no deposit is
    allowed in this operation, as those must go through an internal
    transaction.

    This function will first perform a series of validation checks over
    the L2 operations in [l2_ops]. If all of them are successful, these L2
    operations will be added to [dst_rollup]'s pending list.

    May fail with:
    {ul
      {li [Zk_rollup_feature_disabled] if the ZKRU feature flag is not
        activated.
      }
      {li [Zk_rollup.Errors.Deposit_as_external] if the price of an L2
        operation from [ops] is positive.
      }
      {li [Zk_rollup.Errors.Invalid_deposit_amount] if an L2 operation
        declares no ticket but has a non-zero price or if it declares
        a ticket with a price of zero.
      }
      {li [Zk_rollup.Errors.Invalid_deposit_ticket] if an L2 operation's
        ticket identifier (see [Zk_rollup_operation_repr]) is different from
        the hash of its corresponding ticket and [l1_dst].
      }
      {li [Zk_rollup_storage.Zk_rollup_invalid_op_code op_code] if the
        [op_code] of one of the [operations] is greater or equal
        to the number of declared operations for this [zk_rollup].
      }
    }
*)
val publish :
  ctxt_before_op:t ->
  ctxt:t ->
  zk_rollup:Zk_rollup.t ->
  l2_ops:(Zk_rollup.Operation.t * Zk_rollup.Ticket.t option) list ->
  (t
  * Kind.zk_rollup_publish Apply_results.successful_manager_operation_result
  * Script_typed_ir.packed_internal_operation list)
  tzresult
  Lwt.t

(** [transaction_to_zk_rollup
      ~ctxt ~parameters_ty ~parameters ~payer ~dst_rollup ~since] applies an
    internal transaction to a ZK [dst_rollup].

    Internal transactions are used for deposits into ZK rollups, which can
    be seen as a special case of the publish ZK rollup operation.
    The [parameters] should include a ticket and a ZKRU L2 operation, as
    explained in the {!Zk_rollup_parameters} module's documentation.

    This function will first perform a series of validation checks.
    If successful, the L2 operation from the [parameters] will be added
    to [dst_rollup]'s pending list, and [payer] will pay for the
    added storage.

    May fail with:
    {ul
      {li [Zk_rollup_feature_disabled] if the ZKRU feature flag is not
        activated.
      }
      {li [Zk_rollup.Errors.Ticket_payload_size_limit_exceeded] if the ticket
        found in the [parameters] exceeds the maximum ticket size.
      }
      {li [Script_tc_errors.Forbidden_zero_ticket_quantity] if the ticket
        amount is zero.
      }
      {li [Zk_rollup.Errors.Invalid_deposit_amount] if the amount of the ticket
        transferred to the [dst_rollup] is different from the [price]
        (see {!Zk_rollup_operation_repr}) claimed by the L2 operation.
      }
      {li [Zk_rollup.Errors.Invalid_deposit_ticket] if the L2 operation's
        ticket identifier (see {!Zk_rollup_operation_repr}) is different to
        the hash of the transferred ticket and [dst_rollup].
      }
      {li [Zk_rollup_storage.Zk_rollup_invalid_op_code op_code] if the
        [op_code] of the operation from the [parameters] is greater or equal
        to the number of declared operations for this rollup.
      }
      {li [Zk_rollup.Errors.Wrong_deposit_parameters] if the [parameters]
        are not of the expected type. See {!Zk_rollup_parameters}.
      }
    }
*)
val transaction_to_zk_rollup :
  ctxt:t ->
  parameters_ty:
    ( ('a Script_typed_ir.ticket, bytes) Script_typed_ir.pair,
      'b )
    Script_typed_ir.ty ->
  parameters:('a Script_typed_ir.ticket, bytes) Script_typed_ir.pair ->
  dst_rollup:Zk_rollup.t ->
  since:t ->
  (t
  * Kind.transaction Apply_internal_results.successful_internal_operation_result
  * Script_typed_ir.packed_internal_operation list)
  tzresult
  Lwt.t

(** [update ~ctxt_before_op ~ctxt ~zk_rollup ~update ~source_contract]
    applies an [update] to [zk_rollup].

    A ZKRU update will verify three sorts of ZK circuits:
    {ul
      {li Public operation circuits, that handle a single L2 operation
        from the pending list.}
      {li Private batch circuits, that handle a batch of private L2
        operations.}
      {li Fee circuit, which credits the ZKRU operator with all the aggregated
        fees from the update.}
    }

    The [update] provides some inputs required to perform this verification,
    alongside the proof. See {!Zk_rollup_update_repr}.

    If the verification is successful, the [zk_rollup]'s state is updated,
    a prefix of its pending list is dropped and the exits from the ZKRU are
    performed.

    May fail with:
    {ul
      {li [Zk_rollup_feature_disabled] if the ZKRU feature flag is not
        activated.
      }
      {li [Zk_rollup.Errors.Pending_bound] if the [update] processes fewer
        public operation than allowed.
      }
      {li [Zk_rollup.Errors.Inconsistent_state_update] if the [update] declares
        a new state of incorrect length.
      }
      {li [Zk_rollup.Errors.Invalid_circuit] if a public operation circuit is
        ran as private.
      }
      {li [Zk_rollup.Errors.Invalid_verification] if the PlonK verification
        fails.
      }
      {li [Zk_rollup.Errors.Invalid_deposit_amount] if an L2 operation without
        a corresponding ticket in the pending list has a non-zero price.
      }
      {li [Zk_rollup_storage.Zk_rollup_pending_list_too_short]
        if the [update] tries to process more public operations than those in
        the pending list.
      }
    }
*)
val update :
  ctxt_before_op:t ->
  ctxt:t ->
  zk_rollup:Zk_rollup.t ->
  update:Zk_rollup.Update.t ->
  (t
  * Kind.zk_rollup_update Apply_results.successful_manager_operation_result
  * Script_typed_ir.packed_internal_operation list)
  tzresult
  Lwt.t
