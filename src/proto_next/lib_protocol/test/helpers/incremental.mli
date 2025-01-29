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

open Protocol
open Alpha_context

type t

type incremental = t

val predecessor : incremental -> Block.t

val header : incremental -> Block_header.t

val rev_tickets : incremental -> operation_receipt list

val validation_state : incremental -> validation_state

val level : incremental -> int32

(** [begin_construction ?mempool_mode predecessor] uses
    {!Main.begin_validation_and_application} to create a validation and
    application state on top of [predecessor] for the construction of a
    new block.

    Optional arguments allow to override defaults:

    {ul {li [?mempool_mode:bool]: when [true], use [Partial_construction]
    mode. By default, it is [false] and the mode is [Construction].}} *)
val begin_construction :
  ?timestamp:Time.Protocol.t ->
  ?seed_nonce_hash:Nonce_hash.t ->
  ?mempool_mode:bool ->
  ?policy:Block.baker_policy ->
  ?liquidity_baking_toggle_vote:Per_block_votes_repr.per_block_vote ->
  ?adaptive_issuance_vote:Per_block_votes_repr.per_block_vote ->
  Block.t ->
  incremental tzresult Lwt.t

(** [validate_operation ?expect_failure ?check_size i op] tries to
    validate [op] in the validation state of [i]. If the validation
    succeeds, the function returns the incremental value with a
    validation state updated after the validate. Otherwise raise the
    error from the validation of [op].

    Optional arguments allow to override defaults:

    {ul {li [?expect_failure:(error list -> unit tzresult Lwt.t)]:
    validation of [op] is expected to fail and [expect_failure] should
    handle the error. In case validate does not fail and an
    [expect_failure] is provided, [validate_operation] fails.}

    {li [?check_size:bool]: enable the check that an operation size
    should not exceed [Constants_repr.max_operation_data_length].
    Enabled (set to [true]) by default. }} *)
val validate_operation :
  ?expect_failure:(error list -> unit tzresult Lwt.t) ->
  ?check_size:bool ->
  incremental ->
  Operation.packed ->
  incremental tzresult Lwt.t

(** [add_operation ?expect_failure ?expect_apply_failure
    ?allow_manager_failure ?check_size i op] tries to validate then
    apply [op] in the validation and application state of [i]. If the
    validation of [op] succeeds, the function returns the incremental
    value with a validation state updated after the application of
    [op]. Otherwise raise the error from the validation of [op].

    Optional arguments allow to override defaults:

    @param expect_failure
    validation of [op] is expected to fail and [expect_failure] should
    handle the error. In case validate does not fail and
    [expect_failure] is provided, [validate_operation] fails.

    @param expect_apply_failure
    application of [op] is expected to fail and
    [expect_apply_failure] should handle the errror. In case the
    application of [op] does not fail and [expect_apply_failure] is
    provided, [add_operation] fails.

    @param allow_manager_failure
    marks that manager operation failures after fee taken are ignored.

    @param check_size
    enable the check that an operation size should not exceed
    [Constants_repr.max_operation_data_length]. Enabled (set to [true]) by
    default. *)
val add_operation :
  ?expect_failure:(error list -> unit tzresult Lwt.t) ->
  ?expect_apply_failure:(error list -> unit tzresult Lwt.t) ->
  ?allow_manager_failure:bool ->
  ?check_size:bool ->
  incremental ->
  Operation.packed ->
  incremental tzresult Lwt.t

(** [finalize_block i] creates a [Block.t] based on the protocol
    states and the operations contained in [i]. The function calls
    [Main.finalize_application] to compute a new context. *)
val finalize_block : incremental -> Block.t tzresult Lwt.t

(** [assert_validate_operation_fails expect_failure operation block]
    calls {!begin_construction} on top of [block], then
    {!validate_operation} with [~expect_failure]. *)
val assert_validate_operation_fails :
  (tztrace -> unit tzresult Lwt.t) ->
  Operation.packed ->
  Block.t ->
  unit tzresult Lwt.t

val rpc_ctxt : incremental Environment.RPC_context.simple

val alpha_ctxt : incremental -> Alpha_context.context

val set_alpha_ctxt : incremental -> Alpha_context.context -> incremental
