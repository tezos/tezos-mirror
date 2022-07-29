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
val assert_feature_enabled : t -> unit tzresult Lwt.t

(** [originate ~ctxt_before_op ~ctxt ~public_parameters ~transcript
               ~circuits_info ~init_state ~nb_ops]
    applies the origination operation for a ZK rollup.
    See {!Zk_rollup_storage:originate}.

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
  circuits_info:bool Zk_rollup.Account.SMap.t ->
  init_state:Zk_rollup.State.t ->
  nb_ops:int ->
  (t
  * Kind.zk_rollup_origination Apply_results.successful_manager_operation_result
  * Script_typed_ir.packed_internal_operation list)
  tzresult
  Lwt.t
