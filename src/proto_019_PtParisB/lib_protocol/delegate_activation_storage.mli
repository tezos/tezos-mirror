(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module deals with delegates' activity. Typically, the provided
   functions can be used to deactivate a delegate that has not shown activity
   for a certain number of cycles, and to reactivate it when appropriate.

    This module is responsible for maintaining the following tables:
    - {!Storage.Contract.Inactive_delegate}
    - {!Storage.Contract.Delegate_last_cycle_before_deactivation} *)

val is_inactive :
  Raw_context.t -> Signature.Public_key_hash.t -> bool tzresult Lwt.t

(** [last_cycle_before_deactivation ctxt delegate] is the cycle at which
    the delegate is scheduled to become inactive. *)
val last_cycle_before_deactivation :
  Raw_context.t -> Signature.Public_key_hash.t -> Cycle_repr.t tzresult Lwt.t

(** [set_inactive context delegate] adds [delegate] to the set of inactive
    contracts. *)
val set_inactive :
  Raw_context.t -> Signature.Public_key_hash.t -> Raw_context.t Lwt.t

(** [set_active ctxt delegate] returns a pair [(new_ctxt, is_inactive)] where:
    - [new_ctxt] is a new context, updated from [ctxt], where the [delegate]'s
    last active cycle has been updated
    - [is_inactive] represents the state of [delegate], prior to the update.
  *)
val set_active :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  (Raw_context.t * bool) tzresult Lwt.t
