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

(** Simple abstraction from low-level storage to handle frozen deposits.

    This module is responsible for maintaining the
    {!Storage.Contract.Frozen_deposits} table. *)

(** [init ctxt delegate] returns a new context from [ctxt] where the frozen
   deposits of the implicit contract represented by [delegate] have been initialized to
   {!val:Tez_repr.zero}. *)
val init :
  Raw_context.t -> Signature.Public_key_hash.t -> Raw_context.t tzresult Lwt.t

(** [allocated ctxt contract] checks whether [contract] has frozen deposits in
    [ctxt]. *)
val allocated : Raw_context.t -> Contract_repr.t -> bool Lwt.t

(** [get ctxt contract] retrieves the frozen deposits of [contract] in [ctxt]. *)
val get : Raw_context.t -> Contract_repr.t -> Storage.deposits tzresult Lwt.t

(** [find ctxt contract] retrieves the frozen deposits of [contract] in
    [ctxt], if any. *)
val find :
  Raw_context.t -> Contract_repr.t -> Storage.deposits option tzresult Lwt.t

(** [credit_only_call_from_token ctxt delegate tez] returns a new context from
   [ctxt] where the amount of frozen deposits for the implicit contract
   represented by [delegate] increases by [tez]. *)
val credit_only_call_from_token :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [spend_only_call_from_token ctxt delegate tez] returns a new context from
   [ctxt] where the amount of frozen deposits for the implicit contract
   represented by [delegate] decreases by [tez].*)
val spend_only_call_from_token :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [update_initial_amount ctxt contract tez] returns a new context from [ctxt]
   where the initial_amount of the frozen deposits for [contract] is set to
   [tez]. *)
val update_initial_amount :
  Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t
