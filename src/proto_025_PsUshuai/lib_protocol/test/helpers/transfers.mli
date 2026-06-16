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

open Protocol
open Alpha_context

(** [transfer_and_check_balances b fee src dst amount]
   this function takes a block, an optional parameter fee if fee does not
   given it will be set to zero tez, a source contract, a destination contract
   and the amount that one wants to transfer.

   1- Transfer the amount of tez (w/wo fee) from a source contract to a
       destination contract.

    2- Check the equivalent of the balance of the source/destination
       contract before and after transfer is validated.

   This function returns a pair:
   - A block that added a valid operation
   - a valid operation *)
val transfer_and_check_balances :
  ?with_burn:bool ->
  loc:string ->
  Incremental.t ->
  ?fee:Tez.t ->
  ?expect_apply_failure:(error trace -> unit tzresult Lwt.t) ->
  Contract.t ->
  Contract.t ->
  Tez.t ->
  (Incremental.t * packed_operation) tzresult Lwt.t

(** [n_transactions n b fee source dest amount]
   this function takes a number of "n" that one wish to transfer,
   a block, an optional parameter fee, a source contract,
   a destination contract and an amount one wants to transfer.

   This function will do a transaction from a source contract to
   a destination contract with the amount "n" times. *)
val n_transactions :
  int ->
  Incremental.t ->
  ?fee:Tez.t ->
  Contract.t ->
  Contract.t ->
  Tez.t ->
  Incremental.t tzresult Lwt.t
