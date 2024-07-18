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

(** A module for representing and extracting typed ZK rollup
    parameters. *)

(** A type representing deposit parameters for ZK rollups. Deposit
    parameters consist of a ticket of arbitrary content along with a
    layer-2 ZKRU operation byte representation. *)
type deposit_parameters = {
  ex_ticket : Ticket_scanner.ex_ticket;
  zkru_operation : Alpha_context.Zk_rollup.Operation.t;
}

(** [get_deposit_parameters ty value] returns [ex_ticket] and a
    [zkru_operation] from a michelson typed value. if [ty] is not of a
    pair of ticket and [bytes] then it fails with
    [Zk_rollup_errors.Wrong_deposit_parameters].

    This function is intended to be used to enforce the type of the transaction
    to a [zk_rollup%deposit]. It must be used both in [ticket_diffs_of_operations]
    to account for the ticket deposited and in [apply] to retrieve the ticket
    when applying the transaction to a zk_rollup. *)
val get_deposit_parameters :
  ( ('a Script_typed_ir.ticket, bytes) Script_typed_ir.pair,
    'comparable )
  Script_typed_ir.ty ->
  ('a Script_typed_ir.ticket, bytes) Script_typed_ir.pair ->
  deposit_parameters tzresult
