(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

(** A module for representing and extracting typed transactional rollup
    parameters. *)

open Script_typed_ir

(** A type representing deposit parameters for transactional rollups. Deposit
    parameters consist of a ticket of arbitrary content along with a
    layer-2 destination address. *)
type deposit_parameters = {
  ex_ticket : Ticket_scanner.ex_ticket;
  l2_destination : tx_rollup_l2_address;
}

(** [get_deposit_parameters ty value] returns [ex_ticket] and a
    [tx_rollup_l2_address] from a michelson typed value.

    This function is intended to be used to enforce the type of the transaction
    to a [tx_rollup%deposit]. It must be used both in [ticket_diffs_of_operations]
    to account for the ticket deposited and in [apply] to retrieve the ticket
    when applying the transaction to a tx_rollup. *)
val get_deposit_parameters :
  (('a ticket, tx_rollup_l2_address) pair, 'comparable) ty ->
  ('a ticket, tx_rollup_l2_address) pair ->
  deposit_parameters
