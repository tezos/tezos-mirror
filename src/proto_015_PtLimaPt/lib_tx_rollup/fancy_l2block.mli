(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module describes a fancy representation of a {!L2block.t}. We define
    a fancy block with an associated encoding. The encoded JSON will later on
    be used in RPCs to provide a cleaner and easier to use JSON object. *)

type l2_message =
  | Ok_deposit of Tx_rollup_message.t * Tx_rollup_l2_apply.indexes
      (** The deposit was interpreted with no error, we display only the created indexes if any. *)
  | Failing_deposit of {
      message : Tx_rollup_message.t;
      reason : Environment.Error_monad.error;
      withdrawal : Tx_rollup_withdraw.t;
    }  (** The deposit failed with an error, it produced a withdraw. *)
  | Ok_batch of {
      transactions_and_results :
        (( Indexable.unknown,
           Indexable.unknown )
         Tx_rollup_l2_batch.V1.transaction
        * Tx_rollup_l2_apply.Message_result.transaction_result)
        list;
      withdrawals : Tx_rollup_withdraw.t list;
      indexes : Tx_rollup_l2_apply.indexes;
      aggregated_signature : Tx_rollup_l2_batch.V1.signature;
    }
      (** The batch was interpreted, we list all transaction alongside their results.
          The transactions are marked as [(unknown, unknown) transaction] but we
          try to replace as much as we can the indexes by their values. *)
  | Failing_batch of {
      transactions :
        (Indexable.unknown, Indexable.unknown) Tx_rollup_l2_batch.V1.transaction
        list;
      reasons : tztrace;
      aggregated_signature : Tx_rollup_l2_batch.V1.signature;
    }
      (** The batch was discarded, it could not be interpreted with the l2-apply because of [tztrace]. *)
  | Unparsable_batch of string  (** The batch is unparsable. *)

type fancy_message = {
  message : l2_message;
  l2_context_hash : Inbox.l2_context_hash;
}

type inbox = fancy_message list

(** A fancy block is a classic block where the contents are reorganized. *)
type t = inbox L2block.block

(** [of_l2block ctxt block] uses the [ctxt] to transform a {!L2block.t} to a {!t}.
    It tries to replace the indexes in [block] when there are associated values
    in the [ctxt]. *)
val of_l2block : Context.t -> L2block.t -> t Lwt.t

(** Encoding used for block RPCs. *)
val encoding : t Data_encoding.t
