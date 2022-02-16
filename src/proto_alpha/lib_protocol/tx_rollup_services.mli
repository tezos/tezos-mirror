(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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

open Alpha_context

val state :
  'a #RPC_context.simple ->
  'a ->
  Tx_rollup.t ->
  Tx_rollup_state.t shell_tzresult Lwt.t

(** Returns the inbox for a transaction rollup for current level.

    Returns [Not_found] if the transaction rollup exists, but does not
    have inbox at that level. Fails if the transaction rollup does not
    exist. *)
val inbox :
  'a #RPC_context.simple ->
  'a ->
  Tx_rollup.t ->
  Tx_rollup_inbox.t shell_tzresult Lwt.t

val commitments :
  'a #RPC_context.simple ->
  'a ->
  ?offset:int ->
  Tx_rollup.t ->
  Tx_rollup_commitments.t shell_tzresult Lwt.t

val register : unit -> unit

val current_inbox :
  unit -> ([`GET], 'a, 'a, unit, unit, Tx_rollup_inbox.t option) RPC_service.t

val current_tezos_head :
  unit -> ([`GET], 'a, 'a, unit, unit, Block_hash.t option) RPC_service.t
