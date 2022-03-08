(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

(** [add ctxt tx_rollup lvl message_index withdraw_index] adds
    [withdraw_index] to the list of already consumed withdrawawals for
    [tx_rollup] at [lvl] for the message_result at [message_index]. *)
val add :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_level_repr.t ->
  message_index:int ->
  withdraw_index:int ->
  Raw_context.t tzresult Lwt.t

(** [mem ctxt tx_rollup lvl message_index withdraw_index] checks if
    [withdraw_index] has already been consumed for [tx_rollup] at [lvl] for the
    message_result at [message_index]. This function consumes gas
    and so returns a new context. *)
val mem :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_level_repr.t ->
  message_index:int ->
  withdraw_index:int ->
  (bool * Raw_context.t) tzresult Lwt.t

(** [remove ctxt tx_rollup lvl] removes all withdrawal accounting for
    [tx_rollup] at [lvl]. This must not be called before the
    corresponding commitment is deleted. Otherwise, it would be
    possible to retrieve the same withdrawal multiple times. This
    function consumes gas and so returns a new context.  *)
val remove :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_level_repr.t ->
  inbox_length:int32 ->
  Raw_context.t tzresult Lwt.t
