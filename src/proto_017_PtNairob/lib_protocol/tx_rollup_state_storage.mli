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

(** A collection of functions to manipulate the state of a transaction
    rollup.

    Except if the contrary is explicitly stated, the functions of this
    module are carbonated. *)

(** [init ctxt tx_rollup] initializes the state of [tx_rollup].

    Returns the error [Tx_rollup_already_exists] iff this function has
    already been called for [tx_rollup], which is definitely something
    that should not happen, because the protocol is expected to pick
    fresh addresses when it originates new transaction rollups (and
    does so by relying on the “origination nonce” derived from the
    hash of the operation responsible for the origination, using the
    same procedure as smart contracts).

    Raising this error would therefore indicate a bug in the
    protocol. *)
val init : Raw_context.t -> Tx_rollup_repr.t -> Raw_context.t tzresult Lwt.t

(** [find ctxt tx_rollup] returns the current state of [tx_rollup]. If
    [tx_rollup] is not the address of an existing transaction rollup,
    [None] is returned instead. *)
val find :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  (Raw_context.t * Tx_rollup_state_repr.t option) tzresult Lwt.t

(** [get ctxt tx_rollup] returns the current state of [tx_rollup] in
    the context.

    Returns the [Tx_rollup_does_not_exist] error iff [tx_rollup] is
    not the address of an existing transaction rollup. *)
val get :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  (Raw_context.t * Tx_rollup_state_repr.t) tzresult Lwt.t

(** [update ctxt tx_rollup new_state] replaces the stored state of
    [tx_rollup] with [new_state]. *)
val update :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_state_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [assert_exist ctxt tx_rollup] fails with
    [Tx_rollup_does_not_exist] when [tx_rollup] is not a valid
    transaction rollup address. *)
val assert_exist :
  Raw_context.t -> Tx_rollup_repr.t -> Raw_context.t tzresult Lwt.t
