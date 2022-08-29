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

(** These errors are only to be matched in tests. *)
type error +=
  | Zk_rollup_does_not_exist of Zk_rollup_repr.t
        (** Emitted when trying to perform an operation over a ZK rollup
            that hasn't been initialised. *)

(** [originate context static ~init_state] produces an address [a] for
    a ZK rollup storage using the [origination_nonce] from
    the [context]. This function also initializes the storage,
    indexing the initial ZKRU account by [a].

     Returns the new context and ZKRU address, alongside the size
     of the new account.
*)
val originate :
  Raw_context.t ->
  Zk_rollup_account_repr.static ->
  init_state:Zk_rollup_state_repr.t ->
  (Raw_context.t * Zk_rollup_repr.t * Z.t) tzresult Lwt.t

(** [exists context rollup] returns a boolean representing whether
    [rollup] has been initialized.
*)
val exists :
  Raw_context.t -> Zk_rollup_repr.t -> (Raw_context.t * bool) tzresult Lwt.t
