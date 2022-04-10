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

(** A block-indexed (key x value) store directory. *)
type index

(** Type of persitant context. *)
type context

include
  Protocol.Tx_rollup_l2_context_sig.CONTEXT
    with type t = context
     and type 'a m = 'a tzresult Lwt.t

val index : context -> index

(** Open or initialize a versioned store at a given path. *)
val init :
  ?patch_context:(context -> context tzresult Lwt.t) ->
  ?readonly:bool ->
  ?indexing_strategy:[`Always | `Minimal] ->
  string ->
  index Lwt.t

(** Build an empty context from an index. Don't commit an empty context. *)
val empty : index -> t

(** Close the index. Does not fail when the context is already closed. *)
val close : index -> unit Lwt.t

(** Sync the context with disk. Only useful for read-only instances.
    Does not fail when the context is not in read-only mode. *)
val sync : index -> unit Lwt.t

(** {2 Accessing and Updating Versions} *)

(** Returns true if there is a commit with this context hash *)
val exists : index -> Protocol.Tx_rollup_l2_context_hash.t -> bool Lwt.t

(** Checkout the context associated to a context hash. The context must have
    been committed (with {!commit}). Resolves with [None] if there is no such
    commit. *)
val checkout :
  index -> Protocol.Tx_rollup_l2_context_hash.t -> context option Lwt.t

(** Same as {!checkout} but resolves with an exception if there is no such
    commit. *)
val checkout_exn :
  index -> Protocol.Tx_rollup_l2_context_hash.t -> context Lwt.t

(** Hash a context. The hash can be done with an additional [message]. *)
val hash : ?message:string -> t -> Protocol.Tx_rollup_l2_context_hash.t

(** Create a commit and return the context hash. The hash can be done with an
    additional [message]. *)
val commit :
  ?message:string -> context -> Protocol.Tx_rollup_l2_context_hash.t Lwt.t
