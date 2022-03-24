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

(** The rollup node maintains an inbox of incoming messages.

   The incoming messages for a rollup are published on the layer 1. To
   maintain the state of its inbox, a rollup node retrieves these
   messages each time the tezos blockchain is updated.

   The inbox state is persistent.

*)
open Protocol

(** [update cctxt store chain_event] changes the state of the inbox to react to
    the [chain_event]. In particular, this process requests the tezos node
    through the context client [cctxt] to retrieve the messages published
    during this [chain_event]. *)
val update :
  #RPC_context.simple -> Store.t -> Layer1.chain_event -> unit tzresult Lwt.t

(** [inbox_of_hash store block_hash] returns the rollup inbox at the end of the
    given validation of [block_hash]. *)
val inbox_of_hash :
  Store.t -> Block_hash.t -> Alpha_context.Sc_rollup.Inbox.t Lwt.t

(** [start store rollup] initializes the inbox to track the messages published for
    the given [rollup]. *)
val start : Store.t -> Alpha_context.Sc_rollup.t -> unit Lwt.t
