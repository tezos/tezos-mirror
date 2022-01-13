(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** [originate context ~kind ~boot_sector] produces an address [a] for
   a smart contract rollup using the origination nonce found in
   [context]. This function also initializes the storage with a new
   entry indexed by [a] to remember the [kind] of the rollup at
   address [a] and also to remember its [boot_sector].

   Also returns the number of allocated bytes.  *)
val originate :
  Raw_context.t ->
  kind:Sc_rollup_repr.Kind.t ->
  boot_sector:Sc_rollup_repr.PVM.boot_sector ->
  (Sc_rollup_repr.Address.t * Z.t * Raw_context.t) tzresult Lwt.t

(** [kind context address] returns [Some kind] iff [address] is an
    existing rollup of some [kind]. Returns [None] if [address] is
    not the address of an existing rollup. *)
val kind :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Kind.t option tzresult Lwt.t

(** [add_message context rollup msg] adds [msg] to [rollup]'s inbox.

    This function is carbonated and returns the updated context as well as
    the size diff. *)
val add_messages :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  string list ->
  (Sc_rollup_inbox.t * Z.t * Raw_context.t) tzresult Lwt.t

(** [inbox context rollup] returns the current state of the inbox.

    This function is carbonated. *)
val inbox :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  (Sc_rollup_inbox.t * Raw_context.t) tzresult Lwt.t
