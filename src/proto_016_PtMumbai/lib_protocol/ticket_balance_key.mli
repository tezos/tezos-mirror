(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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

(** This module exposes a function for generating a ticket-balance key-hash
    given an owner and a ticket-token. The key-hash is used for populating the
    global ticket-balance table that tracks ownership of tickets for different tokens.
 *)

(** [make ~owner ~ticketer ~contents_type ~contents] returns [key_hash] of the
    given [owner], [ticketer], [contents_type] and [contents]. Note that the
    [location] of the [Script.node] values [contents_type] and [contents] are
    irrelevant since [Ticket_hash.make] will strip the locations before calculating the hash. *)
val make :
  context ->
  owner:Destination.t ->
  ticketer:Contract.t ->
  contents_type:Script.node ->
  contents:Script.node ->
  (Ticket_hash.t * context) tzresult Lwt.t

(** [of_ex_token ctxt ~owner ex_token] returns the [key_hash] of the
    given [owner] and [ex_token]. *)
val of_ex_token :
  context ->
  owner:Destination.t ->
  Ticket_token.ex_token ->
  (Ticket_hash.t * context) tzresult Lwt.t
