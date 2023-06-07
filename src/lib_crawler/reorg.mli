(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Nomadic Labs, <contact@nomadic-labs.com>                    *)
(* Copyright (c) Functori, <contact@functori.com>                            *)
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

(** Type of chain reorganizations. *)
type 'block t = {
  old_chain : 'block list;
      (** The blocks that were in the old chain and which are not in the new one. *)
  new_chain : 'block list;
      (** The blocks that are now in the new chain. The length of [old_chain] and
      [new_chain] may be different. *)
}

val no_reorg : 'a t

val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t

val map : ('a -> 'b) -> 'a t -> 'b t

val map_es : ('a -> 'b tzresult Lwt.t) -> 'a t -> 'b t tzresult Lwt.t

val map_ep : ('a -> 'b tzresult Lwt.t) -> 'a t -> 'b t tzresult Lwt.t
