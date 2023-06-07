(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Abstraction module for OCaml references *)

(** Type [('perm, 'value) t] represents references containing a value
    of type ['value] and permissions specified by ['perm].  *)
type ('perm, 'value) t

(** Read/write reference {!t}. *)
type 'a rw = ([`Read | `Write], 'a) t

(** Read only reference {!t}. *)
type 'a ro = ([`Read], 'a) t

(** [get r] returns the value held by reference [r]. *)
val get : (_, 'a) t -> 'a

(** [set r v] sets reference [r] to the value [v]. *)
val set : 'a rw -> 'a -> unit

(** [new_ v] creates a fresh reference holding value [v]. *)
val new_ : 'a -> (_, 'a) t

(** [readonly r] casts reference [r] to be read only. *)
val readonly : (_, 'a) t -> 'a ro
