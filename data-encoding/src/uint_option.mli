(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Like most other [.mli] files in this directory, this is not intended for
    end-users. Instead, the interface from this file is used internally to
    assemble the end-user-intended module {!Data_encoding}. Refer to that module
    for doucmentation. *)

(**

   Optional non-negative integers can be represented by an [int]
   provided that [None] is encoded as [-1] and [Some x] is encoded by
   [x]. This module provides a type abstraction to maintain this
   invariant.

   This optimized representation avoids the allocation of a block in
   the heap and also removes one indirection when reading such an
   optional non-negative integer.

   @raise [Assert_failure] when assumptions are broken. This module is meant to
   be used internally only, covered by an extensive test-suite with assertion
   checks on, and packaged with assertion checks off.

*)

type t = private int

(** An absent non-negative integer. *)
val none : t

(** [some x] assumes that [x] is non-negative. *)
val some : int -> t

(** [is_none x] returns [x] iff [x] is an absent non-negative integer. *)
val is_none : t -> bool

(** [is_some x] returns [x] iff [x] is a non-negative integer. *)
val is_some : t -> bool

(** [fold ~none:d some:f x = d] if [x] is absent and [f i] if [x = some i]. *)
val fold : none:'a -> some:(int -> 'a) -> t -> 'a

(** [get x] returns [i] assuming that [x = some i]. *)
val get : t -> int
