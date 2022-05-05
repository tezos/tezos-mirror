(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This helper module allows to handle partially specified limits during the
    injection process. *)

(** A value of type ['a t] is either [unknown] of [known]. *)
type 'a t

val unknown : 'a t

val known : 'a -> 'a t

val of_option : 'a option -> 'a t

val is_unknown : 'a t -> bool

(** [join ~where eq x y] computes the order-theoretic union of [x] and [y].
    If both [x] and [y] are not [Unknown], the function fails iff their
    contents are not equal according to [eq]. *)
val join : where:string -> ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t tzresult

val fold : unknown:'a -> known:('b -> 'a) -> 'b t -> 'a

val get : when_unknown:string -> 'a t -> 'a tzresult

val value : when_unknown:'a -> 'a t -> 'a
