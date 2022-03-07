(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** [is_empty l] returns [true] iff [l = []]. *)
val is_empty : 'a list -> bool

(** [remove nb list] remove the first [nb] elements from the list [list]. *)
val remove : int -> 'a list -> 'a list

(** [repeat n x] is a list of [n] [x]'s *)
val repeat : int -> 'a -> 'a list

(** [product a b] computes the Cartesian product of two lists [a] and [b]. *)
val product : 'a list -> 'b list -> ('a * 'b) list

(** [take_n n l] returns the [n] first elements of [l]. When [compare]
    is provided, it returns the [n] greatest element of [l]. *)
val take_n : ?compare:('a -> 'a -> int) -> int -> 'a list -> 'a list

(** [drop_n n l] returns the suffix of [l] after the first [n] elements,
    or [] if [n > length l]. *)
val drop_n : int -> 'a list -> 'a list

(** [split_n n l] is a pair of lists [(j, k)] where [j] contains the [n] first
    elements of [l] and [k] the remainder elements. If [l] has less than or
    exactly [n] elements, [j] is [l] and [k] is [[]]. *)
val split_n : int -> 'a list -> 'a list * 'a list

(** [rev_sub l n] is [List.rev l] capped to max [n] elements *)
val rev_sub : 'a list -> int -> 'a list

(** [sub l n] is [l] capped to max [n] elements *)
val sub : 'a list -> int -> 'a list

(** [shuffle l] is a list that contains the same elements as [l] but in a random
    order. *)
val shuffle : ?rng_state:Random.State.t -> 'a list -> 'a list
