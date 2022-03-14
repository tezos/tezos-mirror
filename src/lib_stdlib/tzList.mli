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

(** [repeat n x] is [List.of_array (Array.make n x)] but more efficient.

    If [n < 0] it is the empty list [[]]. *)
val repeat : int -> 'a -> 'a list

(** [take_n n l] returns the [n] first elements of [l].

    If [n < 0] it is the empty list [[]].
    If [n > List.length l] it is the list [l] in its entirety. *)
val take_n : int -> 'a list -> 'a list

(** [rev_take_n n l] is [List.rev (take_n n l)] but more efficient. *)
val rev_take_n : int -> 'a list -> 'a list

(** [drop_n n l] returns the suffix of [l] after the first [n] elements.

    If [n < 0] it is the list [l] in its entirety.
    If [n > List.length l] it is the empty list [[]]. *)
val drop_n : int -> 'a list -> 'a list

(** [split_n n l] is [(take_n n l, drop_n n l)] but more efficient. *)
val split_n : int -> 'a list -> 'a list * 'a list

(** [rev_split_n n l] is [(rev_take_n n l, drop_n n l)] but more efficient. *)
val rev_split_n : int -> 'a list -> 'a list * 'a list
