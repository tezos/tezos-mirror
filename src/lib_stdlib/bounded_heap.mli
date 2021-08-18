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

(** Bounded sequence: keep only the [n] greatest elements. *)

module Make (E : Set.OrderedType) : sig
  type t

  (** [create size] create a bounded sequence of at most [size] elements.

      Raise [Invalid_argument] if [size < 0] or [size > Sys.max_array_length].
   *)
  val create : int -> t

  (** [insert e b] adds element [e] to bounded sequence [b] if:
      - [b] is not full (i.e, we have not inserted [size] elements until now); or
      - there is an element [e'] from [b] such that [E.compare e' e < 0]. 

      Worst-case complexity: O(log n) where n is the size of the heap.
   *)
  val insert : E.t -> t -> unit

  (** [get b] returns the contents of [b] as a sorted list in increasing order
     according to [E.compare].

     Worst-case complexity: O(n log n) where n is the size of the heap.
   *)
  val get : t -> E.t list

  (** [peek b] returns [Some e] if [b] is not empty where [e] is the smallest
     element in [b] according to [E.compare], [None] otherwise.

     Worst-case complexity: O(1).
   *)
  val peek : t -> E.t option
end
