(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** This module implements bounded (or refined) versions of data types. *)

(** Bounded [int32]. *)
module Int32 : sig
  (** Bounds.

      Formally each [B : BOUND] represents the interval of all integers
      between [B.min_int] and [B.max_int]. This is a closed interval, e.g.
      the endpoints are included.

      Intervals can be empty, for example [struct let min_int = 1; let max_int
      0 end] is empty.
   *)
  module type BOUNDS = sig
    val min_int : int32

    val max_int : int32
  end

  module type S = sig
    type t

    include BOUNDS

    include Compare.S with type t := t

    val encoding : t Data_encoding.t

    val to_int32 : t -> int32

    val of_int32 : int32 -> t option
  end

  (** Produce a module [_ : S] of bounded integers.

      If the given interval is empty, [S.t] is the empty type, and [of_int32]
      returns [Error] for all inputs.

      {4 Encoding}
      [(Make B).encoding] is based on the underlying [int32] encoding. This
      allow future compatiblity with larger bounds, at the price of addding 1-3
      redundant bytes to each message.
   *)
  module Make (_ : BOUNDS) : S
end
