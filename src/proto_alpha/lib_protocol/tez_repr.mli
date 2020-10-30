(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t

type tez = t

include sig
    type qty

    type error +=
      | Addition_overflow of qty * qty (* `Temporary *)
      | Subtraction_underflow of qty * qty (* `Temporary *)
      | Multiplication_overflow of qty * int64 (* `Temporary *)
      | Negative_multiplicator of qty * int64 (* `Temporary *)
      | Invalid_divisor of qty * int64

    (* `Temporary *)

    val id : string

    val zero : qty

    val one_mutez : qty

    val one_cent : qty

    val fifty_cents : qty

    val one : qty

    val ( -? ) : qty -> qty -> qty tzresult

    val ( +? ) : qty -> qty -> qty tzresult

    val ( *? ) : qty -> int64 -> qty tzresult

    val ( /? ) : qty -> int64 -> qty tzresult

    val to_mutez : qty -> int64

    (** [of_mutez n] (micro tez) is None if n is negative *)
    val of_mutez : int64 -> qty option

    (** [of_mutez_exn n] fails if n is negative.
    It should only be used at toplevel for constants. *)
    val of_mutez_exn : int64 -> qty

    (** It should only be used at toplevel for constants. *)
    val add_exn : qty -> qty -> qty

    (** It should only be used at toplevel for constants. *)
    val mul_exn : qty -> int -> qty

    val encoding : qty Data_encoding.t

    val to_int64 : qty -> int64

    include Compare.S with type t := qty

    val pp : Format.formatter -> qty -> unit

    val of_string : string -> qty option

    val to_string : qty -> string
  end
  with type qty := t
