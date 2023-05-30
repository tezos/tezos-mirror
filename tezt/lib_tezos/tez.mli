(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Helpers for dealing with units of tez.

    Please note that none of the functions here perform any bounds checks. *)

(** A unit of tez *)
type t

(** Make [t] from the whole number of tez. This doesn't perform any bounds
    checks. *)
val of_int : int -> t

(** Make [t] from the whole number of micro tez. This doesn't perform any bounds
    checks. *)
val of_mutez_int : int -> t

(** Make [t] from the whole number of micro tez. This doesn't perform any bounds
    checks. *)
val of_mutez_int64 : int64 -> t

(** 0 tez *)
val zero : t

(** 1 tez *)
val one : t

(** Convert [t] to a string. *)
val to_string : t -> string

(** Convert [t] to a mutez integer. *)
val mutez_int64 : t -> int64

(** Convert [t] to a float of tez. *)
val to_float : t -> float

(** Convert [t] to an [int]. *)
val to_mutez : t -> int

(** Addition. This doesn't perform any bounds checks. *)
val ( + ) : t -> t -> t

(** Subtraction. This doesn't perform any bound checks. *)
val ( - ) : t -> t -> t

(** Division. This doesn't perform any bound checks.

    @raise Division_by_zero when the second operand is zero. *)
val ( /! ) : t -> int64 -> t

(** Parsing. Parse a floating point number of tez.

    Any string of digits followed by an optional point and another string
    of digits should parse successfully, provided that the expressed number
    is within bounds allowed for tez (up to 6 decimal places). For example:
    "123.4356" will parse, while
    "1.24723953794217492" won't, because it's too precise. *)
val parse_floating : string -> t

(** The tez {!Check.typ} *)
val typ : t Check.typ
