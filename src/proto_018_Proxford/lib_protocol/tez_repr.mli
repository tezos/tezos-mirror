(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Internal representation of the Tez currency. Behaves mostly like a natural
   number where number 1 represents 1/1,000,000 Tez (1 micro-Tez or mutez).
   It's protected from ever becoming negative and overflowing by special
   arithmetic functions, which fail in case something undesired would happen.
   When divided, it's always rounded down to 1 mutez.

   Internally encoded as [int64], which may be relevant to guard against
   overflow errors. *)
type repr

(** [t] is made algebraic in order to distinguish it from the other type
    parameters of [Script_typed_ir.ty]. *)
type t = Tez_tag of repr [@@ocaml.unboxed]

type error += Subtraction_underflow of t * t (* `Temporary *)

type tez = t

val zero : t

val one_mutez : t

val one_cent : t

val fifty_cents : t

val one : t

val max_mutez : t

val ( -? ) : t -> t -> t tzresult

(** Same as ( -? ) but returns None instead of an error. *)
val sub_opt : t -> t -> t option

val ( +? ) : t -> t -> t tzresult

val ( *? ) : t -> int64 -> t tzresult

val ( /? ) : t -> int64 -> t tzresult

val div2 : t -> t

(** [div2_sub tez] returns [(⌊tez / 2⌋, tez - ⌊tez / 2⌋)]. *)
val div2_sub : t -> t * t

(** [mul_ratio tez ~num ~den] returns [tez * num / den] without failing
    when [tez * num] overflows. *)
val mul_ratio : t -> num:int64 -> den:int64 -> t tzresult

val to_mutez : t -> int64

(** [of_mutez n] (micro tez) is None if n is negative *)
val of_mutez : int64 -> t option

(** [of_mutez_exn n] fails if n is negative.
    It should only be used at toplevel for constants. *)
val of_mutez_exn : int64 -> t

(** It should only be used at toplevel for constants. *)
val mul_exn : t -> int -> t

(** It should only be used at toplevel for constants. *)
val div_exn : t -> int -> t

val encoding : t Data_encoding.t

include Compare.S with type t := t

val pp : Format.formatter -> t -> unit

val of_string : string -> t option

val to_string : t -> string
