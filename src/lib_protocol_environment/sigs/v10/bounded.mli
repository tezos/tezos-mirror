(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Bounds.

   Formally each [B : BOUND] represents the interval of all values
   between [B.min_value] and [B.max_value]. This is a closed interval,
   i.e.  the endpoints are included.

   Intervals can be empty, for example [struct let min_value = 1; let
   max_value 0 end] is empty. *)
module type BOUNDS = sig
  (** [ocaml_type] is the type used for the internal representation of
     values within the bounded interval.  This is the type that values
     in the interval are converted to and from. E.g., for an interval
     of 32-bit integers [ocaml_type = int32]. *)
  type ocaml_type

  (** [min_value] represents the minimal value (included) reprensatable. *)
  val min_value : ocaml_type

  (** [max_value] represents the maximal value (included)
     reprensatable. *)
  val max_value : ocaml_type
end

(** Signature for an interval of (included values) with an encoding
   and projection functions towards the underlying ocaml datatype. *)
module type S = sig
  (** Internal representation of a bounded value. *)
  type t

  (** Underlying OCaml representation for the bounded value. *)
  type ocaml_type

  include BOUNDS with type ocaml_type := ocaml_type

  include Compare.S with type t := t

  (** A (partial) encoding of the datatype. If the encoded value is
     out of bounds, an exception may be raised. See
     {!val:Data_encoding.conv_with_guard}. *)
  val encoding : t Data_encoding.t

  (** A pretty-printer for values of type [t]. *)
  val pp : Format.formatter -> t -> unit

  (** [to_value t] is a projection to the OCaml representation of the
     bounded value [t]. *)
  val to_value : t -> ocaml_type

  (** [of_value ocaml_value] represents [ocaml_value] as a bounded
     value. Returns [None] if the value is outside of the bounds
     specified by {!val:min_value} and {!val:max_value}. *)
  val of_value : ocaml_type -> t option
end

(** Allows to build interval of int64 integers. The encoding used is
   {!val:Data_encoding.int64} regardless of the actual bounds. *)
module Int64 (B : BOUNDS with type ocaml_type := int64) :
  S with type ocaml_type := int64

(** Allows to build interval of int32 integers. The encoding used is
   {!val:Data_encoding.int32} regardless of the actual bounds. *)
module Int32 (B : BOUNDS with type ocaml_type := int32) :
  S with type ocaml_type := int32

(** Allows to build interval of non negative int32 integers. The
   encoding used is {!val:Data_encoding.int32} regardless of the
   actual bounds. *)
module Non_negative_int32 : S with type ocaml_type := int32

(** Allows to build interval of built-in OCaml int integers. The
   encoding used is {!val:Data_encoding.int31} regardless of the
   actual bounds.

   @raise Invalid_argument if the bounds provided cannot be
   representable on 4 bytes (depends on whether [int] is represented
   on 4 bytes or 8 bytes which depends on the machine architecture)..
   *)
module Int31 (B : BOUNDS with type ocaml_type := int) :
  S with type ocaml_type := int

(** Allows to build interval of int integers representable on 2
   bytes. The encoding used is {!val:Data_encoding.int16} regardless
   of the actual bounds.

   @raise Invalid_argument if the bounds provided cannot be
   representable on 2 bytes.  *)
module Int16 (B : BOUNDS with type ocaml_type := int) :
  S with type ocaml_type := int

(** Allows to build interval of non-negative int integers
   representable on 2 bytes. The encoding used is
   {!val:Data_encoding.uint16} regardless of the actual bounds.

   @raise Invalid_argument if the bounds provided cannot be
   representable on 2 bytes. *)
module Uint16 (B : BOUNDS with type ocaml_type := int) :
  S with type ocaml_type := int

(** Allows to build interval of non-negative int integers
   representable on 1 bytes. The encoding used is
   {!val:Data_encoding.int8} regardless of the actual bounds.

   @raise Invalid_argument if the bounds provided cannot be
   representable on 1 bytes. *)
module Int8 (B : BOUNDS with type ocaml_type := int) :
  S with type ocaml_type := int

(** Allows to build interval of non-negative int integers
   representable on 1 bytes. The encoding used is
   {!val:Data_encoding.uint8} regardless of the actual bounds.

   @raise Invalid_argument if the bounds provided cannot be
   representable on 1 bytes. *)
module Uint8 (B : BOUNDS with type ocaml_type := int) :
  S with type ocaml_type := int
