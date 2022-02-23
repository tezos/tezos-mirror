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

(** Assertions.

    This module defines predicates that call [Test.fail] when they do not hold.
    Using these predicates gives you more consistent error messages. *)

open Base

(** {2 Comparable Types} *)

(** Type descriptions.

    A [t typ] equips a type [t] with a pretty printer, and either an
    equality or a comparison function.

    All types can be used with [eq] and [neq], but some types will result
    in an exception when used with [lt], [le], [gt] and [ge].
    Such types are said to be non comparable. *)
type 'a typ

(** The unit type.

    Not very useful on its own, but may be useful inside more structured types.

    This type is comparable. *)
val unit : unit typ

(** The boolean type.

    Not very useful on its own since one can write [if not b then Test.fail ...],
    but may be useful inside more structured types.

    This type is comparable with [true > false]. *)
val bool : bool typ

(** The character type.

    This type is comparable. Characters are compared by comparing their
    byte representation. *)
val char : char typ

(** The integer type.

    This type is comparable. *)
val int : int typ

(** The 32-bit integer type.

    This type is comparable. *)
val int32 : int32 typ

(** The 64-bit integer type.

    This type is comparable. *)
val int64 : int64 typ

(** The float type.

    Note that testing float equality or inequality is often a bad idea.
    Prefer [float_epsilon] when it makes sense.

    This type is comparable. *)
val float : float typ

(** The float type with an approximative equality function.

    The argument is how much of a difference can two values have and still
    be considered equal.

    This type is comparable. *)
val float_epsilon : float -> float typ

(** The string type.

    This type is comparable in lexicographic order, where each character is
    compared using the same comparison function as [char]. *)
val string : string typ

(** Make an option type.

    If the item type is comparable, the result is comparable.
    [None] is always lesser than [Some _]. *)
val option : 'a typ -> 'a option typ

(** Make a list type.

    If the item type is comparable, the result is comparable in lexicographic order. *)
val list : 'a typ -> 'a list typ

(** Make an array type.

    If the item type is comparable, the result is comparable in lexicographic order. *)
val array : 'a typ -> 'a array typ

(** Make a 2-tuple type.

    If both item types are comparable, the result is comparable in lexicographic order. *)
val tuple2 : 'a typ -> 'b typ -> ('a * 'b) typ

(** Make a 3-tuple type.

    If all item types are comparable, the result is comparable in lexicographic order. *)
val tuple3 : 'a typ -> 'b typ -> 'c typ -> ('a * 'b * 'c) typ

(** Make a 4-tuple type.

    If all item types are comparable, the result is comparable in lexicographic order. *)
val tuple4 : 'a typ -> 'b typ -> 'c typ -> 'd typ -> ('a * 'b * 'c * 'd) typ

(** Make a 5-tuple type.

    If all item types are comparable, the result is comparable in lexicographic order. *)
val tuple5 :
  'a typ -> 'b typ -> 'c typ -> 'd typ -> 'e typ -> ('a * 'b * 'c * 'd * 'e) typ

(** Make a type by encoding to another.

    Usage: [convert encode typ]

    Example: [convert (fun { x; y } -> x, y) (tuple2 int string)]

    Values are converted to [typ] using [encode] before being pretty-printed
    or compared. The result type is comparable if [typ] is comparable. *)
val convert : ('a -> 'b) -> 'b typ -> 'a typ

(** Make a custom type from a pretty-printer and an equality function.

    The result is not comparable. *)
val equalable : (Format.formatter -> 'a -> unit) -> ('a -> 'a -> bool) -> 'a typ

(** Make a custom type from a pretty-printer and a comparison function. *)
val comparable : (Format.formatter -> 'a -> unit) -> ('a -> 'a -> int) -> 'a typ

module type EQUALABLE = sig
  type t

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end

(** Same as {!equalable} but takes a module.

    Example: [equalable_module (module String)]. *)
val equalable_module : (module EQUALABLE with type t = 'a) -> 'a typ

module type COMPARABLE = sig
  type t

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

(** Same as {!comparable} but takes a module.

    Example: [comparable_module (module String)]. *)
val comparable_module : (module COMPARABLE with type t = 'a) -> 'a typ

(** {2 Predicates} *)

(** For all functions of this section, if the test fails, the function calls
    [Test.fail] with the error message given in [~error_msg].
    This [~error_msg] may contain placeholders [%L] and [%R] where:
    - [%L] is replaced by the left-hand side value of the operator;
    - [%R] is replaced by the right-hand side value of the operator.

    Here are some examples of [~error_msg] for inspiration:
    - ["expected filename = %R, got %L"]
    - ["expected list size >= %R, got %L"]
    - ["expected f to be monotonous, got f x = %L and f y = %R with x < y"] *)

(** Check that a value is equal to another.

    Example: [Check.((value = expected) int ~error_msg:"expected value = %R, got %L")] *)
val ( = ) : 'a -> 'a -> 'a typ -> error_msg:string -> unit

(** Check that a value is not equal to another.

    Example: [Check.((value <> wrong) int ~error_msg:"expected value <> %R")] *)
val ( <> ) : 'a -> 'a -> 'a typ -> error_msg:string -> unit

(** Check that a value is less than another.

    Example: [Check.((value < threshold) int ~error_msg:"expected value < %R, got %L")] *)
val ( < ) : 'a -> 'a -> 'a typ -> error_msg:string -> unit

(** Check that a value is less than or equal to another.

    Example: [Check.((value <= threshold) int ~error_msg:"expected value <= %R, got %L")] *)
val ( <= ) : 'a -> 'a -> 'a typ -> error_msg:string -> unit

(** Check that a value is greater than another.

    Example: [Check.((value > threshold) int ~error_msg:"expected value > %R, got %L")] *)
val ( > ) : 'a -> 'a -> 'a typ -> error_msg:string -> unit

(** Check that a value is greater than or equal to another.

    Example: [Check.((value >= threshold) int ~error_msg:"expected value >= %R, got %L")] *)
val ( >= ) : 'a -> 'a -> 'a typ -> error_msg:string -> unit

(** Check that a string matches a regular expression.

    Example: [Check.((value =~ rex) ~error_msg:"expected value =~ %R, got %L")] *)
val ( =~ ) : string -> rex -> error_msg:string -> unit

(** Check that a string does not match a regular expression.

    Example: [Check.((value =~! rex) ~error_msg:"expected value =~! %R, got %L")] *)
val ( =~! ) : string -> rex -> error_msg:string -> unit

(** Check that a value belongs to a list.

    Example: [Check.list_mem ~__LOC__ int i list int ~error_msg:"expected %L to be in the
    list")] * *)
val list_mem :
  'a typ -> ?__LOC__:string -> 'a -> 'a list -> error_msg:string -> unit

(** Check that a value does not belong to a list.

    Example: [Check.list_not_mem ~__LOC__ int i list int ~error_msg:"expected %L to not
    be in the list")] * *)
val list_not_mem :
  'a typ -> ?__LOC__:string -> 'a -> 'a list -> error_msg:string -> unit
