(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** module [Assert] contains Alcotest convenience assertions. *)

(** {1 Type definitions} *)

(** An assertion that a relation between two elements of ['ty] holds true.
   This assertion returns a value of type ['res] when it succeeds, typically
   [unit], as in {!type:check2}. **)
type ('ty, 'res) t = ?loc:string -> ?msg:string -> 'ty -> 'ty -> 'res

(** Alias for {!type:t} used in {!module-type:EQUALITIES}. *)
type ('ty, 'res) assertion = ('ty, 'res) t

(** A ['a check2] is an assertion over two elements of type ['a] returning unit when it holds. *)
type 'ty check2 = ('ty, unit) t

(** A ['a check1] defines the check of a property of a single element of type
    ['a].

    See {!val:is_none} or {!val:is_true} for examples. *)
type 'ty check1 = ?loc:string -> ?msg:string -> 'ty -> unit

(** An [equality_check] is an assertion that the boolean relation defined by
   [eq] holds on two series -- of type ['container_ty] -- of elements of
   ['base_ty],

   This typically defines the type of assertions on lists (see
   {!val:equal_list}), or arrays, but it is also used to defined new checks
   based on an equality function. *)
type ('base_ty, 'container_ty) equality_check =
  ?eq:('base_ty -> 'base_ty -> bool) ->
  ?pp:(Format.formatter -> 'base_ty -> unit) ->
  'container_ty check2

(** A [comparison_check] is similar to a {!type:equality_check} except the
    assertion is based on the optional comparison function [cmp]. *)
type ('base_ty, 'container_ty) comparison_check =
  ?cmp:('base_ty -> 'base_ty -> int) ->
  ?pp:(Format.formatter -> 'base_ty -> unit) ->
  'container_ty check2

(** {1 Base definitions }*)

(** {2 Failures } *)

(** [fail ?loc ?msg pp x y] fails after printing message
    "<loc> <msg>: expected <x> got <y>".

    - when [msg] is omitted "<msg:> " is not printed
    - when [loc] is omitted "<loc> " is not printed
 *)
val fail :
  ?loc:string ->
  ?msg:string ->
  (Format.formatter -> 'a -> unit) ->
  'a ->
  'a ->
  'b

(** [fail_msg fmt] fails after pretty-printing [fmt] *)
val fail_msg : ('a, Format.formatter, unit, 'b) format4 -> 'a

(** {2 Assertions based on (un)equalities} *)

(** [equal ?eq ?pp ?msg ?loc a b] checks that [eq a b] and if not calls [fail pp a b
    msg loc]

    @param msg is not set by default
    @param loc is not set by default
    @param eq defaults to [Stdlib.( = )]
    @param pp is the pretty-printer for type ['a], defaults to printing nothing
    @raise Failure when [eq a b] is [false]
 *)
val equal : ('a, 'a) equality_check

(** [unequal ?eq ?pp ?msg ?loc a b] checks that [eq a b] does not hold.
    See {!val:equal} for the rest of the documentation. *)
val unequal : ('a, 'a) equality_check

(** [equal_list ?eq ?pp ?msg ?loc l1 l2 ] is like {!val:equal} but for
    polymorphic lists ['a list].

    @param msg is not set by default
    @param loc is not set by default
    @param eq defaults to [Stdlib.( = )]
    @param pp is the pretty-printer for type ['a], defaults to printing nothing
    @raise Failure when [List.equal eq a b] is [false]
*)
val equal_list : ('a, 'a list) equality_check

(** [equal_list_list] is like {!val:equal} but for polymorphic lists of lists ['a
    list list].

    @param msg is not set by default
    @param loc is not set by default
    @param eq defaults to [Stdlib.( = )]
    @param pp is the pretty-printer for type ['a], defaults to printing nothing
    @raise Failure when [List.equal (List.equal eq) a b] is [false]
*)
val equal_list_list : ('a, 'a list list) equality_check

(** {2 Assertions based on comparisons} *)

(** [leq ?cmp ?pp ?loc ?msg a b] checks that [cmp a b <= 0].

    @param msg is not set by default
    @param loc is not set by default
    @param cmd defaults to [Stdlib.compare]
    @param pp is the pretty-printer for type ['a], defaults to printing nothing
    @raise Failure when [cmp a b > 0]
*)
val leq : ('a, 'a) comparison_check

(** [lt ?cmp ?pp ?loc ?msg a b] checks that [cmp a b < 0].

    See {!val:leq} for details of the parameters.
    @raise Failure when [cmp a b >= 0]*)
val lt : ('a, 'a) comparison_check

(** [geq ?cmp ?pp ?loc ?msg a b] checks that [cmp a b >= 0].

    See {!val:leq} for details of the parameters.
    @raise Failure when [cmp a b < 0]
 *)
val geq : ('a, 'a) comparison_check

(** [gt ?cmp ?pp ?loc ?msg a b] checks that [cmp a b > 0].

    See {!val:leq} for details of the parameters.
    @raise Failure when [cmp a b <= 0]
 *)
val gt : ('a, 'a) comparison_check

(** {2 Miscellaneous assertions} *)

(** Alcotest check that [b] is [true]. *)
val assert_true : string -> bool -> unit

(** Alcotest check that [b] is [false]. *)
val assert_false : string -> bool -> unit

(** Alcotest version of [assert false]. *)
val impossible : string -> unit

(** Assert that at least one value in [l] satisfies [f]. *)
val check_any : ?msg:string -> ('a -> bool) -> 'a list -> unit

(** [contains m msg x ls] asserts that one testable in [ls] equals
    [x], and otherwise fails with [msg] *)

val contains : 'a Alcotest.testable -> string -> 'a -> 'a list -> unit

(** {1 Monomorphic equality checks}*)

(** Check that an option value is [None].

    @param msg is not set by default
    @param loc is not set by default
    @param eq defaults to [Stdlib.( = )]
    @param pp is the pretty-printer for type ['a], defaults to printing "Some _" as a
    placeholder for non-[None] value.
 *)
val is_none :
  ?loc:string ->
  ?msg:string ->
  ?pp:(Format.formatter -> 'a -> unit) ->
  'a option ->
  unit

(** {3 Module type signatures} *)

(** Assertions for equality/disequality *)
module type EQUALITIES = sig
  type t

  val equal : t check2

  val unequal : t check2

  val fail : (t, 'a) assertion

  module Option : sig
    val equal : t option check2
  end

  module List : sig
    val equal : t list check2
  end

  module Array : sig
    val equal : t array check2
  end

  module List_list : sig
    val equal : t list list check2
  end
end

(** Checks for extra binary relations *)
module type COMPARISONS = sig
  include EQUALITIES

  val leq : t check2

  val lt : t check2

  val geq : t check2

  val gt : t check2
end

(** {3 Predefined modules for base types} *)

module Bytes : COMPARISONS with type t = bytes

module Bool : COMPARISONS with type t = bool

val is_true : bool check1

val is_false : bool check1

module String : sig
  include COMPARISONS with type t = string

  val is_empty : t check1
end

module Int32 : sig
  include COMPARISONS with type t = int32

  val is_zero : t check1
end

module Int64 : sig
  include COMPARISONS with type t = int64

  val is_zero : t check1
end

module Int : sig
  include COMPARISONS with type t = int

  val is_zero : t check1
end

(** {3 Functors} *)

module type COMPARABLE = sig
  type t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end

module type EQUALABLE = sig
  type t

  val pp : Format.formatter -> t -> unit

  val eq : t -> t -> bool
end

(** Provide equality-based assertions for type [X.t] *)
module Make_equalities : functor (X : EQUALABLE) -> EQUALITIES with type t = X.t

(** Provide all comparisons assertions for type [X.t] *)
module Make_comparisons : functor (X : COMPARABLE) ->
  COMPARISONS with type t = X.t
