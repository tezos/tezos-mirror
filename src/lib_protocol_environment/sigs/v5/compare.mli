(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** {1 [Compare]}

    Monomorphic comparison for common ground types and common type constructors.

    [Compare] provides a module signature for the standard comparison functions
    and operators as well as modules of that signature for the common OCaml
    ground types ([int], [bool], etc.) and type constructors ([list], [option],
    etc.).

    [Compare] also provides some additional helpers for comparison-related
    tasks. *)

(** {2 Signatures and a functor} *)

(** [COMPARABLE] is a signature for basic comparison. It is used only for
    instantiating full comparison modules of signature {!module-type-S} via the
    functor {!Make}. *)
module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

(** [S] is a signature for a fully-fledge comparison module. It includes all the
    functions and operators derived from a [compare] function. *)
module type S = sig
  type t

  (** [x = y] iff [compare x y = 0] *)
  val ( = ) : t -> t -> bool

  (** [x <> y] iff [compare x y <> 0] *)
  val ( <> ) : t -> t -> bool

  (** [x < y] iff [compare x y < 0] *)
  val ( < ) : t -> t -> bool

  (** [x <= y] iff [compare x y <= 0] *)
  val ( <= ) : t -> t -> bool

  (** [x >= y] iff [compare x y >= 0] *)
  val ( >= ) : t -> t -> bool

  (** [x > y] iff [compare x y > 0] *)
  val ( > ) : t -> t -> bool

  (** [compare] an alias for the functor parameter's [compare] function *)
  val compare : t -> t -> int

  (** [equal x y] iff [compare x y = 0] *)
  val equal : t -> t -> bool

  (** [max x y] is [x] if [x >= y] otherwise it is [y] *)
  val max : t -> t -> t

  (** [min x y] is [x] if [x <= y] otherwise it is [y] *)
  val min : t -> t -> t
end

module Make (P : COMPARABLE) : S with type t := P.t

(** {2 Base types}

    The specialised comparison and all the specialised functions and operators
    on the base types are compatible with the polymorphic comparison and all the
    polymorphic functions and operators from the {!Stdlib}. *)

module Char : S with type t = char

module Bool : S with type t = bool

(** [Int] is a comparison module. Out of performance concerns, the signature
    actually contains compiler builtins ([external]) rather than [val]. *)
module Int : sig
  type t = int

  external ( = ) : int -> int -> bool = "%equal"

  external ( <> ) : int -> int -> bool = "%notequal"

  external ( < ) : int -> int -> bool = "%lessthan"

  external ( > ) : int -> int -> bool = "%greaterthan"

  external ( <= ) : int -> int -> bool = "%lessequal"

  external ( >= ) : int -> int -> bool = "%greaterequal"

  external compare : int -> int -> int = "%compare"

  val max : int -> int -> int

  val min : int -> int -> int

  external equal : int -> int -> bool = "%equal"
end

module Int32 : S with type t = int32

module Uint32 : S with type t = int32

module Int64 : S with type t = int64

module Uint64 : S with type t = int64

module String : S with type t = string

module Bytes : S with type t = bytes

(** [Z] is a comparison module for Zarith numbers. *)
module Z : S with type t = Z.t

(** {2 Type constructors}

    Provided the functor argument(s) are compatible with the polymorphic
    comparison of the {!Stdlib}, then the specialised comparison and all the
    specialised functions and operators on the derived types are compatible with
    the polymorphic comparison and all the polymorphic functions and operators
    from the {!Stdlib}. *)

module List (P : COMPARABLE) : S with type t = P.t list

module Option (P : COMPARABLE) : S with type t = P.t option

module Result (Ok : COMPARABLE) (Error : COMPARABLE) :
  S with type t = (Ok.t, Error.t) result

(** {2 List lengths}

    Helpers for more readable {!Stdlib.List.compare_lengths} and
    {!Stdlib.List.compare_length_with}.

    These modules are intended to be used as [Module.(expression)], most often
    within an [if] condition. E.g.,

{[
if Compare.List_length_with.(chunks > max_number_of_chunks) then
   raise Maximum_size_exceeeded
else
   ..
]}
    *)

module List_length_with : sig
  (** [Compare.List_length_with.(l = n)] iff [l] is of length [n]. In other
      words iff [Stdlib.List.compare_length_with l n = 0]. Note that, like
      [compare_length_with], this comparison does not explore the list [l]
      beyond its [n]-th element. *)
  val ( = ) : 'a list -> int -> bool

  (** [Compare.List_length_with.(l <> n)] iff [l] is not of length [n]. In other
      words iff [Stdlib.List.compare_length_with l n <> 0]. Note that, like
      [compare_length_with], this comparison does not explore the list [l]
      beyond its [n]-th element. *)
  val ( <> ) : 'a list -> int -> bool

  (** [Compare.List_length_with.(l < n)] iff [l] is of length strictly less than
      [n]. In other words iff [Stdlib.List.compare_length_with l n < 0]. Note
      that, like [compare_length_with], this comparison does not explore the
      list [l] beyond its [n]-th element. *)
  val ( < ) : 'a list -> int -> bool

  (** [Compare.List_length_with.(l <= n)] iff [l] is of length less than [n]. In
      other words iff [Stdlib.List.compare_length_with l n <= 0]. Note that,
      like [compare_length_with], this comparison does not explore the list [l]
      beyond its [n]-th element. *)
  val ( <= ) : 'a list -> int -> bool

  (** [Compare.List_length_with.(l >= n)] iff [l] is of length greater than [n].
      In other words iff [Stdlib.List.compare_length_with l n >= 0]. Note that,
      like [compare_length_with], this comparison does not explore the list [l]
      beyond its [n]-th element. *)
  val ( >= ) : 'a list -> int -> bool

  (** [Compare.List_length_with.(l > n)] iff [l] is of length strictly greater
      than [n]. In other words iff [Stdlib.List.compare_length_with l n > 0].
      Note that, like [compare_length_with], this comparison does not explore
      the list [l] beyond its [n]-th element. *)
  val ( > ) : 'a list -> int -> bool

  (** [Compare.List_length_with.compare] is an alias for
      [Stdlib.List.compare_length_with]. *)
  val compare : 'a list -> int -> int

  (** [Compare.List_length_with.equal] is an alias for
      [Compare.List_length_with.( = )]. *)
  val equal : 'a list -> int -> bool
end

module List_lengths : sig
  (** [Compare.List_lengths.(xs = ys)] iff [xs] and [ys] have the same length.
      In other words, iff [Stdlib.List.compare_lengths xs ys = 0]. Note that,
      like [compare_lengths], this comparison only explores the lists up to the
      length of the shortest one. *)
  val ( = ) : 'a list -> 'b list -> bool

  (** [Compare.List_lengths.(xs <> ys)] iff [xs] and [ys] have different
      lengths. In other words, iff [Stdlib.List.compare_lengths xs ys <> 0].
      Note that, like [compare_lengths], this comparison only explores the lists
      up to the length of the shortest one. *)
  val ( <> ) : 'a list -> 'b list -> bool

  (** [Compare.List_lengths.(xs < ys)] iff [xs] is strictly shorter than [ys].
      In other words, iff [Stdlib.List.compare_lengths xs ys < 0]. Note that,
      like [compare_lengths], this comparison only explores the lists up to the
      length of the shortest one. *)
  val ( < ) : 'a list -> 'b list -> bool

  (** [Compare.List_lengths.(xs <= ys)] iff [xs] is shorter than [ys].
      In other words, iff [Stdlib.List.compare_lengths xs ys <= 0]. Note that,
      like [compare_lengths], this comparison only explores the lists up to the
      length of the shortest one. *)
  val ( <= ) : 'a list -> 'b list -> bool

  (** [Compare.List_lengths.(xs >= ys)] iff [xs] is longer than [ys].
      In other words, iff [Stdlib.List.compare_lengths xs ys >= 0]. Note that,
      like [compare_lengths], this comparison only explores the lists up to the
      length of the shortest one. *)
  val ( >= ) : 'a list -> 'b list -> bool

  (** [Compare.List_lengths.(xs > ys)] iff [xs] is strictly longer than [ys].
      In other words, iff [Stdlib.List.compare_lengths xs ys > 0]. Note that,
      like [compare_lengths], this comparison only explores the lists up to the
      length of the shortest one. *)
  val ( > ) : 'a list -> 'b list -> bool

  (** [Compare.List_lengths.compare] is an alias for
      [Stdlib.List.compare_lengths]. *)
  val compare : 'a list -> 'b list -> int

  (** [Compare.List_lengths.equal] is an alias for
      [Compare.List_lengths.( = )]. *)
  val equal : 'a list -> 'b list -> bool
end

(** {2 Building blocks} *)

(** [or_else c f] is [c] if [c <> 0] or [f ()] otherwise.

    The intended use is
{[
let compare (foo_a, bar_a) (foo_b, bar_b) =
  or_else (Foo.compare foo_a foo_b) (fun () -> Bar.compare bar_a bar_b)
]}
*)
val or_else : int -> (unit -> int) -> int
