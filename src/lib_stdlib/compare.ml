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

module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type t

  val ( = ) : t -> t -> bool

  val ( <> ) : t -> t -> bool

  val ( < ) : t -> t -> bool

  val ( <= ) : t -> t -> bool

  val ( >= ) : t -> t -> bool

  val ( > ) : t -> t -> bool

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val max : t -> t -> t

  val min : t -> t -> t
end

module Make (P : COMPARABLE) = struct
  include P

  let compare = compare

  let ( = ) a b = compare a b = 0

  let ( <> ) a b = compare a b <> 0

  let ( < ) a b = compare a b < 0

  let ( <= ) a b = compare a b <= 0

  let ( >= ) a b = compare a b >= 0

  let ( > ) a b = compare a b > 0

  let equal = ( = )

  let max x y = if x >= y then x else y

  let min x y = if x <= y then x else y
end

module List (P : COMPARABLE) = Make (struct
  type t = P.t list

  let rec compare xs ys =
    match (xs, ys) with
    | ([], []) -> 0
    | ([], _) -> -1
    | (_, []) -> 1
    | (x :: xs, y :: ys) ->
        let hd = P.compare x y in
        if hd <> 0 then hd else compare xs ys
end)

module Option (P : COMPARABLE) = Make (struct
  type t = P.t option

  let compare xs ys =
    match (xs, ys) with
    | (None, None) -> 0
    | (None, _) -> -1
    | (_, None) -> 1
    | (Some x, Some y) -> P.compare x y
end)

module Result (Ok : COMPARABLE) (Error : COMPARABLE) = Make (struct
  type t = (Ok.t, Error.t) result

  (* Note: [Ok _ < Error _] for compatibility with the Stdlib's polymorphic
           comparison. *)
  let compare ra rb =
    match (ra, rb) with
    | (Ok a, Ok b) -> Ok.compare a b
    | (Error a, Error b) -> Error.compare a b
    | (Ok _, Error _) -> -1
    | (Error _, Ok _) -> 1
end)

module Char = Make (Char)

module Bool = Make (struct
  type t = bool

  let compare = Stdlib.compare
end)

module Int = struct
  type t = int

  (**

     Integer comparisons are ubiquitous and must be efficiently
     compiled. The OCaml compiler provides the following builtin
     externals to make sure that aliases to integer comparisons are
     properly detected.

     Moving from the generic comparison, which ultimately calls the C
     function caml_compare, to these builtins divides execution time
     by one or two orders of magnitude.

     This optimization is especially important in critical routines
     like the gas update and the check for the gas exhaustion of the
     Michelson runtime.


     References:
     -----------
     - https://gitlab.com/-/snippets/2031392
     - https://tarides.com/blog/2019-09-13-decompress-experiences-with-ocaml-optimization
     - https://blog.janestreet.com/the-perils-of-polymorphic-compare/

  *)

  external ( = ) : int -> int -> bool = "%equal"

  external ( <> ) : int -> int -> bool = "%notequal"

  external ( < ) : int -> int -> bool = "%lessthan"

  external ( > ) : int -> int -> bool = "%greaterthan"

  external ( <= ) : int -> int -> bool = "%lessequal"

  external ( >= ) : int -> int -> bool = "%greaterequal"

  external compare : int -> int -> int = "%compare"

  external equal : int -> int -> bool = "%equal"

  let max x y = if x >= y then x else y

  let min x y = if x <= y then x else y
end

module Int32 = Make (Int32)
module Int64 = Make (Int64)

module MakeUnsigned
    (Int : S) (Z : sig
      val zero : Int.t
    end) =
struct
  type t = Int.t

  let compare va vb =
    Int.(
      if va >= Z.zero then if vb >= Z.zero then compare va vb else -1
      else if vb >= Z.zero then 1
      else compare va vb)

  let ( = ) = (( = ) : t -> t -> bool)

  let ( <> ) = (( <> ) : t -> t -> bool)

  let ( < ) a b =
    Int.(if Z.zero <= a then a < b || b < Z.zero else b < Z.zero && a < b)

  let ( <= ) a b =
    Int.(if Z.zero <= a then a <= b || b < Z.zero else b < Z.zero && a <= b)

  let ( >= ) a b = b <= a

  let ( > ) a b = b < a

  let equal = ( = )

  let max x y = if x >= y then x else y

  let min x y = if x <= y then x else y
end

module Uint32 =
  MakeUnsigned
    (Int32)
    (struct
      let zero = 0l
    end)

module Uint64 =
  MakeUnsigned
    (Int64)
    (struct
      let zero = 0L
    end)

module Float = Make (struct
  type t = float

  let compare = Stdlib.compare
end)

module String = Make (String)
module Bytes = Make (Bytes)

module Z = struct
  type t = Z.t

  include Z.Compare

  let compare = Z.compare

  let equal = Z.equal

  let max = Z.max

  let min = Z.min
end

module List_length_with = struct
  let ( = ) l i = Stdlib.List.compare_length_with l i = 0

  let ( <> ) l i = Stdlib.List.compare_length_with l i <> 0

  let ( < ) l i = Stdlib.List.compare_length_with l i < 0

  let ( <= ) l i = Stdlib.List.compare_length_with l i <= 0

  let ( >= ) l i = Stdlib.List.compare_length_with l i >= 0

  let ( > ) l i = Stdlib.List.compare_length_with l i > 0

  let compare l i = Stdlib.List.compare_length_with l i

  let equal = ( = )
end

module List_lengths = struct
  let ( = ) l1 l2 = Stdlib.List.compare_lengths l1 l2 = 0

  let ( <> ) l1 l2 = Stdlib.List.compare_lengths l1 l2 <> 0

  let ( < ) l1 l2 = Stdlib.List.compare_lengths l1 l2 < 0

  let ( <= ) l1 l2 = Stdlib.List.compare_lengths l1 l2 <= 0

  let ( >= ) l1 l2 = Stdlib.List.compare_lengths l1 l2 >= 0

  let ( > ) l1 l2 = Stdlib.List.compare_lengths l1 l2 > 0

  let compare l1 l2 = Stdlib.List.compare_lengths l1 l2

  let equal = ( = )
end

let or_else c f = if c <> 0 then c else f ()
