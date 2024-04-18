(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(**

   This module provides support for type equalities and runtime type identifiers.

   For two types [a] and [b], [(a, b) eq] is a witness that [a = b]. This is
   a standard generalized algebraic datatype on top of which type-level
   programming techniques can be implemented.

   Given a type [a], an inhabitant of [a t] is a dynamic identifier for [a].
   Identifiers can be compared for equality. They are also equipped with a
   hash function.

   WARNING: the hash function changes at every run. Therefore, the result
   of the hash function should never be stored.

   Notice that dynamic identifiers are not unique: two identifiers for [a]
   can have distinct hash and can be physically distinct. Hence, only [eq]
   can decide if two type identifiers correspond to the same type.

*)

(** A proof witness that two types are equal. *)
type (_, _) eq = Refl : ('a, 'a) eq

(** A dynamic representation for ['a]. *)
type 'a t

(** [make ()] is a dynamic representation for ['a]. A fresh identifier
   is returned each time [make ()] is evaluated. *)
val make : unit -> 'a t

(** [eq ida idb] returns a proof that [a = b] if [ida] and [idb]
   identify the same type. *)
val eq : 'a t -> 'b t -> ('a, 'b) eq option

(** [hash id] returns a hash for [id]. *)
val hash : 'a t -> int
