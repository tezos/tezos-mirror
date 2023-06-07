(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(* Cost models are expressed in a DSL. Terms of this DSL are used in several
   ways:
   1. They are _evaluated_ when performing parameter inference
   2. They are _displayed_ when producing reports
   3. OCaml code is _generated_ from these terms after substituting the
      results of inference and performing the appropriate unit conversions. *)

(* Signature of the DSL *)

module type S = sig
  type 'a repr

  type size

  val true_ : bool repr

  val false_ : bool repr

  val int : int -> size repr

  val float : float -> size repr

  val ( + ) : size repr -> size repr -> size repr

  val sat_sub : size repr -> size repr -> size repr

  val ( * ) : size repr -> size repr -> size repr

  val ( / ) : size repr -> size repr -> size repr

  val max : size repr -> size repr -> size repr

  val min : size repr -> size repr -> size repr

  val log2 : size repr -> size repr

  val sqrt : size repr -> size repr

  val free : name:Free_variable.t -> size repr

  val lt : size repr -> size repr -> bool repr

  val eq : size repr -> size repr -> bool repr

  val shift_left : size repr -> int -> size repr

  val shift_right : size repr -> int -> size repr

  val lam : name:string -> ('a repr -> 'b repr) -> ('a -> 'b) repr

  val app : ('a -> 'b) repr -> 'a repr -> 'b repr

  val let_ : name:string -> 'a repr -> ('a repr -> 'b repr) -> 'b repr

  val if_ : bool repr -> 'a repr -> 'a repr -> 'a repr
end

(* ------------------------------------------------------------------------- *)
(* Implementations of the signature above. *)

(* Destroys the model into unit *)
module Void : S with type 'a repr = unit and type size = unit

(* Pretty-printing implementation *)
module Pp : S with type 'a repr = string and type size = string

(* Extracting free variables. *)
module Free_variables :
  S with type 'a repr = Free_variable.Set.t and type size = unit

(* Evaluating implementation for closed terms.  Fails if free variables are
   present. Use the [Subst] implementation transformer to get rid of free
   variables. *)
module Eval : S with type 'a repr = 'a and type size = float

(* Evaluating implementation.
   Expects terms to be linear combinations with free variables as coefficients.
   Fails otherwise. Takes a substitution as a parameter. *)
type affine = {linear_comb : Free_variable.Sparse_vec.t; const : float}

type subst = Free_variable.t -> float option

exception Eval_linear_combination of string

module Eval_linear_combination_impl : sig
  include S

  val run : subst -> size repr -> affine
end

(* ------------------------------------------------------------------------- *)
(* Implementation _transformers_. *)

(* Type-preserving code transformation *)
module type Transform = functor (X : S) -> sig
  include S with type size = X.size

  val prj : 'a repr -> 'a X.repr
end

type transform = (module Transform)

(* compose f g = g o f *)
val compose : transform -> transform -> transform

module Identity : Transform

(* [Subst] allows to subtitute free variables by constants. *)
module Subst (P : sig
  val subst : Free_variable.t -> float
end) : Transform

(* [Hash_cons] ensures maximal sharing for a subset of the language. *)
type 'a hash_consed = {repr : 'a; hash : int; tag : int}

module Hash_cons : Transform

(* [Beta_normalize] evaluates beta-redexes. *)
module Beta_normalize : Transform

(* Lift let-bindings out of subexpressions. Warning: this transformation
   does not check that the ~name arguments (used for pretty printing)
   are globally distinct for let bindings. *)
module Let_lift : Transform

(* Partially evaluating & hash-consing interpretation of the DSL *)
module Hash_cons_vector : sig
  include S with type size = Eval_linear_combination_impl.size

  val prj : 'a repr -> 'a Eval_linear_combination_impl.repr
end

module Eval_to_vector : sig
  include S with type size = Hash_cons_vector.size

  val prj : 'a repr -> 'a Hash_cons_vector.repr
end

module Fold_constants : Transform
