(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Errors and their pretty-printing function *)
type inference_error

exception Ill_typed_script of inference_error

val pp_inference_error : Format.formatter -> inference_error -> unit

(** Comparability tag. *)
type comparability = Comparable | Not_comparable | Unconstrained

(** Michelson types. *)
type michelson_type =
  | Base_type of {repr : Type.Base.t option; comparable : comparability}
  | Stack_type of Type.Stack.t option

type transformer = {bef : Type.Stack.t; aft : Type.Stack.t}

(** State of the type inference module *)

(** Store implementation for type representatives *)
module Repr_store : Stores.S with type key = int and type value = michelson_type

(** State monad built on [Repr_store] *)
module Repr_sm :
  Monads.State_sig
    with type state = Repr_store.state
     and type key = int
     and type value = michelson_type

(** Store implementation for instruction type representatives *)
module Annot_instr_store :
  Stores.S with type key = Mikhailsky.Path.t and type value = transformer

(** State monad handling annotations on instructions *)
module Annot_instr_sm :
  Monads.State_sig
    with type state = Annot_instr_store.state
     and type value = transformer
     and type key = Mikhailsky.Path.t

(** Store implementation for data type representatives *)
module Annot_data_store :
  Stores.S with type key = Mikhailsky.Path.t and type value = Type.Base.t

(** State monad handling annotations on data *)
module Annot_data_sm :
  Monads.State_sig
    with type state = Annot_data_store.state
     and type value = Type.Base.t
     and type key = Mikhailsky.Path.t

(** State of the inference module *)
type state = {
  uf : Uf.UF.M.state;
  repr : Repr_sm.state;
  annot_instr : Annot_instr_sm.state;
  annot_data : Annot_data_sm.state;
}

(** State monad of the inference module. *)
module M : sig
  type 'a t = state -> 'a * state

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val empty : unit -> state

  val return : 'a -> 'a t

  val set_repr : int -> michelson_type -> unit t

  val get_repr_exn : int -> michelson_type t

  val get_instr_annot : Annot_data_sm.key -> transformer option t

  val get_data_annot : Annot_data_sm.key -> Type.Base.t option t

  val uf_lift : 'a Uf.UF.M.t -> 'a t

  val repr_lift : 'a Repr_sm.t -> 'a t

  val annot_instr_lift : 'a Annot_instr_sm.t -> 'a t

  val annot_data_lift : 'a Annot_data_sm.t -> 'a t

  val get_state : state t
end

(** Unifies two stack types. *)
val unify : Type.Stack.t -> Type.Stack.t -> unit M.t

(** Unifies two base types. *)
val unify_base : Type.Base.t -> Type.Base.t -> unit M.t

(** Instantiate type variables with the associated terms in a base type. *)
val instantiate_base : Type.Base.t -> Type.Base.t M.t

(** Instantiate type variables with the associated terms in a stack type. *)
val instantiate : Type.Stack.t -> Type.Stack.t M.t

(** Get comparability flag for a base type. *)
val get_comparability : Type.Base.t -> comparability M.t

(** Performs inference on the given Mikhailsky term and returns
    its type (as a pair of [before] and [after] stack) as well as the
    inference engine state. *)
val infer_with_state : Mikhailsky.node -> (Type.Stack.t * Type.Stack.t) * state

(** Performs inference on the given Mikhailsky term and throws
    the inference engine state away. *)
val infer : Mikhailsky.node -> Type.Stack.t * Type.Stack.t

(** Performs inference on a piece of Mikhailsky [data] and
    returns the inference engine state along the inferred type. *)
val infer_data_with_state : Mikhailsky.node -> Type.Base.t * state

(** Performs inference on a piece of Mikhailsky [data] and
    returns the inferred type, throwing the inference engine
    state away. *)
val infer_data : Mikhailsky.node -> Type.Base.t
