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

(** Specifying model arity and eliminator type *)
type (_, _, _) arity =
  | Zero_arity : ('elt, 'elt, unit) arity
  | Succ_arity : ('elt, 'b, 'a) arity -> ('elt, 'elt -> 'b, int * 'a) arity

val arity_0 : ('elt, 'elt, unit) arity

val arity_1 : ('elt, 'elt -> 'elt, int * unit) arity

val arity_2 : ('elt, 'elt -> 'elt -> 'elt, int * (int * unit)) arity

val arity_3 :
  ('elt, 'elt -> 'elt -> 'elt -> 'elt, int * (int * (int * unit))) arity

(** Abstract representation of models.
    Models are strongly typed: {!Model_impl.arg_type} exposes what a model
    expects on input. The relation between [arg_type] and [model_type]
    is encoded through a value of type [arity]. *)
module type Model_impl = sig
  type arg_type

  val name : Namespace.t

  (** If [true], the generated function takes
      [Saturation_repr.may_saturate Saturation_repr.t] as arguments
      instead of [int]s. Otherwise, the generated function takes [int]s.
  *)
  val takes_saturation_reprs : bool

  module Def (X : Costlang.S) : sig
    type model_type

    val arity : (X.size, model_type, arg_type) arity

    val model : model_type X.repr
  end
end

type 'arg model = (module Model_impl with type arg_type = 'arg)

type packed_model = Model : _ model -> packed_model

(** Instantiation of a {!Model_impl.Def} with a {!Costlang.S} *)
module type Instantiated = sig
  type 'a repr

  type size

  type arg_type

  type model_type

  val arity : (size, model_type, arg_type) arity

  val model : arg_type -> size repr
end

module Instantiate (X : Costlang.S) (M : Model_impl) :
  Instantiated
    with type 'a repr = 'a X.repr
     and type size = X.size
     and type arg_type = M.arg_type

(** Model that has been applied to some arguments. Typically, we get an Applied
    model when we apply some workload from a benchmark to its model.
    Can be obtained with {!apply}.
 *)
module type Applied = functor (X : Costlang.S) -> sig
  type t = X.size X.repr

  val applied : t
end

type applied = (module Applied)

(** Model containers for benchmarks. We distinguish two kinds:
    - Abstract, which is simply a ['arg model], with a [conv] function
      that transforms a benchmark's workload into the model's arguments.
    - Aggregate, which is usually obtained from multiple models, a subset
      of which should be in [sub_models].
*)
type 'workload t =
  | Abstract : {conv : 'workload -> 'arg; model : 'arg model} -> 'workload t
  | Aggregate : {
      model : 'workload -> applied;
      sub_models : packed_model list;
    }
      -> 'workload t

val pp : Format.formatter -> _ t -> unit

(** Build [_ t] from [_ model], with a type conversion fucntion [conv].
    If [~takes_saturation_reprs: true] is given, its generated code
    will take [_ Saturation_reprs.t] instead of [int].
*)
val make : ?takes_saturation_reprs:bool -> conv:('a -> 'b) -> 'b model -> 'a t

val make_aggregated :
  model:('a -> applied) -> sub_models:packed_model list -> 'a t

(** [apply] takes a model and an appropriate workload, and returns an applied model *)
val apply : 'a t -> 'a -> applied

(** Addition on models. Note that the results is always an {!Aggregate} *)
val add_model : 'a t -> 'b t -> ('a * 'b) t

(** Transformator for the type of workloads *)
val precompose : ('a -> 'b) -> 'b t -> 'a t

(** Returns the set of free variables of a model *)
val get_free_variable_set : _ model -> Free_variable.Set.t

val get_free_variable_set_of_t : _ t -> Free_variable.Set.t

(** Returns the set of free variables of an applied model

    Note that this can be very different from [get_free_variable_set].
*)
val get_free_variable_set_applied :
  'workload t -> 'workload -> Free_variable.Set.t

(* -------------------------------------------------------------------------- *)
(** Commonly used abstract models
    Except for [zero], they all require a unique name in {!Namespace.t}, and some
    {!Free_variable.t}s. Those free are deduced during the inference in Snoop, and
    the resulting values are (usually) the gas parameters for the protocol.
*)

(** [zero] is the "zero" for the addition on models. It can be seen as the model
    of the empty code.
    [fun () -> 0]
*)
val zero : unit model

(** Model for code that executes in constant time
    [fun () -> const]
*)
val unknown_const1 : name:Namespace.t -> const:Free_variable.t -> unit model

(** Model ignores the arguments.
    [fun n -> const]
*)
val unknown_const1_skip1 :
  name:Namespace.t -> const:Free_variable.t -> (int * unit) model

(** Model ignores the arguments.
    [fun n m -> const]
*)
val unknown_const1_skip2 :
  name:Namespace.t -> const:Free_variable.t -> (int * (int * unit)) model

(** [fun n -> coeff × n] *)
val linear : name:Namespace.t -> coeff:Free_variable.t -> (int * unit) model

(** [fun n -> intercept + coeff × n] *)
val affine :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff:Free_variable.t ->
  (int * unit) model

(** [fun n -> intercept + coeff × (n - offset)] *)
val affine_offset :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff:Free_variable.t ->
  offset:int ->
  (int * unit) model

(** [fun n -> coeff * n²] *)
val quadratic : name:Namespace.t -> coeff:Free_variable.t -> (int * unit) model

(** [fun n -> intercept + coeff × n×log(n)] *)
val nlogn :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff:Free_variable.t ->
  (int * unit) model

(** [fun n -> intercept + coeff × n×sqrt(n)] *)
val nsqrtn_const :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff:Free_variable.t ->
  (int * unit) model

(** [fun n -> coeff * log(n)] *)
val logn : name:Namespace.t -> coeff:Free_variable.t -> (int * unit) model

(** [fun a b -> intercept + coeff × (a+b)] *)
val linear_sum :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff:Free_variable.t ->
  (int * (int * unit)) model

(** [fun a b -> intercept + coeff × (a-b)] *)
val linear_sat_sub :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff:Free_variable.t ->
  (int * (int * unit)) model

(** [fun a b -> intercept + coeff × max(a,b)] *)
val linear_max :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff:Free_variable.t ->
  (int * (int * unit)) model

(** fun a b -> intercept + coeff × min(a,b) *)
val linear_min :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff:Free_variable.t ->
  (int * (int * unit)) model

(** fun a b -> intercept + coeff × (min(a,b) - offset) *)
val linear_min_offset :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff:Free_variable.t ->
  offset:int ->
  (int * (int * unit)) model

(** [fun a b -> intercept + coeff × (a×b)] *)
val linear_mul :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff:Free_variable.t ->
  (int * (int * unit)) model

(** [fun a b -> coeff1 × a + coeff2 × b] *)
val bilinear :
  name:Namespace.t ->
  coeff1:Free_variable.t ->
  coeff2:Free_variable.t ->
  (int * (int * unit)) model

(** [fun a b -> intercept + coeff1 × a + coeff2 × b] *)
val bilinear_affine :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff1:Free_variable.t ->
  coeff2:Free_variable.t ->
  (int * (int * unit)) model

(** [fun a b -> intercept + coeff × b] *)
val affine_skip1 :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff:Free_variable.t ->
  (int * (int * unit)) model

(** [fun n m -> intercept + coeff × n×log(m)] *)
val nlogm :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  coeff:Free_variable.t ->
  (int * (int * unit)) model

(** [fun n m -> intercept + (linear_coeff × n) + (log_coeff × log(m))] *)
val n_plus_logm :
  name:Namespace.t ->
  intercept:Free_variable.t ->
  linear_coeff:Free_variable.t ->
  log_coeff:Free_variable.t ->
  (int * (int * unit)) model

(** [fun a b c -> coeff1 × a + coeff2 × b + coeff3 × c] *)
val trilinear :
  name:Namespace.t ->
  coeff1:Free_variable.t ->
  coeff2:Free_variable.t ->
  coeff3:Free_variable.t ->
  (int * (int * (int * unit))) model

(** A multi-affine model in two parts. The breakpoint [break] indicates the
    point at which the slope changes coefficient.
*)
val breakdown :
  name:Namespace.t ->
  coeff1:Free_variable.t ->
  coeff2:Free_variable.t ->
  break:int ->
  (int * unit) model

(** A multi-affine model in three parts, with breakpoints [break1] and [break2].
    Expects [break1] <= [break2]
 *)
val breakdown2 :
  name:Namespace.t ->
  coeff1:Free_variable.t ->
  coeff2:Free_variable.t ->
  coeff3:Free_variable.t ->
  break1:int ->
  break2:int ->
  (int * unit) model

(** [breakdown2] with a non-zero value at 0 *)
val breakdown2_const :
  name:Namespace.t ->
  coeff1:Free_variable.t ->
  coeff2:Free_variable.t ->
  coeff3:Free_variable.t ->
  const:Free_variable.t ->
  break1:int ->
  break2:int ->
  (int * unit) model

(** [breakdown2] with a non-zero value at 0 and offset *)
val breakdown2_const_offset :
  name:Namespace.t ->
  coeff1:Free_variable.t ->
  coeff2:Free_variable.t ->
  coeff3:Free_variable.t ->
  const:Free_variable.t ->
  break1:int ->
  break2:int ->
  offset:int ->
  (int * unit) model

(* -------------------------------------------------------------------------- *)

module type Binary_operation = sig
  module Def (X : Costlang.S) : sig
    val op : X.size X.repr -> X.size X.repr -> X.size X.repr
  end
end

(** Synthesize two models of the same signature by the specified binary operation.
*)
val synthesize :
  name:Namespace.t ->
  binop:(module Binary_operation) ->
  x_label:string ->
  x_model:'a model ->
  y_label:string ->
  y_model:'a model ->
  'a model
