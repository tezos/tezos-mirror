(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

let arity_0 = Zero_arity

let arity_1 = Succ_arity arity_0

let arity_2 = Succ_arity arity_1

let arity_3 = Succ_arity arity_2

type ('a, 'b) eq = Eq : ('a, 'a) eq

let rec elim_arities :
    type elt m1 m2 a. (elt, m1, a) arity -> (elt, m2, a) arity -> (m1, m2) eq =
  fun (type elt m1 m2 a) (ar1 : (elt, m1, a) arity) (ar2 : (elt, m2, a) arity) ->
   match (ar1, ar2) with
   | (Zero_arity, Zero_arity) -> (Eq : (m1, m2) eq)
   | (Succ_arity a1, Succ_arity a2) -> (
       match elim_arities a1 a2 with Eq -> (Eq : (m1, m2) eq))
   | _ -> .

(** Models are strongly typed: [Model_impl.arg_type] exposes what a model
    expects on input. The relation between [arg_type] and [model_type]
    is encoded through a value of type [arity]. *)
module type Model_impl = sig
  type arg_type

  module Def (X : Costlang.S) : sig
    type model_type

    val arity : (X.size, model_type, arg_type) arity

    val model : model_type X.repr
  end
end

module type Instantiated = sig
  type 'a repr

  type size

  type arg_type

  type model_type

  val arity : (size, model_type, arg_type) arity

  val model : arg_type -> size repr
end

type 'arg model = (module Model_impl with type arg_type = 'arg)

module type Applied = functor (X : Costlang.S) -> sig
  val applied : X.size X.repr
end

type applied = (module Applied)

type _ t =
  | Packaged : {conv : 'workload -> 'arg; model : 'arg model} -> 'workload t
  | Preapplied : {model : 'workload -> applied} -> 'workload t

type for_codegen = For_codegen : _ t -> for_codegen

let apply_model : 'arg -> 'arg model -> applied =
  fun (type e) (elim : e) ((module Impl) : e model) ->
   let module Applied (X : Costlang.S) = struct
     include Impl.Def (X)

     let rec apply :
         type a b c.
         (int -> c X.repr) -> (c, a, b) arity -> a X.repr -> b -> c X.repr =
      fun conv arity f arg ->
       match arity with
       | Zero_arity -> f
       | Succ_arity ar ->
           let (arg, rest) = arg in
           apply conv ar (X.app f (conv arg)) rest

     let applied = apply X.int arity model elim
   end in
   ((module Applied) : applied)

module Instantiate (X : Costlang.S) (M : Model_impl) :
  Instantiated
    with type 'a repr = 'a X.repr
     and type size = X.size
     and type arg_type = M.arg_type = struct
  type 'a repr = 'a X.repr

  type size = X.size

  include M
  include Def (X)

  let rec apply :
      type a b c.
      (int -> c X.repr) -> (c, a, b) arity -> a X.repr -> b -> c X.repr =
   fun conv arity f arg ->
    match arity with
    | Zero_arity -> f
    | Succ_arity ar ->
        let (arg, rest) = arg in
        apply conv ar (X.app f (conv arg)) rest

  let model elim = apply X.int arity model elim
end

let make ~conv ~model = Packaged {conv; model}

let make_preapplied ~model = Preapplied {model}

let apply model workload =
  match model with
  | Packaged {conv; model} -> apply_model (conv workload) model
  | Preapplied {model} -> model workload

let add_model : 'arg model -> 'arg model -> 'arg model =
  fun (type arg) ((module M1) : arg model) ((module M2) : arg model) ->
   let module M = struct
     type arg_type = arg

     module Def (X : Costlang.S) = struct
       module M1 = M1.Def (X)
       module M2 = M2.Def (X)

       type model_type = M1.model_type

       let arity = M1.arity

       let model : model_type X.repr =
         match elim_arities M1.arity M2.arity with
         | Eq ->
             let open X in
             let rec loop :
                 type a b. (size, a, b) arity -> a repr -> a repr -> a repr =
              fun arity m1 m2 ->
               match arity with
               | Zero_arity -> (m1 + m2 : a repr)
               | Succ_arity ar ->
                   lam ~name:"gensym" (fun x ->
                       let m1' = app m1 x in
                       let m2' = app m2 x in
                       loop ar m1' m2')
             in
             loop M1.arity M1.model M2.model
     end
   end in
   ((module M) : arg model)

let precompose : type a b. (a -> b) -> b t -> a t =
 fun f model ->
  match model with
  | Packaged {conv; model} ->
      let conv x = conv (f x) in
      Packaged {conv; model}
  | Preapplied {model} -> Preapplied {model = (fun x -> model (f x))}

(* -------------------------------------------------------------------------- *)
(* Commonly used models *)

let unknown_const1 ~const =
  let module M = struct
    type arg_type = unit

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size

      let arity = arity_0

      let model = free ~name:const
    end
  end in
  (module M : Model_impl with type arg_type = unit)

let unknown_const2 ~const1 ~const2 =
  let module M = struct
    type arg_type = unit

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size

      let arity = arity_0

      let model = free ~name:const1 + free ~name:const2
    end
  end in
  (module M : Model_impl with type arg_type = unit)

let linear ~coeff =
  let module M = struct
    type arg_type = int * unit

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size

      let arity = arity_1

      let model = lam ~name:"size" @@ fun size -> free ~name:coeff * size
    end
  end in
  (module M : Model_impl with type arg_type = int * unit)

let affine ~intercept ~coeff =
  let module M = struct
    type arg_type = int * unit

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size

      let arity = arity_1

      let model =
        lam ~name:"size" @@ fun size ->
        free ~name:intercept + (free ~name:coeff * size)
    end
  end in
  (module M : Model_impl with type arg_type = int * unit)

let affine_split_const ~intercept1 ~intercept2 ~coeff =
  let module M = struct
    type arg_type = int * unit

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size

      let arity = arity_1

      let model =
        lam ~name:"size" @@ fun size ->
        free ~name:intercept1 + free ~name:intercept2 + (free ~name:coeff * size)
    end
  end in
  (module M : Model_impl with type arg_type = int * unit)

let quadratic ~coeff =
  let module M = struct
    type arg_type = int * unit

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size

      let arity = arity_1

      let model =
        lam ~name:"size" @@ fun size -> free ~name:coeff * (size * size)
    end
  end in
  (module M : Model_impl with type arg_type = int * unit)

let nlogn ~intercept ~coeff =
  let module M = struct
    type arg_type = int * unit

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size

      let arity = arity_1

      let model =
        lam ~name:"size" @@ fun size ->
        free ~name:intercept + (free ~name:coeff * (size * log2 (int 1 + size)))
    end
  end in
  (module M : Model_impl with type arg_type = int * unit)

let logn ~coeff =
  let module M = struct
    type arg_type = int * unit

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size

      let arity = arity_1

      let model =
        lam ~name:"size" @@ fun size -> free ~name:coeff * log2 (int 1 + size)
    end
  end in
  (module M : Model_impl with type arg_type = int * unit)

let linear_sum ~intercept ~coeff =
  let module M = struct
    type arg_type = int * (int * unit)

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size

      let arity = arity_2

      let model =
        lam ~name:"size1" @@ fun size1 ->
        lam ~name:"size2" @@ fun size2 ->
        free ~name:intercept + (free ~name:coeff * (size1 + size2))
    end
  end in
  (module M : Model_impl with type arg_type = int * (int * unit))

let linear_max ~intercept ~coeff =
  let module M = struct
    type arg_type = int * (int * unit)

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size

      let arity = arity_2

      let model =
        lam ~name:"size1" @@ fun size1 ->
        lam ~name:"size2" @@ fun size2 ->
        free ~name:intercept + (free ~name:coeff * max size1 size2)
    end
  end in
  (module M : Model_impl with type arg_type = int * (int * unit))

let linear_min ~intercept ~coeff =
  let module M = struct
    type arg_type = int * (int * unit)

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size

      let arity = arity_2

      let model =
        lam ~name:"size1" @@ fun size1 ->
        lam ~name:"size2" @@ fun size2 ->
        free ~name:intercept + (free ~name:coeff * min size1 size2)
    end
  end in
  (module M : Model_impl with type arg_type = int * (int * unit))

let linear_mul ~intercept ~coeff =
  let module M = struct
    type arg_type = int * (int * unit)

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size

      let arity = arity_2

      let model =
        lam ~name:"size1" @@ fun size1 ->
        lam ~name:"size2" @@ fun size2 ->
        free ~name:intercept + (free ~name:coeff * (size1 * size2))
    end
  end in
  (module M : Model_impl with type arg_type = int * (int * unit))

let bilinear ~coeff1 ~coeff2 =
  let module M = struct
    type arg_type = int * (int * unit)

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size

      let arity = arity_2

      let model =
        lam ~name:"size1" @@ fun size1 ->
        lam ~name:"size2" @@ fun size2 ->
        (free ~name:coeff1 * size1) + (free ~name:coeff2 * size2)
    end
  end in
  (module M : Model_impl with type arg_type = int * (int * unit))

let bilinear_affine ~intercept ~coeff1 ~coeff2 =
  let module M = struct
    type arg_type = int * (int * unit)

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size

      let arity = arity_2

      let model =
        lam ~name:"size1" @@ fun size1 ->
        lam ~name:"size2" @@ fun size2 ->
        free ~name:intercept
        + (free ~name:coeff1 * size1)
        + (free ~name:coeff2 * size2)
    end
  end in
  (module M : Model_impl with type arg_type = int * (int * unit))

let nlogm ~intercept ~coeff =
  let module M = struct
    type arg_type = int * (int * unit)

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size

      let arity = arity_2

      let model =
        lam ~name:"size1" @@ fun size1 ->
        lam ~name:"size2" @@ fun size2 ->
        free ~name:intercept
        + (free ~name:coeff * (size1 * log2 (int 1 + size2)))
    end
  end in
  (module M : Model_impl with type arg_type = int * (int * unit))

let n_plus_logm ~intercept ~linear_coeff ~log_coeff =
  let module M = struct
    type arg_type = int * (int * unit)

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size

      let arity = arity_2

      let model =
        lam ~name:"size1" @@ fun size1 ->
        lam ~name:"size2" @@ fun size2 ->
        free ~name:intercept
        + (free ~name:linear_coeff * size1)
        + (free ~name:log_coeff * log2 (int 1 + size2))
    end
  end in
  (module M : Model_impl with type arg_type = int * (int * unit))

let trilinear ~coeff1 ~coeff2 ~coeff3 =
  let module M = struct
    type arg_type = int * (int * (int * unit))

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size -> size

      let arity = arity_3

      let model =
        lam ~name:"size1" @@ fun size1 ->
        lam ~name:"size2" @@ fun size2 ->
        lam ~name:"size3" @@ fun size3 ->
        (free ~name:coeff1 * size1)
        + (free ~name:coeff2 * size2)
        + (free ~name:coeff3 * size3)
    end
  end in
  (module M : Model_impl with type arg_type = int * (int * (int * unit)))

(** A multi-affine model in two parts. The breakpoint [break] indicates the
    point at which the slope changes coefficient. *)
let breakdown ~coeff1 ~coeff2 ~break =
  let module M = struct
    type arg_type = int * unit

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size

      let arity = arity_1

      let model =
        lam ~name:"size" @@ fun size ->
        (free ~name:coeff1 * max (int 0) (min (int break) size))
        + (free ~name:coeff2 * max (int 0) (size - int break))
    end
  end in
  (module M : Model_impl with type arg_type = int * unit)

(** A multi-affine model in three parts, with breakpoints [break1] and [break2].
    Expects [break1] <= [break2]
 *)
let breakdown2 ~coeff1 ~coeff2 ~coeff3 ~break1 ~break2 =
  assert (break1 <= break2) ;
  let module M = struct
    type arg_type = int * unit

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size

      let arity = arity_1

      let model =
        lam ~name:"size" @@ fun size ->
        (free ~name:coeff1 * max (int 0) (min (int break1) size))
        + (free ~name:coeff2 * max (int 0) (min (int break2) size - int break1))
        + (free ~name:coeff3 * max (int 0) (size - int break2))
    end
  end in
  (module M : Model_impl with type arg_type = int * unit)

(** [breakdown2] with a non-zero value at 0 *)
let breakdown2_const ~coeff1 ~coeff2 ~coeff3 ~const ~break1 ~break2 =
  assert (break1 <= break2) ;
  let module M = struct
    type arg_type = int * unit

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size

      let arity = arity_1

      let model =
        lam ~name:"size" @@ fun size ->
        (free ~name:coeff1 * max (int 0) (min (int break1) size))
        + (free ~name:coeff2 * max (int 0) (min (int break2) size - int break1))
        + (free ~name:coeff3 * max (int 0) (size - int break2))
        + free ~name:const
    end
  end in
  (module M : Model_impl with type arg_type = int * unit)
