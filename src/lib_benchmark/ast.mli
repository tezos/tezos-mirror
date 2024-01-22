(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

open Costlang

module type S = sig
  type size

  val size_ty : size Ty.t

  type unop = Log2 | Sqrt

  (* binops of [size -> size -> size] *)
  type binop_size = Add | Sat_sub | Mul | Div | Max | Min

  (* binops of [size -> size -> bool] *)
  type binop_bool = Eq | Lt

  type _ t =
    | Size : Num.t -> size t (* Not [Num.t t]!!! *)
    | Bool : bool -> bool t
    | Unop : unop * size t -> size t
    | Binop_size : binop_size * size t * size t -> size t
    | Binop_bool : binop_bool * size t * size t -> bool t
    | Shift : [`Left | `Right] * size t * int -> size t
    | Free : Free_variable.t -> size t
    | Lam : string * 'a Ty.t * 'b t -> ('a -> 'b) t
    | App : ('a -> 'b) t * 'a t -> 'b t
    | Let : string * 'a t * 'b t -> 'b t
    | If : bool t * size t * size t -> size t
    | Variable : string * 'a Ty.t -> 'a t

  val term_size : 'a. 'a t -> int

  val type_of : 'a t -> 'a Ty.t

  val pp : Format.formatter -> _ t -> unit

  (** To OCaml parsetree *)
  val to_expression : _ t -> Parsetree.expression

  (** Existentials *)

  type packed

  val pack : 'a t -> packed

  val unpack : 'a Ty.t -> packed -> 'a t option

  (** Optimizations *)

  (* Charge at least 10 milli-gas. For example,
     [fun size1 size2 -> size1 + size2]
     => [fun size1 size2 -> max 10 (size1 + size2)]
  *)
  val at_least_10 : 'a t -> 'a t

  (* [let x = e1 in e2] => [e2[e1/x]] *)
  val subst_let : 'a t -> 'a t

  val optimize_affine : 'a t -> 'a t

  val cse : 'a t -> 'a t
end

module Make (Size : sig
  type size

  val size_ty : size Ty.t
end) : S with type size = Size.size

(** Standard AST *)
module Ast : S with type size = Num.t

module To_ast (Ast : S) :
  Costlang.S with type 'a repr = 'a Ast.t and type size = Ast.size

(** Code transformer via the AST tree *)
module Transform (F : functor (Ast : S) -> sig
  val transform : 'a Ast.t -> 'a Ast.t
end) : Costlang.Transform

(** A Costlang transformer for various code optimizations *)
module Optimize : Transform

(** A Costlang transformer taking the max with 10 *)
module At_least_10 : Transform
