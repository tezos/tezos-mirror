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
    | Size : Num.t -> size t
    | Bool : bool -> bool t
    | Unop : unop * size t -> size t
    | Binop_size : binop_size * size t * size t -> size t
    | Binop_bool : binop_bool * size t * size t -> bool t
    | Shift : [`Left | `Right] * size t * int -> size t
    | Free : Free_variable.t -> size t
    | Lam : string * 'a Ty.t * 'b t -> ('a -> 'b) t
    | App : ('a -> 'b) t * 'a t -> 'b t
    | Let : string * 'a t * 'b t -> 'b t
    | If : bool t * 'a t * 'a t -> 'a t
    | Variable : string * 'a Ty.t -> 'a t

  val type_of : 'a t -> 'a Ty.t

  val pp : Format.formatter -> _ t -> unit

  (** To OCaml parsetree *)
  val to_expression : _ t -> Parsetree.expression

  (** Existentials *)

  type packed

  val pack : 'a t -> packed

  val unpack : 'a Ty.t -> packed -> 'a t option
end

(* Need to parameterize the size if we want to make a transformer *)
module Make (A : sig
  type size

  val size_ty : size Ty.t
end) =
struct
  include A

  type unop = Log2 | Sqrt

  (* binops of [size -> size -> size] *)
  type binop_size = Add | Sat_sub | Mul | Div | Max | Min

  (* binops of [size -> size -> bool] *)
  type binop_bool = Eq | Lt

  type _ t =
    | Size : Num.t -> size t
    | Bool : bool -> bool t
    | Unop : unop * size t -> size t
    | Binop_size : binop_size * size t * size t -> size t
    | Binop_bool : binop_bool * size t * size t -> bool t
    | Shift : [`Left | `Right] * size t * int -> size t
    | Free : Free_variable.t -> size t
    | Lam : string * 'a Ty.t * 'b t -> ('a -> 'b) t
    | App : ('a -> 'b) t * 'a t -> 'b t
    | Let : string * 'a t * 'b t -> 'b t
    | If : bool t * 'a t * 'a t -> 'a t
    | Variable : string * 'a Ty.t -> 'a t

  let rec type_of : type a. a t -> a Ty.t = function
    | Size _ -> A.size_ty
    | Bool _ -> Ty.Bool
    | Unop _ -> A.size_ty
    | Shift _ -> A.size_ty
    | Binop_size _ -> A.size_ty
    | Binop_bool _ -> Ty.Bool
    | Free _ -> A.size_ty
    | Lam (_name, ty, b) -> Ty.Arrow (ty, type_of b)
    | Let (_v, _m, t) -> type_of t
    | App (f, _t) -> ( match type_of f with Ty.Arrow (_tf, tr) -> tr)
    | Variable (_, ty) -> ty
    | If (_, t, _) -> type_of t

  module Parsetree = struct
    open Ast_helper

    let loc txt = {Asttypes.txt; loc = Location.none}

    let loc_ident x = {Asttypes.txt = Longident.Lident x; loc = Location.none}

    let loc_str (x : string) = {Asttypes.txt = x; loc = Location.none}

    let ident x = Exp.ident (loc_ident x)

    let pvar x = Pat.var (loc_str x)

    let saturated name = ["S"; name]

    let call f args =
      let f = WithExceptions.Option.get ~loc:__LOC__ @@ Longident.unflatten f in
      let args = List.map (fun x -> (Asttypes.Nolabel, x)) args in
      Exp.(apply (ident (loc f)) args)

    let string_of_fv fv = Format.asprintf "%a" Free_variable.pp fv

    let rec to_expression : type a. a t -> Parsetree.expression = function
      | Bool true -> Exp.construct (loc_ident "true") None
      | Bool false -> Exp.construct (loc_ident "false") None
      | Size (Int i) -> call (saturated "safe_int") [Exp.constant (Const.int i)]
      | Size (Float f) ->
          call
            (saturated "safe_int")
            [
              call
                ["int_of_float"]
                [Exp.constant @@ Const.float (string_of_float f)];
            ]
      | Binop_size (Add, t1, t2) ->
          call ["+"] [to_expression t1; to_expression t2]
      | Binop_size (Sat_sub, t1, t2) ->
          call (saturated "sub") [to_expression t1; to_expression t2]
      | Binop_size (Mul, t1, t2) ->
          call ["*"] [to_expression t1; to_expression t2]
      | Binop_size (Div, t1, t2) ->
          call ["/"] [to_expression t1; to_expression t2]
      | Binop_size (Max, t1, t2) ->
          call (saturated "max") [to_expression t1; to_expression t2]
      | Binop_size (Min, t1, t2) ->
          call (saturated "min") [to_expression t1; to_expression t2]
      | Unop (Log2, t) -> call ["log2"] [to_expression t]
      | Unop (Sqrt, t) -> call ["sqrt"] [to_expression t]
      | Free name -> Exp.ident (loc_ident (string_of_fv name))
      | Binop_bool (Lt, t1, t2) ->
          call ["<"] [to_expression t1; to_expression t2]
      | Binop_bool (Eq, t1, t2) ->
          call ["="] [to_expression t1; to_expression t2]
      | Shift (`Left, t, bits) ->
          call ["lsl"] [to_expression t; Exp.constant (Const.int bits)]
      | Shift (`Right, t, bits) ->
          call ["lsr"] [to_expression t; Exp.constant (Const.int bits)]
      | Lam (name, _ty, b) ->
          let patt = pvar name in
          Exp.fun_ Nolabel None patt (to_expression b)
      | App (f, t) -> Exp.apply (to_expression f) [(Nolabel, to_expression t)]
      | Let (name, m, b) ->
          let var = pvar name in
          let m = to_expression m in
          let b = to_expression b in
          Exp.let_ Nonrecursive [Vb.mk var m] b
      | If (c, t, f) ->
          Exp.ifthenelse
            (to_expression c)
            (to_expression t)
            (Some (to_expression f))
      | Variable (name, _ty) -> ident name
  end

  let to_expression = Parsetree.to_expression

  let pp ppf t = Pprintast.expression ppf @@ Parsetree.to_expression t

  (* Existential *)

  type packed = Packed : 'a Ty.t * 'a t -> packed

  let pack t = Packed (type_of t, t)

  let unpack : type a. a Ty.t -> packed -> a t option =
   fun ty (Packed (ty', t)) ->
    match Ty.equal ty ty' with None -> None | Some Refl -> Some t
end

module Ast = Make (struct
  type size = Num.t

  let size_ty = Ty.num
end)

module To_ast (Ast : S) :
  Costlang.S with type 'a repr = 'a Ast.t and type size = Ast.size = struct
  type size = Ast.size

  let size_ty = Ast.size_ty

  type 'a repr = 'a Ast.t

  open Ast

  let false_ = Bool false

  let true_ = Bool true

  let float f = Size (Float f)

  let int i = Size (Int i)

  let ( + ) x y = Binop_size (Add, x, y)

  let ( * ) x y = Binop_size (Mul, x, y)

  let sat_sub x y = Binop_size (Sat_sub, x, y)

  let ( / ) x y = Binop_size (Div, x, y)

  let max x y = Binop_size (Max, x, y)

  let min x y = Binop_size (Min, x, y)

  let shift_left x s = Shift (`Left, x, s)

  let shift_right x s = Shift (`Right, x, s)

  let log2 x = Unop (Log2, x)

  let sqrt x = Unop (Sqrt, x)

  let free ~name = Free name

  let lt x y = Binop_bool (Lt, x, y)

  let eq x y = Binop_bool (Eq, x, y)

  let lam' ~name ty (f : 'a repr -> 'b repr) =
    Lam (name, ty, f (Variable (name, ty)))

  let lam ~name = lam' ~name size_ty

  let app f arg = App (f, arg)

  let let_ ~name (type a) (m : a repr) (f : a repr -> 'b repr) : 'b repr =
    let var = Variable (name, type_of m) in
    Let (name, m, f var)

  let if_ cond ift iff = If (cond, ift, iff)
end

module Transform (F : functor (Ast : S) -> sig
  val transform : 'a Ast.t -> 'a Ast.t
end) : Costlang.Transform =
functor
  (X : Costlang.S)
  ->
  struct
    module Ast = Make (struct
      type size = X.size

      let size_ty = X.size_ty
    end)

    include To_ast (Ast)
    module T = F (Ast)

    type 'a repr = 'a Ast.t

    type x_repr_ex = X_repr_ex : 'a Ty.t * 'a X.repr -> x_repr_ex

    let rec prj' : type a. (string * x_repr_ex) list -> a repr -> a X.repr =
     fun env t ->
      match t with
      | Variable (name, ty) -> (
          match List.assoc ~equal:String.equal name env with
          | None -> assert false
          | Some (X_repr_ex (ty', xa)) -> (
              match Ty.equal ty ty' with
              | None -> assert false
              | Some Refl -> xa))
      | Lam (name, ty, b) ->
          X.lam' ~name ty (fun xa ->
              let env = (name, X_repr_ex (ty, xa)) :: env in
              prj' env b)
      | Let (name, m, t) ->
          X.let_ ~name (prj' env m) (fun xa ->
              let env = (name, X_repr_ex (Ast.type_of m, xa)) :: env in
              prj' env t)
      | Size (Int i) -> X.int i
      | Size (Float f) -> X.float f
      | Bool true -> X.true_
      | Bool false -> X.false_
      | Unop (Log2, t) -> X.log2 (prj' env t)
      | Unop (Sqrt, t) -> X.sqrt (prj' env t)
      | Binop_size (Add, t1, t2) -> X.( + ) (prj' env t1) (prj' env t2)
      | Binop_size (Sat_sub, t1, t2) -> X.sat_sub (prj' env t1) (prj' env t2)
      | Binop_size (Mul, t1, t2) -> X.( * ) (prj' env t1) (prj' env t2)
      | Binop_size (Div, t1, t2) -> X.( / ) (prj' env t1) (prj' env t2)
      | Binop_size (Max, t1, t2) -> X.max (prj' env t1) (prj' env t2)
      | Binop_size (Min, t1, t2) -> X.min (prj' env t1) (prj' env t2)
      | Binop_bool (Eq, t1, t2) -> X.eq (prj' env t1) (prj' env t2)
      | Binop_bool (Lt, t1, t2) -> X.lt (prj' env t1) (prj' env t2)
      | Shift (`Left, t, n) -> X.shift_left (prj' env t) n
      | Shift (`Right, t, n) -> X.shift_right (prj' env t) n
      | Free name -> X.free ~name
      | App (f, t) -> X.app (prj' env f) (prj' env t)
      | If (c, t, e) -> X.if_ (prj' env c) (prj' env t) (prj' env e)

    let prj t = prj' [] (T.transform t)
  end
