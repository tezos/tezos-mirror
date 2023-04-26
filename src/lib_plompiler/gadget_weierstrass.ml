(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Lang_core
open Lang_stdlib

let zero = S.zero

let one = S.one

let two = S.add one one

let three = S.add two one

let mone = S.negate one

let mtwo = S.negate two

module type AFFINE = functor (L : LIB) -> sig
  open L

  type point = scalar * scalar

  val input_point : ?kind:input_kind -> S.t * S.t -> point repr t

  val assert_is_on_curve : point repr -> unit repr t

  val from_coordinates : scalar repr -> scalar repr -> point repr t

  (** [unsafe_from_coordinates x y] is similar to {!from_coordinates} but
      does not verify the point is on the curve. It can be used to build a
      variable of type *point* without adding any constraint.
  *)
  val unsafe_from_coordinates : scalar repr -> scalar repr -> point repr t

  val get_x_coordinate : point repr -> scalar repr

  val get_y_coordinate : point repr -> scalar repr

  val add : point repr -> point repr -> point repr t

  val double : point repr -> point repr t

  val scalar_mul : bool list repr -> point repr -> bool repr -> point repr t

  val scalar_order : Z.t

  val base_order : Z.t
end

module MakeAffine (Curve : Mec.CurveSig.AffineWeierstrassT) : AFFINE =
functor
  (L : LIB)
  ->
  struct
    open L

    type point = scalar * scalar

    let scalar_order = Curve.Scalar.order

    let base_order = Curve.Base.order

    let param_a = Curve.a |> Curve.Base.to_z |> S.of_z

    let input_point ?(kind = `Private) (x, y) =
      Input.(pair (scalar x) (scalar y)) |> input ~kind

    let get_x_coordinate p = of_pair p |> fst

    let get_y_coordinate p = of_pair p |> snd

    (* 2 constraints *)
    let assert_is_on_curve p =
      with_label ~label:"Weierstrass.assert_is_on_curve"
      @@
      let x, y = of_pair p in
      let* x2 = Num.square x in
      let* y2 = Num.square y in

      (* - y^2 + x^3 + a * x + b = 0
         <=> |  1 * x * x^2 + a * x + b - 1 * tmp = 0
             |  |             |       |   |
             |  qm            ql      qc  qo
             | -1 *  y^2 + 1 * tmp = 0
                |          |
                ql         qr
      *)
      let ql = param_a in
      let qc = Curve.b |> Curve.Base.to_z |> S.of_z in
      let* tmp = Num.custom ~qm:one ~ql ~qc x x2 in
      Num.assert_custom ~ql:mone ~qr:one y2 tmp tmp

    let from_coordinates x y =
      with_label ~label:"Weierstrass.from_coordinates"
      @@
      let p = pair x y in
      assert_is_on_curve p >* ret p

    let unsafe_from_coordinates x y =
      with_label ~label:"Weierstrass.unsafe_from_coordinates" (pair x y |> ret)

    (* 2 constraints *)
    let add p1 p2 = Ecc.weierstrass_add p1 p2

    (* 2 * P1:(x1, y1) = P3:(x2, y2) (!= P1:(x1, y1) + P1:(x1, y1) which fails as the addition is not complete)
       x2 = [(3 * x1^2 + a) / (2 * y1)]^2 - 2 * x1
       y2 = [(3 * x1^2 + a) / (2 * y1)] * (x1 - x2) - y1
       9 constraints
    *)
    let double p =
      with_label ~label:"Weierstrass.double"
      @@
      let x, y = of_pair p in
      (* lambda = (3 * x^2 + a) / (2 * y) *)
      let* num_lambda = Num.custom ~qm:three x x ~qc:param_a in
      let* lambda = Num.div ~den_coeff:two num_lambda y in
      (* x_r = lambda^2 - 2 * x *)
      let* lambda_square = Num.square lambda in
      let* x_r = Num.add lambda_square ~qr:mtwo x in
      (* y_r = lambda * (x - x_r) - y *)
      let* x_minus_xr = Num.add ~qr:mone x x_r in
      let* left = Num.mul lambda x_minus_xr in
      let* y_r = Num.add ~qr:mone left y in
      pair x_r y_r |> ret

    (* We need to check that the variable "flag" is set to false, to do so we can:
          - make the variable "flag" public
          - assert that the variable "flag" is false.
          This needs to be done once for all scalar multiplications.
       List.length(of_list s) * (9 + 2 + 7 + 7 + 1) constraints
    *)
    let scalar_mul s p flag =
      with_label ~label:"Weierstrass.scalar_mul"
      @@
      let init = pair p flag in
      let* res =
        foldM
          (fun acc b ->
            let acc_res, acc_flag = of_pair acc in
            let* acc_res = double acc_res in
            let* sum = add acc_res p in
            let* ite = Bool.ifthenelse acc_flag sum p in
            let* acc_res = Bool.ifthenelse b ite acc_res in
            let* acc_flag = Bool.bor acc_flag b in
            let acc = pair acc_res acc_flag in
            ret acc)
          init
          (List.rev (of_list s))
      in
      let result, _ = of_pair res in
      ret result
  end
