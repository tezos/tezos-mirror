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

module type CURVE_PARAMETERS = sig
  val a : S.t

  val d : S.t

  val scalar_order : Z.t

  val base_order : Z.t
end

let zero = S.zero

let one = S.one

let two = S.add one one

let mone = S.negate one

module type AFFINE = functor (L : LIB) -> sig
  open L

  type point = scalar * scalar

  val input_point : ?kind:input_kind -> S.t * S.t -> point repr t

  val is_on_curve : point repr -> bool repr t

  (** Also checks that the point is on the curve (but not necessarily in the
      subgroup). *)
  val from_coordinates : scalar repr -> scalar repr -> point repr t

  val unsafe_from_coordinates : scalar repr -> scalar repr -> point repr t

  val get_u_coordinate : point repr -> scalar repr

  val get_v_coordinate : point repr -> scalar repr

  (** The identity element of the curve (0, 1). *)
  val id : S.t * S.t

  val add : point repr -> point repr -> point repr t

  val cond_add : point repr -> point repr -> bool repr -> point repr t

  val double : point repr -> point repr t

  val scalar_mul : bool list repr -> point repr -> point repr t

  val scalar_order : Z.t

  val base_order : Z.t

  val multi_scalar_mul : bool list list repr -> point list repr -> point repr t
end

module MakeAffine (Params : CURVE_PARAMETERS) : AFFINE =
functor
  (L : LIB)
  ->
  struct
    include Params
    open L

    type point = scalar * scalar

    let input_point ?(kind = `Private) (u, v) =
      Input.(pair (scalar u) (scalar v)) |> input ~kind

    let get_u_coordinate p = of_pair p |> fst

    let get_v_coordinate p = of_pair p |> snd

    let id = (S.zero, S.one)

    (* 1 constraint *)
    let is_on_curve p =
      with_label ~label:"Edwards.is_on_curve"
      @@
      let u, v = of_pair p in
      let* u2 = Num.square u in
      let* v2 = Num.square v in
      (* x_l = u^2 *)
      (* x_r = v^2 *)
      (* -1 * u^2 + 1 * v^2 - d * u^2 v^2 -  1  = 0  *)
      (*  |         |         |              |    |  *)
      (*  ql        qr        qm             qc   qo *)
      let qm = S.(negate Params.d) in
      (* The last wire is multiplied by 0 so we can put any value, we chose u here. *)
      let* o = Num.custom ~qc:mone ~ql:mone ~qr:one ~qm u2 v2 in
      Num.is_zero o

    let from_coordinates u v =
      with_label ~label:"Edwards.from_coordinates"
      @@
      let p = pair u v in
      with_bool_check (is_on_curve p) >* ret p

    let unsafe_from_coordinates u v =
      with_label ~label:"Edwards.unsafe_from_coordinates" (pair u v |> ret)

    (* P1:(u1, v1) + P2:(u2, v2) = P3:(u3, v3)
       2 constraints
    *)
    let add p1 p2 = Ecc.edwards_add p1 p2

    let cond_add p1 p2 b = Ecc.edwards_cond_add p1 p2 b

    (* 2 * P1:(u1, v1) = P1:(u1, v1) + P1:(u1, v1) = P3:(u3, v3) as the addition is complete
       12 constraints
    *)

    let double p = add p p

    let point_or_zero point b =
      with_label ~label:"Edwards.point_or_zero"
      @@
      let p_u = get_u_coordinate point in
      let p_v = get_v_coordinate point in
      (* if b = 1, return (p_u, p_v); otherwise the zero point (0, 1) *)
      let b = scalar_of_bool b in
      let* u = Num.mul b p_u in
      let* v = Num.custom ~qr:mone ~qc:one ~qm:one p_v b in
      ret @@ pair u v

    let scalar_mul s p =
      let* one = Bool.constant_bool true in
      with_label ~label:"Edwards.scalar_mul"
      @@
      let rev_s = List.rev (of_list s) in
      let* init = point_or_zero p (List.hd rev_s) in
      foldM
        (fun acc b ->
          let* acc = cond_add acc acc one in
          cond_add acc p b)
        init
        (List.tl rev_s)

    (* Computes \prod_i p_i^s_i with inputs:
       - ls: [[s_11; ...; s_1m]; ...; [s_n1; ...; s_nm]]
       - lp: [p1; ...; pn] *)
    let multi_scalar_mul ls lp =
      let* one = Bool.constant_bool true in
      with_label ~label:"Edwards.multi_scalar_mul"
      @@
      (* Check we apply Shamir's trick on at least 2 points *)
      let () = assert (List.(length (of_list ls) > 1)) in
      (* Converting ls to ls' = [[s_11; ...; s_n1]; ...; [s_1m; ...; s_nm]] *)
      let ls = List.map of_list (of_list ls) |> Utils.transpose |> List.rev in
      let points = of_list lp in
      (* Check we perform scalar multiplications on lists of at least 1 bit *)
      assert (List.(length ls > 0)) ;
      (* Initializing the accumulator with the first round of Shamir's trick *)
      let heads = List.hd ls in
      let* init = point_or_zero (List.hd points) (List.hd heads) in
      let* init = fold2M cond_add init (List.tl points) (List.tl heads) in

      (* Applying Shamir's trick on the rest of the rounds *)
      foldM
        (fun acc lb ->
          let* acc = cond_add acc acc one in
          fold2M cond_add acc points (of_list lb))
        init
        List.(map to_list (tl ls))
  end

module Jubjub = MakeAffine (struct
  let a = mone

  let d =
    S.of_string
      "19257038036680949359750312669786877991949435402254120286184196891950884077233"

  let scalar_order =
    Z.of_string
      "6554484396890773809930967563523245729705921265872317281365359162392183254199"

  let base_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)
