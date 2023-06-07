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

open Lang_stdlib
open Lang_core

module Make (Bound : sig
  type 'a t = private Z.t

  val v : 'a t -> Z.t
end) =
struct
  module P : sig
    type 'a t = private Z.t * Z.t

    val make : ?unsafe:bool -> Z.t -> bound:'a Bound.t -> 'a t

    val v : 'a t -> Z.t

    val add : ?unsafe:bool -> 'a t -> 'b t -> unit t

    val add_left : ?unsafe:bool -> 'a t -> 'b t -> 'a t

    val sub_left : ?unsafe:bool -> 'a t -> 'b t -> 'a t

    val succ : ?unsafe:bool -> 'a t -> 'a t

    val random : ?maxv:Z.t -> 'a Bound.t -> 'a t

    val check : 'a t -> bool

    val f : 'a t -> unit t
  end = struct
    type 'a t = Z.t * Z.t

    let make ?(unsafe = false) v ~bound =
      let bound = (bound : 'a Bound.t :> Z.t) in
      assert (bound < Bls12_381.Fr.order) ;
      if not unsafe then assert (v < bound) ;
      (v, bound)

    let v (x, _) = x

    let add ?(unsafe = false) (a, bnd) (b, bnd') =
      let n_bnd = Z.(bnd + bnd') in
      if not unsafe then assert (n_bnd < Bls12_381.Fr.order) ;
      let r = Z.add a b in
      if not unsafe then assert (Z.zero <= r && r < n_bnd) ;
      (r, n_bnd)

    let add_left ?(unsafe = false) (a, bnd) (b, bnd') =
      if not unsafe then assert (Z.(bnd + bnd' < Bls12_381.Fr.order)) ;
      let r = Z.add a b in
      if not unsafe then assert (Z.zero <= r && r < bnd) ;
      (r, bnd)

    let sub_left ?(unsafe = false) (a, bnd) (b, bnd') =
      if not unsafe then assert (Z.(bnd + bnd' < Bls12_381.Fr.order)) ;
      let r = Z.sub a b in
      if not unsafe then assert (Z.zero <= r && r < bnd) ;
      (r, bnd)

    let succ ?(unsafe = false) (a, bnd) =
      if not unsafe then assert (Z.(bnd + one < Bls12_381.Fr.order)) ;
      let r = Z.succ a in
      if not unsafe then assert (Z.zero <= r && r < bnd) ;
      (r, bnd)

    let random ?maxv bound =
      let random_z bound = Random.int64 (Z.to_int64 bound) |> Z.of_int64 in
      let bound = (bound : 'a Bound.t :> Z.t) in
      let maxv =
        match maxv with
        | None -> bound
        | Some maxv ->
            assert (maxv <= bound) ;
            maxv
      in
      (random_z maxv, bound)

    let check (v, bound) = Z.(v >= zero && v < bound)

    let f x = x
  end

  module V (L : LIB) : sig
    open L

    type 'a t = private scalar repr * Z.t

    val make : scalar repr -> bound:'a Bound.t -> 'a t L.t

    val make_unsafe : scalar repr -> bound:'a Bound.t -> 'a t

    val succ : ?unsafe:bool -> 'a t -> 'a t L.t

    val add : ?unsafe:bool -> 'a t -> 'b t -> 'c t L.t

    val add_left : ?unsafe:bool -> 'a t -> 'b t -> 'a t L.t

    val sub_left : ?unsafe:bool -> 'a t -> 'b t -> 'a t L.t

    val f : 'a t -> unit t
  end = struct
    open L

    type 'a t = scalar repr * Z.t

    let make w ~bound =
      let bound = (bound : 'a Bound.t :> Z.t) in
      assert (bound < Bls12_381.Fr.order) ;
      with_bool_check (Num.is_upper_bounded w ~bound) >* ret (w, bound)

    let make_unsafe w ~bound = (w, Bound.v bound)

    let add ?(unsafe = false) (a, bound) (b, bound') =
      assert (Z.(bound + bound' < Bls12_381.Fr.order)) ;
      let* r = Num.add a b in
      if unsafe then ret (r, Z.add bound bound')
      else
        with_bool_check
          (Num.is_upper_bounded_unsafe r ~bound:(Z.add bound bound'))
        >* ret (r, Z.add bound bound')

    let add_left ?(unsafe = false) (a, bound) (b, bound') =
      assert (Z.(bound + bound' < Bls12_381.Fr.order)) ;
      let* r = Num.add a b in
      let nb_bits = Z.(numbits (add bound bound')) in
      if unsafe then ret (r, bound)
      else
        with_bool_check (Num.is_upper_bounded_unsafe ~nb_bits ~bound r)
        >* ret (r, bound)

    let sub_left ?(unsafe = false) (a, bound) (b, bound') =
      assert (Z.(bound + bound' < Bls12_381.Fr.order)) ;
      let* r = Num.add ~qr:S.mone a b in
      if unsafe then ret (r, bound)
      else with_bool_check (Num.geq (a, bound) (b, bound')) >* ret (r, bound)

    let succ ?(unsafe = false) (a, bound) =
      assert (Z.(bound + one < Bls12_381.Fr.order)) ;
      let* r = Num.add_constant S.one a in
      let nb_bits = Z.(numbits (succ bound)) in
      if unsafe then ret (r, bound)
      else
        with_bool_check (Num.is_upper_bounded_unsafe ~nb_bits ~bound r)
        >* ret (r, bound)

    let f x = x
  end

  module Encoding (L : LIB) = struct
    open L
    module VL = V (L)

    open Encoding.Encodings (L)

    type bound_check_safety = Safe | Unsafe | NoCheck

    let encoding ~safety (bound : 'a Bound.t) : ('a P.t, 'a VL.t, _) encoding =
      let z_bound = Bound.v bound in
      let f =
        match safety with
        | Safe -> with_implicit_bool_check (Num.is_upper_bounded ~bound:z_bound)
        | Unsafe ->
            with_implicit_bool_check
              (Num.is_upper_bounded_unsafe ~bound:z_bound)
        | NoCheck -> Fun.id
      in
      f
      @@ conv
           (fun (x : 'a VL.t) ->
             let w, b = (x : 'a VL.t :> scalar repr * Z.t) in
             assert (z_bound = b) ;
             w)
           (fun w -> VL.make_unsafe w ~bound)
           (fun (x : 'a P.t) ->
             let v, _ = (x : 'a P.t :> Z.t * Z.t) in
             S.of_z v)
           (fun v -> P.make ~unsafe:true (S.to_z v) ~bound)
           scalar_encoding
  end
end
