(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
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

exception FailToComputeFinalExponentiation of Fq12.t

module Stubs = struct
  external miller_loop : Fq12.t -> G2.Stubs.affine -> G1.Stubs.affine -> int
    = "caml_blst_miller_loop_stubs"

  external miller_loop_list : Fq12.t -> (G1.t * G2.t) array -> int -> int
    = "caml_blst_miller_loop_list_stubs"

  external final_exponentiation : Fq12.t -> Gt.t -> int
    = "caml_blst_final_exponentiation_stubs"
end

let miller_loop_simple g1 g2 =
  let buffer = Fq12.Stubs.allocate_fq12 () in
  let g1_affine = G1.Stubs.allocate_g1_affine () in
  let g2_affine = G2.Stubs.allocate_g2_affine () in
  ignore @@ G1.Stubs.to_affine g1_affine g1 ;
  ignore @@ G2.Stubs.to_affine g2_affine g2 ;
  ignore @@ Stubs.miller_loop buffer g2_affine g1_affine ;
  buffer

let miller_loop l =
  let out = Fq12.Stubs.allocate_fq12 () in
  let l = Array.of_list l in
  let length = Array.length l in
  ignore @@ Stubs.miller_loop_list out l length ;
  out

let final_exponentiation_opt x =
  if Fq12.is_zero x then None
  else
    let buffer = Fq12.Stubs.allocate_fq12 () in
    ignore @@ Stubs.final_exponentiation buffer x ;
    Some buffer

let final_exponentiation_exn x =
  if Fq12.is_zero x then raise (FailToComputeFinalExponentiation x)
  else
    let buffer = Fq12.Stubs.allocate_fq12 () in
    ignore @@ Stubs.final_exponentiation buffer x ;
    buffer

let pairing g1 g2 =
  let ml = miller_loop_simple g1 g2 in
  final_exponentiation_exn ml

let pairing_check points =
  let res_opt = miller_loop points |> final_exponentiation_opt in
  match res_opt with None -> false | Some res -> Gt.is_zero res
