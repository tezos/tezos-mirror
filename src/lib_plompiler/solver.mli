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
module CS = Csir.CS
open Linear_algebra.Make_VectorSpace(S)
module Tables = Csir.Tables

type wire = W of int [@@ocaml.unboxed]

type row = R of int [@@ocaml.unboxed]

type 'a tagged = Input of 'a | Output of 'a

val untag : 'a tagged -> 'a

type arith_desc = {
  wires : row array;
  linear : S.t array;
  qm : S.t;
  qc : S.t;
  qx5a : S.t;
  qx2b : S.t;
  to_solve : wire;
}

type pow5_desc = {a : int; c : int}

type wires_desc = int array

type lookup_desc = {wires : int tagged array; table : string}

type swap_desc = {b : int; x : int; y : int; u : int; v : int}

type ws_desc = {x1 : int; y1 : int; x2 : int; y2 : int; x3 : int; y3 : int}

type ed_desc = {
  a : S.t;
  d : S.t;
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
  x3 : int;
  y3 : int;
}

type ed_cond_desc = {
  a : S.t;
  d : S.t;
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
  bit : int;
  x3 : int;
  y3 : int;
}

type bits_desc = {nb_bits : int; shift : Z.t; l : int; bits : int list}

type pos128full_desc = {
  x0 : int;
  y0 : int;
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
  k : S.t array;
  matrix : matrix;
}

type pos128partial_desc = {
  a : int;
  b : int;
  c : int;
  a_5 : int;
  b_5 : int;
  c_5 : int;
  x0 : int;
  y0 : int;
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
  k_cols : matrix array;
  matrix : matrix;
}

type anemoi_desc = {
  x0 : int;
  y0 : int;
  w : int;
  v : int;
  x1 : int;
  y1 : int;
  kx : S.t;
  ky : S.t;
}

type anemoi_double_desc = {
  x0 : int;
  y0 : int;
  w0 : int;
  w1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
  kx1 : S.t;
  ky1 : S.t;
  kx2 : S.t;
  ky2 : S.t;
}

type anemoi_custom_desc = {
  x0 : int;
  y0 : int;
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
  kx1 : S.t;
  ky1 : S.t;
  kx2 : S.t;
  ky2 : S.t;
}

type solver_desc =
  | Arith of arith_desc
  | Pow5 of pow5_desc
  | IsZero of wires_desc
  | IsNotZero of wires_desc
  | Lookup of lookup_desc
  | Ecc_Ws of ws_desc
  | Ecc_Ed of ed_desc
  | Ecc_Cond_Ed of ed_cond_desc
  | Swap of swap_desc
  | Skip
  | BitsOfS of bits_desc
  | Poseidon128Full of pos128full_desc
  | Poseidon128Partial of pos128partial_desc
  | AnemoiRound of anemoi_desc
  | AnemoiDoubleRound of anemoi_double_desc
  | AnemoiCustom of anemoi_custom_desc
  | Updater of Optimizer.trace_info

type solvers

type t = {solvers : solvers; initial_size : int; final_size : int}
[@@deriving repr]

val empty_solver : t

val append_solver : solver_desc -> t -> t

val solve : t -> S.t array -> S.t array
