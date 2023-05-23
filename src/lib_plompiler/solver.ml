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

include Lang_core
module CS = Csir.CS
module VS = Linear_algebra.Make_VectorSpace (S)
module Tables = Csir.Tables

type wire = W of int [@@deriving repr] [@@ocaml.unboxed]

type row = R of int [@@deriving repr] [@@ocaml.unboxed]

type 'a tagged = Input of 'a | Output of 'a [@@deriving repr]

type arith_desc = {
  wires : row array;
  linear : S.t array;
  qm : S.t;
  qc : S.t;
  qx5a : S.t;
  qx2b : S.t;
  to_solve : wire;
}
[@@deriving repr]

type pow5_desc = {a : int; c : int} [@@deriving repr]

type wires_desc = int array [@@deriving repr]

type lookup_desc = {wires : int tagged array; table : string} [@@deriving repr]

type ws_desc = {x1 : int; y1 : int; x2 : int; y2 : int; x3 : int; y3 : int}
[@@deriving repr]

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
[@@deriving repr]

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
[@@deriving repr]

type bits_desc = {nb_bits : int; shift : Utils.Z.t; l : int; bits : int list}
[@@deriving repr]

type pos128full_desc = {
  x0 : int;
  y0 : int;
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
  k : VS.t array;
  matrix : VS.matrix;
}

type swap_desc = {b : int; x : int; y : int; u : int; v : int} [@@deriving repr]

(* we define this by hand to avoid doing all linear algebra, VS.t is actually
   S.t but for some reason Repr does not see this equality. *)
let pos128full_desc_t =
  let open Repr in
  record "pos128full_desc" (fun x0 y0 x1 y1 x2 y2 k matrix ->
      {x0; y0; x1; y1; x2; y2; k; matrix})
  |+ field "x0" int (fun t -> t.x0)
  |+ field "y0" int (fun t -> t.y0)
  |+ field "x1" int (fun t -> t.x1)
  |+ field "y1" int (fun t -> t.y1)
  |+ field "x2" int (fun t -> t.x2)
  |+ field "y2" int (fun t -> t.y2)
  |+ field "k" (array S.t) (fun t -> t.k)
  |+ field "matrix" (array (array S.t)) (fun t -> t.matrix)
  |> sealr

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
  (* Can we share these? *)
  k_cols : VS.matrix array;
  matrix : VS.matrix;
}

let pos128partial_desc_t =
  let open Repr in
  record
    "pos128partial_desc"
    (fun a b c a_5 b_5 c_5 x0 y0 x1 y1 x2 y2 k_cols matrix ->
      {a; b; c; a_5; b_5; c_5; x0; y0; x1; y1; x2; y2; k_cols; matrix})
  |+ field "a" int (fun t -> t.a)
  |+ field "b" int (fun t -> t.b)
  |+ field "c" int (fun t -> t.c)
  |+ field "a_5" int (fun t -> t.a_5)
  |+ field "b_5" int (fun t -> t.b_5)
  |+ field "c_5" int (fun t -> t.c_5)
  |+ field "x0" int (fun t -> t.x0)
  |+ field "y0" int (fun t -> t.y0)
  |+ field "x1" int (fun t -> t.x1)
  |+ field "y1" int (fun t -> t.y1)
  |+ field "x2" int (fun t -> t.x2)
  |+ field "y2" int (fun t -> t.y2)
  |+ field "k_cols" (array (array (array S.t))) (fun t -> t.k_cols)
  |+ field "matrix" (array (array S.t)) (fun t -> t.matrix)
  |> sealr

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

let anemoi_desc_t =
  let open Repr in
  record "anemoi_desc" (fun x0 y0 w v x1 y1 kx ky ->
      {x0; y0; w; v; x1; y1; kx; ky})
  |+ field "x0" int (fun t -> t.x0)
  |+ field "y0" int (fun t -> t.y0)
  |+ field "w" int (fun t -> t.w)
  |+ field "v" int (fun t -> t.v)
  |+ field "x1" int (fun t -> t.x1)
  |+ field "y1" int (fun t -> t.y1)
  |+ field "kx" S.t (fun t -> t.kx)
  |+ field "ky" S.t (fun t -> t.ky)
  |> sealr

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
[@@deriving repr]

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
[@@deriving repr]

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
[@@deriving repr]

type solvers = solver_desc list [@@deriving repr]

type t = {solvers : solvers; initial_size : int; final_size : int}
[@@deriving repr]

let empty_solver = {solvers = []; initial_size = 0; final_size = 0}

let append_solver sd t = {t with solvers = sd :: t.solvers}

let untag = function Input a -> a | Output a -> a

let from_tagged = function Input i -> Some i | Output _ -> None

let solve_one trace solver =
  (match solver with
  | Skip -> ()
  | Arith {wires; linear; qm; qc; qx5a; qx2b; to_solve} -> (
      (* A gate with degree strictly greater than 1 must be used on an input wire.
         This is to avoid having several solutions for the same equation.
      *)
      match to_solve with
      | W i ->
          assert (i <> 0 || S.is_zero qx5a) ;
          assert (i <> 1 || S.is_zero qx2b) ;
          let vs = Array.map (fun (R i) -> trace.(i)) wires in
          let qs = Array.copy linear in
          let qi = linear.(i) in
          (* We ignore the i-th term, as we are solving for it *)
          qs.(i) <- S.zero ;
          let sum = Array.map2 S.mul qs vs |> Array.fold_left S.add qc in
          let (R a_row) = wires.(0) in
          let (R b_row) = wires.(1) in
          let av = trace.(a_row) in
          let bv = trace.(b_row) in
          let m_pair = match i with 0 -> bv | 1 -> av | _ -> S.zero in
          let (R i_row) = wires.(i) in
          trace.(i_row) <-
            S.(
              (sum
              + (if i >= 2 then qm * av * bv else S.zero)
              + (qx5a * pow av (Z.of_int 5))
              + (qx2b * (bv * bv)))
              / negate (qi + (m_pair * qm))))
  | Pow5 {a; c} -> trace.(c) <- S.pow trace.(a) (Z.of_int 5)
  | Lookup {wires; table} ->
      let tbl = Tables.find table Csir.table_registry in
      let values = Array.map untag wires in
      let wires = Array.map from_tagged wires in
      let wires = Array.map (Option.map (fun i -> trace.(i))) wires in
      let entry = Option.get Csir.Table.(find wires tbl) in
      Array.iteri (fun i v -> trace.(v) <- entry.(i)) values
  | IsZero wires ->
      let av = trace.(wires.(0)) in
      trace.(wires.(2)) <- S.(if av = zero then one else zero) ;
      trace.(wires.(1)) <- S.(if av = zero then one else S.div_exn one av)
  | IsNotZero wires ->
      let av = trace.(wires.(0)) in
      trace.(wires.(2)) <- S.(if av = zero then zero else one) ;
      trace.(wires.(1)) <- S.(if av = zero then one else S.div_exn one av)
  | Ecc_Ws {x1; y1; x2; y2; x3; y3} ->
      let x1, y1 = (trace.(x1), trace.(y1)) in
      let x2, y2 = (trace.(x2), trace.(y2)) in
      let lambda = S.(sub y2 y1 / sub x2 x1) in
      let x3_v = S.(sub (lambda * lambda) (x1 + x2)) in
      trace.(x3) <- x3_v ;
      trace.(y3) <- S.(sub (lambda * sub x1 x3_v) y1)
  | Ecc_Ed {a; d; x1; y1; x2; y2; x3; y3} ->
      let x1, y1 = (trace.(x1), trace.(y1)) in
      let x2, y2 = (trace.(x2), trace.(y2)) in
      let x1x2 = S.(mul x1 x2) in
      let y1y2 = S.(mul y1 y2) in
      let denom = S.(d * x1x2 * y1y2) in
      let x_res = S.(add (x1 * y2) (x2 * y1) / add one denom) in
      let y_res = S.(sub y1y2 (a * x1x2) / sub one denom) in
      trace.(x3) <- x_res ;
      trace.(y3) <- y_res
  | Ecc_Cond_Ed {a; d; x1; y1; x2; y2; bit; x3; y3} ->
      let x1, y1 = (trace.(x1), trace.(y1)) in
      let x2, y2 = (trace.(x2), trace.(y2)) in
      let b = trace.(bit) in
      let x2' = S.(mul b x2) in
      let y2' = S.(add (mul b y2) (sub one b)) in
      let x1x2' = S.(mul x1 x2') in
      let y1y2' = S.(mul y1 y2') in
      let denom = S.(d * x1x2' * y1y2') in
      let x_res = S.(add (x1 * y2') (x2' * y1) / add one denom) in
      let y_res = S.(sub y1y2' (a * x1x2') / sub one denom) in
      trace.(x3) <- x_res ;
      trace.(y3) <- y_res
  | BitsOfS {nb_bits; shift; l; bits} ->
      let x = trace.(l) |> S.to_z in
      let x = Z.(x + shift) in
      let binary_decomposition = Utils.bool_list_of_z ~nb_bits x in
      List.iter2
        (fun b value -> trace.(b) <- (if value then S.one else S.zero))
        bits
        binary_decomposition
  | Updater ti -> ignore @@ Optimizer.trace_updater ti trace
  | Swap {b; x; y; u; v} ->
      let b, x, y = (trace.(b), trace.(x), trace.(y)) in
      let x_res, y_res = if S.is_zero b then (x, y) else (y, x) in
      trace.(u) <- x_res ;
      trace.(v) <- y_res
  | Poseidon128Full {x0; y0; x1; y1; x2; y2; k; matrix} ->
      let pow5 x = S.pow trace.(x) (Z.of_int 5) in
      let x_vec = [|Array.map pow5 [|x0; x1; x2|]|] |> VS.transpose in
      let y_vec = VS.mul matrix x_vec in
      List.iteri
        (fun i yi -> trace.(yi) <- S.add k.(i) @@ y_vec.(i).(0))
        [y0; y1; y2]
  | Poseidon128Partial
      {a; b; c; a_5; b_5; c_5; x0; y0; x1; y1; x2; y2; k_cols; matrix} ->
      let pow5 x = S.pow x (Z.of_int 5) in
      let ppow5 v = [|v.(0); v.(1); [|pow5 v.(2).(0)|]|] in
      let x_vec = [|[|trace.(x0)|]; [|trace.(x1)|]; [|trace.(x2)|]|] in
      let a_vec = VS.(add (mul matrix @@ ppow5 x_vec) k_cols.(0)) in
      let b_vec = VS.(add (mul matrix @@ ppow5 a_vec) k_cols.(1)) in
      let c_vec = VS.(add (mul matrix @@ ppow5 b_vec) k_cols.(2)) in
      let y_vec = VS.(add (mul matrix @@ ppow5 c_vec) k_cols.(3)) in
      trace.(a) <- a_vec.(2).(0) ;
      trace.(b) <- b_vec.(2).(0) ;
      trace.(c) <- c_vec.(2).(0) ;
      trace.(a_5) <- pow5 trace.(a) ;
      trace.(b_5) <- pow5 trace.(b) ;
      trace.(c_5) <- pow5 trace.(c) ;
      trace.(y0) <- y_vec.(0).(0) ;
      trace.(y1) <- y_vec.(1).(0) ;
      trace.(y2) <- y_vec.(2).(0)
  | AnemoiRound {x0; y0; w; v; x1; y1; kx; ky} ->
      let _w_5', w', v', _u', x1', y1' =
        Gadget_anemoi.Anemoi128.compute_one_round trace.(x0) trace.(y0) kx ky
      in
      trace.(w) <- w' ;
      trace.(v) <- v' ;
      trace.(x1) <- x1' ;
      trace.(y1) <- y1'
  | AnemoiDoubleRound {x0; y0; w0; w1; y1; x2; y2; kx1; ky1; kx2; ky2} ->
      (* First round *)
      let _w_5', w', _v', _u', x1', y1' =
        Gadget_anemoi.Anemoi128.compute_one_round trace.(x0) trace.(y0) kx1 ky1
      in
      (* Computing w *)
      trace.(w0) <- w' ;
      trace.(y1) <- y1' ;
      (* Second round *)
      let _w_5', w', _v', _u', x2', y2' =
        Gadget_anemoi.Anemoi128.compute_one_round x1' y1' kx2 ky2
      in
      trace.(w1) <- w' ;
      trace.(x2) <- x2' ;
      trace.(y2) <- y2'
  | AnemoiCustom {x0; y0; x1; y1; x2; y2; kx1; ky1; kx2; ky2} ->
      (* First round *)
      let _w_5', _w', _v', _u', x1', y1' =
        Gadget_anemoi.Anemoi128.compute_one_round trace.(x0) trace.(y0) kx1 ky1
      in
      trace.(x1) <- x1' ;
      trace.(y1) <- y1' ;
      (* Second round *)
      let _w_5', _w', _v', _u', x2', y2' =
        Gadget_anemoi.Anemoi128.compute_one_round x1' y1' kx2 ky2
      in
      trace.(x2) <- x2' ;
      trace.(y2) <- y2') ;
  trace

let solve : t -> S.t array -> S.t array =
 fun {solvers; initial_size; final_size} inputs ->
  if Array.length inputs <> initial_size then
    failwith
      (Printf.sprintf
         "input size (= %d) != initial_size (= %d)"
         (Array.length inputs)
         initial_size) ;
  let dummy =
    Array.(append inputs (init (final_size - length inputs) (fun _ -> S.zero)))
  in
  List.fold_left solve_one dummy (List.rev solvers)
