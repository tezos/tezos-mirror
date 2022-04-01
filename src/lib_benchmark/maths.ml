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

module Vector = Linalg.Vec.Float
module Matrix = Linalg.Mat.Float

type vector = int Vector.t

type matrix = (int * int) Matrix.t

type out_matrix = (int * int) Matrix.out

let vec_dim (v : vector) = Linalg.Tensor.Int.numel @@ Vector.idim v

let col_dim (m : matrix) = Linalg.Tensor.Int.numel @@ Matrix.cols m

let row_dim (m : matrix) = Linalg.Tensor.Int.numel @@ Matrix.rows m

let matrix_of_array_array (m : float array array) : matrix =
  let r = Array.length m in
  let c = Array.length m.(0) in
  Matrix.make (Linalg.Tensor.Int.rank_two c r) @@ fun (c, r) -> m.(r).(c)

let out_matrix_of_array_array (m : float array array) : out_matrix =
  let r = Array.length m in
  let c = Array.length m.(0) in
  Linalg.OVec (Linalg.Tensor.Int.rank_two c r, fun (c, r) x -> m.(r).(c) <- x)

let mm_ (out : out_matrix) (m1 : matrix) (m2 : matrix) =
  let cols1 = col_dim m1 in
  let rows2 = row_dim m2 in
  assert (cols1 = rows2) ;
  let cols2 = col_dim m2 in
  let rows1 = row_dim m1 in
  let out_shape = Matrix.odim out in
  let out_cols = Linalg.Tensor.Int.numel @@ Linalg.Tensor.Int.fst out_shape in
  let out_rows = Linalg.Tensor.Int.numel @@ Linalg.Tensor.Int.snd out_shape in
  assert (out_cols = cols2 && out_rows = rows1) ;
  let (Linalg.OVec (_, write)) = out in
  for c = 0 to cols2 - 1 do
    for r = 0 to rows1 - 1 do
      let acc = ref 0.0 in
      for k = 0 to cols1 - 1 do
        acc :=
          !acc +. (Matrix.unsafe_get m2 (c, k) *. Matrix.unsafe_get m1 (k, r))
      done ;
      write (c, r) !acc
    done
  done

let vector_to_array (v : vector) : float array =
  let rows = Linalg.Tensor.Int.numel @@ Vector.idim v in
  Array.init rows (fun i -> Vector.get v i)

let vector_of_array (array : float array) =
  Linalg.Vec.Float.make
    (Linalg.Tensor.Int.rank_one (Array.length array))
    (Array.get array)

let vector_to_seq vec =
  let numel = Linalg.Tensor.Int.numel (Linalg.Vec.Float.idim vec) in
  let rec loop i () =
    if i = numel then Seq.Nil
    else Seq.Cons (Linalg.Vec.Float.get vec i, loop (i + 1))
  in
  loop 0

let map_rows f matrix =
  Vector.make (Matrix.rows matrix) (fun r ->
      let row = Matrix.row matrix r in
      f row)

let mapi_rows f matrix =
  Vector.make (Matrix.rows matrix) (fun r ->
      let row = Matrix.row matrix r in
      f r row)

let map_cols f matrix =
  Vector.make (Matrix.cols matrix) (fun c ->
      let col = Matrix.col matrix c in
      f col)

let empty_matrix =
  Matrix.make (Linalg.Tensor.Int.rank_two 0 0) @@ fun _ -> assert false

let l2_norm vec =
  vec |> Vector.map (fun x -> x *. x) |> Vector.reduce ( +. ) 0.0 |> sqrt

let mse vec =
  (vec |> Vector.map (fun x -> x *. x) |> Vector.reduce ( +. ) 0.0)
  /. float_of_int (vec_dim vec)

let average vec = Vector.reduce ( +. ) 0.0 vec /. float_of_int (vec_dim vec)

let std vec =
  let avg = average vec in
  let sum_of_squares =
    vec
    |> Vector.map (fun x ->
           let c = x -. avg in
           c *. c)
    |> Vector.reduce ( +. ) 0.0
  in
  sqrt (sum_of_squares /. float_of_int (vec_dim vec))

let vector_encoding : vector Data_encoding.t =
  Data_encoding.(conv vector_to_array vector_of_array (array float))

let pp_vec fmtr vec =
  let elts = List.of_seq @@ vector_to_seq vec in
  Format.pp_print_list
    ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ",")
    Format.pp_print_float
    fmtr
    elts
