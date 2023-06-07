(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module BA2 = Bigarray.Array2

type t = (float, Bigarray.float64_elt, Bigarray.c_layout) BA2.t

type gen = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t

let create ~(lines : int) ~(cols : int) =
  let mat = BA2.create Bigarray.Float64 Bigarray.C_layout lines cols in
  BA2.fill mat 0.0 ;
  mat

let init ~(lines : int) ~(cols : int) ~f =
  let mat = BA2.create Bigarray.Float64 Bigarray.C_layout lines cols in
  for i = 0 to lines - 1 do
    for j = 0 to cols - 1 do
      BA2.set mat i j (f i j)
    done
  done ;
  mat

let dim1 (mat : t) = BA2.dim1 mat

let dim2 (mat : t) = BA2.dim2 mat

let shape (mat : t) = (dim1 mat, dim2 mat)

let set (mat : t) i j v = BA2.set mat i j v

let get (mat : t) i j = BA2.get mat i j

let map (mat : t) (f : float -> float) =
  let res = create ~lines:(dim1 mat) ~cols:(dim2 mat) in
  for i = 0 to dim1 mat - 1 do
    for j = 0 to dim2 mat - 1 do
      set res i j (f (get mat i j))
    done
  done ;
  res

let column (mat : t) (i : int) =
  let lines, cols = shape mat in
  if i >= cols then
    let msg =
      Printf.sprintf
        "Matrix.column: matrix has shape %d x %d, column %d out of bounds"
        lines
        cols
        i
    in
    Stdlib.failwith msg
  else
    let col = create ~lines ~cols:1 in
    for k = 0 to lines - 1 do
      set col k 0 (get mat k i)
    done ;
    col

let is_column (m : t) : bool =
  let _, cols = shape m in
  cols = 1

let all_equal (l : int list) : int option =
  let result =
    List.sort_uniq (fun (x : int) (y : int) -> Stdlib.compare x y) l
  in
  match result with [uniq] -> Some uniq | _ -> None

let concat_columns_horiz (columns : t list) : t =
  let cols = List.length columns in
  if cols = 0 then Stdlib.failwith "concat_columns_horiz: empty list of columns" ;
  if not (List.for_all is_column columns) then
    Stdlib.failwith "concat_columns_horiz: invalid argument" ;
  let row_dims = List.map dim1 columns in
  match all_equal row_dims with
  | None -> Stdlib.failwith "concat_columns_horiz: invalid argument"
  | Some rows ->
      let columns = Array.of_list columns in
      init ~lines:rows ~cols ~f:(fun l c -> get columns.(c) l 0)

let to_genarray (m : t) : gen = Bigarray.genarray_of_array2 m

let of_genarray (a : gen) : t = Bigarray.array2_of_genarray a

let bigarray_copy (a : gen) =
  let dims = Bigarray.Genarray.dims a in
  let copy = Bigarray.Genarray.create Bigarray.Float64 Bigarray.C_layout dims in
  Bigarray.Genarray.blit a copy ;
  copy

let to_numpy (mat : t) : Pytypes.pyobject =
  Numpy.of_bigarray (bigarray_copy (to_genarray mat))

let to_numpy_vector vec =
  Numpy.of_bigarray (bigarray_copy (Bigarray.genarray_of_array1 vec))

let of_numpy (npy : Pytypes.pyobject) : t =
  let genmat = Numpy.to_bigarray Bigarray.Float64 Bigarray.c_layout npy in
  let genmat = bigarray_copy genmat in
  let ndims = Bigarray.Genarray.num_dims genmat in
  let reshaped =
    if ndims <= 0 || ndims > 2 then
      Stdlib.failwith "numpy_to_mat: input array has dims <= 0 || dims > 2"
    else if ndims = 1 then
      Bigarray.reshape genmat [|Bigarray.Genarray.nth_dim genmat 0; 1|]
    else genmat
  in
  of_genarray reshaped

let numpy_mul (mat1 : t) (mat2 : t) =
  let py_mat =
    Py.Module.get_function
      (Pymodules.numpy ())
      "matmul"
      [|to_numpy mat1; to_numpy mat2|]
  in
  of_numpy py_mat

let numpy_add (mat1 : t) (mat2 : t) =
  let py_mat =
    Py.Module.get_function
      (Pymodules.numpy ())
      "add"
      [|to_numpy mat1; to_numpy mat2|]
  in
  of_numpy py_mat

let numpy_sub (mat1 : t) (mat2 : t) =
  let py_mat =
    Py.Module.get_function
      (Pymodules.numpy ())
      "sub"
      [|to_numpy mat1; to_numpy mat2|]
  in
  of_numpy py_mat

let numpy_show (mat : t) : string =
  let py_str =
    Py.Module.get_function (Pymodules.numpy ()) "array2string" [|to_numpy mat|]
  in
  Py.String.to_string py_str
