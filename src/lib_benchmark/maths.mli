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

(** Vectors with elements indexed by ints *)
type vector = int Vector.t

(** {e Read-only} matrices with elements indexed by pairs (column, row) *)
type matrix = (int * int) Matrix.t

(** {e Write-only} matrices with elements indexed by pairs (column, row) *)
type out_matrix = (int * int) Matrix.out

(** Number of elements of a vector *)
val vec_dim : vector -> int

(** Number of columns of a matrix *)
val col_dim : matrix -> int

(** Number of rows of a matrix *)
val row_dim : matrix -> int

(** Create a {e read-only} [matrix] overlay over an array of arrays.
    Note how we switch from row major to column major in
    order to comply to [Linalg]'s defaults. *)
val matrix_of_array_array : float array array -> matrix

(** Create a {e write-only} overlay over an array of arrays.
    Note how we switch from row major to column major in
    order to comply to [Linalg]'s defaults. *)
val out_matrix_of_array_array : float array array -> out_matrix

(** [mm_ out lhs rhs] computes the matrix product [lhs x rhs] and stores it in [out] *)
val mm_ : out_matrix -> matrix -> matrix -> unit

(** Create a [float array] from a [vector] *)
val vector_to_array : vector -> float array

(** Create a [vector] from a float array *)
val vector_of_array : float array -> vector

(** Construct a sequence out of the elements of a vector *)
val vector_to_seq : vector -> float Seq.t

(** Map a scalar function on the rows of a matrix, yielding a column vector *)
val map_rows : (vector -> float) -> matrix -> vector

(** Map a scalar function on the rows of a matrix, yielding a column vector *)
val mapi_rows : (int -> vector -> float) -> matrix -> vector

(** Map a scalar function on the columns of a matrix, yielding a row vector *)
val map_cols : (vector -> float) -> matrix -> vector

(** An empty matrix. *)
val empty_matrix : matrix

(** l2 norm of a vector *)
val l2_norm : vector -> float

(** Mean square error *)
val mse : vector -> float

(** Average *)
val average : vector -> float

(** Standard deviation *)
val std : vector -> float

(** An encoding for vectors. *)
val vector_encoding : vector Data_encoding.t

(** Pretty printing vectors. *)
val pp_vec : Format.formatter -> vector -> unit
