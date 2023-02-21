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

(** Matrices implemented as 2d bigarrays. *)
type t

(** Genarrays. *)
type gen = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t

(** Create a matrix and initializes it to 0.0 *)
val create : lines:int -> cols:int -> t

(** Create and initializes a matrix. *)
val init : lines:int -> cols:int -> f:(int -> int -> float) -> t

(** Number of lines. *)
val dim1 : t -> int

(** Number of columns. *)
val dim2 : t -> int

(** Lines x Columns *)
val shape : t -> int * int

(** [set m i j v] sets the element on line [i] and column [j] of [m] to v. *)
val set : t -> int -> int -> float -> unit

(** [get m i j] gets the element on line [i] and column [j]. *)
val get : t -> int -> int -> float

(** [map m f] creates a new matrix by mapping [f] pointwise to [m]. *)
val map : t -> (float -> float) -> t

(** [column m i] returns the ith column (starting from 0) *)
val column : t -> int -> t

(** [concat_columns_horiz [c1;...;cn]] returns the matrix having
    c1...cn as rows. The function fails if the columns have incoherent
    dimensions. *)
val concat_columns_horiz : t list -> t

(** Converts the 2d bigarray to a genarray. *)
val to_genarray : t -> gen

(** Converts a 2d genarray back to a matrix. *)
val of_genarray : gen -> t

(** Converts to a numpy Python object. *)
val to_numpy : t -> Pytypes.pyobject

(** Converts a vector to a numpy Python object. *)
val to_numpy_vector :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  Pytypes.pyobject

(** Converts from a numpy Python object. *)
val of_numpy : Pytypes.pyobject -> t

val numpy_mul : t -> t -> t

val numpy_add : t -> t -> t

(** Numpy implementations of matrix operations. *)
val numpy_sub : t -> t -> t

(** String representation of matri via numpy. *)
val numpy_show : t -> string
