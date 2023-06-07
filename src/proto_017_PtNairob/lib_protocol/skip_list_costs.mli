(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** [model_next ~length] returns the gas cost of inserting a cell in a
    skip list of a given [length], assuming basis equals 2. *)
val model_next : length:Z.t -> Saturation_repr.may_saturate Saturation_repr.t

(** [model_hash_cell ~backpointers_count] returns the gas cost of
   hashing the last cell with a given [backpointers_count], assuming
   basis equals 2. *)
val model_hash_cell :
  backpointers_count:int -> Saturation_repr.may_saturate Saturation_repr.t

(** [model_hash_cell_computed_backpointers_count ~index] same as
    {!model_hash_cell} but compute the number of backpointers a specific cell
    will have. Assuming basis equals 2. *)
val model_hash_cell_computed_backpointers_count :
  index:Z.t -> Saturation_repr.may_saturate Saturation_repr.t
