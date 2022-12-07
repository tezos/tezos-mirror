(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* The model for read accesses is the following:

   cost(path_length, read_bytes) = 200_000 + 5000 * path_length + 2 * read_bytes
*)
let read_access ~path_length ~read_bytes =
  let open Saturation_repr in
  let base_cost = safe_int (200_000 + (5000 * path_length)) in
  Gas_limit_repr.atomic_step_cost
    (add base_cost (mul (safe_int 2) (safe_int read_bytes)))

(* The model for write accesses is the following:

   cost(written_bytes) = 200_000 + 4 * written_bytes
*)
let write_access ~written_bytes =
  let open Saturation_repr in
  Gas_limit_repr.atomic_step_cost
    (add (safe_int 200_000) (mul (safe_int 4) (safe_int written_bytes)))

let list_key_values_step_cost = Saturation_repr.safe_int 117

let list_key_values_intercept = Saturation_repr.safe_int 470

let list_key_values_traverse ~size =
  Saturation_repr.(
    add
      list_key_values_intercept
      (mul (safe_int size) list_key_values_step_cost))
