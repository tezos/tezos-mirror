(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module S = Saturation_repr

(* Computed by typing the contract
   "{parameter unit; storage unit; code FAILWITH}"
   and evaluating
   [(8 * Obj.reachable_words (Obj.repr typed_script))]
   where [typed_script] is of type [ex_script] *)
let minimal_size_of_typed_contract_in_bytes = 688

let approximate_cardinal bytes =
  S.safe_int (bytes / minimal_size_of_typed_contract_in_bytes)

let log2 x = S.safe_int (1 + S.numbits x)

let cache_update_constant = S.safe_int 600

let cache_update_coeff = S.safe_int 57

(* Cost of calling [Environment_cache.update]. *)
let cache_update ~cache_size_in_bytes =
  let approx_card = approximate_cardinal cache_size_in_bytes in
  Gas_limit_repr.atomic_step_cost
    S.(add cache_update_constant (mul cache_update_coeff (log2 approx_card)))

(* Cost of calling [Environment_cache.find].
   This overapproximates [cache_find] slightly. *)
let cache_find = cache_update
