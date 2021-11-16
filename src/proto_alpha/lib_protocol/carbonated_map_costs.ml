(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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
open Alpha_context
module S = Saturation_repr

(** This is a good enough approximation *)
let log2 x = S.safe_int (1 + S.numbits x)

(** TODO: https://gitlab.com/tezos/tezos/-/issues/2062
    Plugin benchmarked gas.
    Collect benchmark from [Carbonated_map_benchmarks.Find_benchmark].

    The model is similar to the gas model as from [Michelson_v1_gas.map_get].
    The user is responsible for providing the [compare_key_cost] which depends
    on the size of the [key]. See [Carbonated_map_benchmarks.Find_benchmark] for
    an example.
    The rational for the model is:
    - [intercept] is for paying a fixed cost regardless of size.
    - [compare_key_cost] is for the log2 of steps comparing keys
    - [traversal_overhead] is for the overhead of log2 steps walking the tree
 *)
let find_cost ~compare_key_cost ~size =
  let open Gas in
  let intercept = S.safe_int 80 in
  let size = S.safe_int size in
  let compare_cost = log2 size *@ compare_key_cost in
  let traversal_overhead = log2 size *@ S.safe_int 10 in
  intercept +@ compare_cost +@ traversal_overhead

(**
    Modelling the precise overhead of update compared with [find] is tricky.
    The cost of [find] depends on the cost of comparing keys. When the tree
    is recreated, after looking up the element, this cost is no longer a factor.
    On the other hand, if the old map is no longer used, some nodes are going to
    be garbage collected at a later stage which incurs an extra cost.

    We here use the same model as in [Michelson_v1_gas.map_update]. That is
    providing an overestimate by doubling the cost of [find].
  *)
let update_cost ~compare_key_cost ~size =
  Gas.(S.safe_int 2 *@ find_cost ~compare_key_cost ~size)

(** TODO: https://gitlab.com/tezos/tezos/-/issues/2062
    Plugin benchmarked gas.
    Collect benchmark from [Carbonated_map_benchmarks.To_list_benchmark].

    The cost of producing a list of elements is linear in the size of the map
    and does not depend on the size of the elements nor keys.
*)
let to_list_cost ~size = Gas.(S.safe_int 100 *@ S.safe_int size)
