(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2023 DaiLambda, Inc., <contact@dailambda.jp>                *)
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

(* model encoding/DECODING_MICHELINE *)
let cost_DECODING_MICHELINE size1 size2 size3 =
  let open S.Syntax in
  let traversal_cost = S.safe_int 60 in
  let string_per_byte_cost = S.safe_int 10 in
  let z_per_byte_cost = S.safe_int 10 in
  (traversal_cost * size1) + (z_per_byte_cost * size2)
  + (string_per_byte_cost * size3)

(* model encoding/DECODING_MICHELINE *)
let cost_DECODING_MICHELINE_bytes size =
  let open S.Syntax in
  S.safe_int 20 * size

(* model encoding/ENCODING_MICHELINE *)
let cost_ENCODING_MICHELINE size1 size2 size3 =
  let open S.Syntax in
  let traversal_cost = S.safe_int 100 in
  let string_per_byte_cost = S.safe_int 10 in
  let z_per_byte_cost = S.safe_int 25 in
  (traversal_cost * size1) + (z_per_byte_cost * size2)
  + (string_per_byte_cost * size3)

(* model encoding/ENCODING_MICHELINE *)
let cost_ENCODING_MICHELINE_bytes size =
  let open S.Syntax in
  S.safe_int 33 * size

(* Cost of running [strip_locations] on a term with [size] nodes.
   Note that [strip_locations] will reallocate a fresh Micheline tree.
   This only depends on the total number of nodes (not the size of
   the leaves).

   The run-time cost of [strip_locations] is benchmarked by
   [Tezos_shell_benchmarks.Micheline_benchmarks.Micheline_strip_locations].
*)
(* model micheline/strip_locations_micheline *)
(* fun size -> (51. * size) *)
let cost_strip_locations_micheline size =
  let open S.Syntax in
  let size = S.safe_int size in
  let v0 = size in
  v0 * S.safe_int 51

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2049
   Plugin benchmarked gas.
   Replace this definition, copied from [cost_michelines_strip_locations].
*)
(* Cost of running [strip_annotations] on a term with [size] nodes.
   Note that [strip_annotations] will reallocate a fresh Micheline tree.
   This only depends on the total number of nodes (not the size of
   the leaves).
*)
(* model script_repr/strip_annotations *)
(* fun size -> (51. * size) *)
let cost_strip_annotations size =
  let open S.Syntax in
  let size = S.safe_int size in
  let v0 = size in
  v0 * S.safe_int 51
