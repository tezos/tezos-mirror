(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type location = Micheline.canonical_location

type annot = Micheline.annot

type expr = Michelson_v1_primitives.prim Micheline.canonical

type error += Lazy_script_decode (* `Permanent *)

type lazy_expr = expr Data_encoding.lazy_t

type node = (location, Michelson_v1_primitives.prim) Micheline.node

val location_encoding : location Data_encoding.t

val expr_encoding : expr Data_encoding.t

val lazy_expr_encoding : lazy_expr Data_encoding.t

val lazy_expr : expr -> lazy_expr

type t = {code : lazy_expr; storage : lazy_expr}

val encoding : t Data_encoding.encoding

val deserialization_cost_estimated_from_bytes : int -> Gas_limit_repr.cost

val deserialized_cost : expr -> Gas_limit_repr.cost

val serialized_cost : bytes -> Gas_limit_repr.cost

val bytes_node_cost : bytes -> Gas_limit_repr.cost

val force_decode_cost : lazy_expr -> Gas_limit_repr.cost

val force_decode : lazy_expr -> expr tzresult

val force_bytes_cost : lazy_expr -> Gas_limit_repr.cost

val force_bytes : lazy_expr -> bytes tzresult

val unit_parameter : lazy_expr

val is_unit_parameter : lazy_expr -> bool

val strip_annotations : node -> node

val strip_locations_cost : node -> Gas_limit_repr.cost
