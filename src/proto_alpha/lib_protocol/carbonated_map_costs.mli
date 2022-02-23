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

(** The [Carbonated_map_costs] module contains gas cost functions for
    [Carbonated_map].
*)

(** [find_cost ~compare_key_cost ~size] returns the gas cost for looking up an
    element from a map of size [size]. The user of this function is responsible
    for providing a correct value of [compare_key_cost], representing the cost
    of comparing elements with a given key.
*)
val find_cost :
  compare_key_cost:Alpha_context.Gas.cost -> size:int -> Alpha_context.Gas.cost

(** [update_cost ~compare_key_cost ~size] returns the gas cost for updating an
    element in a map of size [size]. The user of this function is responsible
    for providing a correct value of [compare_key_cost], representing the cost
    of comparing elements with a given key. *)
val update_cost :
  compare_key_cost:Alpha_context.Gas.cost -> size:int -> Alpha_context.Gas.cost

(** [to_list_cost ~size] returns the cost of transforming a map of size [size]
    to a list of key-value pairs. *)
val to_list_cost : size:int -> Alpha_context.Gas.cost
