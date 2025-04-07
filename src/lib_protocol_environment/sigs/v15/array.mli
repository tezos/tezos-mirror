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

(** This module is a very restricted subset of OCaml's Stdlib Array module.
    There is just enough exposed that you can pass arrays around to some
    functions and such, but not enough that you can easily shoot yourself in the
    foot. (See details below.)

    If you need Arrays with more features, you should check the [FallbackArray]
    module. *)

(** The type of native OCaml arrays. You can construct them with the literal
    syntax ([[|"like"; "so"|]]) or obtain them by deserialising data. *)
type 'a t = 'a array

val concat : 'a t list -> 'a t

val length : 'a t -> int

val to_list : 'a t -> 'a list

(**/**)

(* This Array module is the thinnest shim we can get away with for use with Plonk.
   To avoid any issues with arrays — notably to avoid exceptions when getting
   out of bounds and to avoid any issues with mutability — we shadow [get] and
   [set] as well as a few other functions.

   Note that we do not shadow every other function. E.g., [of_list]. This is
   because those functions might be added later. We only shadow functions which
   may cause serious issues. *)

val get : [`You_cannot_access_array_content_in_the_protocol]

val unsafe_get : [`You_cannot_access_array_content_in_the_protocol]

val set : [`You_cannot_modify_array_content_in_the_protocol]

val unsafe_set : [`You_cannot_modify_array_content_in_the_protocol]

(* The [to_list] conversion above is supported, but [to_seq] can be an issue
   because different indexes could be read at different times and the array
   could have been modified in the mean time (not by the protocol but by an
   underlying function. *)
val to_seq : [`You_cannot_traverse_arrays_lazily_in_the_protocol]

val to_seqi : [`You_cannot_traverse_arrays_lazily_in_the_protocol]

(* Make can create sharing which is error prone *)
val make : [`You_cannot_build_arrays_with_implicit_sharing_in_the_protocol]

val create : [`You_cannot_build_arrays_with_implicit_sharing_in_the_protocol]

val make_matrix :
  [`You_cannot_build_arrays_with_implicit_sharing_in_the_protocol]

val create_float : [`You_cannot_use_floats_in_the_protocol]

val make_float : [`You_cannot_use_floats_in_the_protocol]

(* These functions use indexes which can raise exceptions *)
val sub : [`You_cannot_cut_arrays_in_the_protocol]

val fill : [`You_cannot_fill_arrays_in_the_protocol]

val blit : [`You_cannot_blit_arrays_in_the_protocol]

(* *2 functions can raise exceptions *)
val iter2 : [`You_cannot_traverse_2_arrays_at_once_in_the_protocol]

val map2 : [`You_cannot_traverse_2_arrays_at_once_in_the_protocol]

val combine : [`You_cannot_traverse_2_arrays_at_once_in_the_protocol]

(* side-effects *)
val sort : [`You_cannot_sort_arrays_in_the_protocol]

val stable_sort : [`You_cannot_sort_arrays_in_the_protocol]

val fast_sort : [`You_cannot_sort_arrays_in_the_protocol]

module Floatarray : sig end
