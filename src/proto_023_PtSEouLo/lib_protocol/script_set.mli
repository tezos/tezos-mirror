(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(** Functions to ease the manipulation of sets of values in Michelson.

    A set in Michelson is a collection of type-homegeneous values along with the
    functions that operate on the structure (through a first-class module). In
    particular, the {!size} function runs in constant time.
*)

open Script_typed_ir

val make : (module Boxed_set with type elt = 'elt) -> 'elt set

val get : 'elt set -> (module Boxed_set with type elt = 'elt)

(** [empty cmp_ty] creates a set module where elements have size
    [Gas_comparable_input_size.size_of_comparable_value cmp_ty] and are compared
    with [Script_comparable.compare_comparable cmp_ty] (used for sorting values,
    which ensures a reasonable complexity of the set functions).
    The function returns an empty set packaged as a first-class set module. *)
val empty : 'a comparable_ty -> 'a set

val fold : ('elt -> 'acc -> 'acc) -> 'elt set -> 'acc -> 'acc

(** [update v true set] adds [v] to [set], and [update v false set] removes [v]
    from [set]. *)
val update : 'a -> bool -> 'a set -> 'a set

val mem : 'elt -> 'elt set -> bool

(** [size set] runs in constant time. *)
val size : 'elt set -> Script_int.n Script_int.num
