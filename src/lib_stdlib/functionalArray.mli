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

(**

   This module implements functional arrays equipped with accessors
   that cannot raise exceptions following the same design principles
   as {!FallbackArray}:

   Reading out of the bounds of the arrays return a fallback value
   fixed at array construction time, writing out of the bounds of the
   arrays is ignored.

   Contrary to {!FallbackArray}, writing generates a fresh array.

   Please notice that this implementation is naive and should only
   be used for small arrays. If there is a need for large functional
   arrays, it is recommended to implement Backer's trick to get
   constant-time reads and writes for sequences of mutations applied
   to the same array.

*)

(** The type for array containing values of type ['a]. *)
type 'a t

(** [make len v] builds an array [a] initializing [len] cells with
   [v]. The value [v] is the fallback value for [a]. *)
val make : int -> 'a -> 'a t

(** [init len v make] builds an array [a] initializing [len] cells
    where the [i]-th cell value is [make i]. The value [v] is the
    fallback value for [a]. *)
val init : int -> 'a -> (int -> 'a) -> 'a t

(** [fallback a] returns the fallback value for [a]. *)
val fallback : 'a t -> 'a

(** [length a] returns the length of [a]. *)
val length : 'a t -> int

(** [get a idx] returns the contents of the cell of index [idx] in
   [a]. If [idx] < 0 or [idx] >= [length a], [get a idx] =
   [fallback a]. *)
val get : 'a t -> int -> 'a

(** [set a idx value] returns a new array identical to [a] except
   that the cell of index [idx] with [value].
   If [idx] < 0 or [idx] >= [length a], returns a copy of [a]. *)
val set : 'a t -> int -> 'a -> 'a t

(** [iter f a] iterates [f] over the cells of [a] from the
   cell indexed [0] to the cell indexed [length a - 1]. *)
val iter : ('a -> unit) -> 'a t -> unit

(** [iteri f a] iterates [f] over the cells of [a] from the
   cell indexed [0] to the cell indexed [length a - 1] passing
   the cell index to [f]. *)
val iteri : (int -> 'a -> unit) -> 'a t -> unit

(** [map a] computes a new array obtained by applying [f] to each
   cell contents of [a]. Notice that the fallback value of the new
   array is [f (fallback a)]. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [mapi f a] computes a new array obtained by applying [f] to each
   cell contents of [a] passing the index of this cell to [i].
    Notice that the fallback value of the new array is [f (-1) (fallback a)]. *)
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

(** [fold f a init] traverses [a] from the cell indexed [0] to the
   cell indexed [length a - 1] and transforms [accu] into [f accu x]
   where [x] is the content of the cell under focus. [accu] is
   [init] on the first iteration. *)
val fold : ('b -> 'a -> 'b) -> 'a t -> 'b -> 'b

(** [fold_map f a init fallback] traverses [a] from the cell indexed
   [0] to the cell indexed [length a - 1] and transforms [accu] into
   [fst (f accu x)] where [x] is the content of the cell under
   focus. [accu] is [init] on the first iteration. The function also
   returns a fresh array containing [snd (f accu x)] for each [x] and
   initialized with the given [fallback]. *)
val fold_map : ('b -> 'a -> 'b * 'c) -> 'a t -> 'b -> 'c -> 'b * 'c t
