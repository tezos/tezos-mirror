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

   This module implements arrays equipped with accessors that cannot
   raise exceptions. Reading out of the bounds of the arrays return a
   fallback value fixed at array construction time, writing out of the
   bounds of the arrays is ignored.

*)

(** The type for array containing values of type ['a]. *)
type 'a t

(** [make len v] builds an array [a] initialized [len] cells with
   [v]. The value [v] is the fallback value for [a]. *)
val make : int -> 'a -> 'a t

(** [of_list ~fallback ~proj l] builds a fallback array [a] of length
    [List.length l] where each cell [i] is initialized by [proj (List.nth l i)]
    and the fallback value is [fallback]. *)
val of_list : fallback:'b -> proj:('a -> 'b) -> 'a list -> 'b t

(** [fallback a] returns the fallback value for [a]. *)
val fallback : 'a t -> 'a

(** [length a] returns the length of [a]. *)
val length : 'a t -> int

(** [get a idx] returns the contents of the cell of index [idx] in
   [a]. If [idx] < 0 or [idx] >= [length a], [get a idx] =
   [fallback a]. *)
val get : 'a t -> int -> 'a

(** [set a idx value] updates the cell of index [idx] with [value].
    If [idx] < 0 or [idx] >= [length a], [a] is unchanged. *)
val set : 'a t -> int -> 'a -> unit

(** [iter f a] iterates [f] over the cells of [a] from the
   cell indexed [0] to the cell indexed [length a - 1]. *)
val iter : ('a -> unit) -> 'a t -> unit

(** [map f a] computes a new array obtained by applying [f] to each
   cell contents of [a]. Notice that the fallback value of the new
   array is [f (fallback a)]. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [fold f a init] traverses [a] from the cell indexed [0] to the
   cell indexed [length a - 1] and transforms [accu] into [f accu x]
   where [x] is the content of the cell under focus. [accu] is
   [init] on the first iteration. *)
val fold : ('b -> 'a -> 'b) -> 'a t -> 'b -> 'b

(** [fold_map f a init fallback] traverses [a] from the cell indexed
   [0] to the cell indexed [length a - 1] and transforms [accu] into
   [fst (f accu x)] where [x] is the content of the cell under
   focus. [accu] is [init] on the first iteration. The function also
   returns a fresh array containing [snd (f accu x)] for each [x].
   [fallback] is required to initialize a fresh array before it can be
   filled. *)
val fold_map : ('b -> 'a -> 'b * 'c) -> 'a t -> 'b -> 'c -> 'b * 'c t
