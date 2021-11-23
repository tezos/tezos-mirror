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

open Micheline

(** [diff ~prev ~current ()] computes the difference between expressions [prev]
    and [current]. It depends on primitives being ordinary strings so that they
    can be displayed easily. To simplify things, differences in annotations are
    ignored. Annotations from [prev] are preserved.

    Returns [None] if expressions are identical or [Some d] if they're not,
    where [d] is a copy of [prev] with information about differences with
    respect to [current] displayed as comments.

    Parts of the expression that are present in [prev] but missing in [current]
    get "-" comment. Parts present in [current] but missing in [prev] get "+"
    comment. Parts which appear in both versions are the same as in [prev] and
    [current] version, if different, is put after a "->" mark in the comment. *)
val diff :
  prev:(_, string) node ->
  current:(_, string) node ->
  unit ->
  Micheline_printer.node option
