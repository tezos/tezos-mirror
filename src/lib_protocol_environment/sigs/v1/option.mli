(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Option values.

    Option values explicitly indicate the presence or absence of a value.

    @since 4.08 *)

(** {1:options Options} *)

type 'a t = 'a option = None | Some of 'a (**)
(** The type for option values. Either [None] or a value [Some v]. *)

val none : 'a option
(** [none] is [None]. *)

val some : 'a -> 'a option
(** [some v] is [Some v]. *)

val value : 'a option -> default:'a -> 'a
(** [value o ~default] is [v] if [o] is [Some v] and [default] otherwise. *)

val bind : 'a option -> ('a -> 'b option) -> 'b option
(** [bind o f] is [f v] if [o] is [Some v] and [None] if [o] is [None]. *)

val join : 'a option option -> 'a option
(** [join oo] is [Some v] if [oo] is [Some (Some v)] and [None] otherwise. *)

val map : ('a -> 'b) -> 'a option -> 'b option
(** [map f o] is [None] if [o] is [None] and [Some (f v)] is [o] is [Some v]. *)

val fold : none:'a -> some:('b -> 'a) -> 'b option -> 'a
(** [fold ~none ~some o] is [none] if [o] is [None] and [some v] if [o] is
    [Some v]. *)

val iter : ('a -> unit) -> 'a option -> unit
(** [iter f o] is [f v] if [o] is [Some v] and [()] otherwise. *)

(** {1:preds Predicates and comparisons} *)

val is_none : 'a option -> bool
(** [is_none o] is [true] iff [o] is [None]. *)

val is_some : 'a option -> bool
(** [is_some o] is [true] iff [o] is [Some o]. *)

val equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
(** [equal eq o0 o1] is [true] iff [o0] and [o1] are both [None] or if
    they are [Some v0] and [Some v1] and [eq v0 v1] is [true]. *)

val compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int
(** [compare cmp o0 o1] is a total order on options using [cmp] to compare
    values wrapped by [Some _]. [None] is smaller than [Some _] values. *)

(** {1:convert Converting} *)

val to_result : none:'e -> 'a option -> ('a, 'e) result
(** [to_result ~none o] is [Ok v] if [o] is [Some v] and [Error none]
    otherwise. *)

val to_list : 'a option -> 'a list
(** [to_list o] is [[]] if [o] is [None] and [[v]] if [o] is [Some v]. *)


(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** [x >>= f] is an infix notation for [apply ~f x] *)
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option

(** [x >>| f] is an infix notation for [map ~f x] *)
val ( >>| ) : 'a option -> ('a -> 'b) -> 'b option

(** First input of form [Some x], or [None] if both are [None] *)
val first_some : 'a option -> 'a option -> 'a option

(** [pp ~default pp fmt x] pretty-print value [x] using [pp]
    or [default] (["None"] by default) string if there is no value. *)
val pp :
  ?default:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a option ->
  unit
