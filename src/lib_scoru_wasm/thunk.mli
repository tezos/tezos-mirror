(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3388
   Provide a docstring for every functions and types introduced by
   this module. *)

(** This module provides a means of transparently manipulating a
    hierarchy of values stored in a tree. These values are lazily
    loaded on demand, modified in-place, and then encoded back in the
    tree once the computation is done.

    In a nutshell, a ['a thunk] is a lazily loaded collection of
    values whose hierarchy is determined by ['a]. A ['a schema] is a
    declarative description of your data model, that is how to encode
    and decode your values. Developers familiar with [Data_encoding]
    will find the API of [schema] familiar.  Then, ['a lens] is a way
    to traverse a [thunk], reading what’s needing from a tree, and
    modifying in-place your datas. *)

(** ['a value] denotes a value in a data-model of type ['a] that can be
    read from and write to a tree. *)
type 'a value

(** [('a, 'b) dict] denotes a dictionary in a data-model that
    associates sub-hierarchies of value determined by type [b] to keys
    of type [a] (said keys need to be serializable to [string]). *)
type ('a, 'b) dict

module Make (T : Tree.S) : sig
  type tree = T.tree

  module Schema : sig
    type !'a t

    val encoding : 'a Data_encoding.t -> 'a value t

    val custom :
      encoder:(tree -> string list -> 'a -> tree Lwt.t) ->
      decoder:(tree -> 'a option Lwt.t) ->
      'a value t

    type !'a field

    val folders : string list -> 'a t -> 'a t

    val req : string -> 'a t -> 'a field

    val obj2 : 'a field -> 'b field -> ('a * 'b) t

    val obj3 : 'a field -> 'b field -> 'c field -> ('a * 'b * 'c) t

    val obj4 :
      'a field -> 'b field -> 'c field -> 'd field -> ('a * 'b * 'c * 'd) t

    val obj5 :
      'a field ->
      'b field ->
      'c field ->
      'd field ->
      'e field ->
      ('a * 'b * 'c * 'd * 'e) t

    val dict : ('a -> string) -> 'b t -> ('a, 'b) dict t
  end

  type 'a schema = 'a Schema.t

  type !'a t

  type !'a thunk = 'a t

  val decode : 'a schema -> tree -> 'a thunk

  val encode : tree -> 'a thunk -> tree Lwt.t

  val find : 'a value thunk -> 'a option Lwt.t

  val get : 'a value thunk -> 'a Lwt.t

  val set : 'a value thunk -> 'a -> unit

  val cut : 'a thunk -> unit

  type ('a, 'b) lens = 'a thunk -> 'b thunk Lwt.t

  val ( ^. ) : ('a, 'b) lens -> ('b, 'c) lens -> ('a, 'c) lens

  val tup2_0 : ('a * 'b, 'a) lens

  val tup2_1 : ('a * 'b, 'b) lens

  val tup3_0 : ('a * 'b * 'c, 'a) lens

  val tup3_1 : ('a * 'b * 'c, 'b) lens

  val tup3_2 : ('a * 'b * 'c, 'c) lens

  val tup4_0 : ('a * 'b * 'c * 'd, 'a) lens

  val tup4_1 : ('a * 'b * 'c * 'd, 'b) lens

  val tup4_2 : ('a * 'b * 'c * 'd, 'c) lens

  val tup4_3 : ('a * 'b * 'c * 'd, 'd) lens

  val tup5_0 : ('a * 'b * 'c * 'd * 'e, 'a) lens

  val tup5_1 : ('a * 'b * 'c * 'd * 'e, 'b) lens

  val tup5_2 : ('a * 'b * 'c * 'd * 'e, 'c) lens

  val tup5_3 : ('a * 'b * 'c * 'd * 'e, 'd) lens

  val tup5_4 : ('a * 'b * 'c * 'd * 'e, 'e) lens

  val entry : 'a -> (('a, 'b) dict, 'b) lens

  module Lazy_list : sig
    type !'a t

    val schema : 'a schema -> 'a t schema

    val length : 'a t thunk -> int32 Lwt.t

    val nth : check:bool -> int32 -> ('a t, 'a) lens

    val alloc_cons : 'a t thunk -> (int32 * 'a thunk) Lwt.t

    val cons : 'a value t thunk -> 'a -> int32 Lwt.t
  end

  module Syntax : sig
    val ( ^-> ) : 'a thunk -> ('a, 'b) lens -> 'b thunk Lwt.t

    val ( let*^ ) : 'a value thunk Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

    val ( let*^? ) : 'a value thunk Lwt.t -> ('a option -> 'b Lwt.t) -> 'b Lwt.t

    val ( ^:= ) : 'a value thunk Lwt.t -> 'a -> unit Lwt.t
  end
end
