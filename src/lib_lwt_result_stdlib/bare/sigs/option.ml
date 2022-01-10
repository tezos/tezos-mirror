(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** A replacement for {!Stdlib.Option} which
    - is exception-safe,
    - includes Lwt-, result-, and Lwt-result-aware traversors.

    See {!Lwtreslib} and {!Seq} for general description of traversors and the
    meaning of [_s], [_e], and [_es] suffixes. *)
module type S = sig
  type 'a t = 'a option = None | Some of 'a

  val none : 'a option

  val none_e : ('a option, 'trace) result

  val none_s : 'a option Lwt.t

  val none_es : ('a option, 'trace) result Lwt.t

  val some : 'a -> 'a option

  val some_unit : unit option

  val some_unit_e : (unit option, 'error) result

  val some_unit_s : unit option Lwt.t

  val some_unit_es : (unit option, 'error) result Lwt.t

  val some_nil : 'a list option

  val some_nil_e : ('a list option, 'error) result

  val some_nil_s : 'a list option Lwt.t

  val some_nil_es : ('a list option, 'error) result Lwt.t

  val some_true : bool option

  val some_true_e : (bool option, 'error) result

  val some_true_s : bool option Lwt.t

  val some_true_es : (bool option, 'error) result Lwt.t

  val some_false : bool option

  val some_false_e : (bool option, 'error) result

  val some_false_s : bool option Lwt.t

  val some_false_es : (bool option, 'error) result Lwt.t

  val some_e : 'a -> ('a option, 'trace) result

  val some_s : 'a -> 'a option Lwt.t

  val some_es : 'a -> ('a option, 'trace) result Lwt.t

  val value : 'a option -> default:'a -> 'a

  val value_e : 'a option -> error:'trace -> ('a, 'trace) result

  val value_f : 'a option -> default:(unit -> 'a) -> 'a

  val value_fe : 'a option -> error:(unit -> 'trace) -> ('a, 'trace) result

  val bind : 'a option -> ('a -> 'b option) -> 'b option

  val join : 'a option option -> 'a option

  (** [either] picks the first [Some _] value of its arguments if any.
      More formally, [either (Some x) _] is [Some x], [either None (Some y)] is
      [Some y], and [either None None] is [None]. *)
  val either : 'a option -> 'a option -> 'a option

  val either_f : 'a option -> (unit -> 'a option) -> 'a option

  val merge : ('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option

  val merge_e :
    ('a -> 'a -> ('a, 'e) result) ->
    'a option ->
    'a option ->
    ('a option, 'e) result

  val merge_s :
    ('a -> 'a -> 'a Lwt.t) -> 'a option -> 'a option -> 'a option Lwt.t

  val merge_es :
    ('a -> 'a -> ('a, 'e) result Lwt.t) ->
    'a option ->
    'a option ->
    ('a option, 'e) result Lwt.t

  val map : ('a -> 'b) -> 'a option -> 'b option

  val map_s : ('a -> 'b Lwt.t) -> 'a option -> 'b option Lwt.t

  val map_e :
    ('a -> ('b, 'trace) result) -> 'a option -> ('b option, 'trace) result

  val map_es :
    ('a -> ('b, 'trace) result Lwt.t) ->
    'a option ->
    ('b option, 'trace) result Lwt.t

  val fold : none:'a -> some:('b -> 'a) -> 'b option -> 'a

  val fold_s : none:'a -> some:('b -> 'a Lwt.t) -> 'b option -> 'a Lwt.t

  val fold_f : none:(unit -> 'a) -> some:('b -> 'a) -> 'b option -> 'a

  (** [filter p o] is [Some x] iff [o] is [Some x] and [p o] is [true].

      In other words, [filter] is like [List.filter] if [option] is the type of
      lists of either zero or one elements. In fact, the following equality
      holds for all [p] and for all [o]:
      [Option.filter p o = List.hd (List.filter p (Option.to_list o))]

      The other [filter] variants below are also equivalent to their [List]
      counterpart and a similar equality holds. *)
  val filter : ('a -> bool) -> 'a option -> 'a option

  (** [filter_map] is the [Option] counterpart to [List]'s [filter_map].
      Incidentally, [filter_map f o] is also [bind o f]. *)
  val filter_map : ('a -> 'b option) -> 'a option -> 'b option

  (** [filter_s] is [filter] where the predicate returns a promise. *)
  val filter_s : ('a -> bool Lwt.t) -> 'a option -> 'a option Lwt.t

  (** [filter_map_s] is [filter_map] where the function returns a promise. *)
  val filter_map_s : ('a -> 'b option Lwt.t) -> 'a option -> 'b option Lwt.t

  (** [filter_e] is [filter] where the predicate returns a [result]. *)
  val filter_e :
    ('a -> (bool, 'e) result) -> 'a option -> ('a option, 'e) result

  (** [filter_map_e] is [filter_map] where the function returns a [result]. *)
  val filter_map_e :
    ('a -> ('b option, 'e) result) -> 'a option -> ('b option, 'e) result

  (** [filter_es] is [filter] where the predicate returns a promise of a [result]. *)
  val filter_es :
    ('a -> (bool, 'e) result Lwt.t) -> 'a option -> ('a option, 'e) result Lwt.t

  (** [filter_map_es] is [filter_map] where the function returns a promise of a [result]. *)
  val filter_map_es :
    ('a -> ('b option, 'e) result Lwt.t) ->
    'a option ->
    ('b option, 'e) result Lwt.t

  (** [filter_ok o] is [Some x] iff [o] is [Some (Ok x)]. *)
  val filter_ok : ('a, 'e) result option -> 'a option

  (** [filter_error o] is [Some x] iff [o] is [Some (Error x)]. *)
  val filter_error : ('a, 'e) result option -> 'e option

  (** [filter_left o] is [Some x] iff [o] is [Some (Either.Left x)]. *)
  val filter_left : ('a, 'b) Either.t option -> 'a option

  (** [filter_right o] is [Some x] iff [o] is [Some (Either.Right x)]. *)
  val filter_right : ('a, 'b) Either.t option -> 'b option

  val iter : ('a -> unit) -> 'a option -> unit

  val iter_s : ('a -> unit Lwt.t) -> 'a option -> unit Lwt.t

  val iter_e :
    ('a -> (unit, 'trace) result) -> 'a option -> (unit, 'trace) result

  val iter_es :
    ('a -> (unit, 'trace) result Lwt.t) ->
    'a option ->
    (unit, 'trace) result Lwt.t

  val is_none : 'a option -> bool

  val is_some : 'a option -> bool

  val equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool

  val compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int

  val to_result : none:'trace -> 'a option -> ('a, 'trace) result

  val of_result : ('a, 'e) result -> 'a option

  val to_list : 'a option -> 'a list

  val to_seq : 'a option -> 'a Stdlib.Seq.t

  (** [catch f] is [Some (f ())] if [f] does not raise an exception, it is
      [None] otherwise.

      You should only use [catch] when you truly do not care about
      what exception may be raised during the evaluation of [f ()]. If you need
      to inspect the raised exception, or if you need to pass it along, consider
      {!Result.catch} instead.

      If [catch_only] is set, then only exceptions [e] such that [catch_only e]
      is [true] are caught.

      Whether [catch_only] is set or not, this function never catches
      non-deterministic runtime exceptions of OCaml such as {!Stack_overflow}
      and {!Out_of_memory}. *)
  val catch : ?catch_only:(exn -> bool) -> (unit -> 'a) -> 'a option

  (** [catch_o f] is equivalent to [join @@ catch f]. In other words, it is
      [f ()] if [f] doesn't raise any exception, and it is [None] otherwise.

      [catch_only] has the same behaviour and limitations as with [catch]. *)
  val catch_o : ?catch_only:(exn -> bool) -> (unit -> 'a option) -> 'a option

  (** [catch_s f] is a promise that resolves to [Some x] if and when [f ()]
      resolves to [x]. Alternatively, it resolves to [None] if and when [f ()]
      is rejected.

      You should only use [catch_s] when you truly do not care about
      what exception may be raised during the evaluation of [f ()]. If you need
      to inspect the raised exception, or if you need to pass it along, consider
      {!Result.catch_s} instead.

      If [catch_only] is set, then only exceptions [e] such that [catch_only e]
      is [true] are caught.

      Whether [catch_only] is set or not, this function never catches
      non-deterministic runtime exceptions of OCaml such as {!Stack_overflow}
      and {!Out_of_memory}. *)
  val catch_s :
    ?catch_only:(exn -> bool) -> (unit -> 'a Lwt.t) -> 'a option Lwt.t

  (** [catch_os f] is like [catch_s f] where [f] returns a promise that resolves
      to an option. [catch_os f] resolves to [None] if [f ()] resolves to
      [None] or is rejected. It resolves to [Some _] if [f ()] does.

      [catch_only] has the same behaviour and limitations as with [catch]. *)
  val catch_os :
    ?catch_only:(exn -> bool) -> (unit -> 'a option Lwt.t) -> 'a option Lwt.t
end
