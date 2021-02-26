(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** {1 Lwt, result, and Lwt-result monad operators}

    This module provides the necessary functions and operators to use Lwt,
    result and Lwt-result as a monad.

    {2 Basics}

    The three, tiered monads have each their full set of operators:

    The Lwt monad:
    {ul
      {li {!Lwt.return} for return,}
      {li {!Lwt.bind} or {!(>>=)} for bind, and}
      {li {!Lwt.map} or {!(>|=)} for map.}
    }

    The result monad:
    {ul
      {li {!Result.ok} or {!ok} for return,}
      {li {!Result.bind} or {!(>>?)} for bind, and}
      {li {!Result.map} {!(>|?)} for map.}
    }
    In addition, {!Result.error} or {!error} is for failures within the result
    monad.

    The Lwt-result monad:
    {ul
      {li {!return} or {!Lwt.return_ok} for return,}
      {li {!(>>=?)} for bind, and}
      {li {!(>|=?)} for map.}
    }
    In addition, {!fail} is for the failure within the Lwt-result combined
    monad.

    Note that future improvements are planned to (a) make those more uniform,
    (b) allow the opening of selected infix operators only, and (c) provide
    [let*]-binds.

    {2 Preallocated values}

    The module also provides preallocated values for the common types:

    - {!unit_s} (resp {!unit_e}) (resp {!unit_es}) is [Lwt.return ()] (resp
      [Ok ()]) (resp [Lwt.return (Ok ())]),
    - {!none_s} (resp {!none_e}) (resp {!none_es}) is [Lwt.return None] (resp
      [Ok None]) (resp [Lwt.return (Ok None)]),
    - etc. (see full inventory below)

    Note that some of these values are also available in their dedicated
    modules. E.g., [none_*] are available in {!Option}.

    {2 Joins}

    The {!join_p} function takes a list of promises [ps] and returns a single
    promise [p] that resolves with [()] when all the promises of [ps] have
    resolved.

    The {!all_p} function takes a list of promises [ps] and returns a single
    promise [p] that resolves when all the promises of [ps] have resolved. The
    value [p] resolves to is the list of values the promises of [ps] resolve to.
    The order is preserved.

    The {!both_p} function takes two promises [p1] and [p2] and returns a single
    promise [p] that resolves when both promises [p1] and [p2] have resolved.
    The value [p] resolves to is the tuple of values the promises [p1] and [p2]
    resolve to.

    Note that like all [_p] functions, these functions have a best-effort
    semantic: they only resolve once all the underlying promises have resolved.

    The [_e] variants are equivalent for the result monad: the final result is
    [Ok] if all the underlying results are [Ok].

    The [_es] variants are equivalent for the Lwt-result monad: the final
    promise resolves to [Ok] if all the underlying promise resolve to [Ok].

 *)

module type S = sig
  (** lwt monad *)

  val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

  val ( >|= ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

  (** result monad *)

  val ok : 'a -> ('a, 'trace) result

  val error : 'error -> ('a, 'error) result

  val ( >>? ) :
    ('a, 'trace) result -> ('a -> ('b, 'trace) result) -> ('b, 'trace) result

  val ( >|? ) : ('a, 'trace) result -> ('a -> 'b) -> ('b, 'trace) result

  (** lwt-result combined monad *)

  val ok_s : 'a -> ('a, 'trace) result Lwt.t

  val return : 'a -> ('a, 'trace) result Lwt.t

  val error_s : 'error -> ('a, 'error) result Lwt.t

  val fail : 'error -> ('a, 'error) result Lwt.t

  val ( >>=? ) :
    ('a, 'trace) result Lwt.t ->
    ('a -> ('b, 'trace) result Lwt.t) ->
    ('b, 'trace) result Lwt.t

  val ( >|=? ) :
    ('a, 'trace) result Lwt.t -> ('a -> 'b) -> ('b, 'trace) result Lwt.t

  (** Mixing operators *)

  (** All operators follow this naming convention:
      - the first character is [>]
      - the second character is [>] for [bind] and [|] for [map]
      - the next character is [=] for Lwt or [?] for Error
      - the next character (if present) is [=] for Lwt or [?] for Error, it is
      only used for operator that are within both monads. *)

  val ( >>?= ) :
    ('a, 'trace) result ->
    ('a -> ('b, 'trace) result Lwt.t) ->
    ('b, 'trace) result Lwt.t

  val ( >|?= ) :
    ('a, 'trace) result -> ('a -> 'b Lwt.t) -> ('b, 'trace) result Lwt.t

  (** preallocated in-monad values *)

  val unit_s : unit Lwt.t

  val unit_e : (unit, 'trace) result

  val unit_es : (unit, 'trace) result Lwt.t

  val none_s : 'a option Lwt.t

  val none_e : ('a option, 'trace) result

  val none_es : ('a option, 'trace) result Lwt.t

  val some_s : 'a -> 'a option Lwt.t

  val some_e : 'a -> ('a option, 'trace) result

  val some_es : 'a -> ('a option, 'trace) result Lwt.t

  val nil_s : 'a list Lwt.t

  val nil_e : ('a list, 'trace) result

  val nil_es : ('a list, 'trace) result Lwt.t

  val true_s : bool Lwt.t

  val true_e : (bool, 'trace) result

  val true_es : (bool, 'trace) result Lwt.t

  val false_s : bool Lwt.t

  val false_e : (bool, 'trace) result

  val false_es : (bool, 'trace) result Lwt.t

  (** additional preallocated in-monad values

     this is for backwards compatibility and for similarity with Lwt *)

  val ok_unit : (unit, 'error) result

  val return_unit : (unit, 'error) result Lwt.t

  (** joins *)

  val join_p : unit Lwt.t list -> unit Lwt.t

  val all_p : 'a Lwt.t list -> 'a list Lwt.t

  val both_p : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t

  val join_e : (unit, 'trace) result list -> (unit, 'trace list) result

  val all_e : ('a, 'trace) result list -> ('a list, 'trace list) result

  val both_e :
    ('a, 'trace) result -> ('b, 'trace) result -> ('a * 'b, 'trace list) result

  val join_ep :
    (unit, 'trace) result Lwt.t list -> (unit, 'trace list) result Lwt.t

  val all_ep :
    ('a, 'trace) result Lwt.t list -> ('a list, 'trace list) result Lwt.t

  val both_ep :
    ('a, 'trace) result Lwt.t ->
    ('b, 'trace) result Lwt.t ->
    ('a * 'b, 'trace list) result Lwt.t
end
