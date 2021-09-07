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
  (** {1 The tower of monads}

      Each monad is given:
      - a module that groups returns and binds,
      - a set of infix operators. *)

  (** {2 The Lwt monad: for concurrency} *)

  module Lwt : module type of struct
    include Lwt
  end

  (** [(>>=)] is the monad-global infix alias for [Lwt.bind]. *)
  val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

  (** [(>|=)] is the monad-global infix alias for [Lwt.map]. *)
  val ( >|= ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

  (** Note that there is no monad-global alias for [Lwt.return]. *)

  (** {2 The (generic) Result monad: for success/failure} *)

  module Result : Result.S

  (** [ok] is the monad-global alias for [Result.return]. *)
  val ok : 'a -> ('a, 'e) result

  (** [error] is the monad-global alias for [Result.fail]. *)
  val error : 'e -> ('a, 'e) result

  (** [(>>?)] is the monad-global infix alias for [Result.bind]. *)
  val ( >>? ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

  (** [(>|?)] is the monad-global infix alias for [Result.map]. *)
  val ( >|? ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result

  (** {2 The combined Lwt+Result monad: for concurrent successes/failures} *)

  module LwtResult : sig
    val return : 'a -> ('a, 'e) result Lwt.t

    val fail : 'e -> ('a, 'e) result Lwt.t

    val return_unit : (unit, 'e) result Lwt.t

    val return_none : ('a option, 'e) result Lwt.t

    val return_some : 'a -> ('a option, 'e) result Lwt.t

    val return_nil : ('a list, 'e) result Lwt.t

    val return_true : (bool, 'e) result Lwt.t

    val return_false : (bool, 'e) result Lwt.t

    (* Unlike [Lwt], we do not provide [return_ok] and [return_error]. They
              would be as follow and it is not clear they would be useful.

       {[
       val return_ok : 'a -> (('a, 'e) result, 'f) result Lwt.t
       val return_error : 'e -> (('a, 'e) result, 'f) result Lwt.t
       ]}

              Note the availability of [return] and [fail]. *)

    val bind :
      ('a, 'e) result Lwt.t ->
      ('a -> ('b, 'e) result Lwt.t) ->
      ('b, 'e) result Lwt.t

    val bind_error :
      ('a, 'e) result Lwt.t ->
      ('e -> ('a, 'f) result Lwt.t) ->
      ('a, 'f) result Lwt.t

    val map : ('a -> 'b) -> ('a, 'e) result Lwt.t -> ('b, 'e) result Lwt.t

    val map_error : ('e -> 'f) -> ('a, 'e) result Lwt.t -> ('a, 'f) result Lwt.t
  end

  (** [return] is the monad-global alias for [LwtResult.return]. *)
  val return : 'a -> ('a, 'e) result Lwt.t

  (** [fail] is the monad-global alias for [LwtResult.fail]. *)
  val fail : 'e -> ('a, 'e) result Lwt.t

  (** [(>>=?)] is the monad-global infix alias for [LwtResult.bind]. *)
  val ( >>=? ) :
    ('a, 'e) result Lwt.t ->
    ('a -> ('b, 'e) result Lwt.t) ->
    ('b, 'e) result Lwt.t

  (** [(>|=?)] is the monad-global infix alias for [LwtResult.map]. *)
  val ( >|=? ) : ('a, 'e) result Lwt.t -> ('a -> 'b) -> ('b, 'e) result Lwt.t

  (** {1 Mixing operators}

      These are helpers to "go from one monad into another". *)

  (** All mixing operators follow this naming convention:
      - the first character is [>]
      - the second character is [>] for [bind] and [|] for [map]
      - the next character is [=] for Lwt or [?] for Error
      - the next character (if present) is [=] for Lwt or [?] for Error, it is
      only used for operator that are within both monads. *)

  val ( >>?= ) :
    ('a, 'e) result -> ('a -> ('b, 'e) result Lwt.t) -> ('b, 'e) result Lwt.t

  val ( >|?= ) : ('a, 'e) result -> ('a -> 'b Lwt.t) -> ('b, 'e) result Lwt.t

  (** Note that more micing operators are possible. However, their use is
      discouraged because they tend to degrade readability. *)

  (** {1 Joins}

      These functions handle lists (or tuples) of in-monad value. *)

  (** [join_p] is the joining of concurrent unit values (it is [Lwt.join]). *)
  val join_p : unit Lwt.t list -> unit Lwt.t

  (** [all_p] is the joining of concurrent non-unit values (it is [Lwt.all]). *)
  val all_p : 'a Lwt.t list -> 'a list Lwt.t

  (** [both_p] is the joining of two concurrent non-unit values (it is [Lwt.both]). *)
  val both_p : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t

  (** [join_e] is the joining of success/failure unit values. *)
  val join_e : (unit, 'e) result list -> (unit, 'e list) result

  (** [all_e] is the joining of success/failure non-unit values. *)
  val all_e : ('a, 'e) result list -> ('a list, 'e list) result

  (** [both_e] is the joining of two success/failure non-unit values. *)
  val both_e : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e list) result

  (** [join_ep] is the joining of concurrent success/failure unit values. *)
  val join_ep : (unit, 'e) result Lwt.t list -> (unit, 'e list) result Lwt.t

  (** [all_ep] is the joining of concurrent success/failure non-unit values. *)
  val all_ep : ('a, 'e) result Lwt.t list -> ('a list, 'e list) result Lwt.t

  (** [both_ep] is the joining of two concurrent success/failure non-unit values. *)
  val both_ep :
    ('a, 'e) result Lwt.t ->
    ('b, 'e) result Lwt.t ->
    ('a * 'b, 'e list) result Lwt.t
end
