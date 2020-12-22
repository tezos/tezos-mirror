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

(** Modules with the [S] signature are used to instantiate the other modules of
    this library. [S] describes a generic Lwt-Result combined monad, the rest of
    this library builds upon. *)
module type S = sig
  (** [error] are the errors as injected into the monad. In other words,
      [error] is the type of values that are used in primitives that "raise"
      an error. *)
  type error

  (** [trace] are the errors as received from the monad. In other words,
      [trace] is the type of values that are seen when matching on [Error _]
      to, say, recover.

      The types [error] and ['error trace] are kept separate (although they can
      be equal) to support cases such as the following:
      - [trace] are richer than [error], such as by including a
        timestamp, a filename, or some other such metadata.
      - [trace] is slightly different and [private] and [error] is simply
        the type of argument to the functions that construct the private
        [trace].
      - [trace] is a collection of [error] and additional functions (not
        required by this library) allow additional manipulation. E.g., in the
        case of Tezos: errors are built into traces that can be grown.
  *)
  type 'error trace

  val make : 'error -> 'error trace

  val cons : 'error -> 'error trace -> 'error trace

  val conp : 'error trace -> 'error trace -> 'error trace

  (** result monad *)

  val ok : 'a -> ('a, 'trace) result

  val ok_unit : (unit, 'trace) result

  val ok_none : ('a option, 'trace) result

  val ok_some : 'a -> ('a option, 'trace) result

  val ok_nil : ('a list, 'trace) result

  val ok_true : (bool, 'trace) result

  val ok_false : (bool, 'trace) result

  val error : 'error -> ('a, 'error trace) result

  val ( >>? ) :
    ('a, 'trace) result -> ('a -> ('b, 'trace) result) -> ('b, 'trace) result

  val ( >|? ) : ('a, 'trace) result -> ('a -> 'b) -> ('b, 'trace) result

  (** lwt-result combined monad *)

  val return : 'a -> ('a, 'trace) result Lwt.t

  val return_unit : (unit, 'trace) result Lwt.t

  val return_none : ('a option, 'trace) result Lwt.t

  val return_some : 'a -> ('a option, 'trace) result Lwt.t

  val return_nil : ('a list, 'trace) result Lwt.t

  val return_true : (bool, 'trace) result Lwt.t

  val return_false : (bool, 'trace) result Lwt.t

  val fail : 'error -> ('a, 'error trace) result Lwt.t

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

  (** joins *)
  val join_e : (unit, 'error trace) result list -> (unit, 'error trace) result

  val all_e : ('a, 'error trace) result list -> ('a list, 'error trace) result

  val both_e :
    ('a, 'error trace) result ->
    ('b, 'error trace) result ->
    ('a * 'b, 'error trace) result

  val join_p : unit Lwt.t list -> unit Lwt.t

  val all_p : 'a Lwt.t list -> 'a list Lwt.t

  val both_p : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t

  val join_ep :
    (unit, 'error trace) result Lwt.t list -> (unit, 'error trace) result Lwt.t

  val all_ep :
    ('a, 'error trace) result Lwt.t list ->
    ('a list, 'error trace) result Lwt.t

  val both_ep :
    ('a, 'error trace) result Lwt.t ->
    ('b, 'error trace) result Lwt.t ->
    ('a * 'b, 'error trace) result Lwt.t
end
