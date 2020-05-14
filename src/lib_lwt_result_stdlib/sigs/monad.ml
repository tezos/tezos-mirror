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
  (** [in_error] are the errors as injected into the monad. In other words,
      [in_error] is the type of values that are used in primitives that "raise"
      an error. *)
  type in_error

  (** [out_error] are the errors as received from the monad. In other words,
      [out_error] is the type of values that are seen when matching on [Error _]
      to, say, recover.

      The types [in_error] and [out_error] are kept separate (although they can
      be equal) to support cases such as the following:
      - [out_error] are richer than [in_error], such as by including a
        timestamp, a filename, or some other such metadata.
      - [out_error] is slightly different and [private] and [in_error] is simply
        the type of argument to the functions that construct the private
        [out_error].
      - [out_error] is a collection of [in_error] and additional functions (not
        required by this library) allow additional manipulation. E.g., in the
        case of Tezos: errors are built into traces that can be grown.
  *)
  type out_error

  (** result monad *)

  val ok : 'a -> ('a, out_error) result

  val ok_unit : (unit, out_error) result

  val ok_none : ('a option, out_error) result

  val ok_some : 'a -> ('a option, out_error) result

  val ok_nil : ('a list, out_error) result

  val ok_true : (bool, out_error) result

  val ok_false : (bool, out_error) result

  val error : in_error -> ('a, out_error) result

  val ( >>? ) :
    ('a, out_error) result ->
    ('a -> ('b, out_error) result) ->
    ('b, out_error) result

  val ( >|? ) : ('a, out_error) result -> ('a -> 'b) -> ('b, out_error) result

  (** lwt-result combined monad *)

  val return : 'a -> ('a, out_error) result Lwt.t

  val return_unit : (unit, out_error) result Lwt.t

  val return_none : ('a option, out_error) result Lwt.t

  val return_some : 'a -> ('a option, out_error) result Lwt.t

  val return_nil : ('a list, out_error) result Lwt.t

  val return_true : (bool, out_error) result Lwt.t

  val return_false : (bool, out_error) result Lwt.t

  val fail : in_error -> ('a, out_error) result Lwt.t

  val ( >>=? ) :
    ('a, out_error) result Lwt.t ->
    ('a -> ('b, out_error) result Lwt.t) ->
    ('b, out_error) result Lwt.t

  val ( >|=? ) :
    ('a, out_error) result Lwt.t -> ('a -> 'b) -> ('b, out_error) result Lwt.t

  (** Mixing operators *)

  (** All operators follow this naming convention:
      - the first character is [>]
      - the second character is [>] for [bind] and [|] for [map]
      - the next character is [=] for Lwt or [?] for Error
      - the next character (if present) is [=] for Lwt or [?] for Error, it is
      only used for operator that are within both monads. *)

  val ( >>?= ) :
    ('a, out_error) result ->
    ('a -> ('b, out_error) result Lwt.t) ->
    ('b, out_error) result Lwt.t

  val ( >|?= ) :
    ('a, out_error) result -> ('a -> 'b Lwt.t) -> ('b, out_error) result Lwt.t

  (** joins *)
  val join_e : (unit, out_error) result list -> (unit, out_error) result

  val all_e : ('a, out_error) result list -> ('a list, out_error) result

  val both_e :
    ('a, out_error) result ->
    ('b, out_error) result ->
    ('a * 'b, out_error) result

  val join_p : unit Lwt.t list -> unit Lwt.t

  val all_p : 'a Lwt.t list -> 'a list Lwt.t

  val both_p : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t

  val join_ep :
    (unit, out_error) result Lwt.t list -> (unit, out_error) result Lwt.t

  val all_ep :
    ('a, out_error) result Lwt.t list -> ('a list, out_error) result Lwt.t

  val both_ep :
    ('a, out_error) result Lwt.t ->
    ('b, out_error) result Lwt.t ->
    ('a * 'b, out_error) result Lwt.t
end
