(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

(** This module provides an implementation for a monad that carries some
    ['state] in addition to a value of type ['a].
*)

(** The [('state, 'a) t] monad is a function from ['state] to ['state * 'a]. *)
type ('state, 'a) t = 'state -> 'state * 'a

(** The [('state, 'pass, 'fail) check] monad is an instantiation of [('state,
    'a) monad], where ['a] is either [`Pass 'pass] or a [`Fail 'fail]. *)
type ('state, 'pass, 'fail) check =
  ('state, [`Pass of 'pass | `Fail of 'fail]) t

module type S = sig
  (** [bind m f] is the bind function for the [('state, 'a) t] monad. *)
  val bind : ('state, 'a) t -> ('a -> ('state, 'b) t) -> ('state, 'b) t

  (** [get g f] is a variant of [bind], where the resulting value from [g]'s
     call is not returned in the monad. *)
  val get : ('state -> 'a) -> ('a -> ('state, 'b) t) -> ('state, 'b) t

  (** [return v] is the default constuctor of values in the [('state, 'a) t]
      monad. *)
  val return : 'a -> ('state, 'a) t

  (** [check c f] transforms an [('state, 'pass, 'fail) check] monad into an
      [('state, 'b) t] monad, where [y] is returned in case the given monad's
      value is of the form [`Fail y], or the result of [f x] is returned in case
      the value is [`Pass x]. *)
  val check :
    ('state, 'pass, 'fail) check ->
    ('pass -> ('state, 'fail) t) ->
    ('state, 'fail) t

  (** [return_pass v] is the default constuctor of [`Pass] values in the
      [('state, 'pass, 'fail) check] monad. *)
  val return_pass : 'pass -> ('state, 'pass, 'fail) check

  (** [return_fail v] is the default constuctor of [`Fail] values in the
      [('state, 'pass, 'fail) check] monad. *)
  val return_fail : 'fail -> ('state, 'pass, 'fail) check

  (** Infix notations and/or shorter aliases for the functions above. *)
  module Syntax : sig
    (** Infix notation for {!bind}. *)
    val ( let* ) : ('state, 'a) t -> ('a -> ('state, 'b) t) -> ('state, 'b) t

    (** Infix notation for {!check}. *)
    val ( let*? ) :
      ('state, 'pass, 'fail) check ->
      ('pass -> ('state, 'fail) t) ->
      ('state, 'fail) t

    (** Infix notation for {!get}. *)
    val ( let*! ) : ('state -> 'a) -> ('a -> ('state, 'b) t) -> ('state, 'b) t

    (** Re-exporting {!return} function in this sub-module. *)
    val return : 'a -> ('state, 'a) t

    (** Alias for {!return_pass}. *)
    val pass : 'pass -> ('state, 'pass, 'fail) check

    (** Variant of {!pass} function where the input is unit. *)
    val unit : ('state, unit, 'fail) check

    (** Alias for {!return_fail}. *)
    val fail : 'fail -> ('state, 'a, 'fail) check
  end
end

module M : S
