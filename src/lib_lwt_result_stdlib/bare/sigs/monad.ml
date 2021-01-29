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
end
