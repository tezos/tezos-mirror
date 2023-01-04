(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** {1 Monad for values with delayed write effects } *)

(** Type to encapsulate values with a callback that should write to disk. This
    allows to delay some writes to a location where we have write access to the
    context. *)
type ('a, 'store) t

(** [no_write v] returns [v] together with a write function that does
    nothing. *)
val no_write : 'a -> ('a, _) t

(** [delay_write v write] returns a delayed write version of [v] with the
    [write] function to be called later. *)
val delay_write : 'a -> ('store -> unit tzresult Lwt.t) -> ('a, 'store) t

(** [map f dw] returns the delayed write [dw] where [f] is applied to the
    encapsulated value. *)
val map : ('a -> 'b) -> ('a, 'store) t -> ('b, 'store) t

(** [map_es] is like {!map} in the Lwt result monad. *)
val map_es :
  ('a -> ('b, 'c) result Lwt.t) ->
  ('a, 'store) t ->
  (('b, 'store) t, 'c) result Lwt.t

(** [bind f dw] returns a delayed write value where [f] is applied to the
    encapsulated value and the write effects of [f] are added to the effects of
    [dw]. *)
val bind : ('a -> ('b, 'store) t) -> ('a, 'store) t -> ('b, 'store) t

(** [apply node_ctxt dw] applies the write effects on the context [node_ctxt]
    and returns the encapsulated value. *)
val apply : 'store -> ('a, 'store) t -> 'a tzresult Lwt.t

(** [ignore dw] ignores the write effects and returns the encapsulated value. *)
val ignore : ('a, _) t -> 'a

(** {2 Monad for values with delayed write effects on top of the Lwt_result
    monad } *)
module Lwt_result_syntax : sig
  val bind :
    ('a -> (('b, 'store) t, 'c) result Lwt.t) ->
    (('a, 'store) t, 'c) result Lwt.t ->
    (('b, 'store) t, 'c) result Lwt.t

  val map :
    ('a -> 'b) ->
    (('a, 'store) t, 'c) Lwt_result.t ->
    (('b, 'store) t, 'c) Lwt_result.t

  val ( let>* ) :
    (('a, 'store) t, 'b) result Lwt.t ->
    ('a -> (('c, 'store) t, 'b) result Lwt.t) ->
    (('c, 'store) t, 'b) result Lwt.t

  val ( let>+ ) :
    (('a, 'store) t, 'b) Lwt_result.t ->
    ('a -> 'c) ->
    (('c, 'store) t, 'b) Lwt_result.t

  val return : 'a -> (('a, 'store) t, 'b) Lwt_result.t

  val list_fold_left_i_es :
    (int -> 'a -> 'b -> (('a, 'store) t, 'trace) result Lwt.t) ->
    'a ->
    'b list ->
    (('a, 'store) t, 'trace) result Lwt.t
end
