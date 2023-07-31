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

type ('a, 'e) t = ('a, 'e) result = Ok of 'a | Error of 'e (***)

val ok : 'a -> ('a, 'e) result

val ok_s : 'a -> ('a, 'e) result Lwt.t

val error : 'e -> ('a, 'e) result

val error_s : 'e -> ('a, 'e) result Lwt.t

val return : 'a -> ('a, 'e) result

val return_unit : (unit, 'e) result

val return_none : ('a option, 'e) result

val return_some : 'a -> ('a option, 'e) result

val return_nil : ('a list, 'e) result

val return_true : (bool, 'e) result

val return_false : (bool, 'e) result

val value : ('a, 'e) result -> default:'a -> 'a

val value_f : ('a, 'e) result -> default:(unit -> 'a) -> 'a

val bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

val bind_s :
  ('a, 'e) result -> ('a -> ('b, 'e) result Lwt.t) -> ('b, 'e) result Lwt.t

val bind_error : ('a, 'e) result -> ('e -> ('a, 'f) result) -> ('a, 'f) result

val bind_error_s :
  ('a, 'e) result -> ('e -> ('a, 'f) result Lwt.t) -> ('a, 'f) result Lwt.t

val join : (('a, 'e) result, 'e) result -> ('a, 'e) result

val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result

(* NOTE: [map_e] is [bind] *)
val map_e : ('a -> ('b, 'e) result) -> ('a, 'e) result -> ('b, 'e) result

val map_s : ('a -> 'b Lwt.t) -> ('a, 'e) result -> ('b, 'e) result Lwt.t

(* NOTE: [map_es] is [bind_s] *)
val map_es :
  ('a -> ('b, 'e) result Lwt.t) -> ('a, 'e) result -> ('b, 'e) result Lwt.t

val map_error : ('e -> 'f) -> ('a, 'e) result -> ('a, 'f) result

(* NOTE: [map_error_e] is [bind_error] *)
val map_error_e : ('e -> ('a, 'f) result) -> ('a, 'e) result -> ('a, 'f) result

val map_error_s : ('e -> 'f Lwt.t) -> ('a, 'e) result -> ('a, 'f) result Lwt.t

(* NOTE: [map_error_es] is [bind_error_s] *)
val map_error_es :
  ('e -> ('a, 'f) result Lwt.t) -> ('a, 'e) result -> ('a, 'f) result Lwt.t

val fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) result -> 'c

val iter : ('a -> unit) -> ('a, 'e) result -> unit

val iter_s : ('a -> unit Lwt.t) -> ('a, 'e) result -> unit Lwt.t

val iter_error : ('e -> unit) -> ('a, 'e) result -> unit

val iter_error_s : ('e -> unit Lwt.t) -> ('a, 'e) result -> unit Lwt.t

val is_ok : ('a, 'e) result -> bool

val is_error : ('a, 'e) result -> bool

val equal :
  ok:('a -> 'a -> bool) ->
  error:('e -> 'e -> bool) ->
  ('a, 'e) result ->
  ('a, 'e) result ->
  bool

val compare :
  ok:('a -> 'a -> int) ->
  error:('e -> 'e -> int) ->
  ('a, 'e) result ->
  ('a, 'e) result ->
  int

val to_option : ('a, 'e) result -> 'a option

val of_option : error:'e -> 'a option -> ('a, 'e) result

val to_list : ('a, 'e) result -> 'a list

val to_seq : ('a, 'e) result -> 'a Seq.t

(** [catch f] is [try Ok (f ()) with e -> Error e]: it is [Ok x] if [f ()]
    evaluates to [x], and it is [Error e] if [f ()] raises [e].

    See {!WithExceptions.S.Result.to_exn} for a converse function.

    If [catch_only] is set, then only exceptions [e] such that [catch_only e]
    is [true] are caught.

    Whether [catch_only] is set or not, you cannot catch non-deterministic
    runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory} nor system exceptions such as {!Unix.Unix_error}. *)
val catch : ?catch_only:(exn -> bool) -> (unit -> 'a) -> ('a, exn) result

(** [catch_f f handler] is equivalent to [map_error (catch f) handler].
    In other words, it catches exceptions in [f ()] and either returns the
    value in an [Ok] or passes the exception to [handler] for the [Error].

    [catch_only] has the same use as with [catch]. The same restriction on
    catching non-deterministic runtime exceptions applies. *)
val catch_f :
  ?catch_only:(exn -> bool) ->
  (unit -> 'a) ->
  (exn -> 'error) ->
  ('a, 'error) result

(** [catch_s] is [catch] but for Lwt promises. Specifically, [catch_s f]
    returns a promise that resolves to [Ok x] if and when [f ()] resolves to
    [x], or to [Error exc] if and when [f ()] is rejected with [exc].

    If [catch_only] is set, then only exceptions [e] such that [catch_only e]
    is [true] are caught.

    Whether [catch_only] is set or not, you cannot catch non-deterministic
    runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory} nor system exceptions such as {!Unix.Unix_error}. *)
val catch_s :
  ?catch_only:(exn -> bool) -> (unit -> 'a Lwt.t) -> ('a, exn) result Lwt.t
