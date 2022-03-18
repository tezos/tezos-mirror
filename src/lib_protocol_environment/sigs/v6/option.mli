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

(** Signature from Lwtreslib's option module *)

type 'a t = 'a option = None | Some of 'a

val none : 'a option

val none_e : ('a option, 'trace) result

val none_s : 'a option Lwt.t

val none_es : ('a option, 'trace) result Lwt.t

val some : 'a -> 'a option

val some_unit : unit option

val some_nil : 'a list option

val some_e : 'a -> ('a option, 'trace) result

val some_s : 'a -> 'a option Lwt.t

val some_es : 'a -> ('a option, 'trace) result Lwt.t

val value : 'a option -> default:'a -> 'a

val value_e : 'a option -> error:'trace -> ('a, 'trace) result

val value_f : 'a option -> default:(unit -> 'a) -> 'a

val value_fe : 'a option -> error:(unit -> 'trace) -> ('a, 'trace) result

val bind : 'a option -> ('a -> 'b option) -> 'b option

val join : 'a option option -> 'a option

val either : 'a option -> 'a option -> 'a option

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

val to_seq : 'a option -> 'a Seq.t

(** [catch f] is [Some (f ())] if [f] does not raise an exception, it is
    [None] otherwise.

    You should only use [catch] when you truly do not care about
    what exception may be raised during the evaluation of [f ()]. If you need
    to inspect the raised exception, or if you need to pass it along, consider
    {!Result.catch} instead.

    If [catch_only] is set, then only exceptions [e] such that [catch_only e]
    is [true] are caught.

    Whether [catch_only] is set or not, you cannot catch non-deterministic
    runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory} nor system exceptions such as {!Unix.Unix_error}. *)
val catch : ?catch_only:(exn -> bool) -> (unit -> 'a) -> 'a option

(** [catch_s f] is a promise that resolves to [Some x] if and when [f ()]
    resolves to [x]. Alternatively, it resolves to [None] if and when [f ()]
    is rejected.

    You should only use [catch_s] when you truly do not care about
    what exception may be raised during the evaluation of [f ()]. If you need
    to inspect the raised exception, or if you need to pass it along, consider
    {!Result.catch_s} instead.

    If [catch_only] is set, then only exceptions [e] such that [catch_only e]
    is [true] are caught.

    Whether [catch_only] is set or not, you cannot catch non-deterministic
    runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory} nor system exceptions such as {!Unix.Unix_error}. *)
val catch_s :
  ?catch_only:(exn -> bool) -> (unit -> 'a Lwt.t) -> 'a option Lwt.t
