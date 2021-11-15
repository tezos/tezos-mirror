(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** A replacement for {!Stdlib.Unit} which
    - is exception-safe,
    - includes Lwt-, result-, and Lwt-result-aware traversors.

    See {!Lwtreslib} and {!Seq} for general description of traversors and the
    meaning of [_s], [_e], and [_es] suffixes. *)
module type S = sig
  type t = unit = ()

  val unit : t

  val unit_s : t Lwt.t

  val unit_e : (t, 'a) result

  val unit_es : (t, 'a) result Lwt.t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val to_string : t -> string

  (** [catch f] is [f ()], but exceptions are ignored and [()] is returned if
      one is raised.

      You should only use [catch] when you truly do not care about
      what exception may be raised during the evaluation of [f ()]. If you need
      to inspect the raised exception consider {!catch_f} and if you need to
      pass it along consider {!Result.catch}.

      If [catch_only] is set, then only exceptions [e] such that [catch_only e]
      is [true] are caught.

      Whether [catch_only] is set or not, this function never catches
      non-deterministic runtime exceptions of OCaml such as {!Stack_overflow}
      and {!Out_of_memory}. *)
  val catch : ?catch_only:(exn -> bool) -> (unit -> unit) -> unit

  (** [catch_f f handler] is [f ()]. If [f ()] raises an exception then
      [handler] is called.

      No attempt is made to catch the exceptions raised by [handler].

      [catch_only] has the same behaviour and limitations as with [catch]. *)
  val catch_f :
    ?catch_only:(exn -> bool) -> (unit -> unit) -> (exn -> unit) -> unit

  (** [catch_s f] is [f ()]. If [f ()] is rejected or raises an exception, then
      the exception is ignored and it resolves to [()].

      [catch_only] has the same behaviour and limitations as with [catch]. *)
  val catch_s : ?catch_only:(exn -> bool) -> (unit -> unit Lwt.t) -> unit Lwt.t
end
