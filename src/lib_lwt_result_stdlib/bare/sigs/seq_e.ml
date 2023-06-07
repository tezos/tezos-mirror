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

(** The [S] signature is similar to {!Seq.S} except that suspended nodes are
    wrapped in a result.

    This allows some additional traversors ([E.map], etc.) to be applied lazily.

    The functions [of_seq] and [of_seq_e] allow conversion from vanilla
    sequences. *)
module type S = sig
  type ('a, 'e) t = unit -> (('a, 'e) node, 'e) result

  and (+'a, 'e) node = Nil | Cons of 'a * ('a, 'e) t

  include
    Seqes.Sigs.SEQMON2ALL
      with type ('a, 'e) mon := ('a, 'e) result
      with type ('a, 'e) t := ('a, 'e) t

  (** [return_e (Ok x)] is a whole sequence containing the single element [x].
      [return_e (Error e)] is a sequence immediately interrupted by the error
      [e]. *)
  val return_e : ('a, 'e) result -> ('a, 'e) t

  (** [interrupted e] is a sequence immediately interrupted by the error [e]. *)
  val interrupted : 'e -> ('a, 'e) t

  (** [map_error f seq] is a sequence [feq].

      - If [seq] is a whole sequence, then [feq] is the same whole sequence.
      - If [seq] is an interrupted sequence, then [feq] is a sequence
        interrupted by [Error (f e)] where the elements of the successful prefix
        are the elements of the successful prefix of [seq]. *)
  val map_error : ('e -> 'f) -> ('a, 'e) t -> ('a, 'f) t

  (** [iter_p f seq] is a promise [p].

      - If [seq] is a whole sequence, then [p] resolves to [Ok ()] once all the
        promises created by [f] on the elements of [seq] have resolved.
      - If [seq] is interrupted by [Error e], then [p] resolves to [Error e]
        once all the promises created by [f] on the elements of the successful
        prefix of [seq] have resolved.

      Note that the behaviour for interrupted sequences is in line with the
      best-effort semantic of Lwtreslib. *)
  val iter_p : ('a -> unit Lwt.t) -> ('a, 'e) t -> (unit, 'e) result Lwt.t

  (** [cons_e (Ok x) s] is the sequence containing [x] followed by [s]. It is a
      whole sequence if [s] is.

      [cons_e (Error e) s] is a sequence immediately interrupted by [e]. *)
  val cons_e : ('a, 'e) result -> ('a, 'e) t -> ('a, 'e) t

  val take :
    when_negative_length:'err -> int -> ('a, 'e) t -> (('a, 'e) t, 'err) result

  val drop :
    when_negative_length:'err -> int -> ('a, 'e) t -> (('a, 'e) t, 'err) result

  module E :
    Seqes.Sigs.SEQMON2TRANSFORMERS
      with type ('a, 'e) t := ('a, 'e) t
       and type ('a, 'e) mon := ('a, 'e) result
       and type ('a, 'e) callermon := ('a, 'e) result

  module S :
    Seqes.Sigs.SEQMON2TRAVERSORS
      with type ('a, 'e) t := ('a, 'e) t
       and type ('a, 'e) mon := ('a, 'e) result Lwt.t
       and type ('a, 'e) callermon := 'a Lwt.t

  module ES :
    Seqes.Sigs.SEQMON2TRAVERSORS
      with type ('a, 'e) t := ('a, 'e) t
       and type ('a, 'e) mon := ('a, 'e) result Lwt.t
       and type ('a, 'e) callermon := ('a, 'e) result Lwt.t

  (** [of_seq_catch s] is a sequence with the same elements as [s] which is
      interrupted when forcing an element of the sequence raises an exception. *)
  val of_seq_catch : 'a Stdlib.Seq.t -> ('a, exn) t

  (** [of_seq_once ~when_forced_twice s] is a sequence with the same elements as
      [s] which is interrupted when an element of the sequence is forced twice.

      In other words, it is equivalent to
      {[map_error
          (function Seq.Forced_twice -> when_forced_twice | e -> raise e)
          (of_seq_catch (Seq.once s))
      ]}
  *)
  val of_seq_once : when_forced_twice:'e -> 'a Stdlib.Seq.t -> ('a, 'e) t

  val of_seq_e : ('a, 'e) result Stdlib.Seq.t -> ('a, 'e) t
end
