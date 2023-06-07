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
    wrapped in a promise.

    This allows some additional traversors ([S.map], etc.) to be applied lazily.

    The functions [of_seq] and [of_seq_s] allow conversion from vanilla
    sequences. *)
module type S = sig
  (** This is similar to [S.t] but the suspended node is a promise. *)
  type +'a node = Nil | Cons of 'a * 'a t

  and 'a t = unit -> 'a node Lwt.t

  include
    Seqes.Sigs.SEQMON1ALL with type 'a mon := 'a Lwt.t with type 'a t := 'a t

  module E :
    Seqes.Sigs.SEQMON2TRAVERSORS
      with type ('a, 'e) mon := ('a, 'e) result Lwt.t
      with type ('a, 'e) callermon := ('a, 'e) result
      with type ('a, 'e) t := 'a t

  module S :
    Seqes.Sigs.SEQMON1TRANSFORMERS
      with type 'a mon := 'a Lwt.t
      with type 'a callermon := 'a Lwt.t
      with type 'a t := 'a t

  module ES :
    Seqes.Sigs.SEQMON2TRAVERSORS
      with type ('a, 'e) mon := ('a, 'e) result Lwt.t
      with type ('a, 'e) callermon := ('a, 'e) result Lwt.t
      with type ('a, 'e) t := 'a t

  (** [return_s p] is a sequence with the value the promise [p] resolves to as
      its single element. *)
  val return_s : 'a Lwt.t -> 'a t

  (** [cons_s p s] is the sequence containing the value the promise [p] resolves
      to, followed by [s]. *)
  val cons_s : 'a Lwt.t -> 'a t -> 'a t

  (** Similar to {!iter} but wraps the iteration in {!Lwt}. The
      steps of the iteration are started concurrently: one iteration is started
      as soon as the node becomes resolved. The promise [iter_p f s]
      is resolved only once all the promises of the iteration are. At this point
      it is either fulfilled if all promises are, or rejected if at least one of
      them is. *)
  val iter_p : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  (** Similar to {!iteri} but wraps the iteration in {!Lwt}. All the
      steps of the iteration are started concurrently. The promise [iteri_p f s]
      is resolved only once all the promises of the iteration are. At this point
      it is either fulfilled if all promises are, or rejected if at least one of
      them is. *)
  val iteri_p : (int -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val take : when_negative_length:'err -> int -> 'a t -> ('a t, 'err) result

  val drop : when_negative_length:'err -> int -> 'a t -> ('a t, 'err) result

  val of_seq : 'a Stdlib.Seq.t -> 'a t

  val of_seq_s : 'a Lwt.t Stdlib.Seq.t -> 'a t
end
