(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

(** [Root_hash_streamer] is an in-memory data structure for handling pub-sub
    mechanism of streaming data from publishers to subscribers.
*)
module Root_hash_streamer : sig
  (** ['a t] represents an instance of [Root_hash_streamer], where ['a]
      is the type of the data that we stream. *)
  type 'a t

  (** Initializes a [Root_hash_streamer.t] *)
  val init : unit -> 'a t

  (** [publish streamer data] publishes [data] to all attached 
      subscribers of the [streamer].
   *)
  val publish : 'a t -> 'a -> unit tzresult Lwt.t

  (** [make_subscription streamer] returns a new stream of data for the
      subscriber to consume. An [Lwt_watcher.stopper] function is also returned
      for the subscriber to close the stream.
  *)
  val make_subscription :
    'a t -> ('a Lwt_stream.t * Lwt_watcher.stopper) tzresult Lwt.t
end
