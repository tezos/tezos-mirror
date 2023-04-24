(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech  <contact@trili.tech>                       *)
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

(** [Certificate_streamers] handles the streams of certificate updates used to
      notify clients of a Coordinator node that a certificate has been updated.
*)

(** The type of [Certificate_streamers]. Values of type
    [Certificate_streamers.t] map root hashes to
    [Certificate_repr.t Data_streamer.t]. *)
type t

(** [init ()] returns an empty map of certificate streams. *)
val init : unit -> t

(** [handle_subscribe t root_hash] creates a new watcher for [root_hash] in
    [t], and returns the corresponding [Lwt_stream] and [Lwt_watcher.stopper].
*)
val handle_subscribe :
  Dac_plugin.t ->
  t ->
  Dac_plugin.raw_hash ->
  (Certificate_repr.t Lwt_stream.t * Lwt_watcher.stopper) tzresult

(** [push t root_hash certificate] streams the updated certificate for
    [root_hash]. *)
val push :
  Dac_plugin.t ->
  t ->
  Dac_plugin.raw_hash ->
  Certificate_repr.t ->
  unit tzresult

(** [close t root_hash] closes the certificate [Data_streamer.t] for
    [root_hash]. Returns true if the [Data_streamer.t] associated to
    [root_hash] was open before the function was invoked, false otherwise. *)
val close : Dac_plugin.t -> t -> Dac_plugin.raw_hash -> bool tzresult
