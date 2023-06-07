(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Error_monad

(** {1 Key-value store}

    This module defines a simple key-value store. The design is
   minimal and aims to be:

    - easy to use

    - safe when used in a concurrent setting

    This key-value store also features a best effort mechanism relying
    on a cache to avoid I/Os.

    This key-store use one file per key. Consequently, to control
   the number of file descriptors opened, a pool can be used.  *)

(** An abstract representation of a key-value store. *)
type ('key, 'value) t

(** [init ?pool ~lru_size file_of] initialises a key-value store. This
   is a simple design where each [value] is stored into a single file.

    For each [key], we use [file_of key] to get the representation of
   a [value] as a file.

    [lru_size] is a parameter that represents the number of different
   [values] that can be in memory. It is up to the user of this
   library to decide this number depending on the sizes of the values.

    [pool] is an optional parameter that allows to control the maximum
   number of file descriptors opened at the same time.
*)
val init :
  ?pool:unit Lwt_pool.t ->
  lru_size:int ->
  ('key -> 'value Stored_data.file) ->
  ('key, 'value) t

(** [write_value ?(override=false) t key value] writes a value in the
   [key] value store. If a previous writing or read failed, the
   function will try again to write the value. If [override] is [true],
   the value will be written even though there is already a written
   value for this key.  *)
val write_value :
  ?override:bool -> ('key, 'value) t -> 'key -> 'value -> unit tzresult Lwt.t

(** [write_values ?(override=false) t seq] writes a sequence of [keys]
   [values] onto the store (see {!val:write_value}). If an error
   occurs, the first error is returned. This function guarantees that
   up to the data for which the error occured, the values where stored
   onto the disk. *)
val write_values :
  ?override:bool ->
  ('key, 'value) t ->
  ('key * 'value) Seq.t ->
  unit tzresult Lwt.t

(** [read_value t key] reads the value associated to [key] in the
   store. Fails if no value where attached to this [key]. The value
   read is the last one that was produced by a successful write. *)
val read_value : ('key, 'value) t -> 'key -> 'value tzresult Lwt.t

(** [read_values t keys] produces a sequence of [values] associaed to
    the sequence of [keys]. This function is almost instantaneous
    since no reads are performed. Reads are done when the caller
    consumes the values of the sequence returned. *)
val read_values :
  ('key, 'value) t -> 'key Seq.t -> ('key * 'value tzresult) Seq_s.t
