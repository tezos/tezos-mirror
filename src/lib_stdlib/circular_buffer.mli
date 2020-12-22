(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module implements a bufferisation abstraction to store
   temporary raw data chunks (as bytes) when chunks are read
   sequentially. The function write allows to store chunks in the
   buffer and the function read to read them from the buffer.

   The global contract is that if we write consecutively [d1;d2] onto
   the buffer. Then we have to fully read d1 and d2, in that order.

   This contract is not enforced by the library, it is the user
   responsability to respect it.

   If the circular buffer is full, a new temporary buffer is
   allocated to store the chunk of data to be written. *)

(** Type of circular buffers  *)
type t

(** An abstraction over a chunk of data written in the buffer. *)
type data

(** Create a buffer of size maxlength. If the buffer is full, a buffer
   of size [fresh_buf_size] is allocated (by default `2` kb). *)
val create : ?maxlength:int -> ?fresh_buf_size:int -> unit -> t

(** [write ~maxlen ~fill_using:f buffer] calls [fill_using buf offset
   maxlen] where [buf] is a buffer that has room for [maxlen] data
   starting from [offset].

   Assumes that [fill_using] returns the exact amount of written
   bytes.

   Behaviour is unspecified if [fill_using] writes more than [maxlen]
   data or lies on the number of written bytes.

   It returns a data descriptor for the supposedly written chunk.  *)
val write :
  maxlen:int ->
  fill_using:(Bytes.t -> int -> int -> int Lwt.t) ->
  t ->
  data Lwt.t

(** [read data ~len ~into:buf buffer ~offset] blit [len] data from the [data] chunk.
    If [len] is not provided, blit all the data.
    If [len] is less than the amount of data available, it returns a
    new handler the the remainder.

    Assumes that data has been produce by a write attempt in [buffer].
    Assumes that ~len is lesser than [length data].
*)
val read : data -> ?len:int -> t -> into:Bytes.t -> offset:int -> data option

(** [length data] returns the amount of avalaible bytes in [data] *)
val length : data -> int
