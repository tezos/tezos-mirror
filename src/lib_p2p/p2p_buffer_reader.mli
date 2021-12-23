(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module takes care of reading data from a {!readable} into a {!buffer}.

    Its purpose is to take care - via those abstract types - of the tedious
    tracking of byte positions and lengths when copying data around, as well
    as ensuring invariants for safety of call sites, e.g. "do not read too
    much" or "wait if no data is readable right now".
*)

(** A data source. Reading functions read data {i from} it. *)
type readable

(** A data destination. Reading functions copy data {i into} it. *)
type buffer

(** [mk_readable ~read_buffer ~read_queue] creates a {!readable} that uses
    [read_buffer] to store data and [read_queue] to notify asynchronously
    that data was written.
*)
val mk_readable :
  read_buffer:Circular_buffer.t ->
  read_queue:Circular_buffer.data tzresult Lwt_pipe.Maybe_bounded.t ->
  readable

(** [mk_buffer ?pos ?length_to_copy bytes] creates a {!buffer} for copying
    [length_to_copy] bytes into [bytes] starting at position [pos].

    - [pos] defaults to [0].
    - [length_to_copy] defaults to [Bytes.length bytes - pos].

    If you neither specify [pos] nor [length_to_copy], prefer using
    {!mk_buffer_safe} which cannot fail.

    If invalid values are passed, fails with [Invalid_read_request].
*)
val mk_buffer : ?pos:int -> ?length_to_copy:int -> bytes -> buffer tzresult

(** [mk_buffer_safe bytes] creates a {!buffer} that uses the entirety of [bytes].

    Simpler equivalent to [mk_buffer ?pos:None ?length_to_copy:None bytes] as
    the result is not wrapped in a {!result}.
*)
val mk_buffer_safe : bytes -> buffer

(** [read readable buffer] reads the next segment of data from [readable] and
    copies it into [buffer], returning the number of read bytes.

    - If [readable] does not currently contain any data, it waits for a segment
      then reads it.
    - If [readable] already contains data, it reads immediately.

    Note: Even if [buffer] size is [0], this function still waits for data in
    [readable] before returning.

    Invariants:

    - The returned number of bytes is lower than or equal to the current value
      of [buffer.length_to_copy].
    - If the next [readable] segment is smaller than the current value of
      [buffer.length_to_copy] then only this segment is copied into [buffer]
      (i.e. after [read], [buffer.length_to_copy] may or may not be [0])
    - If the next [readable] segment is bigger than the current value of
      [buffer.length_to_copy] then the unused data of that segment is kept for
      the next read (i.e. [readable] does not lose data).
*)
val read : ?canceler:Lwt_canceler.t -> readable -> buffer -> int tzresult Lwt.t

(** [read_full readable buffer] reads from [readable] and copies into [buffer]
    until [buffer] is full.

    - If [readable] does not currently contain enough data to fill [buffer], it
      waits for additional segments and reads them.
    - If [readable] already contains data, it reads immediately.

    Invariants:

    - [buffer.length_to_copy] after [read_full] is guaranteed to be [0] (i.e.
      it is useless to read into [buffer] afterwards).
    - If the last read segment of [readable] is bigger than the remaining
      length of [buffer] then the unused data of that segment is kept for the
      next read (i.e. [readable] does not lose data).
*)
val read_full :
  ?canceler:Lwt_canceler.t -> readable -> buffer -> unit tzresult Lwt.t

(**/**)

module Internal_for_tests : sig
  (** [destruct_buffer buf] returns the [pos], [length_to_copy], and [buf]
      values of the given {!buffer}. See {!mk_buffer}. *)
  val destruct_buffer : buffer -> int * int * Bytes.t
end
