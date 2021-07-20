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

(** Container to write data to when reading bytes from a connection *)
type buffer

(** [mk_buffer ?pos ?len buf] creates an instance of {!buffer},
    for copying [len] bytes starting at [pos] in [buf]. If [pos] is omitted,
    it is defaulted to [0]. If [len] is omitted, it is defaulted to
    [Bytes.length buf - pos]. *)
val mk_buffer : ?pos:int -> ?len:int -> bytes -> (buffer, tztrace) result

(** [mk_buffer_safe buf] creates an instance of {!buffer},
    that uses the entirety of [buf]; i.e. it will read at most
    [Bytes.length buf] bytes from the connection, and will write
    starting at position [0]. *)
val mk_buffer_safe : bytes -> buffer

(** The input type of [read] and [read_full] below *)
type readable

(** [mk_readable read_buffer read_queue] returns a pristine
 *  instance of {!readable} *)
val mk_readable :
  read_buffer:Circular_buffer.t ->
  read_queue:Circular_buffer.data tzresult Lwt_pipe.t ->
  readable

(** [read readable buffer] immediately reads data from [readable] if it
 *  is readily available. Otherwise it waits for data to arrive. *)
val read : ?canceler:Lwt_canceler.t -> readable -> buffer -> int tzresult Lwt.t

(** Like [read], but blits exactly [len] bytes in [buf]. *)
val read_full :
  ?canceler:Lwt_canceler.t -> readable -> buffer -> unit tzresult Lwt.t

(**/**)

module Internal_for_tests : sig
  (** [destruct_buffer buf] returns the [pos], [len], and [buf] values
        of the given {!buffer}. See {!mk_buffer}. *)
  val destruct_buffer : buffer -> int * int * Bytes.t
end
