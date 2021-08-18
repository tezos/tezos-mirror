(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Persistent data manager.

    Every data read/write operation is protected by a mutex preventing
    concurrent data-races. *)

(** The type for the persistent data. *)
type 'a t

(** [get data] accesses the data (cached). *)
val get : 'a t -> 'a Lwt.t

(** [write data value] overwrites the previous [data] with the new
    [value]. *)
val write : 'a t -> 'a -> unit tzresult Lwt.t

(** [write_file encoded_file value] raw writes the [encoded_file] with
   the [value].

    {b Warning} this function should not be used in a normal context
   as it aims to overwrite the target without preserving data
   races. Favour the usage of [write]. *)
val write_file : ('kind, 'a) Naming.encoded_file -> 'a -> unit tzresult Lwt.t

(** [update_with data f] {b atomically} updates [data] with the result
    of the application of [f]. Concurrent accesses to the data will
    block until the value is updated.

    {b Warning} Calling read/write in [f] will result in a deadlock. *)
val update_with : 'a t -> ('a -> 'a Lwt.t) -> unit tzresult Lwt.t

(** [load encoded_file] loads and decode a data from an
    [encoded_file]. *)
val load : ('kind, 'a) Naming.encoded_file -> 'a t tzresult Lwt.t

(** [init encoded_file ~initial_data] creates or load an on-disk
    data. If the file already exists, then the data is read from the
    file. Otherwise, [initial_data] is used. *)
val init :
  ('kind, 'a) Naming.encoded_file -> initial_data:'a -> 'a t tzresult Lwt.t
