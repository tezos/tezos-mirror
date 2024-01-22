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

(** This error is returned when the requested data is not found. *)
type error += Missing_stored_kvs_data of string * int

(** {1 Key-value store}

    This module defines a simple key-value store. The design is
   minimal and aims to be:

    - easy to use

    - safe when used in a concurrent setting

    This key-value store also features a best effort mechanism relying
    on a cache to avoid I/Os.

    This key-value store assumes keys have a flat structure with a first
    layer of virtual directories and fixed-size virtual files in each directory.
    Each virtual directory is backed by a single physical file. Values are stored
    in virtual files, which are mapped sequentially in their virtual directory.  *)

(** An abstract representation of a key-value store. *)
type ('dir, 'file, 'value) t

(** A [directory_spec] specifies the layout of virtual files storing data of type ['value],
    as well as the name of the underlying physical file. *)
type ('file, 'value) directory_spec

(** [directory ?encoded_value_size value_encoding path eq index_of] describes a virtual directory.
    - [encoded_value_size] is the size in bytes of values encoded with [value_encoding].
      If [value_encoding] does not respect this property, the behaviour of the store
      is undefined. If [encoded_value_size] is not given, [value_encoding] must be of fixed length.
    - [value_encoding] is an encoding for the content of virtual files.
    - [path] is the path of the physical file in which the directory is to be stored.
    - [eq] is an equality function on the contents of virtual files.
    - [index_of] gives the index of a given file in the directory.

   @raise Invalid_argument if [encoded_value_size=None] and [value_encoding] does not have a fixed length.
 *)
val directory :
  ?encoded_value_size:int ->
  'value Data_encoding.t ->
  string ->
  ('value -> 'value -> bool) ->
  ('file -> int) ->
  ('file, 'value) directory_spec

(** [init ~lru_size directory_of] initialises a key-value store. This
   is a design where each directory is stored into a single file.

    For each virtual [directory], we use [directory_of] to get its specification,
   including its physical location and how data is layed out in its backing file.

    [lru_size] is a parameter that represents the number of different
   [values] that can be in memory. It is up to the user of this
   library to decide this number depending on the sizes of the values.
*)
val init :
  lru_size:int ->
  ('dir -> ('file, 'value) directory_spec) ->
  ('dir, 'file, 'value) t

(** [close kvs] waits until all pending reads and writes are completed
    and closes the key-value store. *)
val close : ('dir, 'file, 'value) t -> unit Lwt.t

(** [write_value ?(override=false) t key value] writes a value in the
   [key] value store. If a previous writing or read failed, the
   function will try again to write the value. If [override] is [true],
   the value will be written even though there is already a written
   value for this key.  *)
val write_value :
  ?override:bool ->
  ('dir, 'file, 'value) t ->
  'dir ->
  'file ->
  'value ->
  unit tzresult Lwt.t

(** [write_values ?(override=false) t seq] writes a sequence of [keys]
   [values] onto the store (see {!val:write_value}). If an error
   occurs, the first error is returned. This function guarantees that
   up to the data for which the error occured, the values where stored
   onto the disk. *)
val write_values :
  ?override:bool ->
  ('dir, 'file, 'value) t ->
  ('dir * 'file * 'value) Seq.t ->
  unit tzresult Lwt.t

(** [read_value t key] reads the value associated to [key] in the
   store. Fails if no value where attached to this [key]. The value
   read is the last one that was produced by a successful write. *)
val read_value :
  ('dir, 'file, 'value) t -> 'dir -> 'file -> 'value tzresult Lwt.t

(** [read_values t keys] produces a sequence of [values] associaed to
    the sequence of [keys]. This function is almost instantaneous
    since no reads are performed. Reads are done when the caller
    consumes the values of the sequence returned. *)
val read_values :
  ('dir, 'file, 'value) t ->
  ('dir * 'file) Seq.t ->
  ('dir * 'file * 'value tzresult) Seq_s.t

(** Same as {!read_value} expect that this function returns whether the given
    entry exists without reading it. *)
val value_exists : ('dir, 'file, 'value) t -> 'dir -> 'file -> bool Lwt.t

(** Same as {!read_values} expect that this function returns whether the given
    entries exist without reading them. *)
val values_exist :
  ('dir, 'file, 'value) t ->
  ('dir * 'file) Seq.t ->
  ('dir * 'file * bool) Seq_s.t
