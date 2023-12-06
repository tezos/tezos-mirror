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

    The user of the key-value store can specify a "layout". This
    layout aims to specify in which memory-mapped files the keys are
    stored. It is up to the user to decide of the layout to store the
    values. It is recommended that values that will be accessed
    together frequently should be stored in the same file.

    This layout assumes that the size of all the values stored is a
    constant.
 *)

(** An abstract representation of a key-value store. *)
type ('file, 'key, 'value) t

(** A [layout] specifies the layout of virtual files storing data of type ['value],
    as well as the name of the underlying physical file. *)
type ('key, 'value) layout

(** [layout ?encoded_value_size value_encoding path eq index_of] describes a layout.
    - [encoded_value_size] is the size in bytes of values encoded with [value_encoding].
      If [value_encoding] does not respect this property, the behaviour of the store
      is undefined. If [encoded_value_size] is not given, [value_encoding] must be of fixed length.
    - [value_encoding] is an encoding for values
    - [path] is the path of the physical file
    - [eq] is an equality function on values.
    - [index_of] gives the index of a given key in the file.

   @raise Invalid_argument if [encoded_value_size=None] and [value_encoding] does not have a fixed length.
 *)
val layout :
  ?encoded_value_size:int ->
  encoding:'value Data_encoding.t ->
  filepath:string ->
  eq:('value -> 'value -> bool) ->
  index_of:('key -> int) ->
  unit ->
  ('key, 'value) layout

(** [init ~lru_size layout] initialises a key-value store. This is a
    design where all keys/values are stored into a single physical
    file.

    For each file, we rely on a [layout] to explaining where to find
    the values. The layout also specifies the physical location of the
    file.

    [lru_size] is a parameter that represents the number of different
   [values] that can be in memory. It is up to the user of this
   library to decide this number depending on the sizes of the values.
*)
val init :
  lru_size:int -> ('file -> ('key, 'value) layout) -> ('file, 'key, 'value) t

(** [close kvs] waits until all pending reads and writes are completed
    and closes the key-value store. *)
val close : ('file, 'key, 'value) t -> unit Lwt.t

(** [write_value ?(override=false) t file key value] writes a value in
    the [key] value store in file [file]. If a previous writing or
    read failed, the function will try again to write the value. If
    [override] is [true], the value will be written even though there
    is already a written value for this key. *)
val write_value :
  ?override:bool ->
  ('file, 'key, 'value) t ->
  'file ->
  'key ->
  'value ->
  unit tzresult Lwt.t

(** [write_values ?(override=false) t seq] writes the sequence [seq]
    onto the store (see {!val:write_value}). If an error occurs, the
    first error is returned. This function guarantees that up to the
    data for which the error occured, the values where stored onto the
    disk. *)
val write_values :
  ?override:bool ->
  ('file, 'key, 'value) t ->
  ('file * 'key * 'value) Seq.t ->
  unit tzresult Lwt.t

(** [read_value t file key] reads the value associated to [key] in
    [file] in the store. Fails if no value were attached to this
    [key]. The value read is the last one that was produced by a
    successful write. *)
val read_value :
  ('file, 'key, 'value) t -> 'file -> 'key -> 'value tzresult Lwt.t

(** [read_values t keys] produces a sequence of [values] associated to
    the sequence of [keys]. This function is almost instantaneous
    since no reads are performed. Reads are done when the caller
    consumes the values of the sequence returned. *)
val read_values :
  ('file, 'key, 'value) t ->
  ('file * 'key) Seq.t ->
  ('file * 'key * 'value tzresult) Seq_s.t

(** Same as {!read_value} expect that this function returns whether the given
    entry exists without reading it. *)
val value_exists : ('file, 'key, 'value) t -> 'file -> 'key -> bool Lwt.t

(** Same as {!read_values} expect that this function returns whether the given
    entries exist without reading them. *)
val values_exist :
  ('file, 'key, 'value) t ->
  ('file * 'key) Seq.t ->
  ('file * 'key * bool) Seq_s.t
