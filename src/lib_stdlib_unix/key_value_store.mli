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

    Each key-value pair is associated to a virtual file. Each virtual file has a
    layout, given by the user, which specifies, among other things, an
    underlying physical file for storing the key-value pairs.  It is up to the
    user to decide which virtual file to use to store a key-value pair. It is
    recommended that values that will be accessed together frequently should be
    stored in the same file.

    In a virtual file, all the stored values should the same size. Each key is
    associated with an index from 0 to some maximum value (see
    implementation). Different keys should be associated to different
    indexes.

    A maximum of 4096 values can be stored in a file.

    To avoid I/Os, the store keeps recently used values in memory, as
    follows. It maintains an LRU (least-recently used) of a given maximum (see
    {!init}) of open files. For each open file, there is an associated cache
    containing all read/write values since the file was opened (only the most
    recent value for a given key, of course). *)

(** An abstract representation of a file-based key-value store. *)
type ('file, 'key, 'value) t

(** A [layout] specifies the layout of a virtual file storing keys of type
    ['key] and values of type ['value], as well as the path to the underlying
    physical file. *)
type ('key, 'value) layout

(** [layout ?encoded_value_size value_encoding path eq index_of] describes a
    virtual file layout.
    - [encoded_value_size] is the size in bytes of any encoded value.  If
    encoded values have different sizes, the behaviour of the store is
    undefined. If [encoded_value_size] is not given, then the encoded value size
    is deduced from [value_encoding], which must be of fixed length.
    - [value_encoding] is an encoding for values.
    - [path] is the path of the physical file.
    - [eq] is an equality function on values.
    - [index_of] gives the index of a given key.

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

(** [init ~lru_size layout_of] initialises a file-based key-value
    store. [layout_of file] returns a {!type-layout} for a given virtual file
    [file]. All the keys/values associated to [file] are stored in a single
    physical file.

    [lru_size] is a parameter that represents maximum number of open files. It
    is up to the user of this library to decide this number depending on the
    sizes of the values.
*)
val init :
  lru_size:int -> ('file -> ('key, 'value) layout) -> ('file, 'key, 'value) t

(** [close kvs] waits until all pending reads and writes are completed
    and closes the key-value store. *)
val close : ('file, 'key, 'value) t -> unit Lwt.t

(** [write_value ?(override=false) t file key value] writes a value in the [key]
    value store in file [file]. If there is already a written value for this
    key, then the value will be written if and only if [override] is [true]. *)
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
    data for which the error occured, the values were stored onto the
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
val value_exists :
  ('file, 'key, 'value) t -> 'file -> 'key -> bool tzresult Lwt.t

(** Same as {!read_values} expect that this function returns whether the given
    entries exist without reading them. *)
val values_exist :
  ('file, 'key, 'value) t ->
  ('file * 'key) Seq.t ->
  ('file * 'key * bool tzresult) Seq_s.t

(** [remove_file t file] removes the corresponding physical file of
    [file] from the disk as well as the corresponding keys/values of
    the store. In case of concurrent read/write, this function should
    succeed no matter what. The result of [read/write] depends on
    which function was issued first. For example if the [read] was
    issued before the [remove_file], it will returns the corresponding
    value that was stored, and then the file will be removed. *)
val remove_file : ('file, 'key, 'value) t -> 'file -> unit tzresult Lwt.t

(** This function returns the number of entries for a given file. *)
val count_values : ('file, 'key, 'value) t -> 'file -> int tzresult Lwt.t
