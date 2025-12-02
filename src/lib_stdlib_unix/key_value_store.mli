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

type error +=
  | Missing_stored_kvs_data of {filepath : string; index : int}
  | Wrong_encoded_value_size of {
      file : string;
      index : int;
      expected : int;
      got : int;
    }
  | Closed of {action : string}
  | Corrupted_data of {action : string; filepath : string; index : int}
  | Encoding_failed of {filepath : string; index : int}
  | Decoding_failed of {filepath : string; index : int}

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

    A maximum of [file_prefix_bitset_size] (currently 4096) values can be stored in a file.

    To avoid I/Os, the store keeps recently used values in memory, as
    follows. It maintains an LRU (least-recently used) of a given maximum (see
    {!init}) of open files. For each open file, there is an associated cache
    containing all read/write values since the file was opened (only the most
    recent value for a given key, of course). *)

(** A [layout] specifies the layout of a virtual file storing keys of type
    ['key] and values of type ['value], as well as the path to the underlying
    physical file. *)
type ('key, 'value) layout

(** The type of functions to compute the layout of a given file. *)
type ('file, 'key, 'value) file_layout =
  root_dir:string -> 'file -> ('key, 'value) layout

(** Number of bytes reserved at the beginning of each file to store the bitset.
    This bitset indicates which key-value slots are occupied. *)
val file_prefix_bitset_size : int

(** [layout ?encoded_value_size value_encoding path eq index_of
    ~number_of_keys_per_file] describes a virtual file layout.

    - [encoded_value_size] is the size in bytes of any encoded value.  If
    encoded values have different sizes, the behaviour of the store is
    undefined. If [encoded_value_size] is not given, then the encoded value size
    is deduced from [value_encoding], which must be of fixed length.
    - [value_encoding] is an encoding for values.
    - [path] is the path of the physical file.
    - [eq] is an equality function on values.
    - [index_of] gives the index of a given key.

   @raise Invalid_argument if [encoded_value_size=None] and [value_encoding]
     does not have a fixed length.

   The user should provide the number of keys stored per file, which should not
   exceed 4096.
*)
val layout :
  ?encoded_value_size:int ->
  encoding:'value Data_encoding.t ->
  filepath:string ->
  eq:('value -> 'value -> bool) ->
  index_of:('key -> int) ->
  number_of_keys_per_file:int ->
  unit ->
  ('key, 'value) layout

module Read : sig
  (** An abstract representation of a file-based key-value store. *)
  type ('file, 'key, 'value) t

  (** [read_value t file_layout file key] reads the value associated to [key] in the
      [file] in the store. Fails if no value were attached to this [key]. The
      value read is the last one that was produced by a successful write. *)
  val read_value :
    ('file, 'key, 'value) t ->
    ('file, 'key, 'value) file_layout ->
    'file ->
    'key ->
    'value tzresult Lwt.t

  (** [read_values t file_layout keys] produces a sequence of [values] associated to
      the sequence of [keys]. This function is almost instantaneous since no reads
      are performed. Reads are done when the caller consumes the values of the
      sequence returned. *)
  val read_values :
    ('file, 'key, 'value) t ->
    ('file, 'key, 'value) file_layout ->
    ('file * 'key) Seq.t ->
    ('file * 'key * 'value tzresult) Seq_s.t

  (** variant of read_values, but decodes the shards stored in the given bytes
      using the given store's layout. *)
  val read_values_from_bytes :
    ('file, 'key, 'value) file_layout ->
    bytes ->
    ('file * 'key) Seq.t ->
    ('file * 'key * 'value tzresult) Seq_s.t

  (** Same as {!read_value} expect that this function returns whether the given
      entry exists without reading it. *)
  val value_exists :
    ('file, 'key, 'value) t ->
    ('file, 'key, 'value) file_layout ->
    'file ->
    'key ->
    bool tzresult Lwt.t

  (** Same as {!read_values} expect that this function returns whether the given
      entries exist without reading them. *)
  val values_exist :
    ('file, 'key, 'value) t ->
    ('file, 'key, 'value) file_layout ->
    ('file * 'key) Seq.t ->
    ('file * 'key * bool tzresult) Seq_s.t

  (** This function returns the number of entries for a given file. *)
  val count_values :
    ('file, 'key, 'value) t ->
    ('file, 'key, 'value) file_layout ->
    'file ->
    int tzresult Lwt.t

  (** [init ?lockfile ~lru_size root_dir] initialises a read-only file-based
      key-value store. The [root_dir] must exist on disk.

      [lru_size] is the maximum number of open files kept in the LRU cache.
      Choose this value based on your value sizes and available memory.

      The implementation always uses a lockfile to coordinate access. If
      [lockfile] is provided, that path is used; if the lockfile is already
      held by another process, this function returns an error. If [lockfile] is
      not provided, a unique, randomly generated lockfile path is used, allowing
      concurrent opens even from different processes.

      Note that lockfiles do not prevent concurrent opens by the same process;
      use a mutex if needed for intra-process coordination.

      This function is intended for scenarios where multiple processes need
      read-only access to the same store. Since the returned store is read-only,
      concurrent writes from different processes are not possible, avoiding disk
      corruption. However, reads may be inconsistent if performed while another
      process is writing to the underlying files.
  *)
  val init :
    ?lockfile:string ->
    lru_size:int ->
    string ->
    ('file, 'key, 'value) t tzresult Lwt.t

  (** [close kvs] waits until all pending reads and writes are completed
      and closes the key-value store. *)
  val close : ('file, 'key, 'value) t -> unit tzresult Lwt.t
end

(** All functions from the Read module are available at the top level. *)
include module type of Read

(** [init ~lru_size ~root_dir] is the same as {!val:Read.init} but uses a fixed
    lockfile path derived from [root_dir]. This prevents opening multiple
    read-write stores on the same [root_dir] from different processes.
    If [root_dir] is not present, it is created.
*)
val init :
  lru_size:int -> root_dir:string -> ('file, 'key, 'value) t tzresult Lwt.t

(** [root_dir t] returns the [root_dir] directory used to create [t]. *)
val root_dir : ('file, 'key, 'value) t -> string

(** [write_value ?(override=false) t file_layout file key value] writes a value in
    the [key] value store in [file]. If there is already a written
    value for this key, then the value will be written if and only if [override]
    is [true]. *)
val write_value :
  ?override:bool ->
  ('file, 'key, 'value) t ->
  ('file, 'key, 'value) file_layout ->
  'file ->
  'key ->
  'value ->
  unit tzresult Lwt.t

(** [write_values ?(override=false) t file_layout seq] writes the sequence [seq] onto
    the store (see {!val:write_value}). If an error occurs, the first error is
    returned. This function guarantees that up to the data for which the error
    occured, the values were stored onto the disk. *)
val write_values :
  ?override:bool ->
  ('file, 'key, 'value) t ->
  ('file, 'key, 'value) file_layout ->
  ('file * 'key * 'value) Seq.t ->
  unit tzresult Lwt.t

(** [remove_file t file_layout] removes the corresponding physical file of
    [file] from the disk as well as the corresponding keys/values of
    the store. In case of concurrent read/write, this function should
    succeed no matter what. The result of [read/write] depends on
    which function was issued first. For example if the [read] was
    issued before the [remove_file], it will return the corresponding
    value that was stored, and then the file will be removed. *)
val remove_file :
  ('file, 'key, 'value) t ->
  ('file, 'key, 'value) file_layout ->
  'file ->
  unit tzresult Lwt.t

module View : sig
  (** Returns the number of files currently opened by the key value
      store. Do note this number is an upper bound on the number of
      file descriptors opened. This number should always be lower than [lru_size].
  *)
  val opened_files : ('file, 'key, 'value) t -> int

  (** Returns the number of ongoing actions happening on different
    files. This number should always be lower than [lru_size]. *)
  val ongoing_actions : ('file, 'key, 'value) t -> int
end

module Internal_for_tests : sig
  (** Same as {!init} above, except that the user can specify a prefix for the
      lock file (default is lockfile_prefix = "internal_for_tests") to avoid issues
      if the store is already locked by another process, such as:

      IO error in lockf(): Resource temporarily unavailable)
  *)
  val init :
    ?lockfile_prefix:string ->
    lru_size:int ->
    root_dir:string ->
    unit ->
    ('file, 'key, 'value) t tzresult Lwt.t
end
