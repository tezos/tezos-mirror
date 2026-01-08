(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** [default_net_timeout] is the default timeout used by functions in
   this library which admit a timeout value, i.e. [read_bytes_with_timeout],
   [Socket.connect], [Socket.recv]. *)
val default_net_timeout : Ptime.Span.t ref

(** [read_bytes_with_timeout ?timeout ?file_offset ?pos ?len fd buf] reads
    [len-pos] bytes from [fd] into [bytes]. If [file_offset] is given,
    {!Lwt_unix.pread} will be used instead of {!Lwt_unix.read}.

    @raise Lwt_unix.Timeout if the operation failed to finish within a
    [timeout] time span. *)
val read_bytes_with_timeout :
  ?timeout:Ptime.Span.t ->
  ?file_offset:int ->
  ?pos:int ->
  ?len:int ->
  Lwt_unix.file_descr ->
  bytes ->
  unit Lwt.t

(** [read_bytes ?file_offset ?pos ?len fd buf] reads
    [len-pos] bytes from [fd] into [bytes]. If [file_offset] is given,
    {!Lwt_unix.pread} will be used instead of {!Lwt_unix.read}. *)
val read_bytes :
  ?file_offset:int ->
  ?pos:int ->
  ?len:int ->
  Lwt_unix.file_descr ->
  bytes ->
  unit Lwt.t

val write_string :
  ?pos:int -> ?len:int -> Lwt_unix.file_descr -> string -> unit Lwt.t

(** [write_bytes ?file_offset ?pos ?len fd buf] writes [len-pos] bytes
    from [bytes] to [fd]. If [file_offset] is given, {!Lwt_unix.pwrite}
    will be used instead of {!Lwt_unix.write}.

    @raise Lwt_unix.Timeout if the operation failed to finish within a
    [timeout] time span. *)
val write_bytes :
  ?file_offset:int ->
  ?pos:int ->
  ?len:int ->
  Lwt_unix.file_descr ->
  Bytes.t ->
  unit Lwt.t

(** [is_directory path] tests if the given [path] (or the target of
    the symbolic link located at [path]) refers to a directory (file
    kind is [S_DIR]). *)
val is_directory : string -> bool Lwt.t

(** [dir_exists] tests if the given [path] (or the target of the
    symbolic link located at [path]) is an existing directory. [false]
    is returned either if the target does not exist or if it is not a
    directory. *)
val dir_exists : string -> bool Lwt.t

val remove_dir : string -> unit Lwt.t

(** [create_dir ?perm dir] creates the directory at the path [dir] and
    its parents recursively if they doesn't exist *)
val create_dir : ?perm:int -> string -> unit Lwt.t

(** [copy_dir ?perm src dst] copies the content of directory [src] in
    a fresh directory [dst] created with [perm] (0o755 by default).

    @raise Unix_error(ENOTDIR,_,_) if the given file is not a
    directory. *)
val copy_dir : ?perm:int -> string -> string -> unit Lwt.t

(** [hardlink_dir ?perm src dst] creates hardlinks for the content of directory
    [src] in a fresh directory [dst] with the same structure

    @raise Unix_error(ENOTDIR,_,_) if the given file is not a
    directory. *)
val hardlink_dir : ?perm:int -> string -> string -> unit Lwt.t

val read_file : string -> string Lwt.t

(** [copy_file ?buffer_size ~src ~dst ()] copies the file from [src]
    to [dst]. The permissions of the [dst] file are inherited from
    `Lwt_io.with_file`. The [buffer_size] parameter, which defaults to
    4096, can be adjusted to improve performance. *)
val copy_file :
  ?buffer_size:int -> src:string -> dst:string -> unit -> unit Lwt.t

(** [copy_file_raw ?buffer_size ?dst_perm ~src ~dst ()] is very
    similar to [copy_file] but it uses an alternate implementation
    using raw file descriptors, enabling better performances. It
    copies the file from [src] to [dst]. The permissions of the [dst]
    file is 0o666 by default. The [buffer_size] parameter, which
    defaults to 4096*1024, can be adjusted to improve performance. *)
val copy_file_raw :
  ?buffer_size:int ->
  ?dst_perm:int ->
  src:string ->
  dst:string ->
  unit ->
  unit Lwt.t

val create_file :
  ?close_on_exec:bool -> ?perm:int -> string -> string -> unit Lwt.t

val with_tempdir :
  ?temp_dir:string -> string -> (string -> 'a Lwt.t) -> 'a Lwt.t

val safe_close : Lwt_unix.file_descr -> unit tzresult Lwt.t

val getaddrinfo :
  passive:bool ->
  node:string ->
  service:string ->
  (Ipaddr.V6.t * int) list Lwt.t

(** [getpass ()] reads a password from stdio while setting-up the
    terminal to not display the password being typed. *)
val getpass : unit -> string

module Json : sig
  (** Loads a JSON file in memory *)
  val read_file : string -> Data_encoding.json tzresult Lwt.t

  (** (Over)write a JSON file from in memory data *)
  val write_file : string -> Data_encoding.json -> unit tzresult Lwt.t
end

val retry :
  ?log:('error -> unit Lwt.t) ->
  ?n:int ->
  ?sleep:float ->
  (unit -> ('a, 'error) result Lwt.t) ->
  ('a, 'error) result Lwt.t

(** [with_io_error] aims to be used as the error type for the [with_*]
   functions below. The [action] type is the action which trigerred
   the error. *)
type 'action io_error = {
  action : 'action;  (** action which triggerred the error. *)
  unix_code : Unix.error;  (** Unix code error. *)
  caller : string;  (** Unix function which triggerred the error. *)
  arg : string;  (** Argument given to the unix function: generally a path. *)
}

type error += Io_error of [`Close | `Open | `Rename | `Unlink | `Lock] io_error

val tzfail_of_io_error :
  [`Close | `Open | `Rename | `Unlink | `Lock] io_error -> 'b tzresult

(** [with_open_file ~flags ~perm filename f] opens the given file
   using {!Lwt_unix.open_file} and passes the resulting file-descriptor
   to [f]. [with_open_file] ensures that the file-descriptor is closed
   when the promise returned by [f] resolves, or if [f] raises an
   exception.

   See {!Lwt_unix.openfile} for a description of the arguments,
   warnings, and other notes. Default values for [perm] is [0o640].

   Exceptions raised whilst opening or closing the file are wrapped in
   [Error]. When the error is [`Open], the file could not be opened,
   and therefore the function [f] has not been run. When the error is
   [`Closed], the function [f] was run but the file could not be
   closed.

   Other exceptions are reraised. *)
val with_open_file :
  flags:Unix.open_flag list ->
  ?perm:Unix.file_perm ->
  string ->
  (Lwt_unix.file_descr -> 'a Lwt.t) ->
  ('a, [> `Open | `Close] io_error) result Lwt.t

(** [with_open_out ?overwrite filename f] uses [with_open_file] with
   the flags [O_WRONLY; O_CREAT; O_CLOEXEC] and the default
   permissions. The flag [O_TRUNC] is added if [overwrite] is [true],
   which is the case by default. *)
val with_open_out :
  ?overwrite:bool ->
  string ->
  (Lwt_unix.file_descr -> 'a Lwt.t) ->
  ('a, [> `Open | `Close] io_error) result Lwt.t

(** [with_atomic_open_out ?(overwrite=true) filename ?temp_dir f] is a
   wrapper around [with_open_out] that ensures that the data are
   written onto [filename] in an atomic way.

   This function uses a temporary file stored in the [temp_dir]
   directory. Then, this temporary filed is renamed as [filename].

   The renaming may fail, for example if the temporary file is not on
   the same partition as [filename].

   If the renaming fails, an error [`Rename] is returned. See
   {!with_open_file} for a description of the other errors. In that
   case, no write have been done on [filename].

   The default value of [temp_dir] is the same as
   [Filename.temp_file]. *)
val with_atomic_open_out :
  ?overwrite:bool ->
  string ->
  ?temp_dir:string ->
  (Lwt_unix.file_descr -> 'a Lwt.t) ->
  ('a, [> `Open | `Close | `Rename] io_error) result Lwt.t

(** [with_open_in filename f] uses [with_open_file] with the flags
   [O_RDONLY; O_CLOEXEC] and the default permissions. *)
val with_open_in :
  string ->
  (Lwt_unix.file_descr -> 'a Lwt.t) ->
  ('a, [> `Open | `Close] io_error) result Lwt.t

val safe_cancel_on_exit : (unit -> 'a Lwt.t) -> 'a Lwt.t
