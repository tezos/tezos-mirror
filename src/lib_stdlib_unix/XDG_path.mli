(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2023 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Implementation of the XDG Base Directory Specification. *)

(** See specification at:
    https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html *)

(** This module is not portable. It should only be used on systems that follow
    the XDG Base Directory Specification, or for which the default values
    specified by this specification (such as [$HOME/.local/share], [$HOME/.cache]...)
    make sense. In particular, it should only be used on systems for which the
    directory separator is the forward slash [/].

    For portability, one idea would be to implement similar modules for other
    similar conventions, and then provide a wrapper that chooses the right module
    depending on the system. *)

(** File types.

    You should be very careful to choose the right type for your files
    because it determines where it will be stored and, as a result, what
    the retention policy is. If you place data in a configuration directory
    for instance, users may not notice that the backups for their data directory
    do not contain the data of your application. They may lose this data as a result.

    - [Data] files are files like documents, images, databases, etc.
      This includes both data which is needed to run the application
      (such as images and sounds for video games, cryptographic certificates, ...)
      and data which was produced by the user using the application.
      The former can usually be restored by reinstalling, but losing the latter
      is usually bad news for users.

    - [Config] files are files that are used to store parameters
      that control the behavior of an application.
      Losing configuration files can make it harder for the user to produce new data,
      but at least old data is not lost.

    - [State] data is data that should persist between application restarts,
      but that is not important. It can be log files, undo history, layouts,
      list of open files, recently used files... State data is often not
      portable between systems or even between versions of the same application.
      Losing state data is usually a minor inconvenience.

    - [Bin] files are executable files. Usually one wants them to be in the [$PATH].

    - [Cache] files are non-essential files that can be reproduced from other
      files, such as data or configuration files. They are usually used to speed
      up applications. Losing cache files is usually a minor inconvenience.

    - [Runtime] files are non-essential files that are needed by the application
      as long as it is running, such as files used for communication and
      synchronization purposes. This includes sockets and named pipes in particular.
      Those files should not be large. Those files are supposed to be deleted
      by the system when the user logs out completely. *)
type kind = Data | Config | State | Bin | Cache | Runtime

(** What you intend to do with a path.

    If you intend to load a file and never write it, use [Read_only].
    This causes [get] to search for the file in all possible locations.
    Those locations include directories where the user may not have
    write permissions, so you should not access those files for writing.

    If you intend to write into a file, use [Read_write].
    This causes [get] to return a location to which the user should be
    able to write.

    If you intend to both read and write, you may want to either:
    - read using [Read_only] and then write using [Read_write], possibly
      creating a new file in a different location;
    - or read using [Read_write] and then write into the same file, to ensure
      that the file that is written is at the same location as the file that
      was read.

    Choosing between [Read_only] or [Read_write] only makes a difference
    for [Data] and [Config] files. For other file types, there is only one
    possible location which is user-specific and thus both intents give
    the same result. In particular, you cannot use [get Read_only Bin] to find
    globally installed executables; it only returns user-specific files.

    The right choice depends on your application and on the file type.
    For reading configuration files you should use [Read_only],
    so that [get] can find configuration files that are installed globally
    on the system for all users. You can then use [Read_write] to write a
    different configuration file which would be specific to the current user.
    For data files that are packaged with the application, you should also
    use [Read_only], so that globally installed files can be found.
    However, for user-specific data, it may make sense to use [Read_write]
    even if you only intend to read them in the immediate future, if there
    is a chance that you may want to modify them later. *)
type intent = Read_only | Read_write

(** Get the location of a file or directory.

    Usage: [get intent kind ~path]

    This returns [fullpath] equal to [Filename.concat dir path]
    where [dir] is chosen according to [intent] and [kind].

    If [intent] is [Read_write], [fullpath] is the location where
    the requested file or directory should be created.
    It may or may not already exist.

    If [intent] is [Read_only], [fullpath] is the first location for
    which [Sys.file_exists path] returns [true], for the list of possible
    values for [dir]. If no such location exists, [fullpath] is the location
    where the requested file or directory should be created.

    The [get] function is slightly opinionated in places where the
    XDG Base Directory Specification is a bit vague. Below is the list
    of opinionated choices that [get] makes.

    - If [$HOME] is not set or is empty, it defaults to [Filename.current_dir_name].

    - The specification gives default values for when environment variables are
      not set or empty, and says to ignore relative paths. It does not explicitly
      specify what to do when environment variables only contain relative paths.
      [get] chooses to use the default values as if the variable was not set.

    - The specification does not give a default value for [$XDG_RUNTIME_DIR].
      [get] chooses to use [Filename.get_temp_dir_name ()] as the default.
      This is especially debatable because this directory may not satisfy
      the conditions given by the specification of [$XDG_RUNTIME_DIR].
      You should thus check permissions and maybe not rely on the system
      cleaning up files for you in this directory. *)
val get : intent -> kind -> path:string -> string
