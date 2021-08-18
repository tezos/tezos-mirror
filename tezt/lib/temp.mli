(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Temporary files for tests. *)

(** Get a temporary file name.

    For instance:
    - [file "hello.ml"] may return something like ["/tmp/tezt-1234/1/hello.ml"];
    - [file "some/dir/hello.ml"] may return ["/tmp/tezt-1234/1/some/dir/hello.ml"];
    - [file "/dir/hello.ml"] may return ["/tmp/tezt-1234/1/dir/hello.ml"].

    This function also creates the directory (and its parents) needed to host the
    resulting file.

    [perms] is the permissions for parent directories if they are created.
    Default is [0o755], i.e. [rwxr-xr-x].

    If [runner] is specified, the temporary file is registered to be located
    in this remote runner, which means that it will be removed using SSH
    by the [clean up] function.

    [file base] always returns the same result for a given [base]
    and for the same process. *)
val file : ?runner:Runner.t -> ?perms:Unix.file_perm -> string -> string

(** Get a temporary file name and create it as a directory. *)
val dir : ?runner:Runner.t -> ?perms:Unix.file_perm -> string -> string

(** Allow calls to [file] and [dir] until the next [clean_up] or [stop].

    Return the main temporary directory, e.g. ["/tmp/tezt-1234/1"],
    so that it can be displayed to users.

    Calls to [file] and [dir] which are made before [start] result in an error.

    Don't call this directly, it is called by {!Test.run}.
    This prevents you from creating temporary files accidentally before your test
    actually runs. Indeed, if your test is disabled from the command-line it should
    not create temporary files. By using {!Test.run} you also ensure that {!clean_up}
    is called. *)
val start : unit -> string

(** Disallow calls to [file] and [dir] until the next [start].

    This can be called in place of [clean_up] if you want to keep temporary files. *)
val stop : unit -> unit

(** Delete temporary files and directories.

    All files which were returned by [file] are deleted.
    All directories which were returned by [dir] are deleted, but only if they
    did not already exist.
    All parent directories that were created are then deleted, but only if they are empty. *)
val clean_up : unit -> unit
