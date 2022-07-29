(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2020-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Run Tezt regression tests and capture their output.

    NOTE: consider using dune cram when it becomes available:
    https://dune.readthedocs.io/en/stable/tests.html#cram-tests
*)

(** Register a regression test.

    This function is a wrapper around [Test.register]. It adds the "regression" tag
    to the [tags] list provided by the argument.

    Output which is captured (with {!capture}) is recorded in a file named
    [<ROOT>/<DIR>/expected/<BASE>/<FILE>.out] where:
    - [<ROOT>] is the root directory of the project, read from environment variable
      [DUNE_SOURCEROOT] if available, else [PWD] if available, else using [Sys.getcwd];
    - [<DIR>] is [Filename.dirname __FILE__];
    - [<BASE>] is [Filename.basename __FILE__];
    - [<FILE>] is [~file], which defaults to a sanitized and possibly truncated
      version of [~title]. *)
val register :
  __FILE__:string ->
  title:string ->
  tags:string list ->
  ?file:string ->
  (unit -> unit Lwt.t) ->
  unit

(** Capture some output of a regression test.

    Call this to record a string into the expected output file of the current test.
    A newline character [\n] will be added after it.

    This function only records its argument when called while a regression test is running,
    i.e. from the body of [Regression.register]. If you call it outside of
    [Regression.register], it has no effect. So you can define a function that captures
    and use it with or without regression testing.

    A typical use is to define custom process hooks that substitute non-deterministic
    parts of the output with deterministic ones. See also {!hooks}. *)
val capture : string -> unit

(** Hooks that enable regression testing when attached to a process ran from a
    {!register}ed regression test function.

    The hooks will {!capture} the spawned command, its arguments and the output of
    its execution. *)
val hooks : Process_hooks.t
