(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(** Run Tezt regression tests and capture their output.

    NOTE: consider using dune cram when it becomes available:
    https://dune.readthedocs.io/en/stable/tests.html#cram-tests
*)

(** Register a regression test.

    This function is a wrapper around [Test.register]. It adds the "regression" tag
    to the [tags] list provided by the argument.

    The [output_file] specifies the name of the file where the output of the test is stored
    and read to be compared on subsequent runs. The actual filename is
    [Cli.options.regression_dir // output_file ^ ".out"].

    Note that [output_file] can contain subdirectories.
    For instance, using the default [--regression-dir],
    [~output_file:"math/sqrt"] will result in the creation of directory
    ["tezt/_regressions/math"] if needed and regression output will be written
    in ["tezt/_regressions/math/sqrt.out"]. *)
val register :
  __FILE__:string ->
  title:string ->
  tags:string list ->
  output_file:string ->
  (unit -> unit Lwt.t) ->
  unit

(** Capture some output of a regression test.

    Call this to record a string into the [output_file] given to [register].
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
    its execution into the registered [output_file]. *)
val hooks : Process.hooks
