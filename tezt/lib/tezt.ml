(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Tezt. *)

(** Tezt (pronounced "tezty", as in "tasty" with a "z") is a test framework for OCaml.
    It is well suited for writing and executing unit, integration and
    regression tests. It integrates well with continuous integration (CI).

    Tezt provides a function ({!Test.register}) to register tests.
    Tests have a title, some tags, and an implementation (a function).
    Titles and tags can be used on the command-line to select which tests to run.
    The name of the file in which the test was registered can also be used to do so.
    The implementation of the test is ran if the test is selected.
    If this implementation does not raise an exception (for instance by calling
    {!Test.fail}), the test is considered to be successful.
    Along with modules {!Base}, which is supposed to be [open]ed a la [Pervasives]
    and which provides a few useful generic functions, and {!Check}, which provides
    ways to perform assertions with nice error messages, the result is a framework
    that is suitable for unit tests.

    Tests can be ran in a CI, optionally producing JUnit reports.
    Tezt can automatically compute a partition of the set of tests
    where subsets are balanced to take roughly the same amount of time to run.
    The intent is that each of those subset can be one CI job, resulting in automatic
    balanced parallelisation.

    Specific features supporting integration tests include:
    - running external processes;
    - invoking distant runners through SSH;
    - decoding JSON values (e.g. to test REST APIs);
    - cleaning up temporary files automatically.

    Specific features supporting regression tests include:
    - capturing output from local or external processes;
    - applying regular-expression-based substitutions to those outputs;
    - comparing with previous captured outputs.

    Tezt provides a flexible user interface:
    - using colors in logs (e.g. to distinguish external processes or to make error
      messages easy to see);
    - adjusting automatically the verbosity of logs around errors;
    - making it easy to copy-paste the invoked commands to reproduce errors;
    - giving extensive information to precisely locate errors;
    - supporting flaky (randomly failing) tests, by running them repeatedly.

    To get started, register tests with {!Test.register}, then call the main function
    {!Test.run}. Tests are run from the command-line, for instance with [dune runtest]
    or [dune exec]. The latter gives access to a large list of command-line options
    to select which tests to run and how. Execute your program with [--help] to
    get the list. See also the
    {{: https://research-development.nomadic-labs.com/announcing-tezt.html } Tezt mini-tutorial}. *)

(** {2 Backends} *)

(** Tezt executables can be compiled to bytecode, native code, or JavaScript.
    This is reflected by three Dune libraries:
    - code which is common to all backends is provided by [tezt.core];
    - the bytecode and native code backends are provided by [tezt];
    - the JavaScript backend is provided by [tezt.js].

    Function [Test.run] is only available in [tezt] and [tezt.js].
    In other words, to actually run the tests you have to decide whether
    to use the JavaScript backend or not.

    The difference between [tezt] and [tezt.js] is that [tezt.js] does not provide:
    - the [Process] module;
    - the [Temp] module;
    - the [Runner] module.
    So the JavaScript backend is not well suited for integration tests and regression tests.
    But it does support unit tests.

    If you want to run your tests on multiple backends you have to write
    two executables: one linked with [tezt] and one linked with [tezt.js].
    To share the code of your tests, you can write your calls to [Test.register]
    in a library that depends on [tezt.core]. Here is an example of [dune] files:
    {[
; Dune file for the library that calls [Test.register].
(library
 (name tezts)
 (libraries tezt.core)
 (library_flags (:standard -linkall))
 (flags (:standard) -open Tezt_core -open Tezt_core.Base))

; Dune file for the executable used to run tests in native or bytecode mode.
(executable (name main) (libraries tezts tezt))

; Dune file for the executable used to run tests using nodejs.
(executable (name main_js) (modes js) (libraries tezts tezt.js))
    ]} *)

(** {2 Modules} *)

(** Support for running promises in the background.

    Background promises are promises that are not bound.
    Registering them makes sure that Tezt handles their exceptions correctly. *)
module Background = Background

(** Base primitives useful in writing tests.

    This module is intended to be [open]ed, as an extension of [Stdlib].
    It contains functions to manage concurrency, I/Os, regular expressions, etc. *)
module Base = Base

(** Support for expressing assertions. *)
module Check = Check

(** Command-line interface options. *)
module Cli = Cli

(** Compute the difference between two sequences of items. *)
module Diff = Diff

(** JSON handling (encoding/decoding, accessors, error management, etc). *)
module JSON = JSON

(** Functions for logging messages. *)
module Log = Log

(** Managing external processes. *)
module Process = Process

(** Process hooks, in particular for use with regression tests. *)
module Process_hooks = Process_hooks

(** Regression test helpers (run tests, capture output, etc). *)
module Regression = Regression

(** Runner specifications for processes spawned remotely using SSH. *)
module Runner = Runner

(** Temporary file management. *)
module Temp = Temp

(** Base test management (registering, initializations, etc). *)
module Test = struct
  include Test

  (** Run registered tests that should be run.

      This function is not available in [tezt.core].
      It is available in [tezt] and [tezt.js]. *)
  let run = Main.run
end
