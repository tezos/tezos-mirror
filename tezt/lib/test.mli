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

(** Base test functions. *)

(** Add a function to be called before each test start.

    Used to reset counters such as the ones which are used to
    choose default process names. *)
val declare_reset_function : (unit -> unit) -> unit

(** Log an error and stop the test right here.

    If the optional location [__LOC__] is provided,
    it is prepended to the error message.
    You would typically use it simply as [Test.fail ~__LOC__ "..."]
    to prepend location of the failure. *)
val fail : ?__LOC__:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** Register a test.

    The [__FILE__] argument, which should be equal to [__FILE__]
    (i.e. just write [Test.run ~__FILE__]), is used to let the user
    select which files to run from the command-line.

    One should be able to infer, from [title], what the test will do.
    It is typically a short sentence like ["addition is commutative"]
    or ["server runs until client tells it to stop"].

    The list of [tags] must be composed of short strings which are
    easy to type on the command line (lowercase letters, digits
    and underscores). Run the test executable with [--list]
    to get the list of tags which are already used by existing tests.
    Try to reuse them if possible.

    The last argument is a function [f] which implements the test.
    If [f] needs to spawn external processes, it should use
    the [Process] module to do so, so that those processes are automatically
    killed at the end of the test. Similarly, if this function needs to
    create temporary files, it should declare them with [Temp], and if it
    needs to have promises that run in the background, it should use
    [Background.register] (and not [Lwt.async] in particular).

    If [f] raises an exception, act as if [fail] was called (without the
    error location unfortunately).

    You can call [register] several times in the same executable if you want
    it to run several tests. Each of those tests should be standalone, as
    the user is able to specify the list of tests to run on the command-line.

    The test is not actually run until you call {!run}. *)
val register :
  __FILE__:string ->
  title:string ->
  tags:string list ->
  (unit -> unit Lwt.t) ->
  unit

(** Run registered tests that should be run.

    Call this once you have registered all tests.
    This will check command-line options and run the tests that have been selected,
    or display the list of tests. *)
val run : unit -> unit

(** Get the current worker id.

    This returns a value between [0] and [Cli.options.job_count - 1] that uniquely
    identifies the current worker process that is running the current test.

    When called outside of a test, this always returns [0]. *)
val current_worker_id : unit -> int
