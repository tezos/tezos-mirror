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

(** Wrapper to run Alcotest tests with Tezt as a backend. *)

(** This module provides a subset of the interface of the [Alcotest] module
    of the real Alcotest library. But instead of using Alcotest to run the tests,
    it uses Tezt. This allows to transition from Alcotest to Tezt without
    having to actually change the code. In turn, this allows to benefit from
    Tezt features such as auto-balancing in the CI. One can then use Tezt's
    modules in the test, to gradually migrate the test to Tezt and stop using
    this wrapper.

    This module is intended as a way to help transition from Alcotest to Tezt,
    not to use both at the same time forever. First use Alcotezt to have the test
    runnable using Tezt, as a quick win to get auto-balancing etc. Then stop using
    Alcotest functions and migrate existing calls at the pace that is convenient
    for you. *)

(** Return type for tests. *)
type return = unit

(** Speed levels.

    In Alcotest, one can ask not to run slow tests with [-q] from the command-line.
    In Tezt, the equivalent is [-a quick]. *)
type speed_level = [`Quick | `Slow]

(** Test cases.

    The name of the Alcotest test case appears in Tezt's logs,
    but it is not used in the Tezt test title, nor as a tag. *)
type 'a test_case = string * speed_level * ('a -> return)

(** Make a test case. *)
val test_case : string -> speed_level -> ('a -> return) -> 'a test_case

(** Can be raised to fail a test. *)
exception Test_error

(** Tests.

    In Tezt, the name of the test is used as the title of the test. *)
type 'a test = string * 'a test_case list

(** Run a test suite.

    In Tezt, this calls [Test.register] but does not actually run the test suite.
    The name of the test suite is used as the filename for the Tezt test. *)
val run : __FILE__:string -> string -> unit test list -> return

(** Values that can be tested with {!check}. *)
type 'a testable = (module Tezt_core.Check.EQUALABLE with type t = 'a)

(** [testable pp eq] is a new {!type-testable} with the pretty-printer [pp] and
    equality [eq]. *)
val testable :
  (Format.formatter -> 'a -> unit) -> ('a -> 'a -> bool) -> 'a testable

(** [pp t] is [t]'s pretty-printer. *)
val pp : 'a testable -> Format.formatter -> 'a -> return

(** [of_pp pp] tests values which can be printed using [pp] and compared using {!Stdlib.compare} *)
val of_pp : (Format.formatter -> 'a -> return) -> 'a testable

(** [equal t] is [t]'s equality. *)
val equal : 'a testable -> 'a -> 'a -> bool

(** The [string] testable type. *)
val string : string testable

(** The boolean testable type. *)
val bool : bool testable

(** The bytes testable type. *)
val bytes : bytes testable

(** The int64 testable type. *)
val int64 : int64 testable

(** The int32 testable type. *)
val int32 : int32 testable

(** The int testable type. *)
val int : int testable

(** The float testable type. *)
val float : float -> float testable

(** The list testable type. *)
val list : 'a testable -> 'a list testable

(** The array testable type. *)
val array : 'a testable -> 'a array testable

(** The pair testable type. *)
val pair : 'a testable -> 'b testable -> ('a * 'b) testable

(** The triple testable type. *)
val triple :
  'a testable -> 'b testable -> 'c testable -> ('a * 'b * 'c) testable

(** The result testable type. *)
val result : 'a testable -> 'e testable -> ('a, 'e) result testable

(** The option testable type. *)
val option : 'a testable -> 'a option testable

(** Check that two values are equal.

    In Tezt, this becomes [Check.(=)] where [~error_msg] is
    [msg ^ ": expected %L, got %R"] where [msg] is the [string] given to [check]. *)
val check : 'a testable -> string -> 'a -> 'a -> return

(** Check that two values are equal (labeled variant of {!check}). *)
val check' : 'a testable -> msg:string -> expected:'a -> actual:'a -> return

(** Check that an exception is raised. *)
val check_raises : string -> exn -> (unit -> unit) -> return

(** Fail the current test (string version). *)
val fail : string -> 'a

(** Fail the current test (format version). *)
val failf : ('a, Format.formatter, return, 'b) format4 -> 'a
