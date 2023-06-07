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
    this wrapper. *)

(** Return type for tests. *)
type return = unit Lwt.t

(** Speed levels.

    In Alcotest, one can ask not to run slow tests with [-q] from the command-line.
    In Tezt, the equivalent is [-a quick]. *)
type speed_level = [`Quick | `Slow]

(** Test cases.

    The name of the Alcotest test case appears in Tezt's logs,
    but it is not used in the Tezt test title, nor as a tag. *)
type 'a test_case = string * speed_level * ('a -> return)

(** Make a test case. *)
val test_case :
  string -> speed_level -> (Lwt_switch.t -> 'a -> return) -> 'a test_case

(** Make a test case (out of Lwt). *)
val test_case_sync : string -> speed_level -> ('a -> unit) -> 'a test_case

(** Tests.

    In Tezt, the name of the test is used as the title of the test. *)
type 'a test = string * 'a test_case list

(** Run a test suite.

    In Tezt, this calls [Test.register] but does not actually run the test suite.
    The name of the test suite is used as the filename for the Tezt test. *)
val run : __FILE__:string -> string -> unit test list -> return
