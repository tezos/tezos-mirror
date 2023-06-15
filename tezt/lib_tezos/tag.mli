(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Test tags with specific semantics. *)

(** The following values are string constants that can be added to [~tags]
    in [Test.register]. They have specific semantics that is documented here.
    By using [Tag.x] instead of ["x"] in [~tags], you allow developers
    to jump to the definition of [Tag.x] and thus see the documentation of the tag. *)

(** ["flaky"]: the test is flaky.

    This disables the test in the CI just like {!ci_disable}.
    Contrary to {!ci_disable} however, this also gives the reason why the test
    is disabled: the test is flaky.

    Tip: you can list tests that are declared as flaky with [tezt --list flaky].
    You can check if they are still flaky with something like
    [tezt flaky --loop-count 100].

    Since this tag already conveys information about why the test is disabled,
    and since it is easy to get the list of tests with this tag,
    it is not necessarily needed to create an issue for each flaky test. *)
val flaky : string

(** ["ci_disable"]: disable test in the CI.

    The test is not to be run in the CI.
    You must provide a comment to explain why the test is disabled.
    For flaky tests, {!flaky} should be preferred to [ci_disable]. *)
val ci_disable : string
