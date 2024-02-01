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
    to jump to the definition of [Tag.x] and thus see the documentation of the tag.
    You also avoid mistakes like using ["ci_disable"] instead of ["ci_disabled"]. *)

(** ["flaky"]: the test is flaky.

    This disables the test in the CI just like {!ci_disabled}.
    Contrary to {!ci_disabled} however, this also gives the reason why the test
    is disabled: the test is flaky.

    Tip: you can list tests that are declared as flaky with [tezt --list flaky].
    You can check if they are still flaky with something like
    [tezt flaky --loop-count 100].

    Since this tag already conveys information about why the test is disabled,
    and since it is easy to get the list of tests with this tag,
    it is not necessarily needed to create an issue for each flaky test. *)
val flaky : string

(** ["ci_disabled"]: disable test in the CI.

    The test is not to be run in the CI.
    You must provide a comment to explain why the test is disabled.
    For flaky tests, {!flaky} should be preferred to [ci_disabled]. *)
val ci_disabled : string

(** ["memory_3k"]: tag memory hungry tests ( >3 GB of memory ). *)
val memory_3k : string

(** ["memory_4k"]: tag memory hungry tests ( >4 GB of memory ). *)
val memory_4k : string

(** ["time_sensitive"]: tag for time-sensitive tests.

    They are executed with -j 1 to ensure that other tests do not
    affect their executions. However, they are not particularly
    cpu/memory-intensive hence they do not need to run on a particular
    machine contrary to performance regression tests. *)
val time_sensitive : string

(** ["infrastructure"]: tag for tests owned by the Infrastructure product unit. *)
val infrastructure : string

(** ["layer1"]: tag for tests owned by the Layer 1 product unit. *)
val layer1 : string

(** ["tezos2"]: tag for tests owned by the Tezos 2 product unit. *)
val tezos2 : string

(** ["etherlink"]: tag for tests owned by the Etherlink product unit. *)
val etherlink : string

(** ["slow"]: tag for tests that are too slow for the CI's [before_merging]
    pipelines and that will only be run on the scheduled pipeline. *)
val slow : string
