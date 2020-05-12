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

(** Log an error and stop the test right here. *)
val fail : ('a, unit, string, 'b) format4 -> 'a

(** Run a test.

    One should be able to infer, from [title], what the test will do.
    It is typically a short sentence like ["addition is commutative"]
    or ["server runs until client tells it to stop"].

    The list of [tags] must be composed of short strings which are
    easy to type on the command line (lowercase letters, digits
    and underscores). Run the test executable with [--list]
    to get the list of tags which are already used by existing tests.
    Try to reuse them if possible.

    The last argument is a function [f] which implements the test.

    After [f] is done, whatever the result, {!Tezt_process.clean_up} is run.

    If [f] raises an exception, act as if [fail] was called (without the
    error location unfortunately).

    You can call [run] several times in the same executable if you want
    it to run several tests. Each of those tests should be standalone, as
    the user is able to specify the list of tests to run on the command-line. *)
val run : title:string -> tags:string list -> (unit -> unit Lwt.t) -> unit
