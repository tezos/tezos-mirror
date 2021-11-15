(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** [assert] contains Alcotest convenience assertions. *)

open Alcotest

(** Alcotest check that [b] is [true]. *)
let assert_true str b = check bool str true b

(** Alcotest check that [b] is [false]. *)
let assert_false str b = check bool str false b

(** Alcotest version of [assert false]. *)
let impossible str = assert_true str false

(** Assert that at least one value in [l] satisfies [f]. *)
let check_any ?(msg = "No value in the list satifies the condition.") f l =
  if not (List.exists f l) then fail msg

(** [contains m msg x ls] asserts that one testable in [ls] equals
    [x], and otherwise fails with [msg] *)
let contains (type a) (m : a testable) msg (x : a) (ls : a list) : unit =
  let (module M) = m in
  let (module L) = list m in
  if not @@ List.exists (M.equal x) ls then
    failf "%s. Could not find %a in %a" msg M.pp x L.pp ls
