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

(** [Testable] collects {!Alcotest.testable} definitions for OCaml
    base types. *)

include Alcotest

let exn = testable Fmt.exn ( = )

let map (type a b) (f : b -> a) (t : a testable) : b testable =
  let pp fmt v = pp t fmt (f v) in
  let meq b1 b2 = equal t (f b1) (f b2) in
  testable pp meq

let pair3 (type a b c) (t : (a * (b * c)) testable) : (a * b * c) testable =
  let nest ((x, y, z) : a * b * c) : a * (b * c) = (x, (y, z)) in
  map nest t

let pair4 (type a b c d) (t : (a * (b * (c * d))) testable) :
    (a * b * c * d) testable =
  let nest ((x, y, z, w) : a * b * c * d) : a * (b * (c * d)) =
    (x, (y, (z, w)))
  in
  map nest t

let tuple3 ta tb tc : ('a * 'b * 'c) testable = pair3 (pair ta (pair tb tc))

let tuple4 ta tb tc td : ('a * 'b * 'c * 'd) testable =
  pair4 (pair ta (pair tb (pair tc td)))
