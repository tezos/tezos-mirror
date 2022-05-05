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

(** [lwt_assert] contains Alcotest assertions lifted to the Lwt. *)

open Assert

(** Lwt-lifted assertion that [b] is [true]. *)
let lwt_assert_true str b = Lwt.return (assert_true str b)

(** Lwt-lifted assertion that [b] is [false]. *)
let lwt_assert_false str b = Lwt.return (assert_false str b)

(** Lwt-lift of [Alcotest.check]. *)
let lwt_check testable str a b = Lwt.return (Alcotest.check testable str a b)

(** Lwt-lifted Alcotest version of [assert false]. *)
let lwt_impossible str = Lwt.return (impossible str)

(** [lwt_fail str] is a lwt-lifted Alcotest version of [Alcotest.fail str]. *)
let lwt_fail str = Lwt.return (Alcotest.fail str)

(** [lwt_assert_catch p e] runs [p], expecting to catch [e] and fails
    if [p] does not raise that exception *)
let lwt_assert_catch (p : unit -> 'a Lwt.t) (e : exn) =
  let catcher e' =
    Lwt.return (Alcotest.check Testable.exn "Catched unexpected assertion" e e')
  in
  Lwt.(
    let open Syntax in
    let* () = catch p catcher in
    lwt_fail ("Expected an exception " ^ Printexc.to_string e))
