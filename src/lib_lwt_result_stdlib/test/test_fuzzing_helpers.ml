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

open QCheck2.Gen
open Support.Lib.Monad
open Qcheck2_helpers

(* Generators *)

(* Function generators *)

module Fn = struct
  let arith =
    let open QCheck2 in
    fun2 Observable.int Observable.int int
end

(* Wrappers for generated functions *)

(* immediate wrappers *)

module IterOf = struct
  let fn r fn y = r := fn !r y
end

module IteriOf = struct
  let fn r fn i y = r := fn !r (fn i y)
end

(* error-aware wrappers *)

module IterEOf = struct
  open Result_syntax

  let fn r fn y =
    r := fn !r y ;
    return_unit

  let fn_e r fn y =
    let* t = fn !r y in
    r := t ;
    return_unit
end

module IteriEOf = struct
  open Result_syntax

  let fn r fn i y =
    r := fn !r (fn i y) ;
    return_unit

  let fn_e r fn i y =
    let* z = fn i y in
    let* t = fn !r z in
    r := t ;
    return_unit
end

(* lwt-aware wrappers *)

module IterSOf = struct
  open Lwt_syntax

  let fn r fn y =
    r := fn !r y ;
    return_unit

  let monotonous r fn const y =
    r := !r + fn const y ;
    return_unit

  let fn_s r fn y =
    let* t = fn !r y in
    r := t ;
    return_unit
end

module IteriSOf = struct
  open Lwt_syntax

  let fn r fn i y =
    r := fn !r (fn i y) ;
    return_unit

  let fn_s r fn i y =
    let* z = fn i y in
    let* t = fn !r z in
    r := t ;
    return_unit
end

(* error-lwt-aware wrappers *)

module IterESOf = struct
  open Lwt_result_syntax

  let fn r fn y =
    r := fn !r y ;
    return_unit

  let monotonous r fn const y =
    r := !r + fn const y ;
    return_unit

  let fn_e r fn y =
    let* t = Lwt.return @@ fn !r y in
    r := t ;
    return_unit

  let fn_s r fn y =
    let*! t = fn !r y in
    r := t ;
    return_unit

  let fn_es r fn y =
    let* t = fn !r y in
    r := t ;
    return_unit
end

module IteriESOf = struct
  open Lwt_result_syntax

  let fn r fn i y =
    r := fn !r (fn i y) ;
    return_unit

  let fn_e r fn i y =
    let* z = Lwt.return @@ fn i y in
    let* t = Lwt.return @@ fn !r z in
    r := t ;
    return_unit

  let fn_s r fn i y =
    let*! z = fn i y in
    let*! t = fn !r z in
    r := t ;
    return_unit

  let fn_es r fn i y =
    let* z = fn i y in
    let* t = fn !r z in
    r := t ;
    return_unit
end

(* Data generators (we use lists of integers) *)

let one = int

let many = list int

(* equality and lwt/error variants *)

let eq ?pp a b = qcheck_eq ?pp a b

let eq_s ?pp a b =
  Lwt_main.run
    (let open Lwt_syntax in
    let+ a and+ b in
    eq ?pp a b)

(** [eq_es] is a duplicate of {!eq_s} for consistency

   example:
   {[
   eq_s
     Lwt_syntax.(
      let acc = ref init in
      let+ () = M.iter_s (IterSOf.fn_s acc fn) input in
      !acc)
     (M.fold_left_s (FoldSOf.fn_s fn) init input)

   eq_es
     Lwt_result_syntax.(
      let acc = ref init in
      let+ () = M.iter_es (IterESOf.fn acc fn) (M.of_list input) in
      !acc)
     (Lwt.return_ok @@ with_stdlib_iter (fn, init, input))
   ]}
*)
let eq_es ?pp (a : ('a, 'b) result Lwt.t) (b : ('a, 'b) result Lwt.t) =
  eq_s ?pp a b
