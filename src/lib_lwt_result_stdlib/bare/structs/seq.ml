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

open Monad
include Stdlib.Seq

let cons item t () = Cons (item, t)

let rec append ta tb () =
  match ta () with Nil -> tb () | Cons (item, ta) -> Cons (item, append ta tb)

let first s = match s () with Nil -> None | Cons (x, _) -> Some x

let rec fold_left_e f acc seq =
  match seq () with
  | Nil -> Ok acc
  | Cons (item, seq) ->
      let open Result_syntax in
      let* acc = f acc item in
      fold_left_e f acc seq

let rec fold_left_s f acc seq =
  let open Lwt_syntax in
  match seq () with
  | Nil -> return acc
  | Cons (item, seq) ->
      let* acc = f acc item in
      fold_left_s f acc seq

let fold_left_s f acc seq =
  let open Lwt_syntax in
  match seq () with
  | Nil -> return acc
  | Cons (item, seq) ->
      let* acc = lwt_apply2 f acc item in
      fold_left_s f acc seq

let rec fold_left_es f acc seq =
  let open Lwt_result_syntax in
  match seq () with
  | Nil -> return acc
  | Cons (item, seq) ->
      let* acc = f acc item in
      fold_left_es f acc seq

let fold_left_es f acc seq =
  let open Lwt_result_syntax in
  match seq () with
  | Nil -> return acc
  | Cons (item, seq) ->
      let* acc = lwt_apply2 f acc item in
      fold_left_es f acc seq

let rec iter_e f seq =
  let open Result_syntax in
  match seq () with
  | Nil -> return_unit
  | Cons (item, seq) ->
      let* () = f item in
      iter_e f seq

let rec iter_s f seq =
  let open Lwt_syntax in
  match seq () with
  | Nil -> return_unit
  | Cons (item, seq) ->
      let* () = f item in
      iter_s f seq

let iter_s f seq =
  let open Lwt_syntax in
  match seq () with
  | Nil -> return_unit
  | Cons (item, seq) ->
      let* () = Lwt.apply f item in
      iter_s f seq

let rec iter_es f seq =
  let open Lwt_result_syntax in
  match seq () with
  | Nil -> return_unit
  | Cons (item, seq) ->
      let* () = f item in
      iter_es f seq

let iter_es f seq =
  let open Lwt_result_syntax in
  match seq () with
  | Nil -> return_unit
  | Cons (item, seq) ->
      let* () = Lwt.apply f item in
      iter_es f seq

let iter_ep f seq =
  let rec iter_ep f seq (acc : (unit, 'error) result Lwt.t list) =
    match seq () with
    | Nil -> Lwt_result_syntax.join acc
    | Cons (item, seq) -> iter_ep f seq (Lwt.apply f item :: acc)
  in
  iter_ep f seq []

let iter_p f seq =
  let rec iter_p f seq acc =
    match seq () with
    | Nil -> Lwt_syntax.join acc
    | Cons (item, seq) -> iter_p f seq (Lwt.apply f item :: acc)
  in
  iter_p f seq []

let rec unfold f a () =
  match f a with None -> Nil | Some (item, a) -> Cons (item, unfold f a)
