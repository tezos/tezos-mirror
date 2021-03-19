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

(* Like Lwt.apply but specialised for two-parameter functions *)
let apply2 f x y = try f x y with exn -> Lwt.fail exn

let rec fold_left_e f acc seq =
  match seq () with
  | Nil ->
      Ok acc
  | Cons (item, seq) ->
      f acc item >>? fun acc -> fold_left_e f acc seq

let rec fold_left_s f acc seq =
  match seq () with
  | Nil ->
      Lwt.return acc
  | Cons (item, seq) ->
      f acc item >>= fun acc -> fold_left_s f acc seq

let fold_left_s f acc seq =
  match seq () with
  | Nil ->
      Lwt.return acc
  | Cons (item, seq) ->
      apply2 f acc item >>= fun acc -> fold_left_s f acc seq

let rec fold_left_es f acc seq =
  match seq () with
  | Nil ->
      Monad.return acc
  | Cons (item, seq) ->
      f acc item >>=? fun acc -> fold_left_es f acc seq

let fold_left_es f acc seq =
  match seq () with
  | Nil ->
      Monad.return acc
  | Cons (item, seq) ->
      apply2 f acc item >>=? fun acc -> fold_left_es f acc seq

let rec iter_e f seq =
  match seq () with
  | Nil ->
      unit_e
  | Cons (item, seq) ->
      f item >>? fun () -> iter_e f seq

let rec iter_s f seq =
  match seq () with
  | Nil ->
      unit_s
  | Cons (item, seq) ->
      f item >>= fun () -> iter_s f seq

let iter_s f seq =
  match seq () with
  | Nil ->
      unit_s
  | Cons (item, seq) ->
      Lwt.apply f item >>= fun () -> iter_s f seq

let rec iter_es f seq =
  match seq () with
  | Nil ->
      unit_es
  | Cons (item, seq) ->
      f item >>=? fun () -> iter_es f seq

let iter_es f seq =
  match seq () with
  | Nil ->
      unit_es
  | Cons (item, seq) ->
      Lwt.apply f item >>=? fun () -> iter_es f seq

let iter_ep f seq =
  let rec iter_ep f seq (acc : (unit, 'error) result Lwt.t list) =
    match seq () with
    | Nil ->
        join_ep acc
    | Cons (item, seq) ->
        iter_ep f seq (Lwt.apply f item :: acc)
  in
  iter_ep f seq []

let iter_p f seq =
  let rec iter_p f seq acc =
    match seq () with
    | Nil ->
        join_p acc
    | Cons (item, seq) ->
        iter_p f seq (Lwt.apply f item :: acc)
  in
  iter_p f seq []
