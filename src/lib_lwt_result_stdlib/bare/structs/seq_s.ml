(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* This module is about sequences mixed in with Lwt, the most common monad
   here is the Lwt monad, we open its syntax module for the whole file (and
   shadow it when needed. *)
open Lwt_syntax

type +'a node = Nil | Cons of 'a * 'a t

and 'a t = unit -> 'a node Lwt.t

let protect seq () = Lwt.apply seq ()

let nil_s = Lwt.return Nil

let empty () = nil_s

(* we define [return] at the end of the file to avoid shadowing the one from the
   opened monad syntax *)
let return_s p () = Lwt.map (fun x -> Cons (x, empty)) p

let cons item t () = Lwt.return (Cons (item, t))

let cons_s item t () =
  let* item = item in
  return (Cons (item, t))

let rec append ta tb () =
  let* n = ta () in
  match n with
  | Nil -> tb ()
  | Cons (item, ta) -> return (Cons (item, append ta tb))

let first s =
  let* n = s () in
  match n with Nil -> return_none | Cons (x, _) -> return_some x

let rec fold_left f acc seq =
  let* n = seq () in
  match n with
  | Nil -> return acc
  | Cons (item, seq) -> fold_left f (f acc item) seq

let fold_left f acc seq = fold_left f acc @@ protect seq

let rec fold_left_e f acc seq =
  let* n = seq () in
  match n with
  | Nil -> return_ok acc
  | Cons (item, seq) -> (
      match f acc item with
      | Error _ as e -> Lwt.return e
      | Ok acc -> fold_left_e f acc seq)

let fold_left_e f acc seq = fold_left_e f acc @@ protect seq

let rec fold_left_s f acc seq =
  let* n = seq () in
  match n with
  | Nil -> return acc
  | Cons (item, seq) ->
      let* acc = f acc item in
      fold_left_s f acc seq

let fold_left_s f acc seq = fold_left_s f acc @@ protect seq

let rec fold_left_es f acc seq =
  let* n = seq () in
  let open Lwt_result_syntax in
  match n with
  | Nil -> return acc
  | Cons (item, seq) ->
      let* acc = f acc item in
      fold_left_es f acc seq

let fold_left_es f acc seq = fold_left_es f acc @@ protect seq

let rec iter f seq =
  let* n = seq () in
  match n with
  | Nil -> return_unit
  | Cons (item, seq) ->
      f item ;
      iter f seq

let iter f seq = iter f @@ protect seq

let rec iter_e f seq =
  let* n = seq () in
  let open Lwt_result_syntax in
  match n with
  | Nil -> return_unit
  | Cons (item, seq) ->
      let*? () = f item in
      iter_e f seq

let iter_e f seq = iter_e f @@ protect seq

let rec iter_s f seq =
  let* n = seq () in
  match n with
  | Nil -> return_unit
  | Cons (item, seq) ->
      let* () = f item in
      iter_s f seq

let iter_s f seq = iter_s f @@ protect seq

let rec iter_es f seq =
  let* n = seq () in
  let open Lwt_result_syntax in
  match n with
  | Nil -> return_unit
  | Cons (item, seq) ->
      let* () = f item in
      iter_es f seq

let iter_es f seq = iter_es f @@ protect seq

let iter_ep f seq =
  let* ps = fold_left (fun acc item -> Lwt.apply f item :: acc) [] seq in
  Lwt_result_syntax.join ps

let iter_p f seq =
  let* ps = fold_left (fun acc item -> Lwt.apply f item :: acc) [] seq in
  join ps

let rec map f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_s
  | Cons (item, seq) -> return (Cons (f item, map f seq))

let map f seq = map f @@ protect seq

let rec map_s f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_s
  | Cons (item, seq) ->
      let* item = f item in
      return (Cons (item, map_s f seq))

let map_s f seq = map_s f @@ protect seq

let rec filter f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_s
  | Cons (item, seq) ->
      if f item then return (Cons (item, seq)) else filter f seq ()

let filter f seq = filter f @@ protect seq

let rec filter_s f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_s
  | Cons (item, seq) ->
      let* b = f item in
      if b then return (Cons (item, filter_s f seq)) else filter_s f seq ()

let filter_s f seq = filter_s f @@ protect seq

let rec filter_map f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_s
  | Cons (item, seq) -> (
      match f item with
      | None -> filter_map f seq ()
      | Some item -> return (Cons (item, filter_map f seq)))

let filter_map f seq = filter_map f @@ protect seq

let rec filter_map_s f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_s
  | Cons (item, seq) -> (
      let* item_o = f item in
      match item_o with
      | None -> filter_map_s f seq ()
      | Some item -> return (Cons (item, filter_map_s f seq)))

let filter_map_s f seq = filter_map_s f @@ protect seq

let rec unfold f a () =
  match f a with
  | None -> nil_s
  | Some (item, a) -> Lwt.return (Cons (item, unfold f a))

let rec unfold_s f a () =
  let* cont = f a in
  match cont with
  | None -> nil_s
  | Some (item, a) -> return (Cons (item, unfold_s f a))

let rec of_seq seq () =
  match seq () with
  | Stdlib.Seq.Nil -> nil_s
  | Stdlib.Seq.Cons (e, seq) -> return (Cons (e, of_seq seq))

let rec of_seq_s seq () =
  match seq () with
  | Stdlib.Seq.Nil -> nil_s
  | Stdlib.Seq.Cons (p, seq) ->
      let* e = p in
      return (Cons (e, of_seq_s seq))

let return x () = Lwt.return (Cons (x, empty))
