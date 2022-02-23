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

(* This module is about sequences mixed in with Lwt+result, the most common monad
   here is the combined Lwt-Result monad, we open its syntax module for the whole file (and
   shadow it when needed. *)
open Lwt_result_syntax

type (+'a, 'e) node = Nil | Cons of 'a * ('a, 'e) t

and ('a, 'e) t = unit -> (('a, 'e) node, 'e) result Lwt.t

let protect seq () = Lwt.apply seq ()

let nil = Nil

let nil_e = Ok Nil

let nil_es = Lwt.return nil_e

let empty () = nil_es

(* we define [return] at the end of the file to avoid shadowing the opened
   Lwt_result_syntax *)

let return_e r () =
  let*? x = r in
  return (Cons (x, empty))

let return_s p () =
  let open Lwt_syntax in
  let* x = p in
  return_ok (Cons (x, empty))

let return_es p () =
  let* x = p in
  return (Cons (x, empty))

let interrupted e () = Lwt.return (Error e)

let interrupted_s p () = Lwt_syntax.( let* ) p Lwt.return_error

let cons item t () = return (Cons (item, t))

let cons_e item t () =
  match item with
  | Error _ as e -> Lwt.return e
  | Ok item -> return (Cons (item, t))

let cons_s item t () =
  let open Lwt_syntax in
  let* item = item in
  return_ok (Cons (item, t))

let cons_es item t () =
  let* item = item in
  return (Cons (item, t))

let rec append ta tb () =
  let* n = ta () in
  match n with
  | Nil -> tb ()
  | Cons (item, ta) -> return (Cons (item, append ta tb))

let first s =
  let open Lwt_syntax in
  let* n_r = s () in
  match n_r with
  | Ok Nil -> return_none
  | Ok (Cons (x, _)) -> return_some (Ok x)
  | Error _ as error -> return_some error

let rec fold_left f acc seq =
  let* n = seq () in
  match n with
  | Nil -> return acc
  | Cons (item, seq) -> fold_left f (f acc item) seq

let fold_left f acc seq = fold_left f acc @@ protect seq

let rec fold_left_e f acc seq =
  let* n = seq () in
  match n with
  | Nil -> return acc
  | Cons (item, seq) ->
      let*? acc = f acc item in
      fold_left_e f acc seq

let fold_left_e f acc seq = fold_left_e f acc @@ protect seq

let rec fold_left_s f acc seq =
  let* n = seq () in
  match n with
  | Nil -> return acc
  | Cons (item, seq) ->
      let*! acc = f acc item in
      fold_left_s f acc seq

let fold_left_s f acc seq = fold_left_s f acc @@ protect seq

let rec fold_left_es f acc seq =
  let* n = seq () in
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
  match n with
  | Nil -> return_unit
  | Cons (item, seq) ->
      let*? () = f item in
      iter_e f seq

let iter_e f seq = iter_e f @@ protect seq

let rec iter_s f seq =
  let* n = seq () in
  match n with
  | Nil -> Lwt_result_syntax.return_unit
  | Cons (item, seq) ->
      let*! () = f item in
      iter_s f seq

let iter_s f seq = iter_s f @@ protect seq

let rec iter_es f seq =
  let* n = seq () in
  match n with
  | Nil -> Lwt_result_syntax.return_unit
  | Cons (item, seq) ->
      let* () = f item in
      iter_es f seq

let iter_es f seq = iter_es f @@ protect seq

let rec map f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_es
  | Cons (item, seq) -> return (Cons (f item, map f seq))

let map f seq = map f @@ protect seq

let rec map_e f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_es
  | Cons (item, seq) ->
      let*? item = f item in
      return (Cons (item, map_e f seq))

let map_e f seq = map_e f @@ protect seq

let rec map_s f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_es
  | Cons (item, seq) ->
      let*! item = f item in
      return (Cons (item, map_s f seq))

let map_s f seq = map_s f @@ protect seq

let rec map_es f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_es
  | Cons (item, seq) ->
      let* item = f item in
      return (Cons (item, map_es f seq))

let map_es f seq = map_es f @@ protect seq

let rec map_error f seq () =
  let open Lwt_syntax in
  let* n_r = seq () in
  match n_r with
  | Ok Nil -> nil_es
  | Ok (Cons (item, seq)) -> return_ok (Cons (item, map_error f seq))
  | Error e -> return_error (f e)

let map_error f seq = map_error f @@ protect seq

let rec map_error_s f seq () =
  let open Lwt_syntax in
  let* n_r = seq () in
  match n_r with
  | Ok Nil -> nil_es
  | Ok (Cons (item, seq)) -> return_ok (Cons (item, map_error_s f seq))
  | Error e ->
      let* e = f e in
      return_error e

let rec filter f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_es
  | Cons (item, seq) ->
      if f item then return (Cons (item, seq)) else filter f seq ()

let filter f seq = filter f @@ protect seq

let rec filter_e f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_es
  | Cons (item, seq) ->
      let*? b = f item in
      if b then return (Cons (item, filter_e f seq)) else filter_e f seq ()

let filter_e f seq = filter_e f @@ protect seq

let rec filter_s f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_es
  | Cons (item, seq) ->
      let*! b = f item in
      if b then return (Cons (item, filter_s f seq)) else filter_s f seq ()

let filter_s f seq = filter_s f @@ protect seq

let rec filter_es f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_es
  | Cons (item, seq) ->
      let* b = f item in
      if b then return (Cons (item, filter_es f seq)) else filter_es f seq ()

let filter_es f seq = filter_es f @@ protect seq

let rec filter_map f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_es
  | Cons (item, seq) -> (
      match f item with
      | None -> filter_map f seq ()
      | Some item -> return (Cons (item, filter_map f seq)))

let filter_map f seq = filter_map f @@ protect seq

let rec filter_map_e f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_es
  | Cons (item, seq) -> (
      let*? o = f item in
      match o with
      | None -> filter_map_e f seq ()
      | Some item -> return (Cons (item, filter_map_e f seq)))

let filter_map_e f seq = filter_map_e f @@ protect seq

let rec filter_map_s f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_es
  | Cons (item, seq) -> (
      let*! o = f item in
      match o with
      | None -> filter_map_s f seq ()
      | Some item -> return (Cons (item, filter_map_s f seq)))

let filter_map_s f seq = filter_map_s f @@ protect seq

let rec filter_map_es f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_es
  | Cons (item, seq) -> (
      let* o = f item in
      match o with
      | None -> filter_map_es f seq ()
      | Some item -> return (Cons (item, filter_map_es f seq)))

let filter_map_es f seq = filter_map_es f @@ protect seq

let rec unfold f a () =
  match f a with
  | None -> nil_es
  | Some (item, a) -> return (Cons (item, unfold f a))

let rec unfold_s f a () =
  let open Lwt_syntax in
  let* cont = f a in
  match cont with
  | None -> nil_es
  | Some (item, a) -> return_ok (Cons (item, unfold_s f a))

let rec unfold_e f a () =
  match f a with
  | Error _ as e -> Lwt.return e
  | Ok None -> nil_es
  | Ok (Some (item, a)) -> return (Cons (item, unfold_e f a))

let rec unfold_es f a () =
  let* n = f a in
  match n with
  | None -> nil_es
  | Some (item, a) -> return (Cons (item, unfold_es f a))

let rec of_seq seq () =
  match seq () with
  | Stdlib.Seq.Nil -> nil_es
  | Stdlib.Seq.Cons (e, seq) -> return (Cons (e, of_seq seq))

let rec of_seq_e seq () =
  match seq () with
  | Stdlib.Seq.Nil -> nil_es
  | Stdlib.Seq.Cons (Ok e, seq) -> return (Cons (e, of_seq_e seq))
  | Stdlib.Seq.Cons ((Error _ as e), _) -> Lwt.return e

let rec of_seqe seq () =
  match seq () with
  | Ok Seq_e.Nil -> nil_es
  | Ok (Seq_e.Cons (item, seq)) -> return (Cons (item, of_seqe seq))
  | Error _ as e -> Lwt.return e

let rec of_seq_s seq () =
  match seq () with
  | Stdlib.Seq.Nil -> nil_es
  | Stdlib.Seq.Cons (p, seq) ->
      let open Lwt_syntax in
      let* e = p in
      return_ok (Cons (e, of_seq_s seq))

let rec of_seqs seq () =
  let open Lwt_syntax in
  let* n = seq () in
  match n with
  | Seq_s.Nil -> nil_es
  | Seq_s.Cons (e, seq) -> return_ok (Cons (e, of_seqs seq))

let rec of_seq_es seq () =
  match seq () with
  | Stdlib.Seq.Nil -> nil_es
  | Stdlib.Seq.Cons (p, seq) ->
      let* e = p in
      return (Cons (e, of_seq_es seq))

let return x () = return (Cons (x, empty))
