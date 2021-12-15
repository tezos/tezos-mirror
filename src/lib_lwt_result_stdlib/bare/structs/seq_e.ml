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

(* This module is about sequences mixed in with result, the most common monad
   here is the Result monad, we open its syntax module for the whole file (and
   shadow it when needed. *)
open Result_syntax

type (+'a, 'e) node = Nil | Cons of 'a * ('a, 'e) t

and ('a, 'e) t = unit -> (('a, 'e) node, 'e) result

let nil = Nil

let nil_e = Ok Nil

let empty () = Ok Nil

let return x () = Ok (Cons (x, empty))

let return_e r () =
  let* r = r in
  Ok (Cons (r, empty))

let interrupted e () = Error e

let cons item t () = Ok (Cons (item, t))

let cons_e item t () =
  let* item = item in
  Ok (Cons (item, t))

let rec append ta tb () =
  let* n = ta () in
  match n with
  | Nil -> tb ()
  | Cons (item, ta) -> Ok (Cons (item, append ta tb))

let first s =
  match s () with
  | Ok Nil -> None
  | Ok (Cons (x, _)) -> Some (Ok x)
  | Error _ as error -> Some error

let rec fold_left f acc seq =
  let* n = seq () in
  match n with
  | Nil -> Ok acc
  | Cons (item, seq) -> fold_left f (f acc item) seq

let rec fold_left_e f acc seq =
  let* n = seq () in
  match n with
  | Nil -> Ok acc
  | Cons (item, seq) ->
      let* acc = f acc item in
      fold_left_e f acc seq

let rec fold_left_e_discriminated f acc seq =
  let* n = Result.map_error Either.left @@ seq () in
  match n with
  | Nil -> Ok acc
  | Cons (item, seq) ->
      let* acc = Result.map_error Either.right @@ f acc item in
      fold_left_e_discriminated f acc seq

let rec fold_left_s f acc seq =
  match seq () with
  | Error _ as e -> Lwt.return e
  | Ok Nil -> Lwt.return_ok acc
  | Ok (Cons (item, seq)) ->
      let open Lwt_syntax in
      let* acc = f acc item in
      fold_left_s f acc seq

let fold_left_s f acc seq =
  match seq () with
  | Error _ as e -> Lwt.return e
  | Ok Nil -> Lwt.return_ok acc
  | Ok (Cons (item, seq)) ->
      let open Lwt_syntax in
      let* acc = lwt_apply2 f acc item in
      fold_left_s f acc seq

let rec fold_left_es f acc seq =
  let open Lwt_result_syntax in
  match seq () with
  | Error _ as e -> Lwt.return e
  | Ok Nil -> return acc
  | Ok (Cons (item, seq)) ->
      let* acc = f acc item in
      fold_left_es f acc seq

let fold_left_es f acc seq =
  let open Lwt_result_syntax in
  match seq () with
  | Error _ as e -> Lwt.return e
  | Ok Nil -> return acc
  | Ok (Cons (item, seq)) ->
      let* acc = lwt_apply2 f acc item in
      fold_left_es f acc seq

let rec fold_left_es_discriminated f acc seq =
  let open Lwt_result_syntax in
  match seq () with
  | Error e -> Lwt.return_error (Either.left e)
  | Ok Nil -> return acc
  | Ok (Cons (item, seq)) ->
      let* acc = Lwt_result.map_err Either.right @@ f acc item in
      fold_left_es_discriminated f acc seq

let fold_left_es_discriminated f acc seq =
  let open Lwt_result_syntax in
  match seq () with
  | Error e -> Lwt.return_error (Either.Left e)
  | Ok Nil -> return acc
  | Ok (Cons (item, seq)) ->
      let* acc = Lwt_result.map_err Either.right @@ lwt_apply2 f acc item in
      fold_left_es_discriminated f acc seq

let rec iter f seq =
  let* n = seq () in
  match n with
  | Nil -> return_unit
  | Cons (item, seq) ->
      f item ;
      iter f seq

let rec iter_e f seq =
  let* n = seq () in
  match n with
  | Nil -> return_unit
  | Cons (item, seq) ->
      let* () = f item in
      iter_e f seq

let rec iter_e_discriminated f seq =
  let* n = Result.map_error Either.left @@ seq () in
  match n with
  | Nil -> return_unit
  | Cons (item, seq) ->
      let* () = Result.map_error Either.right @@ f item in
      iter_e_discriminated f seq

let rec iter_s f seq =
  match seq () with
  | Error _ as e -> Lwt.return e
  | Ok Nil -> Lwt_result_syntax.return_unit
  | Ok (Cons (item, seq)) ->
      let open Lwt_syntax in
      let* () = f item in
      iter_s f seq

let iter_s f seq =
  match seq () with
  | Error _ as e -> Lwt.return e
  | Ok Nil -> Lwt_result_syntax.return_unit
  | Ok (Cons (item, seq)) ->
      let open Lwt_syntax in
      let* () = Lwt.apply f item in
      iter_s f seq

let rec iter_es f seq =
  let open Lwt_result_syntax in
  match seq () with
  | Error _ as e -> Lwt.return e
  | Ok Nil -> return_unit
  | Ok (Cons (item, seq)) ->
      let* () = f item in
      iter_es f seq

let iter_es f seq =
  let open Lwt_result_syntax in
  match seq () with
  | Error _ as e -> Lwt.return e
  | Ok Nil -> return_unit
  | Ok (Cons (item, seq)) ->
      let* () = Lwt.apply f item in
      iter_es f seq

let rec iter_es_discriminated f seq =
  let open Lwt_result_syntax in
  match seq () with
  | Error e -> Lwt.return_error (Either.Left e)
  | Ok Nil -> return_unit
  | Ok (Cons (item, seq)) ->
      let* () = Lwt_result.map_err Either.right @@ f item in
      iter_es_discriminated f seq

let iter_es_discriminated f seq =
  let open Lwt_result_syntax in
  match seq () with
  | Error e -> Lwt.return_error (Either.Left e)
  | Ok Nil -> return_unit
  | Ok (Cons (item, seq)) ->
      let* () = Lwt_result.map_err Either.right @@ Lwt.apply f item in
      iter_es_discriminated f seq

let iter_p f seq =
  let rec iter_p acc f seq =
    match seq () with
    | Error _ as e ->
        let open Lwt_syntax in
        let* () = join acc in
        return e
    | Ok Nil ->
        let open Lwt_syntax in
        let* () = join acc in
        Lwt_result_syntax.return_unit
    | Ok (Cons (item, seq)) -> iter_p (Lwt.apply f item :: acc) f seq
  in
  iter_p [] f seq

let rec map f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_e
  | Cons (item, seq) -> Ok (Cons (f item, map f seq))

let rec map_e f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_e
  | Cons (item, seq) ->
      let* item = f item in
      Ok (Cons (item, map_e f seq))

let rec map_error (f : 'e -> 'f) (seq : ('a, 'e) t) : ('a, 'f) t =
 fun () ->
  match seq () with
  | Ok Nil -> nil_e
  | Ok (Cons (item, seq)) -> Ok (Cons (item, map_error f seq))
  | Error e -> Error (f e)

let rec filter f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_e
  | Cons (item, seq) ->
      if f item then Ok (Cons (item, seq)) else filter f seq ()

let rec filter_e f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_e
  | Cons (item, seq) ->
      let* b = f item in
      if b then Ok (Cons (item, filter_e f seq)) else filter_e f seq ()

let rec filter_map f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_e
  | Cons (item, seq) -> (
      match f item with
      | None -> filter_map f seq ()
      | Some item -> Ok (Cons (item, filter_map f seq)))

let rec filter_map_e f seq () =
  let* n = seq () in
  match n with
  | Nil -> nil_e
  | Cons (item, seq) -> (
      let* item_o = f item in
      match item_o with
      | None -> filter_map_e f seq ()
      | Some item -> Ok (Cons (item, filter_map_e f seq)))

let rec unfold f a () =
  match f a with
  | None -> nil_e
  | Some (item, a) -> Ok (Cons (item, unfold f a))

let rec unfold_e f a () =
  let* more = f a in
  match more with
  | None -> nil_e
  | Some (item, a) -> Ok (Cons (item, unfold_e f a))

let rec of_seq seq () =
  match seq () with
  | Stdlib.Seq.Nil -> nil_e
  | Stdlib.Seq.Cons (e, seq) -> Ok (Cons (e, of_seq seq))

let rec of_seq_e seq () =
  match seq () with
  | Stdlib.Seq.Nil -> nil_e
  | Stdlib.Seq.Cons (Ok e, seq) -> Ok (Cons (e, of_seq_e seq))
  | Stdlib.Seq.Cons ((Error _ as e), _) -> e
