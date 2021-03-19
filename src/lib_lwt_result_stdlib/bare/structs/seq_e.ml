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

(* Like Lwt.apply but specialised for three parameters *)
let apply3 f x y = try f x y with exn -> Lwt.fail exn

type (+'a, 'e) node = Nil | Cons of 'a * ('a, 'e) t

and ('a, 'e) t = unit -> (('a, 'e) node, 'e) result

let nil = Nil

let nil_e = Ok Nil

let empty () = Ok Nil

let return x () = Ok (Cons (x, empty))

let return_e r () = Result.map (fun x -> Cons (x, empty)) r

let interrupted e () = Error e

let rec fold_left f acc seq =
  seq ()
  >>? function
  | Nil -> Ok acc | Cons (item, seq) -> fold_left f (f acc item) seq

let rec fold_left_e f acc seq =
  seq ()
  >>? function
  | Nil ->
      Ok acc
  | Cons (item, seq) ->
      f acc item >>? fun acc -> fold_left_e f acc seq

let rec fold_left_s f acc seq =
  seq ()
  >>?= function
  | Nil ->
      Monad.return acc
  | Cons (item, seq) ->
      f acc item >>= fun acc -> fold_left_s f acc seq

let fold_left_s f acc seq =
  seq ()
  >>?= function
  | Nil ->
      Monad.return acc
  | Cons (item, seq) ->
      apply3 f acc item >>= fun acc -> fold_left_s f acc seq

let rec fold_left_es f acc seq =
  seq ()
  >>?= function
  | Nil ->
      Monad.return acc
  | Cons (item, seq) ->
      f acc item >>=? fun acc -> fold_left_es f acc seq

let fold_left_es f acc seq =
  seq ()
  >>?= function
  | Nil ->
      Monad.return acc
  | Cons (item, seq) ->
      apply3 f acc item >>=? fun acc -> fold_left_es f acc seq

let rec iter f seq =
  seq () >>? function Nil -> unit_e | Cons (item, seq) -> f item ; iter f seq

let rec iter_e f seq =
  seq ()
  >>? function
  | Nil -> unit_e | Cons (item, seq) -> f item >>? fun () -> iter_e f seq

let rec iter_s f seq =
  seq ()
  >>?= function
  | Nil -> unit_es | Cons (item, seq) -> f item >>= fun () -> iter_s f seq

let iter_s f seq =
  seq ()
  >>?= function
  | Nil ->
      unit_es
  | Cons (item, seq) ->
      Lwt.apply f item >>= fun () -> iter_s f seq

let rec iter_es f seq =
  seq ()
  >>?= function
  | Nil -> unit_es | Cons (item, seq) -> f item >>=? fun () -> iter_es f seq

let iter_es f seq =
  seq ()
  >>?= function
  | Nil ->
      unit_es
  | Cons (item, seq) ->
      Lwt.apply f item >>=? fun () -> iter_es f seq

let iter_p f seq =
  let rec iter_p acc f seq =
    match seq () with
    | Error _ as e ->
        join_p acc >>= fun () -> Lwt.return e
    | Ok Nil ->
        join_p acc >>= fun () -> Monad.unit_es
    | Ok (Cons (item, seq)) ->
        iter_p (Lwt.apply f item :: acc) f seq
  in
  iter_p [] f seq

let rec map f seq () =
  seq ()
  >|? function Nil -> Nil | Cons (item, seq) -> Cons (f item, map f seq)

let rec map_e f seq () =
  seq ()
  >>? function
  | Nil ->
      nil_e
  | Cons (item, seq) ->
      f item >>? fun item -> Ok (Cons (item, map_e f seq))

let rec map_error (f : 'e -> 'f) (seq : ('a, 'e) t) : ('a, 'f) t =
 fun () ->
  match seq () with
  | Ok Nil ->
      Ok Nil
  | Ok (Cons (item, seq)) ->
      Ok (Cons (item, map_error f seq))
  | Error e ->
      Error (f e)

let rec filter f seq () =
  seq ()
  >>? function
  | Nil ->
      nil_e
  | Cons (item, seq) ->
      if f item then Ok (Cons (item, seq)) else filter f seq ()

let rec filter_e f seq () =
  seq ()
  >>? function
  | Nil ->
      nil_e
  | Cons (item, seq) -> (
      f item
      >>? function
      | true -> Ok (Cons (item, filter_e f seq)) | false -> filter_e f seq () )

let rec filter_map f seq () =
  seq ()
  >>? function
  | Nil ->
      nil_e
  | Cons (item, seq) -> (
    match f item with
    | None ->
        filter_map f seq ()
    | Some item ->
        Ok (Cons (item, filter_map f seq)) )

let rec filter_map_e f seq () =
  seq ()
  >>? function
  | Nil ->
      nil_e
  | Cons (item, seq) -> (
      f item
      >>? function
      | None ->
          filter_map_e f seq ()
      | Some item ->
          Ok (Cons (item, filter_map_e f seq)) )

let rec of_seq seq () =
  match seq () with
  | Stdlib.Seq.Nil ->
      nil_e
  | Stdlib.Seq.Cons (e, seq) ->
      Ok (Cons (e, of_seq seq))

let rec of_seq_e seq () =
  match seq () with
  | Stdlib.Seq.Nil ->
      nil_e
  | Stdlib.Seq.Cons (Ok e, seq) ->
      Ok (Cons (e, of_seq_e seq))
  | Stdlib.Seq.Cons ((Error _ as e), _) ->
      e
