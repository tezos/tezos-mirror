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

type +'a node = Nil | Cons of 'a * 'a t

and 'a t = unit -> 'a node Lwt.t

let protect seq () = Lwt.apply seq ()

let nil_s = Lwt.return Nil

let empty () = nil_s

let return x () = Lwt.return (Cons (x, empty))

let return_s p () = Lwt.map (fun x -> Cons (x, empty)) p

let rec fold_left f acc seq =
  seq ()
  >>= function
  | Nil -> Lwt.return acc | Cons (item, seq) -> fold_left f (f acc item) seq

let fold_left f acc seq = fold_left f acc @@ protect seq

let rec fold_left_e f acc seq =
  seq ()
  >>= function
  | Nil ->
      Monad.return acc
  | Cons (item, seq) ->
      Result.bind_s (f acc item) (fun acc -> fold_left_e f acc seq)

let fold_left_e f acc seq = fold_left_e f acc @@ protect seq

let rec fold_left_s f acc seq =
  seq ()
  >>= function
  | Nil ->
      Lwt.return acc
  | Cons (item, seq) ->
      f acc item >>= fun acc -> fold_left_s f acc seq

let fold_left_s f acc seq = fold_left_s f acc @@ protect seq

let rec fold_left_es f acc seq =
  seq ()
  >>= function
  | Nil ->
      Monad.return acc
  | Cons (item, seq) ->
      f acc item >>=? fun acc -> fold_left_es f acc seq

let fold_left_es f acc seq = fold_left_es f acc @@ protect seq

let rec iter f seq =
  seq () >>= function Nil -> unit_s | Cons (item, seq) -> f item ; iter f seq

let iter f seq = iter f @@ protect seq

let rec iter_e f seq =
  seq ()
  >>= function
  | Nil -> unit_es | Cons (item, seq) -> f item >>?= fun () -> iter_e f seq

let iter_e f seq = iter_e f @@ protect seq

let rec iter_s f seq =
  seq ()
  >>= function
  | Nil -> unit_s | Cons (item, seq) -> f item >>= fun () -> iter_s f seq

let iter_s f seq = iter_s f @@ protect seq

let rec iter_es f seq =
  seq ()
  >>= function
  | Nil -> unit_es | Cons (item, seq) -> f item >>=? fun () -> iter_es f seq

let iter_es f seq = iter_es f @@ protect seq

let iter_ep f seq =
  fold_left (fun acc item -> Lwt.apply f item :: acc) [] seq >>= join_ep

let iter_p f seq =
  fold_left (fun acc item -> Lwt.apply f item :: acc) [] seq >>= join_p

let rec map f seq () =
  seq ()
  >|= function Nil -> Nil | Cons (item, seq) -> Cons (f item, map f seq)

let map f seq = map f @@ protect seq

let rec map_s f seq () =
  seq ()
  >>= function
  | Nil ->
      nil_s
  | Cons (item, seq) ->
      f item >|= fun item -> Cons (item, map_s f seq)

let map_s f seq = map_s f @@ protect seq

let rec filter f seq () =
  seq ()
  >>= function
  | Nil ->
      nil_s
  | Cons (item, seq) ->
      if f item then Lwt.return (Cons (item, seq)) else filter f seq ()

let filter f seq = filter f @@ protect seq

let rec filter_s f seq () =
  seq ()
  >>= function
  | Nil ->
      nil_s
  | Cons (item, seq) -> (
      f item
      >>= function
      | true ->
          Lwt.return (Cons (item, filter_s f seq))
      | false ->
          filter_s f seq () )

let filter_s f seq = filter_s f @@ protect seq

let rec filter_map f seq () =
  seq ()
  >>= function
  | Nil ->
      nil_s
  | Cons (item, seq) -> (
    match f item with
    | None ->
        filter_map f seq ()
    | Some item ->
        Lwt.return (Cons (item, filter_map f seq)) )

let filter_map f seq = filter_map f @@ protect seq

let rec filter_map_s f seq () =
  seq ()
  >>= function
  | Nil ->
      nil_s
  | Cons (item, seq) -> (
      f item
      >>= function
      | None ->
          filter_map_s f seq ()
      | Some item ->
          Lwt.return (Cons (item, filter_map_s f seq)) )

let filter_map_s f seq = filter_map_s f @@ protect seq

let rec find f seq =
  seq ()
  >>= function
  | Nil ->
      Monad.none_s
  | Cons (item, seq) ->
      if f item then Lwt.return (Some item) else find f seq

let find f seq = find f @@ protect seq

let rec find_e f seq =
  seq ()
  >>= function
  | Nil ->
      Monad.none_es
  | Cons (item, seq) -> (
      f item >>?= function true -> some_es item | false -> find_e f seq )

let find_e f seq = find_e f @@ protect seq

let rec find_s f seq =
  seq ()
  >>= function
  | Nil ->
      none_s
  | Cons (item, seq) -> (
      f item >>= function true -> some_s item | false -> find_s f seq )

let find_s f seq = find_s f @@ protect seq

let rec find_es f seq =
  seq ()
  >>= function
  | Nil ->
      none_es
  | Cons (item, seq) -> (
      f item >>=? function true -> some_es item | false -> find_es f seq )

let find_es f seq = find_es f @@ protect seq

let rec of_seq seq () =
  match seq () with
  | Stdlib.Seq.Nil ->
      nil_s
  | Stdlib.Seq.Cons (e, seq) ->
      Lwt.return (Cons (e, of_seq seq))

let rec of_seq_s seq () =
  match seq () with
  | Stdlib.Seq.Nil ->
      nil_s
  | Stdlib.Seq.Cons (p, seq) ->
      p >|= fun e -> Cons (e, of_seq_s seq)
