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

type (+'a, 'e) node = Nil | Cons of 'a * ('a, 'e) t

and ('a, 'e) t = unit -> (('a, 'e) node, 'e) result Lwt.t

let protect seq () = Lwt.apply seq ()

let nil = Nil

let nil_e = Ok Nil

let nil_es = Lwt.return nil_e

let empty () = Monad.return Nil

let return x () = Monad.return (Cons (x, empty))

let return_e r () = Monad.( >>?= ) r (fun x -> Monad.return (Cons (x, empty)))

let return_s p () = Lwt.bind p (fun x -> Monad.return (Cons (x, empty)))

let return_es p () = Monad.( >>=? ) p (fun x -> Monad.return (Cons (x, empty)))

let interrupted e () = Lwt.return (Error e)

let interrupted_s p () = Lwt.bind p Lwt.return_error

let rec fold_left f acc seq =
  seq ()
  >>=? function
  | Nil -> Monad.return acc | Cons (item, seq) -> fold_left f (f acc item) seq

let fold_left f acc seq = fold_left f acc @@ protect seq

let rec fold_left_e f acc seq =
  seq ()
  >>=? function
  | Nil ->
      Monad.return acc
  | Cons (item, seq) ->
      f acc item >>?= fun acc -> fold_left_e f acc seq

let fold_left_e f acc seq = fold_left_e f acc @@ protect seq

let rec fold_left_s f acc seq =
  seq ()
  >>=? function
  | Nil ->
      Monad.return acc
  | Cons (item, seq) ->
      f acc item >>= fun acc -> fold_left_s f acc seq

let fold_left_s f acc seq = fold_left_s f acc @@ protect seq

let rec fold_left_es f acc seq =
  seq ()
  >>=? function
  | Nil ->
      Monad.return acc
  | Cons (item, seq) ->
      f acc item >>=? fun acc -> fold_left_es f acc seq

let fold_left_es f acc seq = fold_left_es f acc @@ protect seq

let rec iter f seq =
  seq ()
  >>=? function Nil -> unit_es | Cons (item, seq) -> f item ; iter f seq

let iter f seq = iter f @@ protect seq

let rec iter_e f seq =
  seq ()
  >>=? function
  | Nil -> unit_es | Cons (item, seq) -> f item >>?= fun () -> iter_e f seq

let iter_e f seq = iter_e f @@ protect seq

let rec iter_s f seq =
  seq ()
  >>=? function
  | Nil -> unit_es | Cons (item, seq) -> f item >>= fun () -> iter_s f seq

let iter_s f seq = iter_s f @@ protect seq

let rec iter_es f seq =
  seq ()
  >>=? function
  | Nil -> unit_es | Cons (item, seq) -> f item >>=? fun () -> iter_es f seq

let iter_es f seq = iter_es f @@ protect seq

let rec map f seq () =
  seq ()
  >>=? function
  | Nil -> nil_es | Cons (item, seq) -> Monad.return (Cons (f item, map f seq))

let map f seq = map f @@ protect seq

let rec map_e f seq () =
  seq ()
  >>=? function
  | Nil ->
      nil_es
  | Cons (item, seq) ->
      f item >>?= fun item -> Monad.return (Cons (item, map_e f seq))

let map_e f seq = map_e f @@ protect seq

let rec map_s f seq () =
  seq ()
  >>=? function
  | Nil ->
      nil_es
  | Cons (item, seq) ->
      f item >>= fun item -> Monad.return (Cons (item, map_s f seq))

let map_s f seq = map_s f @@ protect seq

let rec map_es f seq () =
  seq ()
  >>=? function
  | Nil ->
      nil_es
  | Cons (item, seq) ->
      f item >>=? fun item -> Monad.return (Cons (item, map_es f seq))

let map_es f seq = map_es f @@ protect seq

let rec map_error f seq () =
  seq ()
  >>= function
  | Ok Nil ->
      nil_es
  | Ok (Cons (item, seq)) ->
      Monad.return (Cons (item, map_error f seq))
  | Error e ->
      Lwt.return (Error (f e))

let map_error f seq = map_error f @@ protect seq

let rec map_error_s f seq () =
  seq ()
  >>= function
  | Ok Nil ->
      nil_es
  | Ok (Cons (item, seq)) ->
      Monad.return (Cons (item, map_error_s f seq))
  | Error e ->
      f e >>= fun e -> Lwt.return (Error e)

let rec filter f seq () =
  seq ()
  >>=? function
  | Nil ->
      nil_es
  | Cons (item, seq) ->
      if f item then Monad.return (Cons (item, seq)) else filter f seq ()

let filter f seq = filter f @@ protect seq

let rec filter_e f seq () =
  seq ()
  >>=? function
  | Nil ->
      nil_es
  | Cons (item, seq) -> (
      f item
      >>?= function
      | true ->
          Monad.return (Cons (item, filter_e f seq))
      | false ->
          filter_e f seq () )

let filter_e f seq = filter_e f @@ protect seq

let rec filter_s f seq () =
  seq ()
  >>=? function
  | Nil ->
      nil_es
  | Cons (item, seq) -> (
      f item
      >>= function
      | true ->
          Monad.return (Cons (item, filter_s f seq))
      | false ->
          filter_s f seq () )

let filter_s f seq = filter_s f @@ protect seq

let rec filter_es f seq () =
  seq ()
  >>=? function
  | Nil ->
      nil_es
  | Cons (item, seq) -> (
      f item
      >>=? function
      | true ->
          Monad.return (Cons (item, filter_es f seq))
      | false ->
          filter_es f seq () )

let filter_es f seq = filter_es f @@ protect seq

let rec filter_map f seq () =
  seq ()
  >>=? function
  | Nil ->
      nil_es
  | Cons (item, seq) -> (
    match f item with
    | None ->
        filter_map f seq ()
    | Some item ->
        Monad.return (Cons (item, filter_map f seq)) )

let filter_map f seq = filter_map f @@ protect seq

let rec filter_map_e f seq () =
  seq ()
  >>=? function
  | Nil ->
      nil_es
  | Cons (item, seq) -> (
      f item
      >>?= function
      | None ->
          filter_map_e f seq ()
      | Some item ->
          Monad.return (Cons (item, filter_map_e f seq)) )

let filter_map_e f seq = filter_map_e f @@ protect seq

let rec filter_map_s f seq () =
  seq ()
  >>=? function
  | Nil ->
      nil_es
  | Cons (item, seq) -> (
      f item
      >>= function
      | None ->
          filter_map_s f seq ()
      | Some item ->
          Monad.return (Cons (item, filter_map_s f seq)) )

let filter_map_s f seq = filter_map_s f @@ protect seq

let rec filter_map_es f seq () =
  seq ()
  >>=? function
  | Nil ->
      nil_es
  | Cons (item, seq) -> (
      f item
      >>=? function
      | None ->
          filter_map_es f seq ()
      | Some item ->
          Monad.return (Cons (item, filter_map_es f seq)) )

let filter_map_es f seq = filter_map_es f @@ protect seq

let rec find f seq =
  seq ()
  >>=? function
  | Nil ->
      Monad.none_es
  | Cons (item, seq) ->
      if f item then Monad.return (Some item) else find f seq

let find f seq = find f @@ protect seq

let rec find_e f seq =
  seq ()
  >>=? function
  | Nil ->
      Monad.none_es
  | Cons (item, seq) -> (
      f item >>?= function true -> some_es item | false -> find_e f seq )

let find_e f seq = find_e f @@ protect seq

let rec find_s f seq =
  seq ()
  >>=? function
  | Nil ->
      Monad.none_es
  | Cons (item, seq) -> (
      f item >>= function true -> some_es item | false -> find_s f seq )

let find_s f seq = find_s f @@ protect seq

let rec find_es f seq =
  seq ()
  >>=? function
  | Nil ->
      Monad.none_es
  | Cons (item, seq) -> (
      f item >>=? function true -> some_es item | false -> find_es f seq )

let find_es f seq = find_es f @@ protect seq

let rec of_seq seq () =
  match seq () with
  | Stdlib.Seq.Nil ->
      nil_es
  | Stdlib.Seq.Cons (e, seq) ->
      Monad.return (Cons (e, of_seq seq))

let rec of_seq_e seq () =
  match seq () with
  | Stdlib.Seq.Nil ->
      nil_es
  | Stdlib.Seq.Cons (Ok e, seq) ->
      Monad.return (Cons (e, of_seq_e seq))
  | Stdlib.Seq.Cons ((Error _ as e), _) ->
      Lwt.return e

let rec of_seqe seq () =
  match seq () with
  | Ok Seq_e.Nil ->
      nil_es
  | Ok (Seq_e.Cons (item, seq)) ->
      Monad.return (Cons (item, of_seqe seq))
  | Error _ as e ->
      Lwt.return e

let rec of_seq_s seq () =
  match seq () with
  | Stdlib.Seq.Nil ->
      nil_es
  | Stdlib.Seq.Cons (p, seq) ->
      p >>= fun e -> Monad.return (Cons (e, of_seq_s seq))

let rec of_seqs seq () =
  seq ()
  >>= function
  | Seq_s.Nil ->
      nil_es
  | Seq_s.Cons (e, seq) ->
      Monad.return (Cons (e, of_seqs seq))

let rec of_seq_es seq () =
  match seq () with
  | Stdlib.Seq.Nil ->
      nil_es
  | Stdlib.Seq.Cons (p, seq) -> (
      p
      >>= function
      | Error _ as e ->
          Lwt.return e
      | Ok e ->
          Monad.return (Cons (e, of_seq_es seq)) )
