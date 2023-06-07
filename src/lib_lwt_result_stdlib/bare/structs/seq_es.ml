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
include Seqes.Monadic.Make2 (Lwt_result)

let return_e r () =
  let*? x = r in
  Lwt_result_syntax.return (Cons (x, empty))

let return_s p () =
  let*! x = p in
  Lwt_result_syntax.return (Cons (x, empty))

let return_es p () =
  let* x = p in
  Lwt_result_syntax.return (Cons (x, empty))

let interrupted e () = Lwt.return (Error e)

let interrupted_s p () = Lwt_syntax.( let* ) p Lwt.return_error

let rec map_error f s () =
  let*! r = s () in
  match r with
  | Ok Nil -> Lwt_result_syntax.return Nil
  | Ok (Cons (x, s)) -> Lwt_result_syntax.return (Cons (x, map_error f s))
  | Error e -> Lwt_result_syntax.fail (f e)

let rec map_error_s f s () =
  let*! r = s () in
  match r with
  | Ok Nil -> Lwt_result_syntax.return Nil
  | Ok (Cons (x, s)) -> Lwt_result_syntax.return (Cons (x, map_error_s f s))
  | Error e ->
      let*! e = f e in
      Lwt_result_syntax.fail e

let take ~when_negative_length n s =
  if n < 0 then Error when_negative_length else Ok (take n s)

let drop ~when_negative_length n s =
  if n < 0 then Error when_negative_length else Ok (drop n s)

module S =
  Make
    (struct
      type ('a, 'e) t = 'a Lwt.t

      let bind = Lwt.bind

      let return = Lwt.return
    end)
    (struct
      let bind = Lwt.bind
    end)

module E =
  Make
    (Result)
    (struct
      let bind x f =
        match x with Error _ as err -> Lwt.return err | Ok x -> f x
    end)

module ES = M

let cons_e item t () =
  match item with
  | Error _ as e -> Lwt.return e
  | Ok item -> Lwt_result_syntax.return (Cons (item, t))

let cons_s item t () =
  let open Lwt_syntax in
  let* item in
  return_ok (Cons (item, t))

let cons_es item t () =
  let* item in
  Lwt_result_syntax.return (Cons (item, t))

let rec of_seq_e seq () =
  match seq () with
  | Stdlib.Seq.Nil -> empty ()
  | Stdlib.Seq.Cons (Ok e, seq) ->
      Lwt_result_syntax.return (Cons (e, of_seq_e seq))
  | Stdlib.Seq.Cons ((Error _ as e), _) -> Lwt.return e

let rec of_seqe seq () =
  match seq () with
  | Ok Seq_e.Nil -> empty ()
  | Ok (Seq_e.Cons (item, seq)) ->
      Lwt_result_syntax.return (Cons (item, of_seqe seq))
  | Error _ as e -> Lwt.return e

let rec of_seq_s seq () =
  match seq () with
  | Stdlib.Seq.Nil -> empty ()
  | Stdlib.Seq.Cons (p, seq) ->
      let open Lwt_syntax in
      let* e = p in
      return_ok (Cons (e, of_seq_s seq))

let rec of_seqs seq () =
  let open Lwt_syntax in
  let* n = seq () in
  match n with
  | Seq_s.Nil -> empty ()
  | Seq_s.Cons (e, seq) -> return_ok (Cons (e, of_seqs seq))

let rec of_seq_es seq () =
  match seq () with
  | Stdlib.Seq.Nil -> empty ()
  | Stdlib.Seq.Cons (p, seq) ->
      let* e = p in
      Lwt_result_syntax.return (Cons (e, of_seq_es seq))
