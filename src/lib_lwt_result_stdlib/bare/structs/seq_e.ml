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
include Seqes.Monadic.Make2 (Result)
module E = M

module S =
  MakeTraversors
    (struct
      type ('a, 'e) t = 'a Lwt.t

      let return = Lwt.return

      let bind = Lwt.bind
    end)
    (Lwt_result)
    (struct
      let bind = Lwt.bind
    end)
    (struct
      let bind x f =
        match x with Error _ as err -> Lwt.return err | Ok v -> f v
    end)

module ES =
  MakeTraversors (Lwt_result) (Lwt_result)
    (struct
      let bind = Lwt_result.bind
    end)
    (struct
      let bind x f =
        match x with Error _ as err -> Lwt.return err | Ok v -> f v
    end)

let take ~when_negative_length k seq =
  if k < 0 then Error when_negative_length else Ok (take k seq)

let drop ~when_negative_length k seq =
  if k < 0 then Error when_negative_length else Ok (drop k seq)

let return_e r () =
  let* r in
  Ok (Cons (r, empty))

let interrupted e () = Error e

let cons_e item t () =
  let* item in
  Ok (Cons (item, t))

let rec map_error (f : 'e -> 'f) (seq : ('a, 'e) t) : ('a, 'f) t =
 fun () ->
  match seq () with
  | Ok Nil as n -> n
  | Ok (Cons (item, seq)) -> Ok (Cons (item, map_error f seq))
  | Error e -> Error (f e)

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

let rec of_seq_catch seq () =
  match seq () with
  | exception e -> Error e
  | Stdlib.Seq.Nil -> Ok Nil
  | Stdlib.Seq.Cons (e, seq) -> Ok (Cons (e, of_seq_catch seq))

let rec of_seq_once ~when_forced_twice seq () =
  match seq () with
  | exception Seq.Forced_twice -> Error when_forced_twice
  | Stdlib.Seq.Nil -> Ok Nil
  | Stdlib.Seq.Cons (e, seq) ->
      Ok (Cons (e, of_seq_once ~when_forced_twice seq))

let of_seq_once ~when_forced_twice seq =
  of_seq_once ~when_forced_twice (Seq.once seq)

let rec of_seq_e seq () =
  match seq () with
  | Stdlib.Seq.Nil -> Ok Nil
  | Stdlib.Seq.Cons (Ok e, seq) -> Ok (Cons (e, of_seq_e seq))
  | Stdlib.Seq.Cons ((Error _ as e), _) -> e
