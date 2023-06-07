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
include Seqes.Monadic.Make1 (Lwt)

let take ~when_negative_length k seq =
  if k < 0 then Error when_negative_length else Ok (take k seq)

let drop ~when_negative_length k seq =
  if k < 0 then Error when_negative_length else Ok (drop k seq)

let return_s p () = Lwt.map (fun x -> Cons (x, empty)) p

let cons_s item t () =
  let* item in
  Lwt.return (Cons (item, t))

let iter_p f seq =
  let* ps = fold_left (fun acc item -> Lwt.apply f item :: acc) [] seq in
  join ps

let iteri_p f seq =
  let* ps = fold_lefti (fun acc i item -> lwt_apply2 f i item :: acc) [] seq in
  join ps

module E =
  MakeTraversors2
    (struct
      type ('a, 'e) t = ('a, 'e) Result.t

      let bind = Result.bind

      let return = Result.ok
    end)
    (struct
      type ('a, 'e) t = ('a, 'e) Lwt_result.t

      let bind = Lwt_result.bind

      let return = Lwt_result.return
    end)
    (struct
      let bind x f =
        match x with Error _ as err -> Lwt.return err | Ok x -> f x
    end)
    (struct
      let bind = Lwt.bind
    end)

module S = M

module ES =
  MakeTraversors2
    (struct
      type ('a, 'e) t = ('a, 'e) Result.t Lwt.t

      let bind = Lwt_result.bind

      let return = Lwt_result.return
    end)
    (struct
      type ('a, 'e) t = ('a, 'e) Lwt_result.t

      let bind = Lwt_result.bind

      let return = Lwt_result.return
    end)
    (struct
      let bind = Lwt_result.bind
    end)
    (struct
      let bind = Lwt.bind
    end)

let rec of_seq seq () =
  match seq () with
  | Stdlib.Seq.Nil -> empty ()
  | Stdlib.Seq.Cons (e, seq) -> Lwt.return (Cons (e, of_seq seq))

let rec of_seq_s seq () =
  match seq () with
  | Stdlib.Seq.Nil -> empty ()
  | Stdlib.Seq.Cons (p, seq) ->
      let* e = p in
      Lwt.return (Cons (e, of_seq_s seq))
