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

open Lwt.Infix (* >>= *)

let ( >>? ) = Result.bind

let ( >>=? ) v f =
  v >>= function Error _ as err -> Lwt.return err | Ok v -> f v

let ok_nil = Ok []

let return_nil = Lwt.return ok_nil

let[@inline] return v = Lwt.return_ok v

let ok_unit = Ok ()

let return_unit = Lwt.return ok_unit

let rec map f l =
  match l with
  | [] -> ok_nil
  | h :: t ->
      f h >>? fun rh ->
      map f t >>? fun rt -> Ok (rh :: rt)

let mapi f l =
  let rec mapi f i l =
    match l with
    | [] -> ok_nil
    | h :: t ->
        f i h >>? fun rh ->
        mapi f (i + 1) t >>? fun rt -> Ok (rh :: rt)
  in
  mapi f 0 l

let rec map_s f l =
  match l with
  | [] -> return_nil
  | h :: t ->
      f h >>=? fun rh ->
      map_s f t >>=? fun rt -> return (rh :: rt)

let mapi_s f l =
  let rec mapi_s f i l =
    match l with
    | [] -> return_nil
    | h :: t ->
        f i h >>=? fun rh ->
        mapi_s f (i + 1) t >>=? fun rt -> return (rh :: rt)
  in
  mapi_s f 0 l

let rec rev_map_append_s acc f = function
  | [] -> return acc
  | hd :: tl -> f hd >>=? fun v -> rev_map_append_s (v :: acc) f tl

let rev_map_s f l = rev_map_append_s [] f l

let rec map_p f l =
  match l with
  | [] -> return_nil
  | x :: l -> (
      let tx = f x and tl = map_p f l in
      tx >>= fun x ->
      tl >>= fun l ->
      match (x, l) with
      | (Ok x, Ok l) -> Lwt.return_ok (x :: l)
      | (Error trace1, Error trace2) -> Lwt.return_error (trace1 @ trace2)
      | (Ok _, Error trace) | (Error trace, Ok _) -> Lwt.return_error trace)

let mapi_p f l =
  let rec mapi_p f i l =
    match l with
    | [] -> return_nil
    | x :: l -> (
        let tx = f i x and tl = mapi_p f (i + 1) l in
        tx >>= fun x ->
        tl >>= fun l ->
        match (x, l) with
        | (Ok x, Ok l) -> Lwt.return_ok (x :: l)
        | (Error trace1, Error trace2) -> Lwt.return_error (trace1 @ trace2)
        | (Ok _, Error trace) | (Error trace, Ok _) -> Lwt.return_error trace)
  in
  mapi_p f 0 l

let rec map2_s f l1 l2 =
  match (l1, l2) with
  | ([], []) -> return_nil
  | (_ :: _, []) | ([], _ :: _) -> invalid_arg "Error_monad.map2_s"
  | (h1 :: t1, h2 :: t2) ->
      f h1 h2 >>=? fun rh ->
      map2_s f t1 t2 >>=? fun rt -> return (rh :: rt)

let mapi2_s f l1 l2 =
  let rec mapi2_s i f l1 l2 =
    match (l1, l2) with
    | ([], []) -> return_nil
    | (_ :: _, []) | ([], _ :: _) -> invalid_arg "Error_monad.mapi2_s"
    | (h1 :: t1, h2 :: t2) ->
        f i h1 h2 >>=? fun rh ->
        mapi2_s (i + 1) f t1 t2 >>=? fun rt -> return (rh :: rt)
  in
  mapi2_s 0 f l1 l2

let rec map2 f l1 l2 =
  match (l1, l2) with
  | ([], []) -> ok_nil
  | (_ :: _, []) | ([], _ :: _) -> invalid_arg "Error_monad.map2"
  | (h1 :: t1, h2 :: t2) ->
      f h1 h2 >>? fun rh ->
      map2 f t1 t2 >>? fun rt -> Ok (rh :: rt)

let mapi2 f l1 l2 =
  let rec mapi2 i f l1 l2 =
    match (l1, l2) with
    | ([], []) -> ok_nil
    | (_ :: _, []) | ([], _ :: _) -> invalid_arg "Error_monad.mapi2"
    | (h1 :: t1, h2 :: t2) ->
        f i h1 h2 >>? fun rh ->
        mapi2 (i + 1) f t1 t2 >>? fun rt -> Ok (rh :: rt)
  in
  mapi2 0 f l1 l2

let rec filter_map_s f l =
  match l with
  | [] -> return_nil
  | h :: t -> (
      f h >>=? function
      | None -> filter_map_s f t
      | Some rh -> filter_map_s f t >>=? fun rt -> return (rh :: rt))

let rec filter_map_p f l =
  match l with
  | [] -> return_nil
  | h :: t -> (
      let th = f h and tt = filter_map_p f t in
      th >>=? function
      | None -> tt
      | Some rh -> tt >>=? fun rt -> return (rh :: rt))

let rec filter f l =
  match l with
  | [] -> ok_nil
  | h :: t -> (
      f h >>? function
      | true -> filter f t >>? fun t -> Ok (h :: t)
      | false -> filter f t)

let rec filter_s f l =
  match l with
  | [] -> return_nil
  | h :: t -> (
      f h >>=? function
      | false -> filter_s f t
      | true -> filter_s f t >>=? fun t -> return (h :: t))

let rec filter_p f l =
  match l with
  | [] -> return_nil
  | h :: t -> (
      let jh = f h and t = filter_p f t in
      jh >>=? function false -> t | true -> t >>=? fun t -> return (h :: t))

let rec iter f l =
  match l with [] -> ok_unit | h :: t -> f h >>? fun () -> iter f t

let rec iter_s f l =
  match l with [] -> return_unit | h :: t -> f h >>=? fun () -> iter_s f t

let rec iter_p f l =
  match l with
  | [] -> return_unit
  | x :: l -> (
      let tx = f x and tl = iter_p f l in
      tx >>= fun tx_res ->
      tl >>= fun tl_res ->
      match (tx_res, tl_res) with
      | (Ok (), Ok ()) -> Lwt.return_ok ()
      | (Error trace1, Error trace2) -> Lwt.return_error (trace1 @ trace2)
      | (Ok (), Error trace) | (Error trace, Ok ()) -> Lwt.return_error trace)

let iteri_p f l =
  let rec iteri_p i f l =
    match l with
    | [] -> return_unit
    | x :: l -> (
        let tx = f i x and tl = iteri_p (i + 1) f l in
        tx >>= fun tx_res ->
        tl >>= fun tl_res ->
        match (tx_res, tl_res) with
        | (Ok (), Ok ()) -> Lwt.return ok_unit
        | (Error trace1, Error trace2) -> Lwt.return_error (trace1 @ trace2)
        | (Ok (), Error trace) | (Error trace, Ok ()) -> Lwt.return_error trace)
  in
  iteri_p 0 f l

let rec iter2_p f l1 l2 =
  match (l1, l2) with
  | ([], []) -> return_unit
  | ([], _) | (_, []) -> invalid_arg "Error_monad.iter2_p"
  | (x1 :: l1, x2 :: l2) -> (
      let tx = f x1 x2 and tl = iter2_p f l1 l2 in
      tx >>= fun tx_res ->
      tl >>= fun tl_res ->
      match (tx_res, tl_res) with
      | (Ok (), Ok ()) -> Lwt.return_ok ()
      | (Error trace1, Error trace2) -> Lwt.return_error (trace1 @ trace2)
      | (Ok (), Error trace) | (Error trace, Ok ()) -> Lwt.return_error trace)

let iteri2_p f l1 l2 =
  let rec iteri2_p i f l1 l2 =
    match (l1, l2) with
    | ([], []) -> return_unit
    | ([], _) | (_, []) -> invalid_arg "Error_monad.iteri2_p"
    | (x1 :: l1, x2 :: l2) -> (
        let tx = f i x1 x2 and tl = iteri2_p (i + 1) f l1 l2 in
        tx >>= fun tx_res ->
        tl >>= fun tl_res ->
        match (tx_res, tl_res) with
        | (Ok (), Ok ()) -> Lwt.return_ok ()
        | (Error trace1, Error trace2) -> Lwt.return_error (trace1 @ trace2)
        | (Ok (), Error trace) | (Error trace, Ok ()) -> Lwt.return_error trace)
  in
  iteri2_p 0 f l1 l2

let rec fold_left_s f init l =
  match l with
  | [] -> return init
  | h :: t -> f init h >>=? fun acc -> fold_left_s f acc t

let rec fold_right_s f l init =
  match l with
  | [] -> return init
  | h :: t -> fold_right_s f t init >>=? fun acc -> f h acc
