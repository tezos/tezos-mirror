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

open Monad
include Stdlib.Option

let some_unit = Some ()

let some_unit_e = Ok some_unit

let some_unit_s = Lwt.return some_unit

let some_unit_es = Lwt.return some_unit_e

let some_nil = Some []

let some_nil_e = Ok some_nil

let some_nil_s = Lwt.return some_nil

let some_nil_es = Lwt.return some_nil_e

let some_true = Some true

let some_true_e = Ok some_true

let some_true_s = Lwt.return some_true

let some_true_es = Lwt.return some_true_e

let some_false = Some false

let some_false_e = Ok some_false

let some_false_s = Lwt.return some_false

let some_false_es = Lwt.return some_false_e

let some_e v = Ok (Some v)

let some_s v = Lwt.return_some v

let some_es v = Lwt.return (Ok (Some v))

let none_e = Ok None

let none_s = Lwt.return None

let none_es = Lwt.return none_e

let value_e o ~error = to_result ~none:error o

let value_f o ~default = match o with None -> default () | Some v -> v

let value_fe o ~error = match o with None -> Error (error ()) | Some v -> Ok v

let either oa ob = match oa with Some _ -> oa | None -> ob

let either_f oa ob = match oa with Some _ -> oa | None -> ob ()

let merge f oa ob =
  match (oa, ob) with
  | None, None -> None
  | Some r, None | None, Some r -> Some r
  | Some a, Some b -> Some (f a b)

let merge_e f oa ob =
  let open Result_syntax in
  match (oa, ob) with
  | None, None -> return_none
  | Some r, None | None, Some r -> return_some r
  | Some a, Some b ->
      let* r = f a b in
      return_some r

let merge_s f oa ob =
  let open Lwt_syntax in
  match (oa, ob) with
  | None, None -> return_none
  | Some r, None | None, Some r -> return_some r
  | Some a, Some b ->
      let* r = f a b in
      return_some r

let merge_es f oa ob =
  let open Lwt_result_syntax in
  match (oa, ob) with
  | None, None -> return_none
  | Some r, None | None, Some r -> return_some r
  | Some a, Some b ->
      let* r = f a b in
      return_some r

let map_e f o =
  let open Result_syntax in
  match o with
  | None -> return_none
  | Some v ->
      let* r = f v in
      return_some r

let map_s f o =
  let open Lwt_syntax in
  match o with
  | None -> return_none
  | Some v ->
      let* r = f v in
      return_some r

let map_es f o =
  let open Lwt_result_syntax in
  match o with
  | None -> return_none
  | Some v ->
      let* r = f v in
      return_some r

let fold_s ~none ~some = function None -> Lwt.return none | Some v -> some v

let fold_f ~none ~some = function None -> none () | Some v -> some v

let filter p o = match o with Some x when p x -> o | Some _ | None -> None

let filter_s p o =
  let open Lwt_syntax in
  match o with
  | None -> return_none
  | Some x ->
      let* b = p x in
      if b then return o else return_none

let filter_e p o =
  let open Result_syntax in
  match o with
  | None -> return_none
  | Some x ->
      let* b = p x in
      if b then return o else return_none

let filter_es p o =
  let open Lwt_result_syntax in
  match o with
  | None -> return_none
  | Some x ->
      let* b = p x in
      if b then return o else return_none

let filter_map f o = bind o f

let filter_map_s f o = match o with None -> none_s | Some x -> f x

let filter_map_e f o = match o with None -> none_e | Some x -> f x

let filter_map_es f o = match o with None -> none_es | Some x -> f x

let filter_ok = function Some (Ok x) -> Some x | Some (Error _) | None -> None

let filter_error = function
  | Some (Error x) -> Some x
  | Some (Ok _) | None -> None

let filter_left = function
  | Some (Either.Left x) -> Some x
  | Some (Either.Right _) | None -> None

let filter_right = function
  | Some (Either.Right x) -> Some x
  | Some (Either.Left _) | None -> None

let iter_s f = function None -> Lwt.return_unit | Some v -> f v

let iter_e f = function None -> Ok () | Some v -> f v

let iter_es f = function None -> Lwt_syntax.return_ok_unit | Some v -> f v

let of_result = function Ok v -> Some v | Error _ -> None

let catch ?(catch_only = fun _ -> true) f =
  match f () with
  | v -> Some v
  | exception ((Stack_overflow | Out_of_memory) as e) -> Lwt.reraise e
  | exception e -> if catch_only e then None else Lwt.reraise e

let catch_o ?(catch_only = fun _ -> true) f =
  match f () with
  | v -> v
  | exception ((Stack_overflow | Out_of_memory) as e) -> Lwt.reraise e
  | exception e -> if catch_only e then None else Lwt.reraise e

let catch_s ?(catch_only = fun _ -> true) f =
  Lwt.try_bind f Lwt.return_some (function
    | (Stack_overflow | Out_of_memory) as e -> Lwt.reraise e
    | e -> if catch_only e then Lwt.return_none else Lwt.reraise e)

let catch_os ?(catch_only = fun _ -> true) f =
  Lwt.catch f (function
    | (Stack_overflow | Out_of_memory) as e -> Lwt.reraise e
    | e -> if catch_only e then Lwt.return_none else Lwt.reraise e)
