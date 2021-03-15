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

let some_s v = Lwt.return (Some v)

let some_es v = Lwt.return (Ok (Some v))

let none_e = Ok None

let none_s = Lwt.return None

let none_es = Lwt.return none_e

let value_e o ~error = to_result ~none:error o

let value_f o ~default = match o with None -> default () | Some v -> v

let value_fe o ~error =
  match o with None -> Error (error ()) | Some v -> Ok v

let either oa ob = match oa with Some _ -> oa | None -> ob

let either_f oa ob = match oa with Some _ -> oa | None -> ob ()

let map_s f o =
  match o with None -> Lwt.return_none | Some v -> f v >>= Lwt.return_some

let map_e f o = match o with None -> none_e | Some v -> Result.map some (f v)

let map_es f o = match o with None -> none_es | Some v -> f v >|=? some

let fold_s ~none ~some = function None -> Lwt.return none | Some v -> some v

let fold_f ~none ~some = function None -> none () | Some v -> some v

let iter_s f = function None -> Lwt.return_unit | Some v -> f v

let iter_e f = function None -> Ok () | Some v -> f v

let iter_es f = function None -> Lwt.return_ok () | Some v -> f v

let of_result = function Ok v -> Some v | Error _ -> None
