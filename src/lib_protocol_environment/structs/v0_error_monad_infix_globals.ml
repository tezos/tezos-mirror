(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let return = Lwt.return_ok

let return_unit = Lwt.return_ok ()

let return_true = Lwt.return_ok true

let return_false = Lwt.return_ok false

let return_nil = Lwt.return_ok []

let return_none = Lwt.return_ok None

let return_some x = Lwt.return_ok (Some x)

let ( >>= ) = Lwt.bind

let ( >|= ) v f = Lwt.map f v

let ( >>? ) = Result.bind

let ( >|? ) v f = Result.map f v

let ( >>=? ) = Lwt_result.bind

let ( >|=? ) v f = Lwt_result.map f v

let ( >>?= ) v f = match v with Error e -> Lwt.return_error e | Ok o -> f o

let ( >|?= ) v f =
  match v with
  | Error e -> Lwt.return_error e
  | Ok o -> f o >>= fun x -> Lwt.return_ok x

let ok x = Ok x

let ( >>|? ) v f =
  v >>= function Error e -> Lwt.return_error e | Ok v -> Lwt.return_ok (f v)
