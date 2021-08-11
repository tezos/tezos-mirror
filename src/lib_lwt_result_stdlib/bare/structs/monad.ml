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

(** Lwt monad *)

module Lwt = Lwt

let ( >>= ) = Lwt.( >>= )

let ( >|= ) = Lwt.( >|= )

(** result monad *)

module Result = Result

let ok x = Ok x

let error e = Error e

let ( >>? ) v f = match v with Ok v -> f v | Error _ as error -> error

let ( >|? ) v f = match v with Ok v -> Ok (f v) | Error _ as error -> error

(** lwt-result combined monad *)

module LwtResult = struct
  let return x = Lwt.return (Ok x)

  let fail x = Lwt.return (Error x)

  let return_unit = Lwt.return (Ok ())

  let return_none = Lwt.return (Ok None)

  let return_some x = Lwt.return (Ok (Some x))

  let return_true = Lwt.return (Ok true)

  let return_false = Lwt.return (Ok false)

  let return_nil = Lwt.return (Ok [])

  let bind v f = v >>= function Error _ as err -> Lwt.return err | Ok v -> f v

  let bind_error v f =
    v >>= function Error e -> f e | Ok _ as ok -> Lwt.return ok

  let map f v =
    v >>= function
    | Error _ as err -> Lwt.return err
    | Ok v -> Lwt.return (Ok (f v))

  let map_error f v =
    v >>= function
    | Error e -> Lwt.return (Error (f e))
    | Ok _ as ok -> Lwt.return ok
end

let return v = Lwt.return (Ok v)

let fail v = Lwt.return (Error v)

let ( >>=? ) v f =
  v >>= function Error _ as err -> Lwt.return err | Ok v -> f v

let ( >|=? ) v f = v >>=? fun v -> Lwt.return (Ok (f v))

(** Mixing operators *)

(** All operators follow this naming convention:
      - the first character is [>]
      - the second character is [>] for [bind] and [|] for [map]
      - the next character is [=] for Lwt or [?] for Error
      - the next character (if present) is [=] for Lwt or [?] for Error, it is
      only used for operator that are within both monads. *)

let ( >>?= ) v f = match v with Error _ as e -> Lwt.return e | Ok v -> f v

let ( >|?= ) v f =
  match v with
  | Error _ as e -> Lwt.return e
  | Ok v -> f v >>= fun v -> Lwt.return (Ok v)

(* joins *)

let join_p = Lwt.join

let all_p = Lwt.all

let both_p = Lwt.both

let rec join_e_errors errors = function
  | Ok _ :: ts -> join_e_errors errors ts
  | Error error :: ts -> join_e_errors (error :: errors) ts
  | [] -> Error errors

let rec join_e = function
  | [] -> Result.return_unit
  | Ok () :: ts -> join_e ts
  | Error error :: ts -> join_e_errors [error] ts

let all_e ts =
  let rec aux acc = function
    | [] -> Ok (Stdlib.List.rev acc)
    | Ok v :: ts -> aux (v :: acc) ts
    | Error error :: ts -> join_e_errors [error] ts
  in
  aux [] ts

let both_e a b =
  match (a, b) with
  | (Ok a, Ok b) -> Ok (a, b)
  | (Error err, Ok _) | (Ok _, Error err) -> Error [err]
  | (Error erra, Error errb) -> Error [erra; errb]

let join_ep ts = all_p ts >|= join_e

let all_ep ts = all_p ts >|= all_e

let both_ep a b = both_p a b >|= fun (a, b) -> both_e a b

(**/**)

(* For internal use only, not advertised *)

(* Like Lwt.apply but specialised for two-parameters functions *)
let lwt_apply2 f x y = try f x y with exn -> Lwt.fail exn

let lwt_apply3 f a x y = try f a x y with exn -> Lwt.fail exn
