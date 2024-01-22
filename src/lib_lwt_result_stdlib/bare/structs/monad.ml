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

module Lwt_syntax = struct
  include Lwt
  include Lwt.Syntax

  let return_ok_unit = Lwt.return (Ok ())

  let return_ok_true = Lwt.return (Ok true)

  let return_ok_false = Lwt.return (Ok false)

  let return_ok_none = Lwt.return (Ok None)

  let return_ok_nil = Lwt.return (Ok [])
end

module Option_syntax = struct
  let (return [@ocaml.inline "always"]) = fun x -> Some x

  let (fail [@ocaml.inline "always"]) = None

  let return_unit = return ()

  let return_nil = return []

  let return_true = return true

  let return_false = return false

  let ( let* ) = Stdlib.Option.bind

  let ( let+ ) o f = Stdlib.Option.map f o

  let both a b = match (a, b) with Some x, Some y -> Some (x, y) | _ -> None

  let ( and* ) = both

  let ( and+ ) = both
end

module Result_syntax = struct
  let (return [@ocaml.inline "always"]) = fun x -> Ok x

  let (fail [@ocaml.inline "always"]) = fun x -> Error x

  let return_unit = Ok ()

  let return_none = Ok None

  let return_some x = Ok (Some x)

  let return_nil = Ok []

  let return_true = Ok true

  let return_false = Ok false

  let ( let* ) = Result.bind

  let ( let+ ) v f = Result.map f v

  let rec join_errors errors = function
    | Ok _ :: ts -> join_errors errors ts
    | Error error :: ts -> join_errors (error :: errors) ts
    | [] -> Error errors

  let rec join = function
    | [] -> return_unit
    | Ok () :: ts -> join ts
    | Error error :: ts -> join_errors [error] ts

  let all ts =
    let rec aux acc = function
      | [] -> Ok (Stdlib.List.rev acc)
      | Ok v :: ts -> aux (v :: acc) ts
      | Error error :: ts -> join_errors [error] ts
    in
    aux [] ts

  let both a b =
    match (a, b) with
    | Ok a, Ok b -> Ok (a, b)
    | Error err, Ok _ | Ok _, Error err -> Error [err]
    | Error erra, Error errb -> Error [erra; errb]
end

module Lwt_option_syntax = struct
  let (return [@ocaml.inline "always"]) = fun x -> Lwt.return_some x

  let (fail [@ocaml.inline "always"]) = Lwt.return None

  let return_unit = Lwt_syntax.return_some ()

  let return_true = Lwt_syntax.return_some true

  let return_false = Lwt_syntax.return_some false

  let return_nil = Lwt_syntax.return_some []

  let both a b =
    let open Lwt_syntax in
    let+ a, b = both a b in
    Option_syntax.both a b

  let ( let* ) lo f = Lwt.bind lo (function None -> fail | Some x -> f x)

  let ( and* ) = both

  let ( let+ ) lo f = Lwt.map (Stdlib.Option.map f) lo

  let ( and+ ) = both

  let ( let*! ) = Lwt.bind

  let ( let*? ) o f = match o with Some x -> f x | None -> fail
end

module Lwt_result_syntax = struct
  let (return [@ocaml.inline "always"]) = fun x -> Lwt.return (Ok x)

  let (fail [@ocaml.inline "always"]) = fun x -> Lwt.return (Error x)

  let return_unit = Lwt_syntax.return_ok_unit

  let return_none = Lwt_syntax.return_ok_none

  let return_some x = Lwt.return (Ok (Some x))

  let return_true = Lwt_syntax.return_ok_true

  let return_false = Lwt_syntax.return_ok_false

  let return_nil = Lwt_syntax.return_ok_nil

  let ( let* ) = Lwt_result.bind

  let ( let+ ) v f = Lwt_result.map f v

  let lwt_map_error = Lwt_result.map_error

  let ( let*! ) = Lwt.bind

  let ( let*? ) r f =
    match r with Ok v -> f v | Error _ as err -> Lwt.return err

  let join ts =
    let open Lwt_syntax in
    let+ ps = all ts in
    Result_syntax.join ps

  let all ts =
    let open Lwt_syntax in
    let+ ps = all ts in
    Result_syntax.all ps

  let both a b =
    let open Lwt_syntax in
    let+ a, b = both a b in
    Result_syntax.both a b
end

(**/**)

(* For internal use only, not advertised *)

(* Like Lwt.apply but specialised for two-parameters functions *)
let (lwt_apply2 [@ocaml.inline "always"]) =
 fun f x y -> try f x y with exn -> Lwt.fail exn

let (lwt_apply3 [@ocaml.inline "always"]) =
 fun f a x y -> try f a x y with exn -> Lwt.fail exn
