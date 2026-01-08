(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Encoding

let version_case enc choose wrap name nth =
  case
    ~title:(Printf.sprintf "%s version %d" name nth)
    Json_only
    (obj1 (req (Printf.sprintf "%s.v%d" name nth) enc))
    choose
    wrap

let make_encoding ~name l =
  union ~tag_size:`Uint8 (List.mapi (fun nth f -> f name nth) l)

type _ t =
  | Version_0 : 'v0 encoding -> 'v0 t
  | Version_S : {
      previous : 'vn t;
      encoding : 'vnp1 encoding;
      upgrade : 'vn -> 'vnp1;
    }
      -> 'vnp1 t

let first_version e = Version_0 e

let next_version encoding upgrade previous =
  Version_S {encoding; upgrade; previous}

let encoding : type a. name:string -> a t -> a encoding =
 fun ~name version ->
  match version with
  | Version_0 e ->
      make_encoding ~name [version_case e (fun x -> Some x) (fun x -> x)]
  | Version_S {previous; encoding; upgrade} ->
      let rec mk_nones :
          type (* This function generates encoding cases for all the
                  outdated versions.
                  These versions are never encoded to
                  (hence [fun _ -> None]) but are safely decoded with
                  the use of the upgrade functions. *)
          b. (b -> a) -> b t -> (string -> int -> a case) list =
       fun upgr -> function
         | Version_0 e -> [version_case e (fun _ -> None) (fun x -> upgr x)]
         | Version_S {previous; encoding; upgrade} ->
             let others = mk_nones (fun x -> upgr (upgrade x)) previous in
             version_case encoding (fun _ -> None) (fun x -> upgr x) :: others
      in
      let nones = mk_nones upgrade previous in
      let cases =
        version_case encoding (fun x -> Some x) (fun x -> x) :: nones
        |> List.rev
      in
      make_encoding ~name cases
