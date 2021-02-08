(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(*  Copyright (C) 2016, OCamlPro.                                            *)
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

open Resto
module Service = Resto.MakeService (Resto_json.Encoding)
open Service

type meth = [`GET | `POST | `DELETE | `PUT | `PATCH]

module Arg = Arg

module Path = struct
  type 'params t = (unit, 'params) Path.path

  type 'params path = (unit, 'params) Path.path

  let root = Path.root

  let add_suffix = Path.add_suffix

  let add_arg = Path.add_arg

  let ( / ) = add_suffix

  let ( /: ) = add_arg
end

module Query = Query

type ('meth, 'params, 'query, 'input, 'output, 'error) service =
  ('meth, unit, 'params, 'query, 'input, 'output, 'error) Service.t

let get_service = get_service

let post_service = post_service

let delete_service = delete_service

let put_service = put_service

let patch_service = patch_service

type 'input input = 'input Service.input =
  | No_input : unit input
  | Input : 'input Json_encoding.encoding -> 'input input

type 'input request = 'input Service.request = {
  meth : meth;
  uri : Uri.t;
  input : 'input input;
}

let forge_request = forge_request

let query = query

let input_encoding = input_encoding

let output_encoding = output_encoding

let error_encoding = error_encoding

module Description = Resto.Description

type description_service =
  ( [`GET],
    unit * string list,
    Description.request,
    unit,
    Json_schema.schema Description.directory,
    unit )
  service

let description_service ?description path =
  description_service ?description Json_encoding.empty path
