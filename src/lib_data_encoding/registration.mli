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

(** This is for use *within* the data encoding library only. Instead, you should
    use the corresponding module intended for use: {!Data_encoding.Encoding}. *)

type id = string

type registered_encoding

val binary_schema : registered_encoding -> Binary_schema.t
val json_schema : registered_encoding -> Json.schema
val description : registered_encoding -> string option

val json_pretty_printer: registered_encoding -> (Format.formatter -> Json.t -> unit)
val binary_pretty_printer: registered_encoding -> (Format.formatter -> MBytes.t -> unit)

val register :
  id:id ->
  ?description:string ->
  ?pp:(Format.formatter -> 'a -> unit) ->
  'a Encoding.t ->
  unit

val find_opt : id -> registered_encoding option
val list_registered_encodings : unit -> (id * registered_encoding) list

val bytes_of_json : registered_encoding -> Json.t -> MBytes.t option
val json_of_bytes : registered_encoding -> MBytes.t -> Json.t option
