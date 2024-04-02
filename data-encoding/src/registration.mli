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

(** Like most other [.mli] files in this directory, this is not intended for
    end-users. Instead, the interface from this file is used internally to
    assemble the end-user-intended module {!Data_encoding}. Refer to that module
    for doucmentation. *)

type id = string

type t

val binary_schema : t -> Binary_schema.t

val json_schema : t -> Json.schema

val description : t -> string option

val json_pretty_printer : t -> Format.formatter -> Json.t -> unit

val binary_pretty_printer : t -> Format.formatter -> Bytes.t -> unit

val slice :
  t ->
  string ->
  (Binary_slicer.slice list, Binary_error_types.read_error) result

val slice_all : string -> (string * Binary_slicer.slice list) list

val register : ?pp:(Format.formatter -> 'a -> unit) -> 'a Encoding.t -> unit

val find : id -> t option

val list : unit -> (id * t) list

val bytes_of_json : t -> Json.t -> Bytes.t option

val json_of_bytes : t -> Bytes.t -> Json.t option

type introspectable = Any : _ Encoding.t -> introspectable

val find_introspectable : id -> introspectable option

val iter : id:string -> (introspectable -> unit) -> unit
