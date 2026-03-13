(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** WebAssembly module.

    A module is a compiled WebAssembly program. It can be created from
    text (WAT) or binary (WASM) format and then instantiated via
    {!Instance.create}. *)

open Utils
open Vectors
open Api

type t = Types.Module.t Ctypes.ptr

(** [wat2wasm code] converts WebAssembly text format to binary. *)
let wat2wasm code =
  let source = Byte_vector.from_string code in
  let dest = Byte_vector.empty () in
  Functions.wat2wasm (Ctypes.addr source) (Ctypes.addr dest) ;
  dest

(** Textual (WAT) or binary (WASM) representation of WebAssembly. *)
type format = Text | Binary

(** [create store format code] parses and compiles a WebAssembly module
    from [code] in the given [format]. Raises {!Error.Wasmer_error} on
    failure. *)
let create store format code =
  let wasm =
    match format with
    | Binary -> Byte_vector.from_string code
    | Text -> wat2wasm code
  in
  let modul = Functions.Module.new_ store (Ctypes.addr wasm) in
  Byte_vector.delete wasm ;
  check_null_ptr Error.(make_exception Instantiate_module) modul ;
  modul

(** [imports modul] returns a vector of import type descriptors for [modul]. *)
let imports modul =
  let outputs = Import_type_vector.empty () in
  Functions.Module.imports modul (Ctypes.addr outputs) ;
  outputs

(** [exports modul] returns a vector of export type descriptors for [modul]. *)
let exports modul =
  let outputs = Export_type_vector.empty () in
  Functions.Module.exports modul (Ctypes.addr outputs) ;
  outputs

(** [delete modul] destroys the module. Must not be used after this call. *)
let delete = Functions.Module.delete
