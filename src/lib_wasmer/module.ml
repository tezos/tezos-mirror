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

(* For documentation please refer to the [Tezos_wasmer] module. *)

open Utils
open Vectors
open Api

type t = Types.Module.t Ctypes.ptr

let wat2wasm code =
  let source = Byte_vector.from_string code in
  let dest = Byte_vector.empty () in
  Functions.wat2wasm (Ctypes.addr source) (Ctypes.addr dest) ;
  dest

type format = Text | Binary

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

let imports modul =
  let outputs = Import_type_vector.empty () in
  Functions.Module.imports modul (Ctypes.addr outputs) ;
  outputs

let exports modul =
  let outputs = Export_type_vector.empty () in
  Functions.Module.exports modul (Ctypes.addr outputs) ;
  outputs

let delete = Functions.Module.delete
