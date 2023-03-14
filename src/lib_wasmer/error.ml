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

type task =
  | Create_engine
  | Create_store
  | Create_configuration
  | Create_module
  | Instantiate_module

let string_of_task = function
  | Create_engine -> "creating a Wasmer engine"
  | Create_store -> "creating a WebAssembly store"
  | Create_configuration -> "creating a Wasmer configuration"
  | Create_module -> "creating a WebAssembly module"
  | Instantiate_module -> "instantiating a WebAssembly module"

type t = {task : task; reason : string}

exception Wasmer_error of t

let make_exception task reason = Wasmer_error {task; reason}

let () =
  Printexc.register_printer @@ function
  | Wasmer_error {task; reason} ->
      Some
        (Format.asprintf
           "Wasmer errored while %s: %s"
           (string_of_task task)
           reason)
  | _ -> None
