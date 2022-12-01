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
open Api

let make_features () =
  let open Functions.Wasmer.Features in
  let features = new_ () in
  (* These features map to proposals in the WebAssembly spec. They must be kept
     in sync with the features available through Octez' WebAssembly interpreter
     (tezos-webassembly-interpreter). *)
  ignore (bulk_memory features false : bool) ;
  ignore (memory64 features false : bool) ;
  ignore (module_linking features false : bool) ;
  ignore (multi_memory features false : bool) ;
  ignore (multi_value features false : bool) ;
  ignore (reference_types features true : bool) ;
  ignore (simd features true : bool) ;
  ignore (tail_call features false : bool) ;
  ignore (threads features false : bool) ;
  features

type compiler = Types.Wasmer.Compiler.t = CRANELIFT | LLVM | SINGLEPASS

let is_compiler_available = Functions.Wasmer.Compiler.is_available

exception Compiler_unavailable of compiler

type t = {compiler : compiler}

let default = {compiler = SINGLEPASS}

let to_owned desc =
  let conf = Functions.Config.new_ () in
  check_null_ptr Error.(make_exception Create_configuration) conf ;
  let has_compiler = is_compiler_available desc.compiler in
  if not has_compiler then raise (Compiler_unavailable desc.compiler) ;
  Functions.Config.set_compiler conf desc.compiler ;
  Functions.Config.set_features conf (make_features ()) ;
  conf
