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

(** Wasmer engine configuration.

    Controls compiler backend selection and WebAssembly feature flags.
    The feature flags are kept in sync with the features available through
    the Octez WebAssembly interpreter (tezos-webassembly-interpreter). *)

open Utils
open Api

(** [make_features ()] creates a Wasmer feature set matching the features
    supported by the Octez WebAssembly interpreter. *)
let make_features () =
  let open Functions.Wasmer.Features in
  let features = new_ () in
  (* These features map to proposals in the WebAssembly spec. They must be kept
     in sync with the features available through Octez' WebAssembly interpreter
     (tezos-webassembly-interpreter).

     Every setter the C API exposes is called, including the ones we disable:
     [wasmer_features_new] returns Wasmer's own defaults, which enable
     proposals the interpreter does not implement (extended constant
     expressions, threads), and those defaults move between Wasmer releases.
     Relying on them would let a Wasmer upgrade widen the set of modules the
     fast execution accepts without the interpreter following, so the two would
     disagree on whether a kernel is valid.

     The setters are not independent, so these calls are order-sensitive:
     [reference_types true] also enables bulk memory, and [bulk_memory false]
     also disables reference types. Bulk memory is passed [true] explicitly
     rather than left to that coupling -- the interpreter implements it
     ([MemoryCopy], [MemoryFill], [MemoryInit], [DataDrop] in
     src/lib_webassembly) and every kernel rustc emits for
     wasm32-unknown-unknown contains [memory.copy].
     [Test_wasmer.test_wasm_proposals] guards both directions. *)
  ignore (bulk_memory features true : bool) ;
  ignore (exceptions features false : bool) ;
  ignore (extended_const features false : bool) ;
  ignore (memory64 features false : bool) ;
  ignore (module_linking features false : bool) ;
  ignore (multi_memory features false : bool) ;
  ignore (multi_value features false : bool) ;
  ignore (reference_types features true : bool) ;
  ignore (relaxed_simd features false : bool) ;
  ignore (simd features true : bool) ;
  ignore (tail_call features false : bool) ;
  ignore (threads features false : bool) ;
  ignore (wide_arithmetic features false : bool) ;
  features

(** Compiler backend used by the Wasmer engine. *)
type compiler = Types.Wasmer.Compiler.t = CRANELIFT | LLVM | SINGLEPASS

(** [is_compiler_available compiler] checks whether [compiler] is linked
    into the current Wasmer distribution. *)
let is_compiler_available = Functions.Wasmer.Compiler.is_available

exception Compiler_unavailable of compiler

type t = {compiler : compiler}

(** Sensible default configuration. Uses SINGLEPASS for fast compilation. *)
let default = {compiler = SINGLEPASS}

(** [to_owned desc] converts a configuration descriptor into an owned
    Wasmer config pointer ready to pass to {!Engine.create}.
    Raises {!Compiler_unavailable} if the selected compiler backend is
    not available. *)
let to_owned desc =
  let conf = Functions.Config.new_ () in
  check_null_ptr Error.(make_exception Create_configuration) conf ;
  let has_compiler = is_compiler_available desc.compiler in
  if not has_compiler then raise (Compiler_unavailable desc.compiler) ;
  Functions.Config.set_compiler conf desc.compiler ;
  Functions.Config.set_features conf (make_features ()) ;
  conf
