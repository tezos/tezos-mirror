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

(** WebAssembly engine.

    An engine manages the compilation and runtime environment for
    WebAssembly modules. It is configured via {!Config.t} and used to
    create {!Store.t} instances. Ownership of the underlying Wasmer
    object is transferred to the OCaml side; callers must invoke
    {!delete} when the engine is no longer needed.

    {b Warning:} No caller currently calls {!delete}. In the fast
    execution backend the engine is created once and lives for the
    entire process lifetime, so the leak is benign in practice. *)

open Utils
open Api

type t = Types.Engine.t Ctypes.ptr

(** [create config] instantiates a new WebAssembly engine using the given
    [config]. Raises {!Error.Wasmer_error} if engine creation fails. *)
let create config =
  let config = Config.to_owned config in
  let engine = Functions.Engine.new_with_config config in
  check_null_ptr Error.(make_exception Create_engine) engine ;
  engine

(** [delete engine] destroys the engine and frees its resources. The engine
    must not be used after this call. *)
let delete = Functions.Engine.delete
