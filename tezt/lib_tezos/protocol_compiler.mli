(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Run octez-protocol-compiler commands. *)

(** In all functions below, [path] can be used to override the path
    to [octez-protocol-compiler], which is [Constant.tezos_protocol_compiler]
    by default. *)

(** Run [octez-protocol-compiler <protocol_dir>] and return the hash
    of the compiled protocol.

    If [hash_only] is set, then pass [-hash-only] to the protocol
    compiler. *)
val compile :
  ?path:string ->
  ?hooks:Tezt.Process.hooks ->
  ?hash_only:bool ->
  string ->
  string Lwt.t

(** Same as [compile], but do not wait for the process to exit. *)
val spawn_compile :
  ?path:string ->
  ?hooks:Tezt.Process.hooks ->
  ?hash_only:bool ->
  string ->
  Tezt.Process.t
