(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Protocol
open Alpha_context
open Tezos_micheline.Micheline

val seq : loc:'a -> ('a, Script.prim) node list -> ('a, Script.prim) node

val pair :
  loc:'a ->
  ('a, Script.prim) node ->
  ('a, Script.prim) node ->
  ('a, Script.prim) node

val none : loc:'a -> unit -> ('a, Script.prim) node

val some : loc:'a -> ('a, Script.prim) node -> ('a, Script.prim) node

val left : loc:'a -> ('a, Script.prim) node -> ('a, Script.prim) node

val right : loc:'a -> ('a, Script.prim) node -> ('a, Script.prim) node

val int : loc:'a -> Z.t -> ('a, Script.prim) node

val bytes : loc:'a -> bytes -> ('a, Script.prim) node

val string : loc:'a -> string -> ('a, Script.prim) node

val bool : loc:'a -> bool -> ('a, Script.prim) node

val protocol_hash : loc:'a -> Protocol_hash.t -> ('a, Script.prim) node

val public_key : loc:'a -> Signature.Public_key.t -> ('a, Script.prim) node

val pvss_public_key :
  loc:'a -> Pvss_secp256k1.Public_key.t -> ('a, Script.prim) node

val d_unit : loc:'a -> ('a, Script.prim) node

val t_unit : loc:'a -> ('a, Script.prim) node

(** No-op lambda expression of type
    [lambda unit (pair (list operation) (list baker_operation))] *)
val generic_baker_noop : loc:'a -> ('a, Script.prim) node
