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
open Tezos_micheline.Micheline
open Alpha_context

let seq ~loc l = Seq (loc, l)

let pair ~loc a b = Prim (loc, Script.D_Pair, [a; b], [])

let none ~loc () = Prim (loc, Script.D_None, [], [])

let some ~loc a = Prim (loc, Script.D_Some, [a], [])

let left ~loc a = Prim (loc, Script.D_Left, [a], [])

let right ~loc b = Prim (loc, Script.D_Right, [b], [])

let int ~loc i = Int (loc, i)

let bytes ~loc s = Bytes (loc, s)

let string ~loc s = String (loc, s)

let bool ~loc b =
  Prim (loc, (if b then Script.D_True else Script.D_False), [], [])

let protocol_hash ~loc p = string ~loc (Protocol_hash.to_b58check p)

let public_key ~loc k =
  bytes
    ~loc
    (Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding k)

let pvss_public_key ~loc k =
  bytes
    ~loc
    (Data_encoding.Binary.to_bytes_exn Pvss_secp256k1.Public_key.encoding k)

let d_unit ~loc = Prim (loc, Script.D_Unit, [], [])

let t_unit ~loc = Prim (loc, Script.T_unit, [], [])

let generic_baker_noop ~loc =
  seq
    ~loc
    [ Prim (loc, Script.I_DROP, [], []);
      Prim (loc, I_NIL, [Prim (loc, T_baker_operation, [], [])], []);
      Prim (loc, I_NIL, [Prim (loc, T_operation, [], [])], []);
      Prim (loc, I_PAIR, [], []) ]
