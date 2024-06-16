(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* From OCaml values to Micheline expressions *)

let seq ?(loc = 0) l = Tezos_micheline.Micheline.Seq (loc, l)

let pair ?(loc = 0) a b =
  Tezos_micheline.Micheline.Prim (loc, Script.D_Pair, [a; b], [])

let pair_n ?(loc = 0) els =
  Tezos_micheline.Micheline.Prim (loc, Script.D_Pair, els, [])

let comb ?(loc = 0) es =
  Tezos_micheline.Micheline.Prim (loc, Script.D_Pair, es, [])

let none ?(loc = 0) () =
  Tezos_micheline.Micheline.Prim (loc, Script.D_None, [], [])

let some ?(loc = 0) a =
  Tezos_micheline.Micheline.Prim (loc, Script.D_Some, [a], [])

let left ?(loc = 0) a =
  Tezos_micheline.Micheline.Prim (loc, Script.D_Left, [a], [])

let right ?(loc = 0) b =
  Tezos_micheline.Micheline.Prim (loc, Script.D_Right, [b], [])

let unit ?(loc = 0) () =
  Tezos_micheline.Micheline.Prim (loc, Script.D_Unit, [], [])

let int ?(loc = 0) i = Tezos_micheline.Micheline.Int (loc, i)

let bytes ?(loc = 0) s = Tezos_micheline.Micheline.Bytes (loc, s)

let string ?(loc = 0) s = Tezos_micheline.Micheline.String (loc, s)

let mutez ?(loc = 0) m = int ~loc (Z.of_int64 (Tez.to_mutez m))

(* Translate a timestamp to a Micheline expression in optimized
   form *)
let timestamp ?(loc = 0) ts = int ~loc (Script_timestamp.to_zint ts)

let address ?(loc = 0) adr =
  bytes ~loc @@ Data_encoding.Binary.to_bytes_exn Contract.encoding adr

let address_string ?(loc = 0) adr = string ~loc @@ Contract.to_b58check adr

let big_map_id ?(loc = 0) id = int ~loc @@ Big_map.Id.unparse_to_z id

(* From Micheline expressions to OCaml values *)

let timestamp_of_zint zint = Script_timestamp.of_zint zint

let public_key_of_bytes_exn b =
  Data_encoding.Binary.of_bytes_exn Signature.Public_key.encoding b

let address_of_bytes_exn b =
  Data_encoding.Binary.of_bytes_exn Contract.encoding b

type exn += Invalid_address_expr of string

let address_of_string_exn s =
  match Contract.of_b58check s with
  | Ok c -> c
  | Error _ -> raise @@ Invalid_address_expr s

let originated_of_string_exn s =
  match address_of_string_exn s with
  | Contract.Originated addr -> addr
  | Contract.Implicit _ ->
      invalid_arg
        "Expected originated contract address, got implicit contract address"
