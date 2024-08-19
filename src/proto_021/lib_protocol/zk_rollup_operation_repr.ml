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

type price = {id : Ticket_hash_repr.t; amount : Z.t}

type t = {
  op_code : int;
  price : price;
  l1_dst : Signature.Public_key_hash.t;
  rollup_id : Zk_rollup_repr.t;
  payload : Zk_rollup_scalar.t array;
}

let int_to_scalar x = Zk_rollup_scalar.of_z (Z.of_int x)

let pkh_to_scalar x =
  Zk_rollup_scalar.of_bits
    (Data_encoding.Binary.to_string_exn Signature.Public_key_hash.encoding x)

let ticket_hash_to_scalar ticket_hash =
  Zk_rollup_scalar.of_bits
  @@ Data_encoding.Binary.to_string_exn Ticket_hash_repr.encoding ticket_hash

let to_scalar_array {op_code; price; l1_dst; rollup_id; payload} =
  Array.concat
    [
      [|
        int_to_scalar op_code;
        ticket_hash_to_scalar price.id;
        Zk_rollup_scalar.of_z @@ Z.abs price.amount;
        pkh_to_scalar l1_dst;
        Zk_rollup_repr.to_scalar rollup_id;
      |];
      payload;
    ]

let price_encoding =
  Data_encoding.(
    conv
      (fun {id; amount} -> (id, amount))
      (fun (id, amount) -> {id; amount})
      (obj2 (req "id" Ticket_hash_repr.encoding) (req "amount" z)))

let encoding =
  Data_encoding.(
    conv
      (fun {op_code; price; l1_dst; rollup_id; payload} ->
        (op_code, price, l1_dst, rollup_id, payload))
      (fun (op_code, price, l1_dst, rollup_id, payload) ->
        {op_code; price; l1_dst; rollup_id; payload})
      (obj5
         (req "op_code" int31)
         (req "price" price_encoding)
         (req "l1_dst" Signature.Public_key_hash.encoding)
         (req "rollup_id" Zk_rollup_repr.Address.encoding)
         (req "payload" Plonk.scalar_array_encoding)))
