(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Big_map = struct
  let title = "big_map"

  type alloc = {key_type : Script_repr.expr; value_type : Script_repr.expr}

  type update = {
    key : Script_repr.expr;
        (** The key is ignored by [apply_update] but is shown in the receipt,
            as specified in [print_big_map_diff]. *)
    key_hash : Script_expr_hash.t;
    value : Script_repr.expr option;
  }

  type updates = update list

  let alloc_encoding =
    let open Data_encoding in
    conv
      (fun {key_type; value_type} -> (key_type, value_type))
      (fun (key_type, value_type) -> {key_type; value_type})
      (obj2
         (req "key_type" Script_repr.expr_encoding)
         (req "value_type" Script_repr.expr_encoding))

  let update_encoding =
    let open Data_encoding in
    conv
      (fun {key_hash; key; value} -> (key_hash, key, value))
      (fun (key_hash, key, value) -> {key_hash; key; value})
      (obj3
         (req "key_hash" Script_expr_hash.encoding)
         (req "key" Script_repr.expr_encoding)
         (opt "value" Script_repr.expr_encoding))

  let updates_encoding = Data_encoding.list update_encoding
end

type ('alloc, 'updates) t = Big_map : (Big_map.alloc, Big_map.updates) t

type ex = E : (_, _) t -> ex

let all = [(0, E Big_map)]

type (_, _) eq = Eq : ('a, 'a) eq

let eq :
    type a1 u1 a2 u2. (a1, u1) t -> (a2, u2) t -> (a1 * u1, a2 * u2) eq option
    =
 fun k1 k2 -> match (k1, k2) with (Big_map, Big_map) -> Some Eq
