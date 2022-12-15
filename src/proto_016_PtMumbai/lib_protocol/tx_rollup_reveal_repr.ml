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

type t = {
  contents : Script_repr.lazy_expr;
  ty : Script_repr.lazy_expr;
  ticketer : Contract_repr.t;
  amount : Tx_rollup_l2_qty.t;
  claimer : Signature.Public_key_hash.t;
}

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {contents; ty; ticketer; amount; claimer} ->
      (contents, ty, ticketer, amount, claimer))
    (fun (contents, ty, ticketer, amount, claimer) ->
      {contents; ty; ticketer; amount; claimer})
    (obj5
       (req "contents" Script_repr.lazy_expr_encoding)
       (req "ty" Script_repr.lazy_expr_encoding)
       (req "ticketer" Contract_repr.encoding)
       (req "amount" Tx_rollup_l2_qty.encoding)
       (req "claimer" Signature.Public_key_hash.encoding))
