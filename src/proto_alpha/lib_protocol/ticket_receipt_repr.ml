(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

type update = {account : Destination_repr.t; amount : Z.t}

type ticket_token = {
  ticketer : Contract_repr.t;
  contents_type : Script_repr.expr;
  contents : Script_repr.expr;
}

type item = {ticket_token : ticket_token; updates : update list}

type t = item list

let update_encoding =
  let open Data_encoding in
  conv
    (fun {account; amount} -> (account, amount))
    (fun (account, amount) -> {account; amount})
    (obj2 (req "account" Destination_repr.encoding) (req "amount" z))

let ticket_token_encoding =
  let open Data_encoding in
  conv
    (fun {ticketer; contents_type; contents} ->
      (ticketer, contents_type, contents))
    (fun (ticketer, contents_type, contents) ->
      {ticketer; contents_type; contents})
    (obj3
       (req "ticketer" Contract_repr.encoding)
       (req "content_type" Script_repr.expr_encoding)
       (req "content" Script_repr.expr_encoding))

let item_encoding =
  let open Data_encoding in
  conv
    (fun {ticket_token; updates} -> (ticket_token, updates))
    (fun (ticket_token, updates) -> {ticket_token; updates})
    (obj2
       (req "ticket_token" ticket_token_encoding)
       (req "updates" (list update_encoding)))

let encoding = Data_encoding.list item_encoding
