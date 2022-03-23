(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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
  context_hash : Context_hash.t;
  withdraw_list_hash : Tx_rollup_withdraw_list_hash_repr.t;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {context_hash; withdraw_list_hash} ->
      (context_hash, withdraw_list_hash))
    (fun (context_hash, withdraw_list_hash) ->
      {context_hash; withdraw_list_hash})
    (obj2
       (req "context_hash" Context_hash.encoding)
       (req "withdraw_list_hash" Tx_rollup_withdraw_list_hash_repr.encoding))

let empty_l2_context_hash =
  Context_hash.of_b58check_exn
    "CoVu7Pqp1Gh3z33mink5T5Q2kAQKtnn3GHxVhyehdKZpQMBxFBGF"

let init =
  {
    context_hash = empty_l2_context_hash;
    withdraw_list_hash = Tx_rollup_withdraw_list_hash_repr.empty;
  }
