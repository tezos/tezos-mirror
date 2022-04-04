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

let prefix = Tx_rollup_prefixes.withdraw_list_hash.b58check_prefix

module H =
  Blake2B.Make
    (Base58)
    (struct
      let name = "Withdraw_list_hash"

      let title = "A list of withdraw orders"

      let b58check_prefix = prefix

      let size = Some Tx_rollup_prefixes.withdraw_list_hash.hash_size
    end)

include H
include Path_encoding.Make_hex (H)

let () =
  Tx_rollup_prefixes.(check_encoding withdraw_list_hash b58check_encoding)

let hash_uncarbonated l =
  let bytes =
    Data_encoding.(
      Binary.to_bytes_exn (list Tx_rollup_withdraw_repr.encoding) l)
  in
  H.hash_bytes [bytes]

let empty = hash_uncarbonated []
