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

type withdrawal = {
  claimer : Signature.Public_key_hash.t;
  ticket_hash : Ticket_hash_repr.t;
  amount : Tx_rollup_l2_qty.t;
}

type t = withdrawal

let encoding : withdrawal Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {claimer; ticket_hash; amount} -> (claimer, ticket_hash, amount))
    (fun (claimer, ticket_hash, amount) -> {claimer; ticket_hash; amount})
    (obj3
       (req "claimer" Signature.Public_key_hash.encoding)
       (req "ticket_hash" Ticket_hash_repr.encoding)
       (req "amount" Tx_rollup_l2_qty.encoding))

module Withdraw_list_hash = struct
  let withdraw_list_hash = Tx_rollup_prefixes.withdraw_list_hash.b58check_prefix

  include
    Blake2B.Make_merkle_tree
      (Base58)
      (struct
        let name = "Withdraw_list_hash"

        let title = "A hash of withdraw's list"

        let b58check_prefix = withdraw_list_hash

        let size = Some Tx_rollup_prefixes.withdraw_list_hash.hash_size
      end)
      (struct
        type t = withdrawal

        let to_bytes = Data_encoding.Binary.to_bytes_exn encoding
      end)

  let () =
    Tx_rollup_prefixes.(check_encoding withdraw_list_hash b58check_encoding)
end

type withdrawals_merkle_root = Withdraw_list_hash.t

let withdrawals_merkle_root_encoding = Withdraw_list_hash.encoding

type merkle_tree_path = Withdraw_list_hash.path

let merkle_tree_path_encoding = Withdraw_list_hash.path_encoding

let merkelize_list : t list -> withdrawals_merkle_root =
  Withdraw_list_hash.compute

let compute_path : t list -> int -> merkle_tree_path =
  Withdraw_list_hash.compute_path

let check_path : path -> t -> list_hash * int = Withdraw_list_hash.check_path
