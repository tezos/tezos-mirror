(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type signer = {
  alias : string;
  pkh : Signature.public_key_hash;
  pk : Signature.public_key;
  sk : Client_keys.sk_uri;
}

let get_signer cctxt pkh =
  let open Lwt_result_syntax in
  let* (alias, pk, sk) = Client_keys.get_key cctxt pkh in
  return {alias; pkh; pk; sk}

type 'block reorg = {
  ancestor : 'block option;
  old_chain : 'block list;
  new_chain : 'block list;
}

let no_reorg = {ancestor = None; old_chain = []; new_chain = []}

let reorg_encoding block_encoding =
  let open Data_encoding in
  conv
    (fun {ancestor; old_chain; new_chain} -> (ancestor, old_chain, new_chain))
    (fun (ancestor, old_chain, new_chain) -> {ancestor; old_chain; new_chain})
  @@ obj3
       (opt "ancestor" block_encoding)
       (req "old_chain" (list block_encoding))
       (req "new_chain" (list block_encoding))
