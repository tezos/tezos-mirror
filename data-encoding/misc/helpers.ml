(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let cut ?(copy = false) sz bytes =
  let length = Bytes.length bytes in
  if length <= sz then [bytes] (* if the result fits in the given sz *)
  else
    let may_copy = if copy then Bytes.copy else fun t -> t in
    let nb_full = length / sz in
    (* nb of blocks of size sz *)
    let sz_full = nb_full * sz in
    (* size of the full part *)
    let acc =
      (* eventually init acc with a non-full block *)
      if sz_full = length then []
      else [may_copy (Bytes.sub bytes sz_full (length - sz_full))]
    in
    let rec split_full_blocks curr_upper_limit acc =
      let start = curr_upper_limit - sz in
      assert (start >= 0) ;
      (* copy the block [ start, curr_upper_limit [ of size sz *)
      let acc = may_copy (Bytes.sub bytes start sz) :: acc in
      if start = 0 then acc else split_full_blocks start acc
    in
    split_full_blocks sz_full acc
