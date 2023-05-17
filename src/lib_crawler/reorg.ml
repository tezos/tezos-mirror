(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Nomadic Labs, <contact@nomadic-labs.com>                    *)
(* Copyright (c) Functori, <contact@functori.com>                            *)
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

type 'block t = {old_chain : 'block list; new_chain : 'block list}

let no_reorg = {old_chain = []; new_chain = []}

let encoding block_encoding =
  let open Data_encoding in
  conv
    (fun {old_chain; new_chain} -> (old_chain, new_chain))
    (fun (old_chain, new_chain) -> {old_chain; new_chain})
  @@ obj2
       (req "old_chain" (list block_encoding))
       (req "new_chain" (list block_encoding))

let map f {old_chain; new_chain} =
  {
    old_chain = List.rev_map f old_chain |> List.rev;
    new_chain = List.rev_map f new_chain |> List.rev;
  }

let map_es f {old_chain; new_chain} =
  let open Lwt_result_syntax in
  let* old_chain = List.map_es f old_chain in
  let* new_chain = List.map_es f new_chain in
  return {old_chain; new_chain}

let map_ep f {old_chain; new_chain} =
  let open Lwt_result_syntax in
  let* old_chain = List.map_ep f old_chain
  and* new_chain = List.map_ep f new_chain in
  return {old_chain; new_chain}
