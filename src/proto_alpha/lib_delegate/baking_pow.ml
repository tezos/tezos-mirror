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

open Protocol

let default_constant = "\x00\x00\x00\x05"

let is_updated_constant =
  let commit_hash =
    match Hex.to_string (`Hex Tezos_version.Current_git_info.commit_hash) with
    | None -> Tezos_version.Current_git_info.commit_hash
    | Some s -> s
  in
  if String.length commit_hash >= 4 then String.sub commit_hash 0 4
  else default_constant

let is_updated_constant_len = String.length is_updated_constant

(* add a version to the pow *)
let init_proof_of_work_nonce () =
  let buf =
    Bytes.make Alpha_context.Constants.proof_of_work_nonce_size '\000'
  in
  Bytes.blit_string is_updated_constant 0 buf 0 is_updated_constant_len ;
  let max_z_len =
    Alpha_context.Constants.proof_of_work_nonce_size - is_updated_constant_len
  in
  let rec aux z =
    let z_len = (Z.numbits z + 7) / 8 in
    if z_len > max_z_len then Seq.Nil
    else (
      Bytes.blit_string (Z.to_bits z) 0 buf is_updated_constant_len z_len ;
      Seq.Cons (buf, fun () -> aux (Z.succ z)))
  in
  aux Z.zero

(* This was used before November 2018 *)
(* (\* Random proof of work *\)
 * let generate_proof_of_work_nonce () =
 *   Tezos_crypto.Rand.generate Alpha_context.Constants.proof_of_work_nonce_size *)

let empty_proof_of_work_nonce =
  Bytes.make Constants_repr.proof_of_work_nonce_size '\000'

let mine ~proof_of_work_threshold shell builder =
  let rec loop nonce_seq =
    match nonce_seq with
    | Seq.Nil ->
        failwith
          "Client_baking_pow.mine: couldn't find nonce for required proof of \
           work"
    | Seq.Cons (nonce, seq) ->
        let block = builder nonce in
        if
          Alpha_context.Block_header.Proof_of_work
          .check_header_proof_of_work_stamp
            shell
            block
            proof_of_work_threshold
        then return block
        else loop (seq ())
  in
  loop (init_proof_of_work_nonce ())
