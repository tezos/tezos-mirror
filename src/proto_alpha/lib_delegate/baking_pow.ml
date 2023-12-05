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

let with_version_constant =
  let commit_hash =
    match
      Hex.to_string (`Hex Tezos_version_value.Current_git_info.commit_hash)
    with
    | None -> Tezos_version_value.Current_git_info.commit_hash
    | Some s -> s
  in
  if String.length commit_hash >= 4 then String.sub commit_hash 0 4
  else default_constant

let with_version_constant_len = String.length with_version_constant

let proof_of_work_nonce =
  let out =
    Bytes.make Alpha_context.Constants.proof_of_work_nonce_size '\000'
  in
  let () =
    Bytes.blit_string with_version_constant 0 out 0 with_version_constant_len
  in
  out

(* [proof_of_work_nonce] will be modified in place so we make a clean copy to expose to the outside *)
let empty_proof_of_work_nonce = Bytes.copy proof_of_work_nonce

let max_z_len =
  Alpha_context.Constants.proof_of_work_nonce_size - with_version_constant_len

(* Make a string of zeros to restore [proof_of_work_nonce] to its original value in 1 operation *)
let zeros = String.make max_z_len '\000'

let mine ~proof_of_work_threshold shell builder =
  let open Lwt_result_syntax in
  match
    Option.bind
      (Data_encoding.Binary.fixed_length Block_payload_hash.encoding)
      (fun payload ->
        Option.map
          (fun round ->
            let shell =
              Data_encoding.Binary.length
                Block_header.shell_header_encoding
                shell
            in
            shell + payload + round + with_version_constant_len)
          (Data_encoding.Binary.fixed_length Round_repr.encoding))
    (* Where to put the proof of work value is in the bytes of the encoded header *)
  with
  | None -> failwith "Cannot compute block header offset"
  | Some offset ->
      let () =
        (* Restore proof_of_work_nonce to its original value. *)
        Bytes.blit_string
          zeros
          0
          proof_of_work_nonce
          with_version_constant_len
          max_z_len
      in
      (* Build the binary of the block header with 0 as proof of work and compute its hash. *)
      let block_0 = builder proof_of_work_nonce in
      let block_header =
        Data_encoding.Binary.to_bytes_exn
          Alpha_context.Block_header.encoding
          Alpha_context.Block_header.
            {
              shell;
              protocol_data = {contents = block_0; signature = Signature.zero};
            }
      in
      let block_hash = Block_header.hash_raw block_header in
      let block_hash_bytes = Block_hash.to_bytes block_hash in
      (* The loop edits [block_header] and [block_hash] (by editing its subpart [block_hash_bytes]!) in place. *)
      let rec loop z =
        let z_len = (Z.numbits z + 7) / 8 in
        if z_len > max_z_len then
          failwith
            "Client_baking_pow.mine: couldn't find nonce for required proof of \
             work"
        else (
          Bytes.blit_string (Z.to_bits z) 0 block_header offset z_len ;
          (if Hacl_star.AutoConfig2.(has_feature VEC256) then
           Hacl_star.Hacl.Blake2b_256.Noalloc.hash
          else Hacl_star.Hacl.Blake2b_32.Noalloc.hash)
            ~key:Bytes.empty
            ~msg:block_header
            ~digest:block_hash_bytes ;
          if
            Alpha_context.Block_header.Proof_of_work.check_hash
              block_hash
              proof_of_work_threshold
          then
            let () =
              Bytes.blit_string
                (Z.to_bits z)
                0
                proof_of_work_nonce
                with_version_constant_len
                z_len
            in
            let block = builder proof_of_work_nonce in
            return block
          else loop (Z.succ z))
      in
      loop Z.zero
