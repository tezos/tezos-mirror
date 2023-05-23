(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

include
  Blake2B.Make
    (Base58)
    (struct
      let name = "smart_rollup_state_hash"

      let title = "The hash of the VM state of a smart rollup"

      let b58check_prefix = Base58.Prefix.smart_rollup_state

      (* Same size as context hashes, from which they are derived. *)
      let size = Some Context_hash.size
    end)

let () = Base58.check_encoded_prefix b58check_encoding "srs1" 54

let context_hash_to_state_hash h = of_bytes_exn @@ Context_hash.to_bytes h

(* Hackish way to disable hash_bytes and hash_string to force people to use
   context_hash_to_state_hash (without changing content of S.HASH) *)
type unreachable__use_context_hash_to_state_hash =
  | Unreachable__use_context_hash_to_state_hash

let hash_bytes = Unreachable__use_context_hash_to_state_hash

let hash_string = Unreachable__use_context_hash_to_state_hash
