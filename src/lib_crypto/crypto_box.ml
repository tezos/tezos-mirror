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

(** Tezos - X25519/XSalsa20-Poly1305 cryptography *)

open Hacl

type secret_key = secret Box.key

type public_key = public Box.key

type channel_key = Box.combined Box.key

type nonce = Bytes.t

type pow_target = Z.t

module Secretbox = struct
  include Hacl.Secretbox

  let secretbox key msg nonce =
    let msglen = Bytes.length msg in
    let cmsg = Bytes.create (msglen + tagbytes) in
    secretbox ~key ~nonce ~msg ~cmsg ;
    cmsg

  let secretbox_open key cmsg nonce =
    let cmsglen = Bytes.length cmsg in
    let msg = Bytes.create (cmsglen - tagbytes) in
    match secretbox_open ~key ~nonce ~cmsg ~msg with
    | false -> None
    | true -> Some msg
end

module Public_key_hash =
  Blake2B.Make
    (Base58)
    (struct
      let name = "Crypto_box.Public_key_hash"

      let title = "A Cryptobox public key ID"

      let b58check_prefix = Base58.Prefix.cryptobox_public_key_hash

      let size = Some 16
    end)

let () = Base58.check_encoded_prefix Public_key_hash.b58check_encoding "id" 30

let hash pk = Public_key_hash.hash_bytes [Box.unsafe_to_bytes pk]

let tag_length = Box.tagbytes

let random_keypair () =
  let pk, sk = Box.keypair () in
  (sk, pk, hash pk)

let zero_nonce = Bytes.make Nonce.size '\x00'

let random_nonce = Nonce.gen

let increment_nonce = Nonce.increment

let generate_nonce bytes_list =
  let hash = Blake2B.hash_bytes bytes_list in
  let s = Blake2B.to_bytes hash in
  Nonce.of_bytes_exn @@ Bytes.sub s 0 Nonce.size

let init_to_resp_seed = Bytes.of_string "Init -> Resp"

let resp_to_init_seed = Bytes.of_string "Resp -> Init"

let generate_nonces ~incoming ~sent_msg ~recv_msg =
  let (init_msg, resp_msg, false | resp_msg, init_msg, true) =
    (sent_msg, recv_msg, incoming)
  in
  let nonce_init_to_resp =
    generate_nonce [init_msg; resp_msg; init_to_resp_seed]
  in
  let nonce_resp_to_init =
    generate_nonce [init_msg; resp_msg; resp_to_init_seed]
  in
  if incoming then (nonce_init_to_resp, nonce_resp_to_init)
  else (nonce_resp_to_init, nonce_init_to_resp)

let precompute sk pk = Box.dh pk sk

let fast_box_noalloc k nonce tag buf = Box.box_noalloc ~k ~nonce ~buf ~tag

let fast_box_open_noalloc k nonce tag buf =
  Box.box_open_noalloc ~k ~nonce ~buf ~tag

let fast_box k nonce msg =
  let cmsg = Bytes.create (Bytes.length msg + tag_length) in
  Box.box ~k ~nonce ~msg ~cmsg ;
  cmsg

let fast_box_open k nonce cmsg =
  let cmsglen = Bytes.length cmsg in
  assert (cmsglen >= tag_length) ;
  let msg = Bytes.create (cmsglen - tag_length) in
  if Box.box_open ~k ~nonce ~cmsg ~msg then Some msg else None

let compare_pow_target hash pow_target =
  let hash = Z.of_bits (Blake2B.to_string hash) in
  Z.compare hash pow_target <= 0

let make_pow_target f =
  if f < 0. || 256. < f then invalid_arg "Cryptobox.target_of_float" ;
  let frac, shift = modf f in
  let shift = int_of_float shift in
  let m =
    Z.of_int64
    @@
    if frac = 0. then Int64.(pred (shift_left 1L 54))
    else Int64.of_float (2. ** (54. -. frac))
  in
  if shift < 202 then
    Z.logor
      (Z.shift_left m (202 - shift))
      (Z.pred @@ Z.shift_left Z.one (202 - shift))
  else Z.shift_right m (shift - 202)

let default_pow_target = make_pow_target 24.

let target_0 = make_pow_target 0.

let check_proof_of_work pk nonce pow_target =
  let hash = Blake2B.hash_bytes [Box.unsafe_to_bytes pk; nonce] in
  compare_pow_target hash pow_target

(* This is the non-yielding function to generate an identity. It performs a
   bounded number of attempts ([n]). This function is not exported. Instead, the
   wrapper below, [generate_proof_of_work], uses this function repeatedly but it
   intersperses calls to [Lwt.pause] to yield explicitly. *)
let generate_proof_of_work_n_attempts ~max:n pk pow_target =
  let rec loop nonce attempts =
    if attempts > n then raise Not_found
    else if check_proof_of_work pk nonce pow_target then nonce
    else loop (Nonce.increment nonce) (attempts + 1)
  in
  loop (random_nonce ()) 0

let generate_proof_of_work_with_target_0 pk =
  generate_proof_of_work_n_attempts ~max:1 pk target_0

let rec generate_proof_of_work ?(yield_every = 10000) ?max pk pow_target =
  let open Lwt.Syntax in
  match max with
  | None -> (
      try
        let pow =
          generate_proof_of_work_n_attempts ~max:yield_every pk pow_target
        in
        Lwt.return pow
      with Not_found ->
        let* () = Lwt.pause () in
        generate_proof_of_work ~yield_every pk pow_target)
  | Some max -> (
      if max <= 0 then Lwt.apply raise Not_found
      else
        let attempts = min max yield_every in
        try
          let pow =
            generate_proof_of_work_n_attempts ~max:attempts pk pow_target
          in
          Lwt.return pow
        with Not_found ->
          let* () = Lwt.pause () in
          let max = max - attempts in
          generate_proof_of_work ~yield_every ~max pk pow_target)

let public_key_to_bytes pk = Bytes.copy (Box.unsafe_to_bytes pk)

let public_key_of_bytes buf = Box.unsafe_pk_of_bytes (Bytes.copy buf)

let public_key_size = Box.pkbytes

let secret_key_to_bytes sk = Bytes.copy (Box.unsafe_to_bytes sk)

let secret_key_of_bytes buf = Box.unsafe_sk_of_bytes (Bytes.copy buf)

let secret_key_size = Box.skbytes

let nonce_size = Nonce.size

let public_key_encoding =
  let open Data_encoding in
  conv public_key_to_bytes public_key_of_bytes (Fixed.bytes public_key_size)

let secret_key_encoding =
  let open Data_encoding in
  conv secret_key_to_bytes secret_key_of_bytes (Fixed.bytes secret_key_size)

let nonce_encoding = Fixed.bytes nonce_size

let neuterize : secret_key -> public_key = Box.neuterize

let equal : public_key -> public_key -> bool = Box.equal

let pp_pk ppf pk = Hex.pp ppf (Hex.of_bytes (public_key_to_bytes pk))

module For_testing_only = struct
  let generate_proof_of_work_n_attempts = generate_proof_of_work_n_attempts
end
