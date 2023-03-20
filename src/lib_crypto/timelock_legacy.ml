(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Tezos_hacl

type rsa_secret = {p : Z.t; q : Z.t}

type rsa_public = Z.t (* RSA modulus = p * q*)

type time_lock_proof = Z.t

type locked_value = Z.t

type unlocked_value = Z.t

type symmetric_key = Crypto_box.Secretbox.key

type ciphertext = {nonce : Crypto_box.nonce; payload : bytes}

(*Should be sufficient since the public key will not be exposed for a long time*)
let size_modulus = 2048

(* Creates a symmetric key using hash based key derivation from the time locked value*)
let unlocked_value_to_symmetric_key unlocked_value =
  let kdf_key = "Tezoskdftimelockv0" in
  let to_hash = Z.to_string unlocked_value in
  let hash = Blake2B.(to_bytes @@ hash_string ~key:kdf_key [to_hash]) in
  Crypto_box.Secretbox.unsafe_of_bytes hash

(* A random Z arith element of size [size] bytes *)
let random_z size = Hacl.Rand.gen size |> Bytes.to_string |> Z.of_bits

let random_prime_z size =
  let rec aux () =
    let trial = random_z size in
    if Z.probab_prime trial 25 = 0 then aux () else trial
  in
  aux ()

(* Generates and RSA key pair of 2048 bits (= 256 bytes) *)
let gen_rsa_keys () =
  (* We divide by 8 to convert to bytes and by 2 because we generate two primes *)
  let size = size_modulus / (2 * 8) in
  let p = random_prime_z size in
  let q = random_prime_z size in
  (Z.(p * q), {p; q})

(* Generates almost uniformly a Zarith element between 0 and [public key].
   Intended for generating the time lock *)
let gen_locked_value rsa_public =
  (* We divide by 8 to convert to bytes *)
  Z.erem (random_z ((size_modulus / 8) + 16)) rsa_public

(* The resulting prime has size 256 bits or slightly more. *)
let hash_to_prime rsa_public ~time value key =
  let personalization = Bytes.of_string "\032" in
  let s =
    String.concat
      "\xff\x00\xff\x00\xff\x00\xff\x00"
      (Int.to_string time :: List.map Z.to_bits [rsa_public; value; key])
  in
  let (Hacl.Blake2b.Hash hash_result) =
    Hacl.Blake2b.direct ~key:personalization (Bytes.of_string s) 32
  in
  Z.(nextprime (of_bits (Bytes.to_string hash_result)))

let prove_without_secret rsa_public ~time locked_value unlocked_value =
  let l = hash_to_prime rsa_public ~time locked_value unlocked_value in
  let pow = Z.(pow (of_int 2) time / l) in
  Z.powm locked_value pow rsa_public

let prove_with_secret secret ~time locked_value unlocked_value =
  let rsa_public = Z.(secret.p * secret.q) in
  let l = hash_to_prime rsa_public ~time locked_value unlocked_value in
  let phi = Z.((secret.p - one) * (secret.q - one)) in
  let pow = Z.(pow (of_int 2) time / l mod phi) in
  Z.powm locked_value pow rsa_public

(* The proof is verified by checking that
   lv ^ (2 ^ time) = (proof ^ l) * (lv ^ r) mod public
   which is equivalent to
   2 ^ time = (((2 ^ time) / l) * l) + (2 ^ time mod l) mod phi
   see https://eprint.iacr.org/2018/712.pdf section 3.2 for this proof
*)
let verify_time_lock rsa_public ~time locked_value unlocked_value proof =
  let l = hash_to_prime rsa_public ~time locked_value unlocked_value in
  let r = Z.(powm (of_int 2) (Z.of_int time) l) in
  unlocked_value
  = Z.(powm proof l rsa_public * powm locked_value r rsa_public mod rsa_public)

(* Gives the value that was time_locked from the time_lock, the secret and the
   time. Works in logarithmic time in [time] *)
let unlock_with_secret secret ~(time : int) (locked_value : locked_value) =
  let phi = Z.((secret.p - one) * (secret.q - one)) in
  let e = Z.powm (Z.of_int 2) (Z.of_int time) phi in
  Z.powm locked_value e Z.(secret.p * secret.q)

let unlock_and_prove_with_secret secret ~(time : int)
    (locked_value : locked_value) =
  let unlocked_value = unlock_with_secret secret ~time locked_value in
  let pi = prove_with_secret secret ~time locked_value unlocked_value in
  (unlocked_value, pi)

let locked_value_to_symmetric_key_with_secret secret ~(time : int)
    (locked_value : locked_value) : symmetric_key =
  unlocked_value_to_symmetric_key (unlock_with_secret secret ~time locked_value)

(* Gives the value that was time_locked from the time_lock, the public modulus
   and the time. Works in linear time in [time] *)
let unlock_and_prove_without_secret rsa_public ~time locked_value =
  let rec aux time v =
    if time = 0 then v else aux Int.(pred time) Z.(v * v mod rsa_public)
  in
  let unlocked_value = aux time locked_value in
  let pi = prove_without_secret rsa_public ~time locked_value unlocked_value in
  (unlocked_value, pi)

let locked_value_to_symmetric_key_with_proof (rsa_public : rsa_public)
    ~(time : int) locked_value unlocked_value proof =
  if verify_time_lock rsa_public ~time locked_value unlocked_value proof then
    Some (unlocked_value_to_symmetric_key unlocked_value)
  else None

let encrypt symmetric_key plaintext =
  let nonce = Crypto_box.random_nonce () in
  {
    nonce;
    payload = Crypto_box.Secretbox.secretbox symmetric_key plaintext nonce;
  }

let decrypt symmetric_key ciphertext =
  Crypto_box.Secretbox.secretbox_open
    symmetric_key
    ciphertext.payload
    ciphertext.nonce

(* ------------*)
type chest_key = {unlocked_value : unlocked_value; proof : time_lock_proof}

type chest = {
  locked_value : locked_value;
  rsa_public : rsa_public;
  ciphertext : ciphertext;
}

let proof_encoding = Data_encoding.n

let chest_key_encoding =
  let open Data_encoding in
  def "timelock.chest_key"
  @@ conv
       (fun chest_key -> (chest_key.unlocked_value, chest_key.proof))
       (fun (unlocked_value, proof) -> {unlocked_value; proof})
       (obj2
          (req "unlocked_value" Data_encoding.n)
          (req "proof" Data_encoding.n))

let ciphertext_encoding =
  let open Data_encoding in
  def "timelock.ciphertext"
  @@ conv_with_guard
       (fun ciphertext -> (ciphertext.nonce, ciphertext.payload))
       (fun (nonce, payload) ->
         if Bytes.length payload <= Crypto_box.tag_length then
           Error "The ciphertext has a negative size"
         else Ok {nonce; payload})
       (obj2
          (req "timelock.nonce" Crypto_box.nonce_encoding)
          (req "timelock.payload" bytes))

let min_rsa_modulus = Z.(shift_left (of_int 2) 2000)

let chest_encoding =
  let open Data_encoding in
  def "timelock.chest"
  @@ conv_with_guard
       (fun chest -> (chest.locked_value, chest.rsa_public, chest.ciphertext))
       (fun (locked_value, rsa_public, ciphertext) ->
         if Z.Compare.(locked_value < Z.zero || locked_value >= rsa_public) then
           Error "locked value is not in the rsa group"
         else if Z.leq rsa_public min_rsa_modulus then
           Error "rsa modulus is too small"
         else Ok {locked_value; rsa_public; ciphertext})
       (obj3
          (req "locked_value" n)
          (req "rsa_public" n)
          (req "ciphertext" ciphertext_encoding))

type opening_result = Correct of Bytes.t | Bogus_cipher | Bogus_opening

let open_chest chest chest_key ~time =
  if time < 0 then failwith "Timelock: trying to open with a negative time"
  else
    let sym_key_opt =
      locked_value_to_symmetric_key_with_proof
        chest.rsa_public
        ~time
        chest.locked_value
        chest_key.unlocked_value
        chest_key.proof
    in
    match sym_key_opt with
    | None -> Bogus_opening
    | Some sym_key -> (
        let plaintext_opt = decrypt sym_key chest.ciphertext in
        match plaintext_opt with
        | None -> Bogus_cipher
        | Some plaintext -> Correct plaintext)

let create_chest_and_chest_key ~payload ~time =
  let rsa_public, rsa_secret = gen_rsa_keys () in
  let locked_value = gen_locked_value rsa_public in
  let unlocked_value, proof =
    unlock_and_prove_with_secret rsa_secret ~time locked_value
  in
  let sym_key = unlocked_value_to_symmetric_key unlocked_value in
  let ciphertext = encrypt sym_key payload in
  ({locked_value; rsa_public; ciphertext}, {unlocked_value; proof})

let create_chest_key chest ~time =
  let unlocked_value, proof =
    unlock_and_prove_without_secret chest.rsa_public ~time chest.locked_value
  in
  {unlocked_value; proof}

let get_plaintext_size chest =
  Bytes.length chest.ciphertext.payload - Crypto_box.tag_length

(*-------- sampling function for gas benchmarks-----*)
(* Those function are unsafe for wallet usage as they use the OCaml
   random generator. This is used to easily reproduce benchmarks. *)
let gen_random_bytes_unsafe size =
  Bytes.init size (fun _ -> Char.chr (Random.int 256))

let gen_random_z_unsafe size =
  gen_random_bytes_unsafe size |> Bytes.to_string |> Z.of_bits

let gen_random_prime_unsafe size = gen_random_z_unsafe size |> Z.nextprime

let gen_rsa_keys_unsafe () =
  let size = size_modulus / (2 * 8) in
  let p = gen_random_prime_unsafe size in
  let q = gen_random_prime_unsafe size in
  (Z.(p * q), {p; q})

let gen_locked_value_unsafe rsa_public =
  Z.erem (gen_random_z_unsafe (size_modulus / 8)) rsa_public

let encrypt_unsafe symmetric_key plaintext =
  let nonce =
    Data_encoding.Binary.of_bytes_exn
      Crypto_box.nonce_encoding
      (gen_random_bytes_unsafe Crypto_box.nonce_size)
  in
  {
    nonce;
    payload = Crypto_box.Secretbox.secretbox symmetric_key plaintext nonce;
  }

let chest_sampler ~rng_state ~plaintext_size ~time =
  Random.set_state rng_state ;
  let plaintext = gen_random_bytes_unsafe plaintext_size in
  let rsa_public, rsa_secret = gen_rsa_keys_unsafe () in
  let locked_value = gen_locked_value_unsafe rsa_public in
  let unlocked_value, proof =
    unlock_and_prove_with_secret rsa_secret ~time locked_value
  in
  let sym_key = unlocked_value_to_symmetric_key unlocked_value in
  let ciphertext = encrypt_unsafe sym_key plaintext in
  ({locked_value; rsa_public; ciphertext}, {unlocked_value; proof})
