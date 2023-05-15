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

(* -------- Helpers I/O functions -------- *)
let add_path r n = r ^ "/" ^ n

let read_enc filepath filename enc =
  let inc = open_in (add_path filepath filename) in
  let file_size = In_channel.length inc |> Int64.to_int in
  let data = Stdlib.really_input_string inc file_size in
  close_in inc ;
  match Data_encoding.Json.from_string data with
  | Ok json -> Data_encoding.Json.destruct enc json
  | Error _ -> raise (Invalid_argument "Could not read file")

let write_enc filepath filename enc data =
  let outc = open_out (add_path filepath filename) in
  Printf.fprintf outc "%s" Data_encoding.Json.(construct enc data |> to_string) ;
  close_out outc

(* Timelock encryption scheme *)
type symmetric_key = Crypto_box.Secretbox.key

type ciphertext = {nonce : Crypto_box.nonce; payload : bytes}

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

(* -------- Timelock types, conversion functions and encodings -------- *)
(* RSA group rsa2048 = p * q where p and q are prime numbers *)
type rsa_public = Z.t

(* default RSA rsa2048: the 2048 bit RSA rsa2048 challenge
   c.f. https://en.wikipedia.org/wiki/RSA_numbers#RSA-2048 *)
let rsa2048 =
  Z.of_string
    "25195908475657893494027183240048398571429282126204032027777137836043662020707595556264018525880784406918290641249515082189298559149176184502808489120072844992687392807287776735971418347270261896375014971824691165077613379859095700097330459748808428401797429100642458691817195118746121515172654632282216869987549182422433637259085141865462043576798423387184774447920739934236584823824281198163815010674810451660377306056201619676256133844143603833904414952634432190114657544454178424020924616515723350778707749817125772467962926386356373289912154831438167899885040445364023527381951378636564391212010397122822120720357"

let rsa_public_encoding =
  let open Data_encoding in
  def "timelock.rsa2048"
  @@ conv_with_guard
       (fun rsa_public -> rsa_public)
       (fun rsa_public ->
         if Z.equal rsa_public rsa2048 then Ok rsa_public
         else Error "not RSA2048 rsa2048")
       (obj1 (req "rsa_public" n))

(* Timelock challenge, also called "locked" value *)
type locked_value = Z.t

let to_locked_value_opt x =
  let y = Z.of_string x in
  if y >= rsa2048 then None else Some y

let to_locked_value_unsafe = Z.of_string

(* Timelock opening, also called "unlocked" value. *)
type unlocked_value = Z.t

(* VDF proof (Wesolowski https://eprint.iacr.org/2018/623.pdf) *)
type vdf_proof = Z.t

(* Timelock tuple: challenge, opening and VDF proof *)
type vdf_tuple = {
  locked_value : locked_value;
  unlocked_value : unlocked_value;
  vdf_proof : vdf_proof;
}

let vdf_tuple_encoding =
  let open Data_encoding in
  def "timelock.vdf_tuple"
  @@ conv_with_guard
       (fun vdf_tuple ->
         (vdf_tuple.locked_value, vdf_tuple.unlocked_value, vdf_tuple.vdf_proof))
       (fun (locked_value, unlocked_value, vdf_proof) ->
         Ok {locked_value; unlocked_value; vdf_proof})
       (obj3
          (req "locked_value" n)
          (req "unlocked_value" n)
          (req "vdf_proof" n))

let to_vdf_tuple_unsafe x y z =
  {
    locked_value = Z.of_string x;
    unlocked_value = Z.of_string y;
    vdf_proof = Z.of_string z;
  }

(* Timelock proof:
   - a VDF tuple, and a random coin
   - a scalar, either the random coin for the precomputer or 1 *)
type timelock_proof = {vdf_tuple : vdf_tuple; nonce : Z.t}

let proof_encoding =
  let open Data_encoding in
  def "timelock.proof"
  @@ conv_with_guard
       (fun proof -> (proof.vdf_tuple, proof.nonce))
       (fun (vdf_tuple, nonce) -> Ok {vdf_tuple; nonce})
       (obj2 (req "vdf_tuple" vdf_tuple_encoding) (req "nonce" n))

(* -------- Timelock low level functions -------- *)
(* A random Z arith element of size [size] bytes *)
let random_z size = Hacl.Rand.gen size |> Bytes.to_string |> Z.of_bits

(* Generates almost uniformly a Zarith element between 0 and [public key].
   Intended for generating the timelock *)
let gen_locked_value_unsafe rsa_public =
  let size_rsa2048 = Z.to_bits rsa_public |> String.length in
  (* We divide by 8 to convert to bytes *)
  Z.erem (random_z ((size_rsa2048 / 8) + 16)) rsa_public

let gen_locked_value_opt rsa_public =
  if not @@ Z.equal rsa_public rsa2048 then None
  else Some (gen_locked_value_unsafe rsa_public)

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
  (* Beware, the function nextprime gives a biased distribution,
     using it here is fine as the input is already uniformly distributed *)
  Z.(nextprime (of_bits (Bytes.to_string hash_result)))

(* Proof generation optimisation taken from page 3 of the following paper:
   https://crypto.stanford.edu/~dabo/pubs/papers/VDFsurvey.pdf page 3
   where g is the time-locked value.
*)
let prove_wesolowski rsa_public ~time locked_value unlocked_value =
  let l = hash_to_prime rsa_public ~time locked_value unlocked_value in
  let pi, r = Z.(ref one, ref one) in
  for _ = 1 to time do
    let two_r = Z.(!r lsl 1) in
    (* r <- 2*r mod l *)
    (r := Z.(two_r mod l)) ;
    let pi_sqr = Z.(!pi * !pi mod rsa_public) in
    (* pi <- pi^2 * locked_value^b where b = floor(2*r/l) in [0,1] *)
    pi := if two_r >= l then Z.(pi_sqr * locked_value) else pi_sqr
  done ;
  Z.(!pi mod rsa_public)

let prove rsa_public ~time locked_value unlocked_value =
  let vdf_proof =
    prove_wesolowski rsa_public ~time locked_value unlocked_value
  in
  let vdf_tuple = {locked_value; unlocked_value; vdf_proof} in
  {vdf_tuple; nonce = Z.one}

let verify_wesolowski rsa_public ~time vdf_tuple =
  let l =
    hash_to_prime
      rsa_public
      ~time
      vdf_tuple.locked_value
      vdf_tuple.unlocked_value
  in
  let r = Z.(powm (of_int 2) (Z.of_int time) l) in
  vdf_tuple.unlocked_value
  = Z.(
      powm vdf_tuple.vdf_proof l rsa_public
      * powm vdf_tuple.locked_value r rsa_public
      mod rsa_public)

let to_vdf_tuple_opt rsa_public ~time x y z =
  let tuple = to_vdf_tuple_unsafe x y z in
  let x, y, z = Z.(of_string x, of_string y, of_string z) in
  let b_group = x < rsa_public && y < rsa_public && z < rsa_public in
  let b_weso = verify_wesolowski rsa_public ~time tuple in
  if b_group && b_weso then Some tuple else None

let verify rsa_public ~time locked_value proof =
  (* Verify link between precomputed tuple, challenge and evaluation *)
  let randomized_challenge =
    Z.powm proof.vdf_tuple.locked_value proof.nonce rsa_public
  in
  let b_exp = Z.(equal randomized_challenge locked_value) in
  (* Verify Wesolowski proof *)
  let b_weso = verify_wesolowski rsa_public ~time proof.vdf_tuple in
  (* Return *)
  b_exp && b_weso

let rec unlock_timelock rsa_public ~time locked_value =
  if time = 0 then locked_value
  else
    unlock_timelock
      rsa_public
      ~time:Int.(pred time)
      Z.(locked_value * locked_value mod rsa_public)

(* Gives the value that was timelocked from the timelock, the public modulus
   and the time. Works in linear time in [time] *)
let unlock_and_prove rsa_public ~time locked_value =
  let unlocked_value = unlock_timelock rsa_public ~time locked_value in
  prove rsa_public ~time locked_value unlocked_value

let precompute_timelock ?(locked_value = None) ?(precompute_path = None) ~time
    () =
  let locked_value =
    match locked_value with
    | None -> gen_locked_value_unsafe rsa2048
    | Some c -> Z.(c mod rsa2048)
  in
  let compute_tuple () =
    let unlocked_value = unlock_timelock rsa2048 ~time locked_value in
    (prove rsa2048 ~time locked_value unlocked_value).vdf_tuple
  in
  match precompute_path with
  | None -> compute_tuple ()
  | Some filepath ->
      let brsa = Z.to_bits rsa2048 in
      let file_prefix = Blake2B.(hash_string [brsa] |> to_hex) |> Hex.show in
      let filename = file_prefix ^ "_" ^ string_of_int time ^ ".json" in
      let file_exists = Sys.file_exists (add_path filepath filename) in
      if file_exists then read_enc filepath filename vdf_tuple_encoding
      else
        let precomputed = compute_tuple () in
        write_enc filepath filename vdf_tuple_encoding precomputed ;
        precomputed

let proof_of_vdf_tuple rsa_public ~time vdf_tuple =
  if
    Z.compare vdf_tuple.locked_value rsa_public > 0
    || Z.compare vdf_tuple.unlocked_value rsa_public > 0
  then
    raise
      (Invalid_argument "Invalid timelock tuple, its elements are not in group.") ;
  if verify_wesolowski rsa_public ~time vdf_tuple then
    let nonce = random_z (128 + (Z.to_bits rsa_public |> String.length)) in
    let randomized_locked_value =
      Z.powm vdf_tuple.locked_value nonce rsa_public
    in
    let proof = {vdf_tuple; nonce} in
    (randomized_locked_value, proof)
  else raise (Invalid_argument "Timelock tuple verification failed.")

(* Creates a symmetric key using hash based key derivation from the time locked value*)
let timelock_proof_to_symmetric_key rsa_public proof =
  let updated = Z.powm proof.vdf_tuple.unlocked_value proof.nonce rsa_public in
  let kdf_key = "Tezoskdftimelockv1" in
  let to_hash = Z.to_string updated in
  let hash = Blake2B.(to_bytes @@ hash_string ~key:kdf_key [to_hash]) in
  Crypto_box.Secretbox.unsafe_of_bytes hash

let locked_value_to_symmetric_key rsa_public ~time locked_value proof =
  if verify rsa_public ~time locked_value proof then
    Some (timelock_proof_to_symmetric_key rsa_public proof)
  else None

(* -------- Timelock high level functions (used in Tezos) -------- *)
type chest = {
  locked_value : locked_value;
  rsa_public : rsa_public;
  ciphertext : ciphertext;
}

let chest_encoding =
  let open Data_encoding in
  def "timelock.chest"
  @@ conv_with_guard
       (fun chest -> (chest.locked_value, chest.rsa_public, chest.ciphertext))
       (fun (locked_value, rsa_public, ciphertext) ->
         if Z.Compare.(locked_value < Z.zero || locked_value >= rsa_public) then
           Error "locked value is not in the rsa group"
         else if not @@ Z.equal rsa_public rsa2048 then
           Error "not RSA2048 rsa2048"
         else Ok {locked_value; rsa_public; ciphertext})
       (obj3
          (req "locked_value" n)
          (req "rsa_public" n)
          (req "ciphertext" ciphertext_encoding))

type chest_key = timelock_proof

let chest_key_encoding = proof_encoding

type opening_result = Correct of Bytes.t | Bogus_cipher | Bogus_opening

let create_chest_and_chest_key ?(precompute_path = None) ~payload ~time () =
  let locked_value, proof =
    let vdf_tuple = precompute_timelock ~time ~precompute_path () in
    proof_of_vdf_tuple rsa2048 ~time vdf_tuple
  in
  let sym_key = timelock_proof_to_symmetric_key rsa2048 proof in
  let ciphertext = encrypt sym_key payload in
  ({locked_value; rsa_public = rsa2048; ciphertext}, proof)

let create_chest_key chest ~time =
  unlock_and_prove chest.rsa_public ~time chest.locked_value

let get_plaintext_size chest =
  Bytes.length chest.ciphertext.payload - Crypto_box.tag_length

let open_chest chest chest_key ~time =
  if time < 0 then failwith "Timelock: trying to open with a negative time"
  else
    let sym_key_opt =
      locked_value_to_symmetric_key
        chest.rsa_public
        ~time
        chest.locked_value
        chest_key
    in
    match sym_key_opt with
    | None -> Bogus_opening
    | Some sym_key -> (
        let plaintext_opt = decrypt sym_key chest.ciphertext in
        match plaintext_opt with
        | None -> Correct Bytes.empty
        | Some plaintext -> Correct plaintext)

module Internal_for_tests = struct
  let rsa_public_to_z x = x

  let locked_value_to_z x = x

  let unlocked_value_to_z x = x

  let vdf_proof_to_z x = x

  let hash_to_prime = hash_to_prime

  let prove_wesolowski = prove_wesolowski

  let verify_wesolowski = verify_wesolowski
end

(* -------- Sampling functions for gas benchmarks -------- *)
(* Those function are unsafe for wallet usage as they use the OCaml
   random generator. This is used to easily reproduce benchmarks. *)

let vdf_tuples =
  Array.map
    (Data_encoding.Binary.of_string_exn vdf_tuple_encoding)
    Timelock_precompute.vdf_tuples

let gen_random_bytes_bench_unsafe size =
  Bytes.init size (fun _ -> Char.chr (Random.int 256))

let gen_random_z_unsafe size =
  gen_random_bytes_bench_unsafe size |> Bytes.to_string |> Z.of_bits

let encrypt_unsafe symmetric_key plaintext =
  let nonce =
    Data_encoding.Binary.of_bytes_exn
      Crypto_box.nonce_encoding
      (gen_random_bytes_bench_unsafe Crypto_box.nonce_size)
  in
  {
    nonce;
    payload = Crypto_box.Secretbox.secretbox symmetric_key plaintext nonce;
  }

let proof_of_vdf_tuple_unsafe rsa_public ~time vdf_tuple =
  if verify_wesolowski rsa_public ~time vdf_tuple then
    let nonce =
      gen_random_z_unsafe (128 + (Z.to_bits rsa_public |> String.length))
    in
    let randomized_locked_value =
      Z.powm vdf_tuple.locked_value nonce rsa_public
    in
    let proof = {vdf_tuple; nonce} in
    (randomized_locked_value, proof)
  else raise (Invalid_argument "Timelock tuple verification failed.")

let chest_sampler ~rng_state ~plaintext_size ~time =
  Random.set_state rng_state ;
  let plaintext = gen_random_bytes_bench_unsafe plaintext_size in
  (* As we only benchmark the type encodings and the verification function,
     it is safe to sample chests on known precomputed tuples in order to
     generate them quickly. As such, we assert here that the time input is a
     power of 2 lower than 30 (we only precomputed tuples up to that value). *)
  let locked_value, proof =
    let log_time = Float.(of_int time |> log2 |> to_int) in
    let vdf_tuple =
      if log_time < 30 then vdf_tuples.(log_time)
      else failwith "Timelock: trying to sample chest with too high time."
    in
    proof_of_vdf_tuple_unsafe rsa2048 ~time vdf_tuple
  in
  let sym_key = timelock_proof_to_symmetric_key rsa2048 proof in
  let ciphertext = encrypt_unsafe sym_key plaintext in
  ({locked_value; rsa_public = rsa2048; ciphertext}, proof)
