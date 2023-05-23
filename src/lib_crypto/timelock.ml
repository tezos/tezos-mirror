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

(* -------- Helpers I/O functions -------- *)
let blake : ?key:string -> string -> bytes =
 fun ?key s ->
  let key = Option.map Bytes.of_string key in
  let module Blake2b = Hacl_star.Hacl.Blake2b_32 in
  Blake2b.hash ?key (Bytes.of_string s) 32

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

(* default RSA rsa2048: the 2048 bit RSA rsa2048 challenge
   c.f. https://en.wikipedia.org/wiki/RSA_numbers#RSA-2048 *)
let rsa2048 =
  Z.of_string
    "25195908475657893494027183240048398571429282126204032027777137836043662020707595556264018525880784406918290641249515082189298559149176184502808489120072844992687392807287776735971418347270261896375014971824691165077613379859095700097330459748808428401797429100642458691817195118746121515172654632282216869987549182422433637259085141865462043576798423387184774447920739934236584823824281198163815010674810451660377306056201619676256133844143603833904414952634432190114657544454178424020924616515723350778707749817125772467962926386356373289912154831438167899885040445364023527381951378636564391212010397122822120720357"

(* RSA2048 RSA modulus size. *)
let size_rsa2048 = 2048

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
         if Z.Compare.(locked_value < Z.zero || locked_value >= rsa2048) then
           Error "locked_value is not in the rsa group"
         else if Z.Compare.(locked_value <= Z.one) then
           Error "invalid value for locked_value"
         else if
           Z.Compare.(unlocked_value < Z.zero || unlocked_value >= rsa2048)
         then Error "unlocked_value is not in the rsa group"
         else if Z.Compare.(vdf_proof < Z.zero || vdf_proof >= rsa2048) then
           Error "VDF proof is not in the rsa group"
         else Ok {locked_value; unlocked_value; vdf_proof})
       (obj3
          (req "locked_value" n)
          (req "unlocked_value" n)
          (req "vdf_proof" n))

(* Timelock proof:
   - a VDF tuple, and a random coin
   - a scalar, either the random coin for the precomputer or 1 *)
type timelock_proof = {vdf_tuple : vdf_tuple; nonce : Z.t}

let proof_encoding =
  let open Data_encoding in
  def "timelock.proof"
  @@ conv_with_guard
       (fun proof -> (proof.vdf_tuple, proof.nonce))
       (fun (t, nonce) ->
         if Z.Compare.(t.locked_value < Z.zero || t.locked_value >= rsa2048)
         then Error "locked_value is not in the rsa group"
         else if Z.Compare.(t.locked_value <= Z.one) then
           Error "invalid value for locked_value"
         else if
           Z.Compare.(t.unlocked_value < Z.zero || t.unlocked_value >= rsa2048)
         then Error "unlocked_value is not in the rsa group"
         else if Z.Compare.(t.vdf_proof < Z.zero || t.vdf_proof >= rsa2048) then
           Error "VDF proof is not in the rsa group"
         else if Z.Compare.(nonce < Z.one) then
           Error "nonce is null or negative"
         else Ok {vdf_tuple = t; nonce})
       (obj2 (req "vdf_tuple" vdf_tuple_encoding) (req "nonce" n))

(* -------- Timelock low level functions -------- *)

(* Generates almost uniformly a Zarith element between 0 and [public key].
   Intended for generating the timelock *)
let gen_locked_value_unsafe () =
  (* We divide by 8 to convert to bytes *)
  let random_z size = Hacl.Rand.gen size |> Bytes.to_string |> Z.of_bits in
  Z.erem (random_z ((size_rsa2048 / 8) + 16)) rsa2048

let gen_locked_value_opt () =
  try Some (gen_locked_value_unsafe ()) with _ -> None

(* Generates almost uniformly a Zarith element between 2 and [public key].
   Optional argument [rand] allows to use an unsafe function to generate
   randomness for benching. *)
let rec generate_z ?(rand = Hacl.Rand.gen) () =
  (* A random Z arith element of size [size] bytes *)
  let random_z size = rand size |> Bytes.to_string |> Z.of_bits in
  let res = Z.erem (random_z (size_rsa2048 + 16)) rsa2048 in
  if Z.(equal res zero) || Z.(equal res one) then generate_z () else res

(* The resulting prime has size 256 bits or slightly more. *)
let hash_to_prime ~time value key =
  let personalization = "\032" in
  let to_hash =
    String.concat
      "\xff\x00\xff\x00\xff\x00\xff\x00"
      (Int.to_string time :: List.map Z.to_bits [rsa2048; value; key])
  in
  let hash_result = blake ~key:personalization to_hash in
  (* Beware, the function nextprime gives a biased distribution,
     using it here is fine as the input is already uniformly distributed *)
  Z.(nextprime (of_bits (Bytes.to_string hash_result)))

(* Proof generation optimisation taken from page 3 of the following paper:
   https://crypto.stanford.edu/~dabo/pubs/papers/VDFsurvey.pdf page 3
   where g is the time-locked value.
*)
let prove_wesolowski ~time locked_value unlocked_value =
  let l = hash_to_prime ~time locked_value unlocked_value in
  let pi, r = Z.(ref one, ref one) in
  for _ = 1 to time do
    let two_r = Z.(!r lsl 1) in
    (* r <- 2*r mod l *)
    (r := Z.(two_r mod l)) ;
    let pi_sqr = Z.(!pi * !pi mod rsa2048) in
    (* pi <- pi^2 * locked_value^b where b = floor(2*r/l) in [0,1] *)
    pi := if two_r >= l then Z.(pi_sqr * locked_value) else pi_sqr
  done ;
  Z.(!pi mod rsa2048)

let prove ~time locked_value unlocked_value =
  let vdf_proof = prove_wesolowski ~time locked_value unlocked_value in
  let vdf_tuple = {locked_value; unlocked_value; vdf_proof} in
  {vdf_tuple; nonce = Z.one}

let verify_wesolowski ~time vdf_tuple =
  let l = hash_to_prime ~time vdf_tuple.locked_value vdf_tuple.unlocked_value in
  let r = Z.(powm (of_int 2) (Z.of_int time) l) in
  vdf_tuple.unlocked_value
  = Z.(
      powm vdf_tuple.vdf_proof l rsa2048
      * powm vdf_tuple.locked_value r rsa2048
      mod rsa2048)

let verify ~time locked_value proof =
  (* Verify link between precomputed tuple, challenge and evaluation *)
  let randomized_challenge =
    Z.powm proof.vdf_tuple.locked_value proof.nonce rsa2048
  in
  let b_exp = Z.(equal randomized_challenge locked_value) in
  (* Verify Wesolowski proof *)
  let b_weso = verify_wesolowski ~time proof.vdf_tuple in
  (* Return *)
  b_exp && b_weso

let rec unlock_timelock ~time locked_value =
  if time = 0 then locked_value
  else if locked_value = Z.zero then Z.zero
  else if locked_value = Z.one then Z.one
  else
    unlock_timelock
      ~time:Int.(pred time)
      Z.(locked_value * locked_value mod rsa2048)

(* Gives the value that was timelocked from the timelock, the public modulus
   and the time. Works in linear time in [time] *)
let unlock_and_prove ~time locked_value =
  let unlocked_value = unlock_timelock ~time locked_value in
  prove ~time locked_value unlocked_value

let precompute_timelock ?(locked_value = None) ?(precompute_path = None) ~time
    () =
  let locked_value =
    match locked_value with
    | None -> generate_z ()
    | Some c ->
        let c_mod = Z.(c mod rsa2048) in
        assert (Z.compare c_mod Z.one = 1) ;
        c_mod
  in

  let compute_tuple () =
    let unlocked_value = unlock_timelock ~time locked_value in
    (prove ~time locked_value unlocked_value).vdf_tuple
  in
  match precompute_path with
  | None -> compute_tuple ()
  | Some filepath ->
      let brsa = Z.to_bits rsa2048 in
      let file_prefix = blake brsa |> Hex.of_bytes |> Hex.show in
      let filename = file_prefix ^ "_" ^ string_of_int time ^ ".json" in
      let file_exists = Sys.file_exists (add_path filepath filename) in
      if file_exists then read_enc filepath filename vdf_tuple_encoding
      else
        let precomputed = compute_tuple () in
        write_enc filepath filename vdf_tuple_encoding precomputed ;
        precomputed

(* Optional argument [rand] allows to use an unsafe function to generate
   randomness for benching. *)
let proof_of_vdf_tuple_aux ?rand ~time vdf_tuple =
  if Z.compare vdf_tuple.locked_value Z.one < 1 then
    raise (Invalid_argument "Timelock puzzle is smaller than 1.") ;
  if Z.compare vdf_tuple.unlocked_value Z.zero < 1 then
    raise (Invalid_argument "Timelock solution is smaller than 0.") ;
  if Z.compare vdf_tuple.vdf_proof Z.zero < 1 then
    raise (Invalid_argument "Timelock proof is smaller than 0.") ;
  if
    Z.compare vdf_tuple.locked_value rsa2048 > 0
    || Z.compare vdf_tuple.unlocked_value rsa2048 > 0
    || Z.compare vdf_tuple.vdf_proof rsa2048 > -1
  then
    raise
      (Invalid_argument
         "Invalid timelock tuple, its elements are not in the RSA group.") ;
  if verify_wesolowski ~time vdf_tuple then
    let nonce = generate_z ?rand () in
    let randomized_locked_value = Z.powm vdf_tuple.locked_value nonce rsa2048 in
    let proof = {vdf_tuple; nonce} in
    (randomized_locked_value, proof)
  else raise (Invalid_argument "Timelock tuple verification failed.")

let proof_of_vdf_tuple ~time vdf_tuple = proof_of_vdf_tuple_aux ~time vdf_tuple

(* Creates a symmetric key using hash based key derivation from the time locked value*)
let timelock_proof_to_symmetric_key proof =
  let updated = Z.powm proof.vdf_tuple.unlocked_value proof.nonce rsa2048 in
  let kdf_key = "Tezoskdftimelockv1" in
  let hash = blake ~key:kdf_key (Z.to_string updated) in
  Crypto_box.Secretbox.unsafe_of_bytes hash

(* -------- Timelock high level functions (used in Tezos) -------- *)
type chest = {locked_value : locked_value; ciphertext : ciphertext}

let chest_encoding =
  let open Data_encoding in
  def "timelock.chest"
  @@ conv_with_guard
       (fun chest -> (chest.locked_value, chest.ciphertext))
       (fun (locked_value, ciphertext) ->
         if Z.Compare.(locked_value < Z.zero) then
           Error "locked value is not in the rsa group"
         else if Z.Compare.(locked_value <= Z.one) then
           Error "invalid locked_value"
         else if not @@ (Bytes.length ciphertext.payload > Crypto_box.tag_length)
         then Error "unexpected payload (smaller than expected tag length)"
         else Ok {locked_value; ciphertext})
       (obj2 (req "locked_value" n) (req "ciphertext" ciphertext_encoding))

type chest_key = timelock_proof

let chest_key_encoding = proof_encoding

type opening_result = Correct of Bytes.t | Bogus_opening

let create_chest_and_chest_key ?(precompute_path = None) ~payload ~time () =
  let locked_value, proof =
    if time <= 0 then
      raise
        (Invalid_argument
           "Timelock.create_chest_and_chest_key: the time bound must be \
            positive") ;
    let vdf_tuple = precompute_timelock ~time ~precompute_path () in
    proof_of_vdf_tuple ~time vdf_tuple
  in
  let sym_key = timelock_proof_to_symmetric_key proof in
  let ciphertext = encrypt sym_key payload in
  ({locked_value; ciphertext}, proof)

let create_chest_key chest ~time =
  if time <= 0 then
    raise
      (Invalid_argument
         "Timelock.create_chest_key: the time bound must be positive") ;

  unlock_and_prove ~time chest.locked_value

let get_plaintext_size chest =
  assert (Bytes.length chest.ciphertext.payload > Crypto_box.tag_length) ;
  Bytes.length chest.ciphertext.payload - Crypto_box.tag_length

let open_chest chest chest_key ~time =
  if time <= 0 then
    raise
      (Invalid_argument "Timelock.open_chest: the time bound must be positive")
  else if not @@ verify ~time chest.locked_value chest_key then Bogus_opening
  else
    let sym_key = timelock_proof_to_symmetric_key chest_key in
    match decrypt sym_key chest.ciphertext with
    | None -> Correct Bytes.empty
    | Some plaintext -> Correct plaintext

module Internal_for_tests = struct
  let rsa2048 = rsa2048

  let locked_value_to_z x = x

  let unlocked_value_to_z x = x

  let vdf_proof_to_z x = x

  let to_vdf_tuple_unsafe locked_value unlocked_value vdf_proof =
    {locked_value; unlocked_value; vdf_proof}

  let hash_to_prime = hash_to_prime

  let prove_wesolowski = prove_wesolowski

  let verify_wesolowski = verify_wesolowski
end

(* -------- Sampling functions for gas benchmarks -------- *)
(* Those function are unsafe for wallet usage as they use the OCaml
   random generator. This is used to easily reproduce benchmarks. *)

let gen_random_bytes_bench_unsafe ~rng_state size =
  Bytes.init size (fun _ -> Char.chr (Random.State.int rng_state 256))

let chest_sampler ~rng_state ~plaintext_size ~time =
  if time <= 0 then
    raise
      (Invalid_argument "Timelock.open_chest: the time bound must be positive") ;
  let plaintext = gen_random_bytes_bench_unsafe ~rng_state plaintext_size in
  (* [create_chest_and_chest_key] uses random not based on [rng_state] *)
  create_chest_and_chest_key ~payload:plaintext ~time ()
