(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Testing
    -------
    Component:    Crypto
    Invocation:   dune build @src/lib_crypto/runtest
    Subject:      On Publicly Verifiable Secret Sharing [Schoenmakers, 1999]
*)

(* We reshadow the List module with Stdlib's because there are many safe uses of
   double-list traversors *)
module List = Stdlib.List
module Pvss = Pvss_secp256k1
module Sp = Secp256k1_group

module Setup : sig
  val shares : Pvss.Encrypted_share.t list

  val commitments : Pvss.Commitment.t list

  val proof : Pvss.proof

  val secret_scalar : Sp.Group.Scalar.t

  val secret : Pvss.Secret_key.t

  val public_secret : Pvss.Public_key.t

  val other_shares : Pvss.Encrypted_share.t list

  val other_commitments : Pvss.Commitment.t list

  val other_proof : Pvss.proof

  val other_secret : Pvss.Secret_key.t

  type keypair = {
    secret_key : Pvss.Secret_key.t;
    public_key : Pvss.Public_key.t;
  }

  val public_keys : Pvss.Public_key.t list

  val keypairs : keypair list

  val random_keypairs : int -> keypair list

  val reveals :
    (Pvss.Encrypted_share.t * (Pvss.Clear_share.t * Pvss.proof)) list

  val convert_encoding : 'a Data_encoding.t -> 'b Data_encoding.t -> 'a -> 'b

  val group_encoding : Sp.Group.t Data_encoding.t
end = struct
  type keypair = {
    secret_key : Pvss.Secret_key.t;
    public_key : Pvss.Public_key.t;
  }

  let group_encoding =
    Data_encoding.(conv Sp.Group.to_bits Sp.Group.of_bits_exn string)

  let scalar_encoding =
    Data_encoding.(
      conv Sp.Group.Scalar.to_bits Sp.Group.Scalar.of_bits_exn string)

  let convert_encoding de1 de2 x =
    Data_encoding.Binary.of_bytes_exn
      de2
      (Data_encoding.Binary.to_bytes_exn de1 x)

  (** Random value of Z in the range [0,2^256] *)
  let rand_Z () =
    [Random.int64 Int64.max_int |> Z.of_int64 |> Z.to_bits]
    |> Blake2B.hash_string |> Blake2B.to_string |> Z.of_bits

  (** Generates n random keypairs *)
  let random_keypairs n =
    List.init n (fun _ ->
        let s = Sp.Group.Scalar.of_Z (rand_Z ()) in
        let secret_key =
          convert_encoding scalar_encoding Pvss.Secret_key.encoding s
        in
        {secret_key; public_key = Pvss.Secret_key.to_public_key secret_key})

  let threshold = 5

  let n = 8

  let random_scalar () = Sp.Group.Scalar.of_Z (rand_Z ())

  let secret_of_scalar s =
    convert_encoding scalar_encoding Pvss.Secret_key.encoding s

  let secret_scalar = random_scalar ()

  let secret = secret_of_scalar secret_scalar

  let public_secret = Pvss.Secret_key.to_public_key secret

  let other_secret = secret_of_scalar (random_scalar ())

  let keypairs = random_keypairs n

  let public_keys = List.map (fun {public_key; _} -> public_key) keypairs

  let ( (shares, commitments, proof),
        (other_shares, other_commitments, other_proof) ) =
    ( Pvss.dealer_shares_and_proof ~secret ~threshold ~public_keys,
      Pvss.dealer_shares_and_proof ~secret:other_secret ~threshold ~public_keys
    )

  let reveals =
    List.map2
      (fun share keypair ->
        ( share,
          Pvss.reveal_share
            share
            ~secret_key:keypair.secret_key
            ~public_key:keypair.public_key ))
      shares
      keypairs
end

(** Checks the dealer's proof of validity of encrypted shares. *)
let test_dealer_proof () =
  let shr = (Setup.shares, Setup.other_shares)
  and cmt = (Setup.commitments, Setup.other_commitments)
  and prf = (Setup.proof, Setup.other_proof) in
  for i = 0 to 1 do
    for j = 0 to 1 do
      for k = 0 to 1 do
        let pick = function 0 -> fst | _ -> snd in
        assert (
          Pvss.check_dealer_proof
            (pick i shr)
            (pick j cmt)
            ~proof:(pick k prf)
            ~public_keys:Setup.public_keys
          = (i = j && j = k))
      done
    done
  done

module Proof = struct
  module G = Sp.Group

  type exponent = G.Scalar.t

  type proof = exponent * exponent list

  type t = proof

  let encoding = Data_encoding.(tup2 G.Scalar.encoding (list G.Scalar.encoding))

  let mangle : t -> t = fun (e, es) -> (G.Scalar.(mul e e), es)
end

(** A dealer's proof which is meant to be invalid by falsifying it
    with [mangle].
*)
let test_invalid_dealer_proof () =
  let proof : Proof.t =
    Setup.convert_encoding Pvss.proof_encoding Proof.encoding Setup.proof
  in
  let mangled =
    Proof.mangle proof
    |> Setup.convert_encoding Proof.encoding Pvss.proof_encoding
  in
  assert (
    not
      (Pvss.check_dealer_proof
         Setup.shares
         Setup.commitments
         ~proof:mangled
         ~public_keys:Setup.public_keys))

(** Checks revealing shares, i.e. each participant honestly decrypts
    its share.
*)
let test_share_reveal () =
  let shares_valid =
    List.map2
      (fun (share, (reveal, proof)) public_key ->
        Pvss.check_revealed_share share reveal ~public_key proof)
      Setup.reveals
      Setup.public_keys
  in
  List.iteri
    (fun i b ->
      print_endline (string_of_int i) ;
      assert b)
    shares_valid

module Encrypted_share = struct
  include Sp.Group

  let mangle : t -> t = fun share -> Sp.Group.(share + share)
end

(** A dishonestly-revealed share can be checked. *)
let test_invalid_share_reveal () =
  let mangle_share : Pvss.Encrypted_share.t -> Pvss.Encrypted_share.t =
   fun share ->
    let share : Encrypted_share.t =
      Setup.convert_encoding
        Pvss.Encrypted_share.encoding
        Encrypted_share.encoding
        share
    in
    Encrypted_share.mangle share
    |> Setup.convert_encoding
         Encrypted_share.encoding
         Pvss.Encrypted_share.encoding
  in
  (* check invalid reveal shares *)
  let shares_valid =
    List.map2
      (fun (share, (reveal, proof)) public_key ->
        let share = mangle_share share in
        Pvss.check_revealed_share share reveal ~public_key proof)
      Setup.reveals
      Setup.public_keys
  in
  List.iteri
    (fun i b ->
      print_endline (string_of_int i) ;
      assert (not b))
    shares_valid

(** Reconstruct the shared secret. *)
let test_reconstruct () =
  let indices = [0; 1; 2; 3; 4] in
  let reconstructed =
    Pvss.reconstruct
      (List.map
         (fun n ->
           let _, (r, _) = List.nth Setup.reveals n in
           r)
         indices)
      indices
  in
  assert (
    Sp.Group.(( = ))
      (Setup.convert_encoding
         Pvss.Public_key.encoding
         Setup.group_encoding
         reconstructed)
      (Setup.convert_encoding
         Pvss.Public_key.encoding
         Setup.group_encoding
         Setup.public_secret))

(** Try to reconstruct with n < threshold. *)
let test_invalid_reconstruct () =
  let indices = [0; 1; 2; 3] in
  let reconstructed =
    Pvss.reconstruct
      (List.map
         (fun n ->
           let _, (r, _) = List.nth Setup.reveals n in
           r)
         indices)
      indices
  in
  assert (
    Setup.convert_encoding
      Pvss.Public_key.encoding
      Setup.group_encoding
      reconstructed
    != Setup.convert_encoding
         Pvss.Public_key.encoding
         Setup.group_encoding
         Setup.public_secret)

(** Test:
    This test covers a scenario of using PVSS for randomness generation in the
    Tezos protocol given in 3 steps.
*)
let test_randomness_commitment_protocol () =
  (* 1st step: cycle `n - 2` *)
  let threshold = 3 in
  (* 30 bakers *)
  let bakers = Setup.random_keypairs 30 in
  (* 10 endorsers *)
  let endorsers = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let endorsers_public_keys =
    List.map
      (fun endorser -> (List.nth bakers endorser).Setup.public_key)
      endorsers
  in
  (* Client: A baker creates a randomness commitment *)
  let secret_nonce, public_nonce =
    Setup.random_keypairs 1 |> List.hd |> fun Setup.{secret_key; public_key} ->
    (secret_key, public_key)
  in
  (* Client: A baker creates shares for block endorsers, a list of commitments
     of length equal to the threshold and a proof *)
  let shares, commitments, proof =
    Pvss.dealer_shares_and_proof
      ~secret:secret_nonce
      ~threshold
      ~public_keys:endorsers_public_keys
  in
  (* Protocol: The protocol verifies the shares and commitments with the proof,
     the number of commitments and stores them together with the public nonce. *)
  assert (
    Pvss.check_dealer_proof
      shares
      commitments
      ~proof
      ~public_keys:endorsers_public_keys) ;
  assert (Compare.List_length_with.(commitments = threshold)) ;
  (* 2nd step: first half of cycle `n - 1` *)
  (* Protocol: The revealed nonce, if any, is checked by converting it to public
     key and comparing it with the secret nonce. *)
  let revealed_nonce = Pvss.Secret_key.to_public_key secret_nonce in
  assert (Pvss.Public_key.(public_nonce = revealed_nonce)) ;
  (* 3rd step: second half of cycle `n - 1` *)
  let revealed_shares = [1; 4; 8] in
  assert (Compare.List_length_with.(revealed_shares >= threshold)) ;
  let clear_shares =
    List.map
      (fun index ->
        let encrypted_share = List.nth shares index in
        let Setup.{secret_key; public_key} = List.nth bakers index in
        (* Client: Endorsers may reveal their shares *)
        let clear_share, proof =
          Pvss.reveal_share encrypted_share ~secret_key ~public_key
        in
        (* Protocol: The revealed shares are verified with the proof *)
        assert (
          Pvss.check_revealed_share
            encrypted_share
            clear_share
            ~public_key
            proof) ;
        clear_share)
      revealed_shares
  in
  (* Protocol: The protocol may reconstruct the nonce from the revealed shares *)
  let reconstructed_nonce = Pvss.reconstruct clear_shares revealed_shares in
  assert (Pvss.Public_key.(public_nonce = reconstructed_nonce))

let tests =
  [
    ( "pvss",
      [
        ("dealer proof", `Quick, test_dealer_proof);
        ("invalid dealer proof", `Quick, test_invalid_dealer_proof);
        ("reveal", `Quick, test_share_reveal);
        ("invalid reveal", `Quick, test_invalid_share_reveal);
        ("reconstruct", `Quick, test_reconstruct);
        ("invalid reconstruct", `Quick, test_invalid_reconstruct);
        ( "randomness commitment protocol",
          `Quick,
          test_randomness_commitment_protocol );
      ] );
  ]
