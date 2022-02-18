(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** This module implements all the core functionalities. It contains also the low
    level Rustzcash type equalities and should be used in its Raw form only for
    testing.
    For all other uses refer to its Client or Validator interfaces. *)
module Raw = struct
  module R = Rustzcash

  let init_params = R.init_params

  module Spending_key = struct
    (** Authorisation spending key: secret key used to sign once randomized. *)
    type ask = R.ask

    (** Nullifier secret key. Used to compute nullifier. *)
    type nsk = R.nsk

    type ovk = R.ovk

    (* 96 bytes *)
    type expanded_spending_key = R.expanded_spending_key = {
      ask : ask;
      nsk : nsk;
      ovk : ovk;
    }

    let _to_expanded_spending_key = R.to_expanded_spending_key

    let expsk_encoding =
      let open Data_encoding in
      def "sapling.wallet.expanded_spending_key"
      @@ conv
           (fun t -> (t.ask, t.nsk, t.ovk))
           (fun (ask, nsk, ovk) -> {ask; nsk; ovk})
           (obj3
              (req "ask" (conv R.of_ask R.to_ask (Fixed.bytes 32)))
              (req "nsk" (conv R.of_nsk R.to_nsk (Fixed.bytes 32)))
              (req "ovk" (conv R.of_ovk R.to_ovk (Fixed.bytes 32))))

    (** Type t contains ask, nsk, ovk and zip-32 related info. *)
    type t = R.zip32_expanded_spending_key = {
      depth : Bytes.t;
      parent_fvk_tag : Bytes.t;
      child_index : Bytes.t;
      chain_code : Bytes.t;
      expsk : expanded_spending_key;
      dk : Bytes.t;
    }

    let of_bytes b = Option.catch (fun () -> R.to_zip32_expanded_spending_key b)

    let to_bytes = R.of_zip32_expanded_spending_key

    let encoding =
      let open Data_encoding in
      def "sapling.wallet.spending_key"
      @@ conv
           (fun t ->
             ( t.depth,
               t.parent_fvk_tag,
               t.child_index,
               t.chain_code,
               t.expsk,
               t.dk ))
           (fun (depth, parent_fvk_tag, child_index, chain_code, expsk, dk) ->
             {depth; parent_fvk_tag; child_index; chain_code; expsk; dk})
           (obj6
              (req "depth" (Fixed.bytes 1))
              (req "parent_fvk_tag" (Fixed.bytes 4))
              (req "child_index" (Fixed.bytes 4))
              (req "chain_code" (Fixed.bytes 32))
              (req "expsk" expsk_encoding)
              (req "dk" (Fixed.bytes 32)))

    type Base58.data += Data of t

    let b58check_encoding =
      let to_raw sk = Bytes.to_string @@ to_bytes sk in
      let of_raw str = of_bytes (Bytes.of_string str) in
      Base58.register_encoding
        ~prefix:Base58.Prefix.sapling_spending_key
        ~length:169
        ~to_raw
        ~of_raw
        ~wrap:(fun x -> Data x)

    let () = Base58.check_encoded_prefix b58check_encoding "sask" 241

    let of_seed = R.zip32_xsk_master

    let derive_key = R.zip32_xsk_derive

    let child_index sk = Bytes.get_int32_be sk.child_index 0
  end

  module Viewing_key = struct
    (** Public signature key.
        Note: if this key is exposed there is privacy loss! Only randomised
        version of it (rk) are published. *)
    type ak = R.ak

    (** Public nullifier key.
        Note: if this key is exposed there is privacy loss! Only randomised
        version of it (rk) are published. *)
    type nk = R.nk

    type ivk = R.ivk

    type pkd = R.pkd

    type ovk = R.ovk

    (** Used to create an address from a viewing key. *)
    type diversifier = R.diversifier

    let diversifier_encoding =
      let open Data_encoding in
      def "sapling.wallet.diversifier"
      @@ conv
           R.of_diversifier
           (fun b ->
             match R.to_diversifier b with
             | Some diversifier -> diversifier
             | None -> raise (Invalid_argument "diversifier_encoding: decoding"))
           (Fixed.bytes 11)

    (** Full viewing key contains ak, nsk, ovk *)
    type full_viewing_key = R.full_viewing_key = {ak : ak; nk : nk; ovk : ovk}

    let fvk_encoding =
      let open Data_encoding in
      def "sapling.wallet.full_viewing_key"
      @@ conv
           (fun t -> (R.of_ak t.ak, R.of_nk t.nk, R.of_ovk t.ovk))
           (fun (ak, nk, ovk) ->
             {ak = R.to_ak ak; nk = R.to_nk nk; ovk = R.to_ovk ovk})
           (obj3
              (req "ak" (Fixed.bytes 32))
              (req "nk" (Fixed.bytes 32))
              (req "ovk" (Fixed.bytes 32)))

    let of_expsk expsk =
      {
        ak = R.ask_to_ak Spending_key.(expsk.ask);
        nk = R.nsk_to_nk Spending_key.(expsk.nsk);
        ovk = expsk.ovk;
      }

    (** Type t additionally contains zip-32 related info *)
    type t = R.zip32_full_viewing_key = {
      depth : Bytes.t;
      parent_fvk_tag : Bytes.t;
      child_index : Bytes.t;
      chain_code : Bytes.t;
      fvk : full_viewing_key;
      dk : Bytes.t;
    }

    let ovk_of_xfvk xfvk = xfvk.fvk.ovk

    let encoding : t Data_encoding.t =
      let open Data_encoding in
      def "sapling.wallet.viewing_key"
      @@ conv
           (fun t ->
             ( t.depth,
               t.parent_fvk_tag,
               t.child_index,
               t.chain_code,
               t.fvk,
               t.dk ))
           (fun (depth, parent_fvk_tag, child_index, chain_code, fvk, dk) ->
             {depth; parent_fvk_tag; child_index; chain_code; fvk; dk})
           (obj6
              (req "depth" (Fixed.bytes 1))
              (req "parent_fvk_tag" (Fixed.bytes 4))
              (req "child_index" (Fixed.bytes 4))
              (req "chain_code" (Fixed.bytes 32))
              (req "expsk" fvk_encoding)
              (req "dk" (Fixed.bytes 32)))

    let to_bytes = R.of_zip32_full_viewing_key

    let of_bytes b = Option.catch (fun () -> R.to_zip32_full_viewing_key b)

    let of_sk (sk : Spending_key.t) =
      Spending_key.
        {
          depth = sk.depth;
          parent_fvk_tag = sk.parent_fvk_tag;
          child_index = sk.child_index;
          chain_code = sk.chain_code;
          fvk = of_expsk sk.expsk;
          dk = sk.dk;
        }

    (* diversifier_index is 11 bytes long but we treat is as 8 byte long int64 *)
    type index = R.diversifier_index

    let compare_index = R.compare_diversifier_index

    (* partial function to convert an 11 byte index to a 8 byte int64 *)
    let index_to_int64 idx =
      let b = R.of_diversifier_index idx in
      assert (
        Bytes.get b 8 = '\000'
        && Bytes.get b 9 = '\000'
        && Bytes.get b 10 = '\000') ;
      Bytes.get_int64_le b 0

    (* int64 is padded back to 11 bytes *)
    let index_of_int64 i =
      let b = Bytes.make 11 '\000' in
      Bytes.set_int64_le b 0 i ;
      R.to_diversifier_index b

    let index_encoding =
      let open Data_encoding in
      def "sapling.transaction.diversifier_index"
      @@ conv index_to_int64 index_of_int64 int64

    let default_index = index_of_int64 0L

    let index_succ b = index_to_int64 b |> Int64.succ |> index_of_int64

    type address = {diversifier : diversifier; pkd : pkd}

    let address_encoding =
      let open Data_encoding in
      def "sapling.wallet.address"
      @@ conv
           (fun i -> (i.diversifier, R.of_pkd i.pkd))
           (fun (diversifier, pkd) -> {diversifier; pkd = R.to_pkd pkd})
           (obj2
              (req "diversifier" diversifier_encoding)
              (req "pkd" (Fixed.bytes 32)))

    type Base58.data += Data of address

    let address_b58check_encoding =
      let to_raw address =
        Data_encoding.Binary.to_string_exn address_encoding address
      in
      let of_raw str =
        Data_encoding.Binary.of_string_opt address_encoding str
      in
      Base58.register_encoding
        ~prefix:Base58.Prefix.sapling_address
        ~length:43
        ~to_raw
        ~of_raw
        ~wrap:(fun x -> Data x)

    let () = Base58.check_encoded_prefix address_b58check_encoding "zet1" 69

    let new_address vk j =
      match R.zip32_xfvk_address vk j with
      | None -> failwith "Exhausted available indices for the sapling key."
      | Some (i, diversifier, pkd) -> (i, {diversifier; pkd})

    let to_ivk xfvk = R.crh_ivk xfvk.fvk.ak xfvk.fvk.nk

    (* Creates an unspendable address since the ivk is generated at random *)
    let dummy_address () =
      (* NOTE: the density of valid diversifiers is roughly half. This loop is
         likely to be short. *)
      let rec random_diversifier () =
        match R.to_diversifier @@ Hacl.Rand.gen 11 with
        | Some diversifier -> diversifier
        | None -> random_diversifier ()
      in
      let diversifier = random_diversifier () in
      (* A random ivk is 32 bytes with the first 5 bits set to 0 (if in big
         endian). As ivk is encoded in little endian, we apply the mask on the
         last byte.
      *)
      let rand = Hacl.Rand.gen 32 in
      let mask = 0b00000111 in
      let int = Char.code @@ Bytes.get rand (32 - 1) in
      let int_masked = int land mask in
      Bytes.set rand (32 - 1) (Char.chr int_masked) ;
      let ivk = R.to_ivk rand in
      let pkd = R.ivk_to_pkd ivk diversifier in
      {diversifier; pkd}
  end

  (** See spec section 4.17 *)
  module DH = struct
    type esk = R.esk

    let esk_encoding =
      let open Data_encoding in
      def "sapling.DH.esk" @@ conv R.of_esk R.to_esk (Fixed.bytes 32)

    let esk_random () = R.to_esk @@ R.generate_r ()

    type epk = R.epk

    let epk_encoding =
      let open Data_encoding in
      def "sapling.DH.epk" @@ conv R.of_epk R.to_epk (Fixed.bytes 32)

    (** Derives the public part of the DH for the sender.
       (ie. epk is used for symkey_receiver). *)
    let derive_ephemeral address esk =
      R.ka_derivepublic Viewing_key.(address.diversifier) esk

    (* used to derive symmetric keys from the diffie hellman. *)
    let kdf_key = "KDFSaplingForTezosV1"

    (** Derives a symmetric key to be used to create the ciphertext on the
        sender side. *)
    let symkey_sender esk pkd =
      let symkey =
        Bytes.unsafe_to_string @@ R.of_symkey @@ R.ka_agree_sender pkd esk
      in
      let hash = Blake2B.(to_bytes @@ hash_string ~key:kdf_key [symkey]) in
      Crypto_box.Secretbox.unsafe_of_bytes hash

    let symkey_receiver epk ivk =
      let symkey =
        Bytes.unsafe_to_string @@ R.of_symkey @@ R.ka_agree_receiver epk ivk
      in
      let hash = Blake2B.(to_bytes @@ hash_string ~key:kdf_key [symkey]) in
      Crypto_box.Secretbox.unsafe_of_bytes hash

    let symkey_out ovk (cv, cm, epk) =
      let key = Bytes.of_string "OCK_keystringderivation_TEZOS" in
      let ock =
        Blake2B.(
          to_bytes
            (hash_bytes
               ~key
               [R.of_cv cv; R.of_commitment cm; R.of_epk epk; R.of_ovk ovk]))
      in
      Crypto_box.Secretbox.unsafe_of_bytes ock
  end

  module Rcm = struct
    type t = R.rcm

    let random () = R.to_rcm @@ R.generate_r ()

    let encoding =
      let open Data_encoding in
      def "sapling.transaction.rcm" @@ conv R.of_rcm R.to_rcm (Fixed.bytes 32)

    (* Check that rcm < r_j as specified by the spec.
       to_scalar reduces mod r_j but takes as input 64 bytes so concat rcm with
       0's and check to scalar (rcm) = rcm *)
    let assert_valid =
      let zeros = Bytes.make 32 '\000' in
      fun rcm ->
        assert (rcm = R.to_rcm (R.to_scalar (Bytes.cat (R.of_rcm rcm) zeros)))
  end

  module Commitment = struct
    type t = R.commitment

    let to_bytes = R.of_commitment

    let of_bytes_exn = R.to_commitment

    let encoding =
      let open Data_encoding in
      def "sapling.transaction.commitment"
      @@ conv R.of_commitment R.to_commitment (Fixed.bytes 32)

    let compute address ~amount rcm =
      let open Viewing_key in
      R.compute_cm address.diversifier address.pkd ~amount rcm

    let valid_position = R.valid_position
  end

  module Nullifier = struct
    type t = R.nullifier

    let encoding =
      let open Data_encoding in
      def "sapling.transaction.nullifier"
      @@ conv R.of_nullifier R.to_nullifier (Fixed.bytes 32)

    let compare nf1 nf2 =
      Bytes.compare (R.of_nullifier nf1) (R.of_nullifier nf2)

    let compute address xfvk ~amount rcm ~position =
      let open Viewing_key in
      let pkd = R.ivk_to_pkd (to_ivk xfvk) address.diversifier in
      R.compute_nf
        address.diversifier
        pkd
        ~amount
        rcm
        xfvk.fvk.ak
        xfvk.fvk.nk
        ~position
  end

  module CV = struct
    type t = R.cv

    let of_bytes b = Option.catch (fun () -> R.to_cv b)

    let encoding =
      let open Data_encoding in
      def "sapling.transaction.commitment_value"
      @@ conv R.of_cv R.to_cv (Fixed.bytes 32)
  end

  module Ciphertext = struct
    type t = {
      cv : CV.t;
      (* Public key to be used with symkey_receiver *)
      epk : DH.epk;
      (* authenticated encryption of diversifier, value, rcm, and an optional
         memo *)
      payload_enc : Bytes.t;
      (* nonce for the authenticated encryption of payload_enc *)
      nonce_enc : Crypto_box.nonce;
      (* authenticated encryption of pkd and esk,
         allows to recover the symkey with symkey_sender *)
      payload_out : Bytes.t;
      (* nonce for the authenticated encryption of payload_out *)
      nonce_out : Crypto_box.nonce;
    }

    let encoding =
      let open Data_encoding in
      let payload_out_size =
        Binary.(
          (WithExceptions.Option.get ~loc:__LOC__
          @@ fixed_length DH.esk_encoding)
          + (WithExceptions.Option.get ~loc:__LOC__
            @@ fixed_length DH.epk_encoding)
          + Crypto_box.tag_length)
      in
      def "sapling.transaction.ciphertext"
      @@ conv
           (fun o ->
             ( o.cv,
               o.epk,
               o.payload_enc,
               o.nonce_enc,
               o.payload_out,
               o.nonce_out ))
           (fun (cv, epk, payload_enc, nonce_enc, payload_out, nonce_out) ->
             {cv; epk; payload_enc; nonce_enc; payload_out; nonce_out})
           (obj6
              (req "cv" CV.encoding)
              (req "epk" DH.epk_encoding)
              (req "payload_enc" bytes)
              (req "nonce_enc" Crypto_box.nonce_encoding)
              (req "payload_out" (Fixed.bytes payload_out_size))
              (req "nonce_out" Crypto_box.nonce_encoding))

    type plaintext = {
      diversifier : Viewing_key.diversifier;
      amount : int64;
      rcm : Rcm.t;
      memo : Bytes.t;
    }

    let plaintext_encoding =
      let open Data_encoding in
      def "sapling.transaction.plaintext"
      @@ conv
           (fun o ->
             assert (R.valid_amount o.amount) ;
             (o.diversifier, o.amount, o.rcm, o.memo))
           (fun (diversifier, amount, rcm, memo) ->
             assert (R.valid_amount amount) ;
             {diversifier; amount; rcm; memo})
           (obj4
              (req "diversifier" Viewing_key.diversifier_encoding)
              (req "amount" int64)
              (req "rcm" Rcm.encoding)
              (req "memo" bytes))

    let get_memo_size ciphertext =
      let payload_size = Bytes.length ciphertext.payload_enc in
      (* Payload contains fixed length fields and a variable size memo.
         The fixed length things are diversifier, amount, rcm and a message
         authentication code and 4 bytes added by the encoding for the length
         of the variable length field memo. *)
      let size_besides_memo =
        let open Data_encoding in
        (WithExceptions.Option.get ~loc:__LOC__
        @@ Binary.fixed_length Viewing_key.diversifier_encoding)
        + (WithExceptions.Option.get ~loc:__LOC__ @@ Binary.fixed_length int64)
        + (WithExceptions.Option.get ~loc:__LOC__
          @@ Binary.fixed_length Rcm.encoding)
        + Crypto_box.tag_length + 4
      in
      payload_size - size_besides_memo

    let decompose_plaintext_out plaintext =
      assert (Bytes.length plaintext = 32 + 32) ;
      let pkd = Bytes.create 32 in
      let esk = Bytes.create 32 in
      let () = Bytes.blit plaintext 0 pkd 0 32 in
      let () = Bytes.blit plaintext 32 esk 0 32 in
      (R.to_pkd pkd, R.to_esk esk)

    let encrypt_aux key_agreed_out amount address rcm memo esk cv =
      let epk = DH.derive_ephemeral address esk in
      let nonce_enc = Crypto_box.random_nonce () in
      let payload_enc =
        let key_agreed_enc = DH.symkey_sender esk address.pkd in
        let plaintext_enc =
          Data_encoding.Binary.to_bytes_exn
            plaintext_encoding
            {diversifier = Viewing_key.(address.diversifier); amount; rcm; memo}
        in
        Crypto_box.Secretbox.secretbox key_agreed_enc plaintext_enc nonce_enc
      in
      let nonce_out = Crypto_box.random_nonce () in
      let payload_out =
        let plaintext_out =
          Bytes.cat (R.of_pkd Viewing_key.(address.pkd)) (R.of_esk esk)
        in
        Crypto_box.Secretbox.secretbox key_agreed_out plaintext_out nonce_out
      in
      {epk; payload_enc; nonce_enc; payload_out; nonce_out; cv}

    let encrypt amount address vk rcm memo (cv, cm, epk) esk =
      let key_agreed_out =
        DH.symkey_out Viewing_key.(vk.fvk.ovk) (cv, cm, epk)
      in
      encrypt_aux key_agreed_out amount address rcm memo esk cv

    (* useful for not revealing your outgoing transactions or creating dummy
       output *)
    let encrypt_without_ovk amount address rcm memo esk cv =
      let key_agreed_out =
        Crypto_box.Secretbox.unsafe_of_bytes @@ Hacl.Rand.gen 32
      in
      encrypt_aux key_agreed_out amount address rcm memo esk cv

    (* Since this is an authenticated encryption scheme, decrypt fails if the
       MAC does. This happens if the ciphertext was not intended to us, or was
       not correctly computed *)
    let decrypt ciphertext xfvk =
      let ivk = Viewing_key.to_ivk xfvk in
      let symkey = DH.symkey_receiver ciphertext.epk ivk in
      let ( >?? ) = Option.bind in
      Crypto_box.Secretbox.secretbox_open
        symkey
        ciphertext.payload_enc
        ciphertext.nonce_enc
      >?? fun plaintext ->
      let {diversifier; amount; rcm; memo} =
        Data_encoding.Binary.of_bytes_exn plaintext_encoding plaintext
      in
      let pkd = R.ivk_to_pkd ivk diversifier in
      Some (Viewing_key.{pkd; diversifier}, amount, rcm, memo)

    (* Get ciphertext with only the outgoing viewing key,
       can fail if we decided to encrypt with encrypt_without_ovk *)
    let decrypt_ovk ciphertext ovk (cm, epk) =
      (* symkey for payload_out *)
      let symkey = DH.symkey_out ovk (ciphertext.cv, cm, epk) in
      let ( >?? ) = Option.bind in
      Crypto_box.Secretbox.secretbox_open
        symkey
        ciphertext.payload_out
        ciphertext.nonce_out
      >?? fun plaintext ->
      let (pkd, esk) = decompose_plaintext_out plaintext in
      (* symkey for payload_enc *)
      let symkey = DH.symkey_sender esk pkd in
      Crypto_box.Secretbox.secretbox_open
        symkey
        ciphertext.payload_enc
        ciphertext.nonce_enc
      >?? fun plaintext ->
      let {diversifier; amount; rcm; memo} =
        Data_encoding.Binary.of_bytes_exn plaintext_encoding plaintext
      in
      Some (Viewing_key.{pkd; diversifier}, amount, rcm, memo)
  end

  module Hash = struct
    type t = R.hash

    let compare = R.hash_compare

    let encoding =
      let open Data_encoding in
      def "sapling.transaction.commitment_hash"
      @@ conv R.of_hash R.to_hash (Fixed.bytes 32)

    let merkle_hash = R.merkle_hash

    let uncommitted_hashes =
      lazy
        (let max_height = 32 in
         let res = Array.make (max_height + 1) R.tree_uncommitted in
         for height = 0 to max_height - 1 do
           let h = res.(height) in
           res.(height + 1) <- R.merkle_hash ~height h h
         done ;
         res)

    let uncommitted ~height = (Lazy.force uncommitted_hashes).(height)

    let of_bytes_exn = R.to_hash

    let to_bytes = R.of_hash

    let of_commitment = R.hash_of_commitment

    let to_commitment = R.commitment_of_hash
  end

  module UTXO = struct
    type rk = R.rk

    type spend_proof = R.spend_proof

    (** What gets signed. Has to be a hash of an input plus anti-replay string. *)
    type sighash = R.sighash

    (* Creates sighash for spend_sig *)
    let hash_input cv nf rk proof key_string =
      let key = Bytes.of_string key_string in
      let h =
        Blake2B.(
          to_bytes
            (hash_bytes
               ~key
               [
                 R.of_cv cv;
                 R.of_nullifier nf;
                 R.of_rk rk;
                 R.of_spend_proof proof;
               ]))
      in
      R.to_sighash h

    type spend_sig = R.spend_sig

    (* Spend description *)
    type input = {
      cv : CV.t;
      nf : Nullifier.t;
      rk : rk;
      proof_i : spend_proof;
      signature : spend_sig;
    }

    let input_encoding =
      let open Data_encoding in
      let open R in
      def "sapling.transaction.input" ~description:"Input of a transaction"
      @@ conv
           (fun i ->
             ( i.cv,
               i.nf,
               of_rk i.rk,
               of_spend_proof i.proof_i,
               of_spend_sig i.signature ))
           (fun (cv, nf, rk, proof_i, signature) ->
             {
               cv;
               nf;
               rk = to_rk rk;
               proof_i = to_spend_proof proof_i;
               signature = to_spend_sig signature;
             })
           (obj5
              (req "cv" CV.encoding)
              (req "nf" Nullifier.encoding)
              (req "rk" (Fixed.bytes 32))
              (req "proof_i" (Fixed.bytes (48 + 96 + 48)))
              (req "signature" (Fixed.bytes 64)))

    type output_proof = R.output_proof

    (* output description *)
    type output = {
      cm : Commitment.t;
      proof_o : output_proof;
      ciphertext : Ciphertext.t;
    }

    let output_encoding =
      let open Data_encoding in
      let open R in
      def "sapling.transaction.output" ~description:"Output of a transaction"
      @@ conv
           (fun o -> (o.cm, of_output_proof o.proof_o, o.ciphertext))
           (fun (cm, proof_o, ciphertext) ->
             {cm; proof_o = to_output_proof proof_o; ciphertext})
           (obj3
              (req "cm" Commitment.encoding)
              (req "proof_o" (Fixed.bytes (48 + 96 + 48)))
              (req "ciphertext" Ciphertext.encoding))

    type binding_sig = R.binding_sig

    let binding_sig_encoding =
      let open Data_encoding in
      def
        "sapling.transaction.binding_sig"
        ~description:"Binding signature of a transaction"
      @@ conv R.of_binding_sig R.to_binding_sig (Fixed.bytes 64)

    (* Create sighash for binding_sig *)
    let hash_transaction inputs outputs key =
      let input_bytes =
        List.map (Data_encoding.Binary.to_string_exn input_encoding) inputs
      in
      let output_bytes =
        List.map (Data_encoding.Binary.to_string_exn output_encoding) outputs
      in
      let h =
        Blake2B.(to_bytes (hash_string ~key (input_bytes @ output_bytes)))
      in
      R.to_sighash h

    type transaction = {
      inputs : input list;
      outputs : output list;
      binding_sig : binding_sig;
      balance : int64;
      root : Hash.t;
    }

    let transaction_encoding =
      let open Data_encoding in
      let check_memo_size outputs =
        let size =
          match outputs with
          | o :: _ -> Ciphertext.get_memo_size o.ciphertext
          | _ ->
              (* never actually used *)
              -1
        in
        List.iter
          (fun output ->
            assert (Ciphertext.get_memo_size output.ciphertext = size))
          outputs
      in
      def
        "sapling.transaction"
        ~description:
          "A Sapling transaction with inputs, outputs, balance, root and \
           binding sig."
      @@ conv
           (fun t ->
             check_memo_size t.outputs ;
             (t.inputs, t.outputs, t.binding_sig, t.balance, t.root))
           (fun (inputs, outputs, binding_sig, balance, root) ->
             check_memo_size outputs ;
             {inputs; outputs; binding_sig; balance; root})
           (obj5
              (req "inputs" (list ~max_length:5208 input_encoding))
              (req "outputs" (list ~max_length:2019 output_encoding))
              (req "binding_sig" binding_sig_encoding)
              (req "balance" int64)
              (req "root" Hash.encoding))

    let max_amount = R.max_amount

    let valid_amount = R.valid_amount
  end

  module Proving = struct
    type t = R.proving_ctx

    type ar = R.ar

    let with_proving_ctx = R.with_proving_ctx

    let ar_random () = R.to_ar @@ R.generate_r ()

    let spend_proof ctx xfvk xsp address rcm ar ~amount ~root ~witness =
      let open Viewing_key in
      R.spend_proof
        ctx
        xfvk.fvk.ak
        Spending_key.(xsp.expsk.nsk)
        address.diversifier
        rcm
        ar
        ~amount
        ~root
        ~witness

    let spend_sig xsp ar cv nf rk proof key_string =
      let sighash = UTXO.hash_input cv nf rk proof key_string in
      R.spend_sig Spending_key.(xsp.expsk.ask) ar sighash

    let output_proof ctx esk address rcm ~amount =
      R.output_proof
        ctx
        esk
        Viewing_key.(address.diversifier)
        address.pkd
        rcm
        ~amount

    let make_binding_sig ctx inputs outputs ~balance key =
      let sighash = UTXO.hash_transaction inputs outputs key in
      R.make_binding_sig ctx ~balance sighash
  end

  module Verification = struct
    (* A verifying ctx is an elliptic curve point in the Rust library *)
    type t = R.verification_ctx

    let with_verification_ctx = R.with_verification_ctx

    let check_spend ctx input root key =
      let open UTXO in
      (* Hash the transaction with the anti-replay string *)
      let sighash = hash_input input.cv input.nf input.rk input.proof_i key in
      (* Checks the signature on sighash under rk, the zero-knowledge proof
         and adds info into the context (multiply by cv). *)
      R.check_spend
        ctx
        input.cv
        root
        input.nf
        input.rk
        input.proof_i
        input.signature
        sighash

    let check_output ctx output =
      let open UTXO in
      (* Checks the zero-knowledge proof and add infos into the context
         (divides by cv). *)
      R.check_output
        ctx
        output.ciphertext.cv
        output.cm
        output.ciphertext.epk
        output.proof_o

    (* Checks that the binding_sig is a valid signature of sighash
       under ctx times (commitment of the balance with randomness zero) *)
    let final_check ctx transaction key =
      let open UTXO in
      let hash = hash_transaction transaction.inputs transaction.outputs key in
      R.final_check ctx transaction.balance transaction.binding_sig hash
  end

  module Forge = struct
    module Input = struct
      (* A Forge.Input.t allows to create an UTXO.Input.t
         assuming the address is derived from our key *)
      type t = {
        rcm : Rcm.t;
        pos : int64;
        amount : int64;
        address : Viewing_key.address;
      }

      let encoding =
        let open Data_encoding in
        def
          "sapling.forge.input"
          ~description:"Data to forge the input of a transaction"
        @@ conv
             (fun i ->
               assert (Commitment.valid_position i.pos) ;
               assert (UTXO.valid_amount i.amount) ;
               (i.rcm, i.pos, i.amount, i.address))
             (fun (rcm, pos, amount, address) ->
               assert (Commitment.valid_position pos) ;
               assert (UTXO.valid_amount amount) ;
               {rcm; pos; amount; address})
             (obj4
                (req "rcm" Rcm.encoding)
                (req "pos" int64)
                (req "amount" int64)
                (req "addr" Viewing_key.address_encoding))

      let compare fi1 fi2 =
        let v = Int64.compare fi1.amount fi2.amount in
        if v = 0 then Int64.compare fi1.pos fi2.pos else v

      (* Gets the information from the ciphertext provided the right viewing key *)
      let of_ciphertext ~pos cipher vk =
        Option.map
          (fun (address, amount, rcm, memo) ->
            (memo, {rcm; pos; amount; address}))
          (Ciphertext.decrypt cipher vk)

      (* Gets the information from the ciphertext provided the right outgoing viewing
         key and that the cipher is valid *)
      let of_ciphertext_out ~pos cipher ovk cm =
        let to_be_hashed = (cm, Ciphertext.(cipher.epk)) in
        Option.map
          (fun (address, amount, rcm, memo) ->
            (memo, {rcm; pos; amount; address}))
          (Ciphertext.decrypt_ovk cipher ovk to_be_hashed)

      (* Used to check that the decrypted information is correct by re-computing
         the cm *)
      let check_cm i existing_cm =
        Rcm.assert_valid i.rcm ;
        let computed_cm = Commitment.compute i.address ~amount:i.amount i.rcm in
        if existing_cm = computed_cm then true else false
    end

    module Output = struct
      (* Forge.Output.t allows to create a UTXO.Output.t *)
      type t = {address : Viewing_key.address; amount : int64; memo : Bytes.t}

      (* Prepare the ciphertext and the commitment *)
      let to_ciphertext o cv vk rcm esk =
        let cm = Commitment.compute o.address ~amount:o.amount rcm in
        let epk = DH.derive_ephemeral o.address esk in
        let to_be_hashed = (cv, cm, epk) in
        let ciphertext =
          Ciphertext.encrypt o.amount o.address vk rcm o.memo to_be_hashed esk
        in
        (ciphertext, cm)

      (* Prepare the ciphertext that won't decrypt under ovk and the commitment *)
      let to_ciphertext_without_ovk o rcm esk cv =
        let cm = Commitment.compute o.address ~amount:o.amount rcm in
        let ciphertext =
          Ciphertext.encrypt_without_ovk o.amount o.address rcm o.memo esk cv
        in
        (ciphertext, cm)
    end
  end
end

module Client : Core_sig.Client = Raw

module Validator :
  Core_sig.Validator
    with type Ciphertext.t = Client.Ciphertext.t
     and type Commitment.t = Client.Commitment.t
     and type CV.t = Client.CV.t
     and type Hash.t = Client.Hash.t
     and type Nullifier.t = Client.Nullifier.t
     and module UTXO = Client.UTXO =
  Client

module Wallet :
  Core_sig.Wallet
    with type Spending_key.t = Client.Spending_key.t
     and type Viewing_key.t = Client.Viewing_key.t
     and type Viewing_key.address = Client.Viewing_key.address =
  Client

let () =
  Data_encoding.Registration.register Raw.Ciphertext.encoding ;
  Data_encoding.Registration.register Raw.Ciphertext.plaintext_encoding ;
  Data_encoding.Registration.register Raw.Commitment.encoding ;
  Data_encoding.Registration.register Raw.CV.encoding ;
  Data_encoding.Registration.register Raw.Hash.encoding ;
  Data_encoding.Registration.register Raw.Nullifier.encoding ;
  Data_encoding.Registration.register Raw.Rcm.encoding ;
  Data_encoding.Registration.register Raw.Spending_key.encoding ;
  Data_encoding.Registration.register Raw.UTXO.binding_sig_encoding ;
  Data_encoding.Registration.register Raw.UTXO.input_encoding ;
  Data_encoding.Registration.register Raw.UTXO.output_encoding ;
  Data_encoding.Registration.register Raw.UTXO.transaction_encoding ;
  Data_encoding.Registration.register Raw.Viewing_key.address_encoding ;
  Data_encoding.Registration.register Raw.Viewing_key.index_encoding ;
  Data_encoding.Registration.register Raw.Viewing_key.encoding
