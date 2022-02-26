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

(** Reference specification is version 2020.1.2
    https://github.com/zcash/zips/blob/master/protocol/sapling.pdf
 *)

(** Each instance of the Sapling protocol should be identified by a unique string
    identifier which is used as anti-replay. It should typically contain a chain
    identifier and the identifier of a smart-contract.
 **)

module type T_bytes = sig
  type t

  val to_bytes : t -> bytes

  val of_bytes_exn : bytes -> t
end

module type T_encoding = sig
  type t

  val encoding : t Data_encoding.t
end

module type T_encoding_bytes = sig
  include T_bytes

  include T_encoding with type t := t
end

module type T_encoding_compare = sig
  include T_encoding

  val compare : t -> t -> int
end

(** Spending keys allow to spend and derive viewing keys.
    It contains key material and information to derive deterministically more
    spending keys using the zip-32 standard (equivalent of bip-32 for ZCash).
    See spec section 3.1 **)
module type Spending_key = sig
  include T_encoding

  (** Outgoing viewing keys allow to keep track of spendings from a key.
      This feature is optional, a payer can decide to not keep track of its
      outgoing transactions. **)
  type ovk

  val of_bytes : bytes -> t option

  val to_bytes : t -> bytes

  (** Generate a key from a 32 byte uniformly random seed.
      The source of randomness should be of cryptographic quality. **)
  val of_seed : bytes -> t

  (** [derive_key k n] derives the nth key from [k] **)
  val derive_key : t -> int32 -> t

  (** Returns the next available index from a key **)
  val child_index : t -> int32

  val b58check_encoding : t Base58.encoding
end

(** Viewing keys allow to see incoming and outgoing transactions without
    giving the ability to spend. Stored in the zip-32 format. **)
module type Viewing_key = sig
  type spending_key

  include T_encoding

  val of_bytes : bytes -> t option

  val to_bytes : t -> bytes

  val of_sk : spending_key -> t

  (** Indexes to derive addresses. Starting from [default_index],
      the following addresses can be derived with [index_succ] **)
  type index

  val compare_index : index -> index -> int

  val default_index : index

  val index_succ : index -> index

  val index_to_int64 : index -> int64

  val index_of_int64 : int64 -> index

  val index_encoding : index Data_encoding.t

  (** To be given to the payer. Does not appear on the blockchain.
      Different addresses can be derived from one key which is necessary to
      prevent a loss of anonymity because of several payers colluding.
      e.g. two payers can conclude that they are paying the same person
      if they send money to the same address. **)
  type address

  val address_encoding : address Data_encoding.t

  val address_b58check_encoding : address Base58.encoding

  (** Returns a new address and a new index to be used for the next address
      generation. **)
  val new_address : t -> index -> index * address

  (** A dummy address can be generated to create dummy inputs or outputs of
      value 0. Note that tokens sent to dummy addresses are lost! **)
  val dummy_address : unit -> address

  (** Incoming viewing keys can be used to see transactions received to any
      address generated from this key.
      This feature is not enforced by the protocol, a payer could spend tokens
      on-chain and transmit to the recipient invalid information. The recipient
      should always check the received transactions with its ivk.
   **)
  type ivk

  val to_ivk : t -> ivk

  type ovk

  val ovk_of_xfvk : t -> ovk
end

module type Wallet = sig
  module Spending_key : Spending_key

  module Viewing_key : Viewing_key with type spending_key := Spending_key.t
end

module type Hash = sig
  (** Pedersen's Commitment of a transaction output (i.e. address and value).
      A merkle tree is formed with the existing commitment and filled with
      a default uncommitted value **)
  type commitment

  include T_encoding_compare

  include T_bytes with type t := t

  val uncommitted : height:int -> t

  (** Hash function to compute the merkle tree (Pedersen's hash on JubJub curve).
      Height is the height we are hashing at in the merkle tree. **)
  val merkle_hash : height:int -> t -> t -> t

  (** Hashes and commitments are the same object but are given different types
      to avoid confusing nodes and leaves. **)
  val of_commitment : commitment -> t

  val to_commitment : t -> commitment
end

module type UTXO = sig
  (** Ciphertexts are encrypted information used to retrieve payments and can
      be decrypted with full keys or just ovk.
      They can be stored on-chain or transmitted off-chain from payer to
      recipient. **)
  type ciphertext

  (** A commitment is the equivalent of a transaction output, with the important
      difference of not leaking any information. Must be stored on chain. **)
  type commitment

  (** Commitment value. The value of a transaction output is committed
      (Pedersen's commitment) separately. **)
  type cv

  type hash

  (** Nullifiers are used to invalidated a commitment, that is marking it as
      spent. However they can't be linked to the commitment that they invalidate.
      Nullifiers are derived from a commitment and a secret key using a pseudo
      random function. **)
  type nullifier

  (** Randomised signature keys.
      All inputs are signed with a randomised version of a secret key. **)
  type rk

  (** Zero knowledge proofs needed to spend a transaction outputs.
      See spec section 4.15 **)
  type spend_proof

  (** Signature needed to spend tokens. Computed with a randomised version of
      the secret key and verifies under a randomised version of the public key.
      Signs a hash of an input concatenated with an optional anti-replay string.
   **)
  type spend_sig

  (** Zero-knowledge proof needed to create money.
      See spec section 4.15 **)
  type output_proof

  (** Contains the necessary information to spend tokens
      (except the root which we include in the transaction). **)
  type input = {
    cv : cv;
    nf : nullifier;
    rk : rk;
    proof_i : spend_proof;
    signature : spend_sig;
  }

  val input_encoding : input Data_encoding.t

  (** Contains the necessary information to create tokens. **)
  type output = {
    cm : commitment;
    proof_o : output_proof;
    ciphertext : ciphertext;
  }

  val output_encoding : output Data_encoding.t

  (** Ties a transaction to a balance (difference between the tokens created
      and spent).
      Proves with the commitment values that sum of values of inputs minus
      sums of values of output equals balance. **)
  type binding_sig

  val binding_sig_encoding : binding_sig Data_encoding.t

  (** Transaction that is sent to a verifier.
      The root corresponds to a merkle tree where the inputs are present.
      Even if this root can in principle be very old, a verifier may
      keep only the last n known roots considering anything older as invalid.
      [bound_data] is arbitrary data that gets signed by the Sapling keys and
      can typically be used to connect the Sapling protocol to another one.
      For example it can contain the recipient address of an unshield operation.
      The memo_size field is checked at encoding and encoding to be the real
      memo size of all outputs.
      A transaction leaks the balance between inputs and outputs and the number
      of inputs and outputs. Note that the number of inputs is limited to 5208
      and number of outputs to 2019, by a check in the encoding.
      This is important to avoid invalidating a
      proof over the balance as described in section 4.12 of the spec. *)
  type transaction = {
    inputs : input list;
    outputs : output list;
    binding_sig : binding_sig;
    balance : int64;
    root : hash;
    bound_data : string;
  }

  (** The encoding enforces the limits on number of inputs and outputs. *)
  val transaction_encoding : transaction Data_encoding.t

  (** Maximum amount of shielded tokens. This value is imposed by
      librustzcash. **)
  val max_amount : int64

  val valid_amount : int64 -> bool

  module Legacy : sig
    type transaction_new = transaction

    (* This type is for backward compatibility with a previous definition of
       [transaction] which didn't have any [bound_data]. *)
    type transaction = {
      inputs : input list;
      outputs : output list;
      binding_sig : binding_sig;
      balance : int64;
      root : hash;
    }

    val transaction_encoding : transaction Data_encoding.t

    val cast : transaction -> transaction_new
  end
end

(** Regroups what needs to be exposed to a Validator **)
module type Validator = sig
  (** Loads the ZCash parameters for Groth16, searching them in:
      - [/usr/share/zcash-params]
      - [${OPAM_SWITCH_PREFIX}/share/zcash-params]
      - [${HOME}/.zcash-params]
      Only Sapling's parameters are loaded, not Sprout's.

     This function must be called before any of the proving and verification
     functions requiring a context.
     Usually you should not need to call this function directly as it is done
     by the fist call to `with_\{proving,verification\}_ctx`.
     However you can call this function in order to:
     - pay its cost upfront and have more predictable latency later
     - make sure that the parameters are present in the system and avoid a
       failure later. *)
  val init_params : unit -> unit

  module Ciphertext : sig
    include T_encoding

    val get_memo_size : t -> int
  end

  module Commitment : sig
    include T_encoding_bytes

    val valid_position : int64 -> bool
  end

  module CV : T_encoding

  module Hash : Hash with type commitment := Commitment.t

  module Nullifier : T_encoding_compare

  module UTXO :
    UTXO
      with type ciphertext := Ciphertext.t
       and type commitment := Commitment.t
       and type cv := CV.t
       and type hash := Hash.t
       and type nullifier := Nullifier.t

  module Verification : sig
    (** A verification context.
        Stores information involving the commitment values of inputs and
        outputs and it is used to verify the binding_sig.
        A context should created with [init], passed to [check_spend] for all
        inputs and [check_output] for all outputs, passed to [final_check] and
        finally freed with [free]. **)
    type t

    val with_verification_ctx : (t -> 'a) -> 'a

    (** Checks the zero-knowledge proof for spending money, the spend_sig,
        and stores info in the context. String is the anti-replay. **)
    val check_spend : t -> UTXO.input -> Hash.t -> string -> bool

    (** Checks the zero-knowledge proof for creating money
        and stores information in the context **)
    val check_output : t -> UTXO.output -> bool

    (** Utilise the context to check the balance. String is the anti-replay
        string. **)
    val final_check : t -> UTXO.transaction -> string -> bool
  end
end

(** A Diffie-Hellman key exchange is done between the payer and the recipient
    to encrypt the ciphertext. This can be done off or on chain. **)
module type DH_esk = sig
  type esk

  (** Generate a random scalar to be used by the sender for DH. **)
  val esk_random : unit -> esk
end

module type Commitment = sig
  (** Randomness used for the commitment. **)
  type rcm

  type viewing_key_address

  include T_encoding_bytes

  val compute : viewing_key_address -> amount:int64 -> rcm -> t

  val valid_position : int64 -> bool
end

module type CV = sig
  include T_encoding

  val of_bytes : bytes -> t option
end

(** See spec section 3.8 **)
module type Nullifier = sig
  type rcm

  type viewing_key_address

  type viewing_key

  include T_encoding_compare

  val compute :
    viewing_key_address ->
    viewing_key ->
    amount:int64 ->
    rcm ->
    position:int64 ->
    t
end

module type Rcm = sig
  include T_encoding

  val random : unit -> t

  val assert_valid : t -> unit
end

(** Signatures for Client/Validator/Storage + some functions used in tests *)
module type Client = sig
  include Wallet

  module Rcm : Rcm

  module Nullifier :
    Nullifier
      with type rcm := Rcm.t
       and type viewing_key_address := Viewing_key.address
       and type viewing_key := Viewing_key.t

  module Commitment :
    Commitment
      with type rcm := Rcm.t
       and type viewing_key_address := Viewing_key.address

  module CV : CV

  include
    Validator
      with module Commitment := Commitment
       and module CV := CV
       and module Nullifier := Nullifier

  module DH : DH_esk

  module Proving : sig
    (** A proving context.
        Stores information about the commitment values and is used
        for the binding_sig.
        A context should created with [init], passed to [spend_proof] for all
        inputs and [output_proof] for all outputs, passed to [make_binding_sig]
        and finally freed with [free]. **)
    type t

    (** Randomness to randomise the signature key. **)
    type ar

    val with_proving_ctx : (t -> 'a) -> 'a

    val ar_random : unit -> ar

    (** String is the anti-replay. **)
    val spend_sig :
      Spending_key.t ->
      ar ->
      CV.t ->
      Nullifier.t ->
      UTXO.rk ->
      UTXO.spend_proof ->
      string ->
      UTXO.spend_sig

    val spend_proof :
      t ->
      Viewing_key.t ->
      Spending_key.t ->
      Viewing_key.address ->
      Rcm.t ->
      ar ->
      amount:int64 ->
      root:Hash.t ->
      witness:Bytes.t ->
      CV.t * UTXO.rk * UTXO.spend_proof

    val output_proof :
      t ->
      DH.esk ->
      Viewing_key.address ->
      Rcm.t ->
      amount:int64 ->
      CV.t * UTXO.output_proof

    (** See spec section 4.12 **)
    val make_binding_sig :
      t ->
      UTXO.input list ->
      UTXO.output list ->
      balance:int64 ->
      bound_data:string ->
      string ->
      UTXO.binding_sig
  end

  module Forge : sig
    module Input : sig
      (** Necessary infos to create spend some money belonging to our key. **)
      type t = {
        rcm : Rcm.t;
        pos : int64;
        amount : int64;
        address : Viewing_key.address;
      }

      include T_encoding with type t := t

      val compare : t -> t -> int

      (** Decrypt a ciphertext to create a input. The decryption can fail if the
          description key is incorrect or the ciphertext is incorrect.
          The returned bytes are a memo added by the sender **)
      val of_ciphertext :
        pos:int64 -> Ciphertext.t -> Viewing_key.t -> (Bytes.t * t) option

      (** Same as of_ciphertext but requires only the ovk. **)
      val of_ciphertext_out :
        pos:int64 ->
        Ciphertext.t ->
        Spending_key.ovk ->
        Commitment.t ->
        (Bytes.t * t) option

      (** Check that a commitment corresponds to an input **)
      val check_cm : t -> Commitment.t -> bool
    end

    (** Necessary information to create tokens. **)
    module Output : sig
      type t = {address : Viewing_key.address; amount : int64; memo : Bytes.t}

      (** Creates a ciphertext that the recipient can use to spend his tokens,
          and our ovk can see as spent tokens. **)
      val to_ciphertext :
        t ->
        CV.t ->
        Viewing_key.t ->
        Rcm.t ->
        DH.esk ->
        Ciphertext.t * Commitment.t

      (** Same as to_ciphertext but does not allow the ovk to decrypt (ie.
          [of_ciphertext_out] will return None). **)
      val to_ciphertext_without_ovk :
        t -> Rcm.t -> DH.esk -> CV.t -> Ciphertext.t * Commitment.t
    end
  end
end
