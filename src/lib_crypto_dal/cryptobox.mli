(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Cryptobox_intf

(** {1 Cryptography for the Data Availability Layer}

    The Data Availability Layer (DAL) reduces the storage strain on the
    blockchain by only storing on-chain constant-size cryptographic
    {!type:commitment}s to arbitrary data blobs called {!type:slot}s.
    The slots themselves are stored off-chain and are made available by the DAL.

    A slot is encoded with some redundancy using a so-called MDS (Maximum
    Distance Separable) code. The resulting encoded slot is partitioned into
    {!type:shard}s, allowing retrieval of the slot with any subset of
    [{!field:parameters.number_of_shards}/{!field:parameters.redundancy_factor}]
    out of [{!field:parameters.number_of_shards}] shards. By doing so,
    we can guarantee high data availability provided a certain fraction of
    the DAL nodes is storing and supplying the data. This fraction can be
    made as small as desired at the expense of a higher data redundancy
    {!field:parameters.redundancy_factor}. MDS codes have no unnecessary
    redundancy.

    One can verify in constant time that the correct shard was retrieved
    using a constant-sized
    {{: https://www.iacr.org/archive/asiacrypt2010/6477178/6477178.pdf}KZG proof}
    {!type:shard_proof} (see function [verifyEval] in section 3.3) and the
    slot commitment.

    A {!type:slot} is partioned into
    [{!field:parameters.slot_size}/{!field:parameters.page_size}] segments
    called {!type:Verifier.page}s of size {!field:parameters.page_size}.
    One can also verify in constant time that the correct page
    was retrieved using a KZG proof {!type:page_proof} and the slot commitment.

    A challenge is to keep the proving time for the {!type:shard_proof}s
    almost proportional to the length [n] of the slot encoded with the MDS
    code: we've chosen and implemented a technique to produce the proofs in
    time [O(n log n)]
    (see {{: https://eprint.iacr.org/2023/033.pdf}Fast amortized KZG proofs}). *)

(** Initial values for the parameters of the DAL cryptographic primitives.
    It used to build a value of type [t]. *)
type parameters = Dal_config.parameters = {
  redundancy_factor : int;
  page_size : int;
  slot_size : int;
  number_of_shards : int;
}

(** Encapsulates parameters required to use the cryptographic primitives
    exported by this module. A value of type [t] contains both initial
    [parameters] and computed values depending on it. *)
type t

(** Because of the shell/protocol separation, cryptographic primitives
   need to be splitted. An interface, called the {!module:Verifier}
   aims to be provided for the economic protocol. The other interface,
   called the [Builder] is for the shell.

    A [Verifier], as hinted by the name, mainly needs to check
   proofs:

    1. A proof that a commitment is valid

    2. A proof that a page is valid

   A technicality is that the economic protocol is able to configure
   those cryptographic primitives via several constants.  Also, an SRS
   (aka trusted setup) is required.

   It is the responsibility of the shell and the protocol to ensure
   that both the [Verifier] and the [Builder] are instantiated with the
   same parameters and use the same trusted setup. *)

type commitment

type commitment_proof

type page_proof

type ('a, 'b) error_container = {given : 'a; expected : 'b}

open Error_monad

(** [Failed_to_load_trusted_setup], thrown by {!Config.init_dal}. *)
type error += Failed_to_load_trusted_setup of string

(** [Invalid_precomputation_hash], thrown by {!load_precompute_shards_proofs}. *)
type error += Invalid_precomputation_hash of (string, string) error_container

module Verifier :
  VERIFIER
    with type t = t
     and type commitment = commitment
     and type commitment_proof = commitment_proof
     and type page_proof = page_proof
     and type ('a, 'b) error_container = ('a, 'b) error_container

include
  VERIFIER
    with type t := t
     and type parameters := Dal_config.parameters
     and type commitment := commitment
     and type commitment_proof := commitment_proof
     and type page_proof := page_proof
     and type ('a, 'b) error_container := ('a, 'b) error_container

(** The primitives exposed in this modules require some
   preprocessing. This preprocessing generates data from an unknown
   secret. For the security of those primitives, it is important that
   the secret is unknown. *)
type initialisation_parameters

module Commitment : sig
  include COMMITMENT with type t = commitment

  val rpc_arg : commitment Resto.Arg.t
end

(** A slot is a byte sequence corresponding to some data. *)
type slot = bytes

(** The finited field used by the polynomial. *)
type scalar

(** A polynomial is another representation for a slot. One advantage
     of this representation is that a commitment can be computed from
     a polynomial. A commitment has nice properties:

      1. A commitment ensures that the size of the [slot] has a
     bounded size (typically [slot_size]).

      2. A commitment can be used to verify that a page of fixed size
      (typically [page_size]) is part of the original slot. *)
type polynomial

(** [polynomial_degree polynomial] returns the degree of the
     polynomial. *)
val polynomial_degree : polynomial -> int

(** [polynomial_evaluate polynomial x] evaluates [polynomial(x)]. *)
val polynomial_evaluate : polynomial -> scalar -> scalar

(** [polynomial_from_slot t slot] returns a polynomial from the a slot [slot].

    Requires:
    - [Bytes.length slot] is the slot size declared in [t].

    Ensures:
    - For any [slot] satisfying [Bytes.length slot] is equal to the
      declared slot size of [t],
      [polynomial_to_slot (polynomial_from_slot slot) = slot].

    Fails with [`Slot_wrong_size] when the slot size is not equal to
    the value slot size declared in [t].

    Note:
    - [polynomial_from_slot] is injective. *)
val polynomial_from_slot :
  t -> slot -> (polynomial, [> `Slot_wrong_size of string]) Result.t

(** [polynomial_to_slot t polynomial] returns a slot from a [polynomial].

    Ensures:
    - For any [slot] satisfying [Bytes.length slot = parameters.slot_size],
      [polynomial_to_slot (polynomial_from_slot slot) = slot]. *)
val polynomial_to_slot : t -> polynomial -> slot

(** [commit t polynomial] returns the commitment associated to a
     polynomial [p].

      Fails with [`Invalid_degree_strictly_less_than_expected _]
      if the degree of [p] exceeds the SRS size.

      Fails with [`Prover_SRS_not_loaded] if the prover’s SRS is not loaded
      (ie: [init_dal_verifier] has been used to load the SRS). *)
val commit :
  t ->
  polynomial ->
  ( commitment,
    [> `Invalid_degree_strictly_less_than_expected of (int, int) error_container
    | `Prover_SRS_not_loaded ] )
  Result.t

(** [pp_commit_error fmt error] pretty-prints the error returned by {!val:commit}. *)
val pp_commit_error :
  Format.formatter ->
  [< `Invalid_degree_strictly_less_than_expected of (int, int) error_container
  | `Prover_SRS_not_loaded ] ->
  unit

(** [string_of_commit_error error] returns an error string message for [error]. *)
val string_of_commit_error :
  [< `Invalid_degree_strictly_less_than_expected of (int, int) error_container
  | `Prover_SRS_not_loaded ] ->
  string

(** A portion of the data represented by a polynomial. *)
type share

(** Encoding of a share. *)
val share_encoding : share Data_encoding.t

(** A shard is share with its index (see
     {!val:shards_from_polynomial}). *)
type shard = {index : int; share : share}

(** An encoding of a share. *)
val shard_encoding : shard Data_encoding.t

(** [encoded_share_size t] returns the size of a share in byte depending on [t] *)
val encoded_share_size : t -> int

(** [polynomial_from_shards t shards] computes the original polynomial
    from [shards]. The proportion of shards needed is [1] over
    [redundancy_factor] the total number of shards declared in [t].

    Requires:
    - [Seq.length shards >= number_of_shards / redundancy_factor]
    (where [number_of_shards] and [redundancy_factor] are found in [t])
.

    Ensures:
    - For any [p], let [shards = shards_from_polynomial p],
    for any subset S of shards of [polynomial_length / shard_length] elements,
    [polynomial_from_shards S = p].
    Here, [polynomial_length] and [shard_length] are parameters declared in [t].

    Fails with:
    - [Error (`Not_enough_shards msg)] if there aren't at least
    [number_of_shards / redundancy_factor] shards (where these two parameters are found in [t])
    - [Error (`Shard_index_out_of_range msg)] if one shard index is not within the
    range [0, number_of_shards - 1] (where [number_of_shards] is declared in [t]).
    - [Error (`Invalid_shard_length msg)] if one shard is not of the expected length. *)
val polynomial_from_shards :
  t ->
  shard Seq.t ->
  ( polynomial,
    [> `Not_enough_shards of string
    | `Shard_index_out_of_range of string
    | `Invalid_shard_length of string ] )
  result

(** [shards_from_polynomial t polynomial] computes all the shards
    encoding the original [polynomial].

    Ensures:
    - For any [p], let [shards = shards_from_polynomial p],
    for any subset S of shards of [polynomial_length / shard_length] elements,
    [polynomial_from_shards S = p].
    Here, [polynomial_length] and [shard_length] are parameters declared in [t].
    The shards in the returned sequence have increasing indexes. *)
val shards_from_polynomial : t -> polynomial -> shard Seq.t

(** A proof that a shard belongs to some commitment. *)
type shard_proof

(** An encoding of a shard proof. *)
val shard_proof_encoding : shard_proof Data_encoding.t

(** [verify_shard t commitment shard proof] returns [Ok ()]
    if [shard] is an element of [shards_from_polynomial p] where
    [commitment = commit t p] for some polynomial [p].

    The verification time is constant.

    Requires:
    - The SRS (structured reference string) contained in [t]
    should be the same as the one used to produce the [commitment]
    and [proof].

    Fails with:
    - [Error `Invalid_shard] if the verification fails
    - [Error `Invalid_degree_strictly_less_than_expected _] if the
    SRS contained in [t] is too small to proceed with the verification
    - [Error `Shard_length_mismatch] if the shard is not of the expected
    length [shard_length] given for the initialisation of [t]
    - [Error (`Shard_index_out_of_range msg)] if the shard index
    is not within the range [0, number_of_shards - 1]
    (where [number_of_shards] is found in [t]).

    Ensures:
    - [verify_shard t commitment shard proof = Ok ()] if
    and only if
    [Array.mem shard (shards_from_polynomial t polynomial]),
    [precomputation = precompute_shards_proofs t],
    [proof = (prove_shards t ~precomputation ~polynomial).(shard.index)],
    and [commitment = commit t p]. *)
val verify_shard :
  t ->
  commitment ->
  shard ->
  shard_proof ->
  ( unit,
    [> `Invalid_degree_strictly_less_than_expected of (int, int) error_container
    | `Invalid_shard
    | `Shard_length_mismatch
    | `Shard_index_out_of_range of string ] )
  Result.t

(**
    Batched version of verify_shard, for better verifier performance.
    [verify_shard_multi t commitment shard_list proof_list]
    returns [Ok ()]
    if for all i List.nth i [shard] is an element of [shards_from_polynomial p]
    where [commitment = commit t p] for some polynomial [p],
    and the proofs are correctly generated.

    The verification time smaller than calling List.lenght shard_list
    times the verify_shard function.

    Requires:
    - The SRS (structured reference string) contained in [t]
    should be the same as the one used to produce the [commitment]
    and [proof].

    Fails with:
    - [Error `Invalid_shard] if the verification fails
    - [Error `Invalid_degree_strictly_less_than_expected _] if the
    SRS contained in [t] is too small to proceed with the verification
    - [Error `Shard_length_mismatch] if one of the shard is not of the expected
    length [shard_length] given for the initialisation of [t]
    - [Error (`Shard_index_out_of_range msg)] if one of the shard index
    is not within the range [0, number_of_shards - 1]
    (where [number_of_shards] is found in [t]).

    Ensures:
    - [verify_shard_multi t commitment shard_list proof_list = Ok ()] if
    and only if
    [Array.mem (List.nth i shard_list) (shards_from_polynomial t polynomial]),
    [precomputation = precompute_shards_proofs t],
    [List.nth i proof_list = (prove_shards t ~precomputation ~polynomial).
    (List. nth i (shard_list.index))], for all i
    and [commitment = commit t p]. *)
val verify_shard_multi :
  t ->
  commitment ->
  shard list ->
  shard_proof list ->
  ( unit,
    [> `Invalid_degree_strictly_less_than_expected of (int, int) error_container
    | `Invalid_shard
    | `Shard_length_mismatch
    | `Shard_index_out_of_range of string ] )
  Result.t

(** [prove_commitment t polynomial] produces a proof that the slot represented
    by [polynomial] has its size bounded by [slot_size] declared in [t].

    Fails with:
    - [Error `Invalid_degree_strictly_less_than_expected _] if the SRS
    contained in [t] is too small to produce the proof
    - [Error `Prover_SRS_not_loaded] if the prover’s SRS is not loaded
    (ie: [init_dal_verifier] has been used to load the SRS). *)
val prove_commitment :
  t ->
  polynomial ->
  ( commitment_proof,
    [> `Invalid_degree_strictly_less_than_expected of (int, int) error_container
    | `Prover_SRS_not_loaded ] )
  Result.t

(** [prove_page t polynomial n] produces a proof for the [n]-th page of
    the [slot] such that [polynomial = polynomial_from_slot t slot].
    This proof can be used to verify given a commitment to a slot that
    a byte sequence is indeed the [n]-th page of the slot
    (see Ensures section below).

    Fails with:
    - [Error `Invalid_degree_strictly_less_than_expected _] if the SRS
    contained in [t] is too small to produce the proof
    - [Error (`Page_index_out_of_range msg)] if the page index
    is not within the range [0, slot_size/page_size - 1]
    (where [slot_size] and [page_size] are found in [t]).
    - [Error `Prover_SRS_not_loaded] if the SRS has been loaded with
    [init_dal_verifier].

    Ensures:
    - [verify_page t commitment ~page_index page page_proof = Ok ()] if
    and only if
    [page = Bytes.sub slot (page_index * t.page_size) t.page_size]),
    [page_proof = prove_page t polynomial page_index],
    [p = polynomial_from_slot t slot],
    and [commitment = commit t p]. *)
val prove_page :
  t ->
  polynomial ->
  int ->
  ( page_proof,
    [> `Invalid_degree_strictly_less_than_expected of (int, int) error_container
    | `Page_index_out_of_range
    | `Prover_SRS_not_loaded ] )
  Result.t

(** The precomputation used to produce shard proofs. *)
type shards_proofs_precomputation

val shards_proofs_precomputation_encoding :
  shards_proofs_precomputation Data_encoding.t

(** [precomputation_shard_proofs t] returns the precomputation used to
   produce shard proofs. *)
val precompute_shards_proofs : t -> shards_proofs_precomputation

(** [save_precompute_shards_proofs precomputation ~filename] saves the
   given [precomputation] to disk with the given [filename]. *)
val save_precompute_shards_proofs :
  shards_proofs_precomputation ->
  filename:string ->
  unit Error_monad.tzresult Lwt.t

(** [load_precompute_shards_proofs ~hash ~filename ()] loads the precomputation
    from disk from the given [filename]. If [hash] is not [None], an integrity
    check of the retrieved precomputation is performed.

    Returns the error {!type:Invalid_precomputation_hash} if the integrity check fails. *)
val load_precompute_shards_proofs :
  hash:Tezos_crypto.Blake2B.t option ->
  filename:string ->
  unit ->
  shards_proofs_precomputation Error_monad.tzresult Lwt.t

(** [hash_precomputation precomputation] returns the {!Tezos_crypto.Blake2B.t}
    hash of the {!Data_encoding.t} value of [precomputation].

    @raises a {!Data_encoding.Binary.Write_error} if [precomputation] can't be
    serialized to a value {!val:shards_proofs_precomputation_encoding}. *)
val hash_precomputation : shards_proofs_precomputation -> Tezos_crypto.Blake2B.t

(** [prove_shards t ~precomputation ~polynomial] produces
   [number_of_shards] proofs [(π_0, ..., π_{number_of_shards - 1})] for the elements
   of [polynomial_from_shards polynomial] (where [number_of_shards]
   is declared in [t]) using the [precomputation].

   Requires:
   - [polynomial = polynomial_from_slot t s] for some slot [s] and the
   same value [t] used in [prove_shards]. Since the caller of [prove_shards]
   knows [polynomial], it is its responsibility to enforce this requirement.
   - [precomputation = precompute_shards_proofs t] with the same value [t]
   used in [prove_shards]. There is no way for this function to check that
   the [precomputation] is correct since it doesn't compute it.

   Ensures:
   - [verify_shard t commitment shard proof = Ok ()] if
   and only if
   [Array.mem shard (shards_from_polynomial t polynomial])
   [proof = (prove_shards t polynomial).(shard.index)],
   and [commitment = commit t polynomial]. *)
val prove_shards :
  t ->
  precomputation:shards_proofs_precomputation ->
  polynomial:polynomial ->
  shard_proof array

module Internal_for_tests : sig
  (** The initialisation parameters can be too large for testing
     purposes. This function creates an unsafe initialisation
     parameters using default parameters designed to handle test cases. *)
  val init_prover_dal : unit -> unit

  (** This function creates an unsafe initialisation parameters for the
      verifier using default parameters designed to handle test cases. *)
  val init_verifier_dal : unit -> unit

  (** This function loads in memory the default verifier SRS. The
      difference with [init_verifier_dal] is that the latter loads a
      SRS that should be only used for tests. *)
  val init_verifier_dal_default : unit -> unit

  (** Returns a randomized valid sequence of shards using the random state
     [state] for the given parameters. *)
  val make_dummy_shards : t -> state:Random.State.t -> shard Seq.t

  (** [polynomials_equal p1 p2] returns true if and only if [p1] and [p2]
     represent the same polynomial. *)
  val polynomials_equal : polynomial -> polynomial -> bool

  (** [page_proof_equal proof1 proof2] returns true if and only if [proof1]
     and [proof2] represent the same proof. *)
  val page_proof_equal : page_proof -> page_proof -> bool

  (** [alter_page_proof page_proof] returns a different page proof than the
     input. *)
  val alter_page_proof : page_proof -> page_proof

  (** [alter_shard_proof shard_proof] returns a different shard proof than
     the input. *)
  val alter_shard_proof : shard_proof -> shard_proof

  (** [alter_commitment_proof commitment_proof] returns a different commitment
     proof than the input. *)
  val alter_commitment_proof : commitment_proof -> commitment_proof

  (** [minimum_number_of_shards_to_reconstruct_slot t] returns the minimum
     number of shards to reconstruct a slot using [polynomial_from_shards]. *)
  val minimum_number_of_shards_to_reconstruct_slot : t -> int

  val dummy_commitment : state:Random.State.t -> unit -> commitment

  val dummy_page_proof : state:Random.State.t -> unit -> page_proof

  val dummy_shard_proof : state:Random.State.t -> unit -> shard_proof

  val make_dummy_shard :
    state:Random.State.t -> index:int -> length:int -> shard

  val number_of_pages : t -> int

  val shard_length : t -> int

  val dummy_polynomial : state:Random.State.t -> degree:int -> polynomial

  val srs_size_g1 : t -> int

  (** [select_fft_domain domain_size] selects a suitable domain for the FFT.

     The domain size [domain_size] is expected to be strictly positive.
     Return [(size, power_of_two, remainder)] such that:
     * If [domain_size > 1], then [size] is the smallest integer greater or
     equal to [domain_size] and is of the form 2^a * 3^b * 11^c * 19^d,
     where:
     {v
     a ∈ ⟦0, 32⟧, b ∈ {0, 1}, c ∈ {0, 1}, d ∈ {0, 1}
     v}
     * If [domain_size = 1], then [size = 2].
     * [size = power_of_two * remainder], [power_of_two] is a power of two,
     and [remainder] is not divisible by 2. *)
  val select_fft_domain : int -> int * int * int

  val precomputation_equal :
    shards_proofs_precomputation -> shards_proofs_precomputation -> bool

  val encoded_share_size : t -> int

  (** [ensure_validity parameters] returns true if the [parameters] are valid.
     See implementation file for details. *)
  val ensure_validity : parameters -> bool

  (** Same as [ensure_validity parameters], except that it returns an error if the
      [parameters] aren't valid and doesn't check the SRS. *)
  val ensure_validity_without_srs :
    parameters -> (unit, [> `Fail of string]) result

  val slot_as_polynomial_length : slot_size:int -> page_size:int -> int
end

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4380

   This configuration module is currently used by each process that
   needs to initialize DAL. Given that in the default case [init_dal]
   may take several seconds, it would be better to call this function
   only once. *)

(** node parameters for the DAL. *)
module Config : sig
  type t = Dal_config.t = {
    activated : bool;
    use_mock_srs_for_testing : parameters option;
    bootstrap_peers : string list;
  }

  val encoding : t Data_encoding.t

  val default : t

  (** [init_dal find_trusted_setup_files ?(srs_size_log2=21) config] initializes the
     DAL according to the dal configuration [config], a function to find the SRS
     files [find_trusted_setup_files] and the optional log2 of the SRS size
     [srs_size_log2].

      When [config.use_mock_srs_for_testing = None],
     [init_dal] loads [initialisation_parameters] from the files at the
     paths provided by [find_trusted_setup_files ()]. It is important that
     every time the primitives above are used, they are used with the very
     same initialization parameters. (To ensure this property, an integrity
     check is run.) In this case, [init_dal] can take several seconds
     to run. *)
  val init_verifier_dal : t -> unit Error_monad.tzresult Lwt.t

  val init_prover_dal :
    find_srs_files:(unit -> (string * string) Error_monad.tzresult) ->
    ?srs_size_log2:int ->
    t ->
    unit Error_monad.tzresult Lwt.t
end
