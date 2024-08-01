open Bls

(** Public_parameters are needed for commit, preprocess, prove & verify
    Notably the type is the same for both prover & verifier, but the needed SRS
    size is shorter for the verifier (= max_polynomial_length - shard_length)
    than for the prover (= max_polynomial_length) *)
type public_parameters = {
  max_polynomial_length : int;
  shard_length : int;
  srs_g1 : Srs_g1.t;
  number_of_shards : int;
}

(** The preprocess is the SRS under a certain form. It is a necessary step for
    proving *)
type preprocess

val preprocess_encoding : preprocess t

type shard_proof = Commitment.Single_G1.t

type commitment = Commitment.Single_G1.t

(** Comparison function for preprocess, used for test purposes *)
val preprocess_equal : preprocess -> preprocess -> bool

val commit : public_parameters -> Poly.t -> commitment

val preprocess_multiple_multi_reveals : public_parameters -> preprocess

val multiple_multi_reveals :
  public_parameters ->
  preprocess:preprocess ->
  coefficients:scalar array ->
  shard_proof array

val verify :
  public_parameters ->
  commitment:commitment ->
  srs_point:G2.t ->
  domain:Domain.t ->
  root:scalar ->
  evaluations:scalar array ->
  proof:shard_proof ->
  bool

(** Verifies shard proofs in a batch : compared to using verify several times,
    this reduces the time spend to commit (only one commit for all proofs)
    and the time in pairing (only one pairing for all proofs). *)
val verify_multi :
  public_parameters ->
  commitment:commitment ->
  srs_point:G2.t ->
  domain:Domain.t ->
  root_list:scalar list ->
  evaluations_list:scalar array list ->
  proof_list:shard_proof list ->
  bool
