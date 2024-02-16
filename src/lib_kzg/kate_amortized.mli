open Bls

type public_parameters = {
  max_polynomial_length : int;
  shard_length : int;
  srs_g1 : Srs_g1.t;
  number_of_shards : int;
}

type preprocess

val preprocess_encoding : preprocess t

type shard_proof = Commitment.Single_G1.t

val preprocess_equal : preprocess -> preprocess -> bool

val commit : public_parameters -> Poly.t -> shard_proof

val preprocess_multiple_multi_reveals : public_parameters -> preprocess

val multiple_multi_reveals :
  public_parameters ->
  preprocess:preprocess ->
  coefficients:scalar array ->
  shard_proof array

val verify :
  public_parameters ->
  commitment:shard_proof ->
  srs_point:G2.t ->
  domain:Domain.t ->
  root:scalar ->
  evaluations:scalar array ->
  proof:shard_proof ->
  bool
