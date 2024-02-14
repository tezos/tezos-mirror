open Bls
module FFT = Utils.FFT
module Commitment = Commitment.Single_G1

type public_parameters = {
  max_polynomial_length : int;
  shard_length : int;
  srs_g1 : Srs_g1.t;
  number_of_shards : int;
}

type preprocess = Domain.t * G1_carray.t array

let preprocess_encoding : preprocess t =
  let open Data_encoding in
  tup2 Domain.encoding (array G1_carray.encoding)

(*Adding repr to inlcude the proof in the transcript*)
type shard_proof = Commitment.t [@@deriving repr]

type commitment = Commitment.t

(*Adding repr to include evaluations_list in the transcript*)
type eval_list = Scalar.t array list [@@deriving repr]

let commit t = Commitment.commit t.srs_g1

let preprocess_equal (d1, a1) (d2, a2) =
  Domain.equal d1 d2 && Array.for_all2 G1_carray.eq a1 a2

(* Notations
   =========

   [x]_1 (resp. [x]_2) is a shorthand for x.g where
   - [x] is a scalar element of type [Scalar.t]
   - [g] is a generator of the subgroup [Bls12_381.G1] (resp. [Bls12_381.G2])
   - ( . ) is the elliptic curve scalar multiplication in the subgroup
   [Bls12_381.G1] (resp. [Bls12_381.G2])

   The SRS (Structure Reference String) is defined as:
   ([1]_1, [τ]_1, [τ^2]_1, [τ^3]_1, ..., [τ^{Srs_g1.length t.srs.raw.srs_g1 - 1}]_1)
   where τ is secret.

   e : [Bls12_381.G1] * [Bls12_381.G2] → [Bls12_381.GT] is a pairing
   (bilinear, non-degenerate map such that e(g1, g2) = gT where
   G1=<g1>, G2=<g2>, GT=<gT>).

   Multi-reveals
   =============

   This feature is described in the KZG extended paper under section 3.4 as
   batch opening https://link.springer.com/chapter/10.1007/978-3-642-17373-8_11
   on arbitrary points. The paper https://eprint.iacr.org/2023/033 shows how
   to commit and verify quickly when the points form cosets of a group of
   roots of unity.

   For n dividing [Scalar.order - 1], let w be a primitive n-th root of unity.
   For l dividing n, let z=w^{n/l} be a primitive l-th root of unity and Z=<z>.

   For i=0, ..., n/l - 1, the proof of the evaluations of P(x) at the l points
   w^i Z is the KZG commitment to the quotient of the euclidean division of
   P(x) by the polynomial x^l - w^{i*l} whose only roots are w^i Z.
   In other words, given the euclidean division
   P(x)=(x^l-w^{i*l}) * q_i(x) + r_{i}(x), deg r(x) < l,
   the proof is π_i = [q_i(τ)]_1.
   Opening at one point corresponds to the case l=1 where r_{i}(x)=P(w^i).

   To verify the proof, we gather the alleged evaluations of P(x) at the points
   w^i Z. From these possibly correct evaluations, we can construct an alleged
   remainder r_{i}(x) by computing the inverse DFT on the domain w^i Z, as
   r_{i}(x)=P(x) on this domain, and as r_{i}(x) is determined by its
   evaluations at l distinct points. We then check
   e(c-[r_i(τ)]_1, g_2) ?= e(π, [τ^l]_2 - [w^{i*l}]_2).


   Multiple multi-reveals
   ======================

   We now wish to reveal not on the domain W=<w>, but on several subdomains:
   the n/l>1 cosets w^i W_0 of l elements each. The committed polynomial P(x)
   has degree k-1 where k corresponds to the dimension of the Reed-Solomon code
   (as a vector subspace of dimension k of F^n). We present the result from
   https://eprint.iacr.org/2023/033, which assumes the size of the domains n
   and of their cosets l to be powers of two for correct FFT sizes.

   Computing the proofs for all such cosets would cost n/l euclidean divisions
   and multi-exponentiations. Even though the euclidean division by
   x^l-w^{i*l} is linear in the degree of the committed polynomial,
   as well as the multi-exponentiation thanks to the Pippenger algorithm
   (See https://cr.yp.to/papers/pippenger.pdf), computing all proofs leads to
   a complexity O(n/l * k). It turns out the proofs for the cosets are related,
   so all proofs can be computed in time O(n/l log (n/l)).

   Again, for i=0, ..., n/l-1, given the euclidean division
   P(x)=(x^l-w^{i*l}) q_i(x) + r_i(x), deg r_i(x) < l, the proofs
   to be computed are π_i ≔ [q_i(τ)]_1.

   We denote d=deg P, m the next power of 2 of (d + 1), and set
   P_m, P_{m-1}, ..., P_{d+1}=0. For our purposes we further assume
   l | m, l < m so that z ≔ w^l a primitive n/l-th root of unity.

   The floor designates here the truncated division, where
   terms x^i for i<0 are dropped:

   q_i(x) = (P(x) - r_i(x)) / (x^l-w^{i*l})
          = floor((P(x)-r_i(x)) / (x^l-w^{i*l}))
          = floor(P(x)/(x^l-w^{i*l})) since deg r_i < l
          = floor(sum_{k=0}^infty P(x)/(x^{(k+1)*l}) w^{k*i*l} (formal power series of 1/(x^l+c))
          = sum_{k=0}^{m/l-1} floor(P(x)/(x^{(k+1)*l})) z^{i*k}.

   So:
   q_i(x) = sum_{k=0}^{m/l-1} (P_m x^{m-(k+1)*l} + P_{m-1}x^{m-(k+1)*l-1}
            + ... + P_{(k+1)*l+1}x + P_{(k+1)*l}) z^{ik}.

   If l <= d < 2l, then the powers of z are absent of the quotient:
   q_i(x) = P_l + P_{l+1} x + ... + P_d x^{d-l}.
   In this case, all proofs are equal since their value doesn't depend on [i].

   Thus,
   π_i = [q_i(x)]_1
        = sum_{k=0}^{m/l-1} (P_m[τ^{m-(k+1)*l}] + P_{m-1}[τ^{m-(k+1)*l-1}]
          + ... + P_{(k+1)*l+1}[τ] + P_{(k+1)*l}) z^{i*k}.

   Letting
   - for 0 <= k <= m/l, h_{k} ≔ sum_{j=k*l}^{m} f_j[τ^{j-k*l}]
   - for m/l < k <= n/l, h_k ≔ 0,
   we obtain π_i = sum_{k=0}^{n/l-1}  h_{k+1} z^{i*k}.

   So by definition π=(π_0, ..., π_{n/l-1}) is the
   EC-DFT_z of the vector (h_1, ..., h_{n/l}) in F^{n/l} ( * ).

   Now, let's address the computation of the coefficients of interest
   h_k for k=1, ..., n/l. To this end, the authors of
   https://eprint.iacr.org/2023/033 observe that the computation of the h_k's
   can be decomposed into the computation of the l "offset" sums:

   forall j=0, ...,l-1,
   h_{k,j} = P_{m-j}[τ^{m-k*l-j}] + P_{m-l-j}[τ^{m-(k+1)*l-j}]
   + ... + P_{(m-j) % l + kl}[τ^{(m-j) % l}].

   So the desired coefficients can then be obtained with
   h_k=sum_{j=0}^{l-1} h_{k,j}. This decomposition of the calculation
   allows the l vectors (h_{1,j}, ..., h_{floor((m-j)/l), j})
   for j=0, ..., l-1 to be computed with l Toeplitz matrix-vector multiplications:

   (h_{1,j} h_{2,j} ... h_{floor((m-j)/l) - 1, j} h_{floor((m-j)/l), j})^T
   =
   |P_{m-j} P_{m-l-j} P_{m-2*l-j} ...   P_{(m-j)%l+2*l}  P_{(m-j)%l+l}    |
   |0       P_{m-j}   P_{m-l-j}   ...   P_{(m-j)%l+3*l}  P_{(m-j)%l+2*l}  |
   |0       0         P_{m-j}     ...   P_{(m-j)%l+4*l}  P_{(m-j)%l+3*l}  |
   |.       .         .    .           .                 .                |
   |.       .         .       .        .                 .                |
   |.       .         .          .     .                 .                |
   |......................................................................|
   |0       0         0          ...   P_{m-j}           P_{m-l-j}        |
   |0       0         0          ...   0                 P_{m-j}          |
   *
   (τ^{m-l-j} τ^{m-2*l-j} ... τ^{(m-j)%l+l} τ^{(m-j)%l})^T

   a || b is the concatenation of a and b.

   We can extend this Toeplitz matrix to form a circulant matrix whose columns
   are shifted versions of the vector
   c=P_{m-j} || 0^{floor((m-j)/l)-1} || P_{(m-j)%l+l} ... P_{m-j-l}.
   We can then compute circulant matrix-vector multiplication with the FFT.
   See this presentation from Kyle Kloster, student at Purdue University:
   https://www.youtube.com/watch?v=w0peHpfFVpc.

   Given the euclidean divisions m-j = q*l+r, 0 <= r < l for j=0, ...,l-1:
   1. Compute l EC-FFTs over G_1: forall j=0, ...,l-1,
   s_j=EC-FFT_{2m/l}(srs_{m-j-l} srs_{m-j-2*l} srs_{m-j-3*l} ... srs_{m-j-q*l=r} || 0^{2m/l - floor((m-j)/l)}).

   The above calculation can be done once per trusted setup and can thus be cached.

   2. Compute l FFTs over the [Scalar] field: forall j=0, ..., l-1:

   P'_j = FFT_{2m/l}(P_{m-j} || 0^{q+2*padding+1} || P_{r+l} P_{r+2l}
               ... P_{r+(q-1)l=m-j-l} || 0^{2m/l- (2*q+2*padding+1)})
   where [q = floor ((m-j)/l) = quotient] and [padding] is the difference
   between [quotient] and the next power of two of [quotient].

   3. Then compute {h}=(h_k)_{k in ⟦1, n/l⟧} with circulant matrix-vector
   multiplication via FFT:
       h = sum_{j=0}^{l-1} (h_{1,j} ... h_{floor((m-j)/l), j} || 0^{2m/l- floor((m-j)/l)})
         = sum_{j=0}^{l-1}EC-IFFT_{2m/l}(P'_j o_{G_1} s_j)
         = EC-IFFT_{2m/l} (sum_{j=0}^{l-1} (P_j o_{G_1} s_j))
   where o_{G_1} is the pairwise product for vectors with components in G_1.

   4. The first n/l coefficients is the result of the multiplication by the
   Toeplitz vector (with a bit of zero padding starting from the m/l-th coefficient):
   let's call this vector h'. The n/l KZG proofs are given by
   π=EC-FFT_{n/l}(h') following the observation ( * ).


   Complexity of multiple multi-reveals
   ====================================

   For the preprocessing part (step 1), we count l EC-FFTs on G_1, so the asymptotic complexity
   of the step is O(l * (m/l) log (m/l))=O(m log(m/l)).

   For the KZG proofs generation part (steps 2 to 4), we count l FFTs on the scalar field F,
   two EC-FFTs on G_1, and l * 2m/l elliptic curve scalar multiplications in G_1:
   the runtime complexity is O(l * T_{F}(m/l) + T_{G_1}(n/l) + m),
   where T_{F} and T_{G_1} represent the runtime cost of the FFT and EC-FFT.
   Both have the same complexity, even though the latter hides a bigger constant
   (log of scalar size in bits, here log 256) due to the elliptic curve scalar multiplication.

   Let's recall that l is in our application the length of a shard, n is the length of
   the erasure code, α its redundancy factor and m ≈ k is the dimension of the erasure code.
   Calling s the number of shards, we obtain l = n/s = α*k/s.
   The runtime of the precomputation part can be rewritten as O(k * log (s/α)).
   And the computation of the n/l KZG proofs becomes
   O(k * log (s/α) + s * log s).
   This explains why the algorithm is more efficient with bigger erasure code redundancies α,
   especially the precomputation part as it performs EC-FFTs.

   For our purposes the length of a shard s << k, so the bottleneck is the pointwise
   scalar multiplication in G_1. *)

(* Step 1, returns the pair made of the vectors s_j and the [domain] of length
   [2 * m / l = 2 * t.max_polynomial_length / t.shard_size] used for the computation of the s_j. *)
let preprocess_multiple_multi_reveals t =
  (* The length of a coset [t.shard_length] divides the domain length [t.max_polynomial_length].
     This is because [t.shard_length] divides [t.erasure_encoded_polynomial_length], [t.max_polynomial_length] divides [t.erasure_encoded_polynomial_length]
     and [t.max_polynomial_length > t.shard_length] (see why [m > 2l] above, where [m = t.max_polynomial_length] and
     [l = t.shard_length] here). *)
  assert (t.max_polynomial_length mod t.shard_length = 0) ;
  let domain_length = 2 * t.max_polynomial_length / t.shard_length in
  (* TODO https://gitlab.com/tezos/tezos/-/issues/6585
     The length of the discrete Fourier transforms is a power of two,
     though we could relax the constraints to a product of primes dividing
     the order of the group G1 thanks to the Prime Factorization Algorithm
     as we currently do with the FFTs on scalar elements. *)
  assert (domain_length <> 0 && domain_length land (domain_length - 1) = 0) ;
  let domain = Domain.build domain_length in
  (* Computes
     points = srs_{m-j-l} srs_{m-j-2l} srs_{m-j-3l} ... srs_{m-j-ql=r}
              || 0^{2m/l - floor((m-j)/l)},
     s_j = EC-FFT_{2m/l}(points). *)
  let s_j j =
    (* According to the documentation of [( / )], "x / y is the greatest
       integer less than or equal to the real quotient of x by y". Thus it
       equals [floor (x /. y)]. *)
    let quotient = (t.max_polynomial_length - j) / t.shard_length in
    let points =
      G1_carray.init domain_length (fun i ->
          if i < quotient then
            Srs_g1.get
              t.srs_g1
              (t.max_polynomial_length - j - ((i + 1) * t.shard_length))
          else G1.(zero))
    in
    G1_carray.evaluation_ecfft ~domain ~points
  in
  (domain, Array.init t.shard_length s_j)

(* [multiple_multi_reveals t preprocess coefficients] returns the proofs
   for each of the [t.number_of_shards] shards.

   Implements the "Multiple multi-reveals" section above. *)
let multiple_multi_reveals t ~preprocess:(domain, sj) ~coefficients :
    shard_proof array =
  (* [t.max_polynomial_length > l] where [l = t.shard_length]. *)
  assert (t.shard_length < t.max_polynomial_length) ;
  (* Step 2. *)
  let domain_length = Domain.length domain in
  let h_j j =
    let remainder = (t.max_polynomial_length - j) mod t.shard_length in
    let quotient = (t.max_polynomial_length - j) / t.shard_length in
    let padding = Utils.diff_next_power_of_two quotient in
    (* points = P_{m-j} || 0^{q+2*padding+1} || P_{r+l} P_{r+2l}
               ... P_{r+(q-1)l=m-j-l} || 0^{2m/l- (2*q+2*padding+1)}
               where [q = floor ((m-j)/l) = quotient]. *)
    let points =
      Poly.init domain_length (fun i ->
          let idx =
            remainder + ((i - (quotient + (2 * padding))) * t.shard_length)
          in
          if i = 0 then Scalar.copy coefficients.(t.max_polynomial_length - j)
          else if
            i <= quotient + (2 * padding) || idx > t.max_polynomial_length
            (* The second inequality is here in the case
               [t.max_polynomial_length = 2*t.shard_length]
               thus [domain_length = 2*t.max_polynomial_length/t.shard_length=4]
               and [padding=0].
               In this case, either
               [quotient = 2] thus [points = P_{m-j} 0 0 P_{r+l=m-j-l}],
               or [quotient = 1] thus
               [points] = P_{m-j} 0 P_{m-j} 0. *)
          then Scalar.(zero)
          else coefficients.(idx))
    in
    (* FFT of step 2. *)
    Evaluations.evaluation_fft domain points
  in

  (* Pairwise product of step 3. *)
  let evaluations = Array.init t.shard_length h_j in
  let h_j = G1_carray.mul_arrays ~evaluations ~arrays:sj in
  (* Sum of step 3. *)
  let sum = h_j.(0) in
  for i = 1 to t.shard_length - 1 do
    G1_carray.add_arrays_inplace sum h_j.(i)
  done ;

  (* Step 3. Toeplitz matrix-vector multiplication *)
  G1_carray.interpolation_ecfft_inplace ~domain ~points:sum ;

  (* Keep first n / l coefficients *)
  let len = Domain.length domain / 2 in
  let points = G1_carray.sub sum ~off:0 ~len in
  (* Step 4. *)
  let domain = Domain.build t.number_of_shards in
  G1_carray.(to_array (evaluation_ecfft ~domain ~points))

(* [interpolation_poly root domain evaluations] returns the
   polynomial P of minimam degree ([< Domains.length domain]) verifying
   P(root * domain[i]) = evaluations[i].

   Requires:
   - [(Array.length evaluations = Domains.length domain)] *)
let interpolation_poly ~root ~domain ~evaluations =
  assert (Array.length evaluations = Domain.length domain) ;
  let size = Domain.length domain in
  let evaluations =
    FFT.ifft_inplace domain (Evaluations.of_array (size - 1, evaluations))
  in
  (* Computes root_inverse = 1/root. *)
  let root_inverse = Scalar.inverse_exn root in
  (* Computes evaluations[i] = evaluations[i] * root_inverse^i. *)
  snd
    (Poly.fold_left_map
       (fun root_pow_inverse coefficient ->
         ( Scalar.mul root_pow_inverse root_inverse,
           Scalar.mul coefficient root_pow_inverse ))
       Scalar.(one)
       evaluations)

(* [verify t commitment srs_point domain root evaluations proof]
   verifies that P(root * domain.(i)) = evaluations.(i),
   where
   - [P = commit t s] for some slot [s]
   - [l := Array.length evaluations = Domains.length domain]
   - [srs_point = Srs_g2.get t.srs.raw.srs_g2 l]
   - [root = w^i] where [w] is a primitive [erasure_encoded_polynomial_length]-th root of unity for [l] dividing [erasure_encoded_polynomial_length]
   - [domain = (1, z, z^2, ..., z^{l - 1})] where [z = w^{n/l}] is a primitive
   [l]-th root of unity

   Implements the "Multi-reveals" section above. *)
let verify t ~commitment ~srs_point ~domain ~root ~evaluations ~proof =
  let open Bls12_381 in
  (* Compute r_i(x). *)
  let remainder = interpolation_poly ~root ~domain ~evaluations in
  (* Compute [r_i(τ)]_1. *)
  let commitment_remainder = commit t remainder in
  (* Compute [w^{i * l}]. *)
  let root_pow = Scalar.pow root (Z.of_int (Domain.length domain)) in
  (* Compute [τ^l]_2 - [w^{i * l}]_2). *)
  let commit_srs_point_minus_root_pow =
    G2.(add srs_point (negate (mul (copy one) root_pow)))
  in
  (* Compute [r_i(τ)]_1-c. *)
  let diff_commits = G1.(add commitment_remainder (negate commitment)) in
  (* Checks e(c-[r_i(τ)]_1, g_2) ?= e(π, [τ^l]_2 - [w^{i * l}]_2)
     by checking
     [0]_1 ?= -e(c-[r_i(τ)]_1, g_2) + e(π, [τ^l]_2 - [w^{i * l}]_2)
            = e([r_i(τ)]_1-c, g_2) + e(π, [τ^l]_2 - [w^{i * l}]_2). *)
  Pairing.pairing_check
    [(diff_commits, G2.(copy one)); (proof, commit_srs_point_minus_root_pow)]

(*Batched version of the verify functions, which verify the correctness of opening
  of multiple multi reveals.
  Inspired from page 13 of https://eprint.iacr.org/2019/953.pdf
  With commmitment = c, remainder list = R_i
  (IFFT of the evaluations, root_list = w_i
  (indicating the shifts of the subgroup we are verifying evaluations agains, proof_list = pi_i,
  we generate a pseudo random alpha and verify :
  e(c, \sum_i w_i R_i,1)*e(sum_i aplha_i*w_i*pi_i,[X]) =? 1 *)
let verify_multi t ~commitment ~srs_point ~domain ~root_list ~evaluations_list
    ~proof_list =
  let open Bls12_381 in
  (* Compute r_i(x). *)
  let remainder_list =
    List.map2
      (fun root evaluations -> interpolation_poly ~root ~domain ~evaluations)
      root_list
      evaluations_list
  in
  let alpha =
    (* We use Fiat-Shamir here to get some pseudo randomness.
       Since alpha is only used on the verifier side we could
       use real randomness as well. *)
    List.fold_left
      (fun transcript proof ->
        Utils.Transcript.expand shard_proof_t proof transcript)
      Utils.Transcript.empty
      proof_list
    |> Utils.Transcript.expand eval_list_t evaluations_list
    |> Utils.Transcript.expand shard_proof_t commitment
    |> Utils.Fr_generation.random_fr |> fst
  in
  (* TODO optimize *)
  let alpha_list =
    List.init (List.length proof_list) (fun i -> Scalar.pow alpha (Z.of_int i))
  in
  let batched_remainder =
    List.fold_left2
      (fun acc remainder_i alpha_i ->
        Poly.(acc + mul_by_scalar alpha_i remainder_i))
      Poly.zero
      remainder_list
      alpha_list
  in
  let commitment_remainder_batched = commit t batched_remainder in
  let root_pow_list =
    List.map
      (fun root -> Scalar.pow root (Z.of_int (Domain.length domain)))
      root_list
  in
  let sum_alpha_i = List.fold_left Scalar.add Scalar.zero alpha_list in
  let batched_commitment = G1.mul commitment sum_alpha_i in
  let alpha_i_root_pow_i = List.map2 Scalar.mul alpha_list root_pow_list in
  let w_batched =
    G1.pippenger (Array.of_list proof_list) (Array.of_list alpha_i_root_pow_i)
  in
  let left =
    G1.(
      add
        batched_commitment
        (add w_batched (negate commitment_remainder_batched)))
  in
  let w_batched_2 =
    G1.(
      negate @@ pippenger (Array.of_list proof_list) (Array.of_list alpha_list))
  in
  Pairing.pairing_check [(left, G2.one); (w_batched_2, srs_point)]
