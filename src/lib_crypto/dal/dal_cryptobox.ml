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

open Error_monad

(** The cryptographic primitives for the data availability layer (DAL). *)
module type DAL_cryptobox_sig = sig
  module Scalar : Ff_sig.PRIME with type t = Bls12_381.Fr.t

  module IntMap : Tezos_error_monad.TzLwtreslib.Map.S with type key = int

  type polynomial

  (** Commitment to a polynomial. *)
  type commitment

  (** Proof of evaluations of shards. *)
  type proof_shards

  (** Proof of degree. *)
  type proof_degree

  (** Proof of evaluation at a single point. *)
  type proof_single

  (** Proof of a slot segment. *)
  type proof_slot_segment

  (** A share is a part of the encoded data. *)
  type share = Scalar.t array

  (** A shard is defined by its number and the part of the encoded data it
      contains. *)
  type shard = int * share

  (** Collection of shards. *)
  type shards_map = share IntMap.t

  type shards_proofs_precomputation

  type slot_segments_proofs_precomputation

  module Encoding : sig
    val commitment_encoding : commitment Data_encoding.t

    val proof_shards_encoding : proof_shards Data_encoding.t

    val proof_degree_encoding : proof_degree Data_encoding.t

    val proof_single_encoding : proof_single Data_encoding.t

    val share_encoding : share Data_encoding.t

    val shard_encoding : shard Data_encoding.t

    val shards_encoding : shards_map Data_encoding.t

    val shards_proofs_precomputation_encoding :
      shards_proofs_precomputation Data_encoding.t

    val slot_segments_proofs_precomputation_encoding :
      slot_segments_proofs_precomputation Data_encoding.t
  end

  (** Length of the erasure-encoded slot in terms of scalar elements. *)
  val erasure_encoding_length : int

  val polynomial_degree : polynomial -> int

  (** [polynomial_evaluate p z] evaluates [p] in [z]. *)
  val polynomial_evaluate : polynomial -> Scalar.t -> Scalar.t

  (** [polynomial_from_bytes slot] returns a polynomial from the input [slot]. *)
  val polynomial_from_bytes :
    bytes -> (polynomial, [> `Slot_wrong_size of string]) Result.t

  (** [polynomial_to_bytes polynomial] returns a slot from a [polynomial]. *)
  val polynomial_to_bytes : polynomial -> bytes

  (** [to_shards polynomial] returns the Reed-Solomon-encoded data in shards. *)
  val to_shards : polynomial -> shards_map

  (** [from_shards shards] returns the Reed-Solomon-decoded polynomial. *)
  val from_shards :
    shards_map ->
    ( polynomial,
      [> `Invert_zero of string | `Not_enough_shards of string] )
    Result.t

  (** [commit p] is the commitment to [p]. *)
  val commit :
    polynomial ->
    (commitment, [> `Degree_exceeds_srs_length of string]) Result.t

  (** [prove_degree p n] produces a proof that [p] has degree less
      than [n]. The algorithm fails if that is not the case. *)
  val prove_degree :
    polynomial ->
    int ->
    (proof_degree, [> `Degree_exceeds_srs_length of string]) Result.t

  (** [verify_degree commitment proof n] returns true if and only if the
      committed polynomial has degree less than [n]. *)
  val verify_degree :
    commitment ->
    proof_degree ->
    int ->
    (bool, [> `Degree_exceeds_srs_length of string]) Result.t

  (** [precompute_shards_proofs ()] returns the precomputation used to prove
      shards. *)
  val precompute_shards_proofs : unit -> shards_proofs_precomputation

  val save_precompute_shards_proofs :
    shards_proofs_precomputation -> string -> unit

  val load_precompute_shards_proofs : string -> shards_proofs_precomputation

  (** [prove_shards preprocess p] creates a proof of evaluation for each
      shard. *)
  val prove_shards :
    polynomial -> preprocess:shards_proofs_precomputation -> proof_shards array

  (** [verify_shard commitment shard proof] returns true if and only if the
      shard is consistent with the [commitment] and [proof]. *)
  val verify_shard : commitment -> shard -> proof_shards -> bool

  (** [prove_single p z] returns a proof of evaluation of [p] at [z]. *)
  val prove_single :
    polynomial ->
    Scalar.t ->
    (proof_single, [> `Degree_exceeds_srs_length of string]) Result.t

  (** [verify_single cm pi] returns true if the proof is correct with regard to
      the opening. *)
  val verify_single :
    commitment -> point:Scalar.t -> evaluation:Scalar.t -> proof_single -> bool

  (** [precompute_slot_segments_proofs ()] returns the precomputation used to
      prove slot segments. *)
  val precompute_slot_segments_proofs :
    unit -> slot_segments_proofs_precomputation

  val save_precompute_slot_segments_proofs :
    slot_segments_proofs_precomputation -> string -> unit

  val load_precompute_slot_segments_proofs :
    string -> slot_segments_proofs_precomputation

  (** [prove_slot_segments p preprocess] where [p] is the output of
      [polynomial_from_bytes slot], returns proofs for all slot segments. *)
  val prove_slot_segments :
    polynomial ->
    preprocess:slot_segments_proofs_precomputation ->
    proof_slot_segment array

  (** [verify_slot_segment cm ~slot_segment ~slot_segment_index proof] returns
      true if the [slot_segment] whose index is [slot_segment_index] is
      correct. *)
  val verify_slot_segment :
    commitment ->
    slot_segment:bytes ->
    slot_segment_index:int ->
    proof_slot_segment ->
    bool
end

(** Parameters of the DAL relevant to the cryptographic primitives. *)
module type Params_sig = sig
  (** Redundancy factor of the erasure code. *)
  val redundancy_factor : int

  (** Size in bytes of a slot, must be a power of two. *)
  val slot_size : int

  (** Size in bytes of a slot segment, must be a power of two. *)
  val slot_segment_size : int

  (** Each erasure-encoded slot splits evenly into the given amount of shards. *)
  val shards_amount : int
end

module Make (Params : Params_sig) : DAL_cryptobox_sig = struct
  open Kate_amortized

  (* Scalars are elements of the prime field Fr from BLS. *)
  module Scalar = Bls12_381.Fr

  (* Operations on vector of scalars *)
  module Evaluations = Bls12_381_polynomial.Polynomial.Evaluations

  (* Domains for the Fast Fourier Transform (FTT). *)
  module Domains = Bls12_381_polynomial.Polynomial.Domain
  module Polynomial = Bls12_381_polynomial.Polynomial.Polynomial
  module IntMap = Tezos_error_monad.TzLwtreslib.Map.Make (Int)

  type polynomial = Polynomial.t

  type commitment = Kate_amortized.commitment

  type proof_shards = Kate_amortized.proof

  type proof_degree = Bls12_381.G1.t

  type proof_single = Bls12_381.G1.t

  type proof_slot_segment = Bls12_381.G1.t

  type share = Scalar.t array

  type shards_map = share IntMap.t

  type shard = int * share

  type shards_proofs_precomputation =
    Scalar.t array * proof_slot_segment array array

  type slot_segments_proofs_precomputation =
    Scalar.t array * proof_slot_segment array array

  module Encoding = struct
    open Data_encoding

    let fr_encoding = conv Bls12_381.Fr.to_bytes Bls12_381.Fr.of_bytes_exn bytes

    let g1_encoding =
      conv
        Bls12_381.G1.to_compressed_bytes
        Bls12_381.G1.of_compressed_bytes_exn
        bytes

    let commitment_encoding = g1_encoding

    let proof_shards_encoding = g1_encoding

    let proof_degree_encoding = g1_encoding

    let proof_single_encoding = g1_encoding

    let share_encoding = array fr_encoding

    let shard_encoding = tup2 int31 share_encoding

    let shards_encoding =
      conv
        IntMap.bindings
        (fun bindings -> IntMap.of_seq (List.to_seq bindings))
        (list (tup2 int31 share_encoding))

    let shards_proofs_precomputation_encoding =
      tup2 (array fr_encoding) (array (array g1_encoding))

    let slot_segments_proofs_precomputation_encoding =
      tup2 (array fr_encoding) (array (array g1_encoding))
  end

  (* Number of bytes fitting in a Scalar.t. Since scalars are integer modulo
     r~2^255, we restrict ourselves to 248-bit integers (31 bytes). *)
  let scalar_bytes_amount = Scalar.size_in_bytes - 1

  (* k and n are the parameters of the erasure code. *)
  let k =
    1 lsl Z.(log2up (of_int Params.slot_size / of_int scalar_bytes_amount))

  let n = Params.(redundancy_factor * k)

  let erasure_encoding_length = n

  (* Check code parameters. *)
  let _ =
    let open Params in
    let is_pow_of_two x =
      let logx = Z.(log2 (of_int x)) in
      1 lsl logx = x
    in

    (* According to the specification the lengths of a slot a slot segment are
       in MiB *)
    assert (is_pow_of_two slot_size) ;
    assert (is_pow_of_two slot_segment_size) ;

    assert (is_pow_of_two n) ;

    (* n must be at most 2^32, the biggest subgroup of 2^i roots of unity in the
       multiplicative group of Fr, because the FFTs operate on such groups. *)
    assert (Z.(log2 (of_int n)) <= 32) ;

    assert (is_pow_of_two k) ;
    assert (n > k) ;

    assert (is_pow_of_two shards_amount) ;
    (* Shards must contain at least two elements. *)
    assert (n > shards_amount)

  (* Memoize intermediate domains for the FFTs. *)
  let intermediate_domains : Evaluations.domain IntMap.t ref = ref IntMap.empty

  (* Builds group of nth roots of unity, a valid domain for the FFT. *)
  let make_domain n = Domains.build ~log:Z.(log2up (of_int n))

  let domain_k = make_domain k

  let domain_2k = make_domain (2 * k)

  let domain_n = make_domain n

  (* Length of a shard in terms of scalar elements. *)
  let shard_size = Params.(n / shards_amount)

  let nb_segments = Params.(slot_size / slot_segment_size)

  (* Since the remainder of the division of slot_segment_size (power of 2) by
     scalar_bytes_amount (= 31) is non-zero. *)
  let segment_len = Int.div Params.slot_segment_size scalar_bytes_amount + 1

  let remaining_bytes = Params.slot_segment_size mod scalar_bytes_amount

  let polynomial_degree = Polynomial.degree

  let polynomial_evaluate = Polynomial.evaluate

  (* https://github.com/janestreet/base/blob/master/src/list.ml#L1117 *)
  let take n t_orig =
    if n <= 0 then []
    else
      let rec loop n t accum =
        if n = 0 then accum
        else
          match t with [] -> t_orig | hd :: tl -> loop (n - 1) tl (hd :: accum)
      in
      loop n t_orig []

  let fft_mul d ps =
    let open Evaluations in
    let evaluations = List.map (evaluation_fft d) ps in
    interpolation_fft d (mul_c ~evaluations ())

  (* Divide & conquer polynomial multiplication with FFTs, assuming leaves are
     polynomials of equal length. For n the degree of the returned polynomial,
     k = |ps|, runs in time O(n log n log k). *)
  let poly_mul ps =
    let split = List.fold_left (fun (l, r) x -> (x :: r, l)) ([], []) in
    let rec poly_mul_aux ps =
      match ps with
      | [x] -> x
      | _ ->
          let a, b = split ps in
          let a = poly_mul_aux a in
          let b = poly_mul_aux b in
          let deg = Polynomial.degree a + 1 (* deg a = deg b in our case. *) in
          (* Computes adequate domain for the FFTs. *)
          let d =
            match IntMap.find deg !intermediate_domains with
            | Some d -> d
            | None ->
                let d =
                  Domains.subgroup ~log:(Z.log2up (Z.of_int (2 * deg))) domain_n
                in
                intermediate_domains := IntMap.add deg d !intermediate_domains ;
                d
          in
          fft_mul d [a; b]
    in
    poly_mul_aux ps

  (* We encode by segments of [slot_segment_size] bytes each. *)
  let polynomial_from_bytes' slot =
    if Bytes.length slot <> Params.slot_size then
      Error
        (`Slot_wrong_size
          (Printf.sprintf "message must be %d bytes long" Params.slot_size))
    else
      let offset = ref 0 in
      let res = Array.init k (fun _ -> Scalar.(copy zero)) in
      for segment = 0 to nb_segments - 1 do
        for elt = 0 to segment_len - 1 do
          if !offset > Params.slot_size then ()
          else if elt = segment_len - 1 then (
            let dst = Bytes.create remaining_bytes in
            Bytes.blit slot !offset dst 0 remaining_bytes ;
            offset := !offset + remaining_bytes ;
            res.((elt * nb_segments) + segment) <- Scalar.of_bytes_exn dst)
          else
            let dst = Bytes.create scalar_bytes_amount in
            Bytes.blit slot !offset dst 0 scalar_bytes_amount ;
            offset := !offset + scalar_bytes_amount ;
            res.((elt * nb_segments) + segment) <- Scalar.of_bytes_exn dst
        done
      done ;
      Ok res

  let polynomial_from_bytes slot =
    let open Result_syntax in
    let* data = polynomial_from_bytes' slot in
    Ok (Evaluations.interpolation_fft2 domain_k data)

  (* The segments are arranged in cosets to evaluate in batch with Kate
     amortized. *)
  let polynomial_to_bytes p =
    let p = Evaluations.evaluation_fft2 domain_k p in
    let slot = Bytes.init Params.slot_size (fun _ -> '0') in
    let offset = ref 0 in
    for segment = 0 to nb_segments - 1 do
      for elt = 0 to segment_len - 1 do
        let idx = (elt * nb_segments) + segment in
        let coeff = Scalar.to_bytes (Array.get p idx) in
        if elt = segment_len - 1 then (
          Bytes.blit coeff 0 slot !offset remaining_bytes ;
          offset := !offset + remaining_bytes)
        else (
          Bytes.blit coeff 0 slot !offset scalar_bytes_amount ;
          offset := !offset + scalar_bytes_amount)
      done
    done ;
    slot

  let encode = Evaluations.evaluation_fft2 domain_n

  (* The shards are arranged in cosets to evaluate in batch with Kate
     amortized. *)
  let to_shards p =
    let codeword = encode p in
    let len_shard = n / Params.shards_amount in
    let rec loop i map =
      match i with
      | i when i = Params.shards_amount -> map
      | _ ->
          let shard = Array.init len_shard (fun _ -> Scalar.(copy zero)) in
          for j = 0 to len_shard - 1 do
            shard.(j) <- codeword.((Params.shards_amount * j) + i)
          done ;
          loop (i + 1) (IntMap.add i shard map)
    in
    loop 0 IntMap.empty

  (* Computes the polynomial N(X) := \sum_{i=0}^{k-1} n_i x_i^{-1} X^{z_i}. *)
  let compute_n eval_a' shards =
    let w = Domains.get domain_n 1 in
    let n_poly = Array.init n (fun _ -> Scalar.(copy zero)) in
    let open Result_syntax in
    let c = ref 0 in
    let* () =
      IntMap.iter_e
        (fun z_i arr ->
          if !c >= k then Ok ()
          else
            let rec loop j =
              match j with
              | j when j = Array.length arr -> Ok ()
              | _ -> (
                  let c_i = arr.(j) in
                  let z_i = (Params.shards_amount * j) + z_i in
                  let x_i = Scalar.pow w (Z.of_int z_i) in
                  let tmp = Evaluations.get eval_a' z_i in
                  Scalar.mul_inplace tmp tmp x_i ;
                  match Scalar.inverse_exn_inplace tmp tmp with
                  | exception _ -> Error (`Invert_zero "can't inverse element")
                  | () ->
                      Scalar.mul_inplace tmp tmp c_i ;
                      n_poly.(z_i) <- tmp ;
                      c := !c + 1 ;
                      loop (j + 1))
            in
            loop 0)
        shards
    in
    Ok n_poly

  let from_shards shards =
    if k > IntMap.cardinal shards * shard_size then
      Error
        (`Not_enough_shards
          (Printf.sprintf
             "there must be at least %d shards to decode"
             (k / shard_size)))
    else
      (* 1. Computing A(x) = prod_{i=0}^{k-1} (x - w^{z_i}).
         Let w be a primitive nth root of unity and
         Ω_0 = {w^{shards_amount j}}_{j=0 to (n/shards_amount)-1}
         be the (n/shards_amount)-th roots of unity and Ω_i = w^i Ω_0.

         Together, the Ω_i's form a partition of the subgroup of the n-th roots
         of unity: 𝕌_n = disjoint union_{i ∈ {0, ..., shards_amount-1}} Ω_i.

         Let Z_j := Prod_{w ∈ Ω_j} (x − w). For a random set of shards
         S⊆{0, ..., shards_amount-1} of length k/shard_size, we reorganize the
         product A(x) = Prod_{i=0}^{k-1} (x − w^{z_i}) into
         A(x) = Prod_{j ∈ S} Z_j.

         Moreover, Z_0 = x^|Ω_0| - 1 since x^|Ω_0| - 1 contains all roots of Z_0
         and conversely. Multiplying each term of the polynomial by the root w^j
         entails Z_j = x^|Ω_0| − w^{j*|Ω_0|}.

         The intermediate products Z_j have a lower Hamming weight (=2) than
         when using other ways of grouping the z_i's into shards.

         This also reduces the depth of the recursion tree of the poly_mul
         function from log(k) to log(shards_amounts), so that the decoding time
         reduces from O(k*log^2(k) + n*log(n)) to O(n*log(n)). *)
      let factors =
        IntMap.bindings shards
        (* We always consider the first k codeword vector components. *)
        |> take (k / shard_size)
        |> List.rev_map (fun (i, _) ->
               Polynomial.of_coefficients
                 [
                   (Scalar.negate (Domains.get domain_n (i * shard_size)), 0);
                   (Scalar.(copy one), shard_size);
                 ])
      in
      let a_poly = poly_mul factors in

      (* 2. Computing formal derivative of A(x). *)
      let a' = Polynomial.derivative a_poly in

      (* 3. Computing A'(w^i) = A_i(w^i). *)
      let eval_a' = Evaluations.evaluation_fft domain_n a' in

      (* 4. Computing N(x). *)
      let open Result_syntax in
      let* n_poly = compute_n eval_a' shards in
      (* 5. Computing B(x). *)
      let b = Evaluations.interpolation_fft2 domain_n n_poly in
      let b = Polynomial.copy ~len:k b in
      Polynomial.mul_by_scalar_inplace b (Scalar.of_int n) b ;

      (* 6. Computing Lagrange interpolation polynomial P(x). *)
      let p = fft_mul domain_2k [a_poly; b] in
      let p = Polynomial.copy ~len:k p in
      Polynomial.opposite_inplace p ;
      Ok p

  let shard_size = n / Params.shards_amount

  let evaluations_log = Z.(log2 (of_int n))

  let evaluations_per_proof_log = Z.(log2 (of_int shard_size))

  let proofs_log = evaluations_log - evaluations_per_proof_log

  (* Mock SRS, TODO: import srs files *)
  let secret =
    Scalar.of_string
      "43455265682179153414401896409006117694844195495630810185710628957923813326493"

  let kate_amortized_srs_g1 = Kate_amortized.gen_srs_g1 ~size:k secret

  let kate_amortized_srs_g2_shards =
    Kate_amortized.gen_srs_g2 ~l:(1 lsl evaluations_per_proof_log) secret

  let kate_amortized_srs_g2_segments =
    Kate_amortized.gen_srs_g2 ~l:(1 lsl Z.(log2up (of_int segment_len))) secret

  let kzg_srs_size = k

  let build_array init next len =
    let xi = ref init in
    Array.init len (fun _ ->
        let i = !xi in
        xi := next !xi ;
        i)

  let create_srs :
      type t.
      (module Bls12_381.CURVE with type t = t) -> int -> Scalar.t -> t array =
   fun (module G) d x -> build_array G.(copy one) (fun g -> G.(mul g x)) d

  (* Mock SRS, TODO: import srs files *)
  let kzg_srs_g2 = create_srs (module Bls12_381.G2) kzg_srs_size secret

  let kzg_srs = create_srs (module Bls12_381.G1) kzg_srs_size secret

  let commit' :
      type t.
      (module Bls12_381.CURVE with type t = t) ->
      polynomial ->
      t array ->
      (t, [> `Degree_exceeds_srs_length of string]) Result.t =
   fun (module G) p srs ->
    let p = Polynomial.to_dense_coefficients p in
    if p = [||] then Ok G.(copy zero)
    else if Array.(length p > length srs) then
      Error
        (`Degree_exceeds_srs_length
          (Printf.sprintf
             "polynomial degree, %i, exceeds  srs’ length, %i."
             (Array.length p)
             (Array.length srs)))
    else Ok (G.pippenger ~start:0 ~len:(Array.length p) srs p)

  let commit p = commit' (module Bls12_381.G1) p kzg_srs

  (* p(X) of degree n. Max degree that can be committed: d, which is also the
     SRS's length - 1.

     We consider the bilinear groups (G_1, G_2, G_T) with G_1=<g> and G_2=<h>.
     - Commit (p X^{d-n}) such that deg (p X^{d-n}) = d the max degree
     that can be committed
     - Verify: checks if e(commit(p), commit(X^{d-n})) = e(commit(p X^{d-n}), h)
     using the commitments for p and p X^{d-n}, and computing the commitment for
     X^{d-n} on G_2.*)

  let prove_degree p n =
    let d = kzg_srs_size - 1 in
    commit'
      (module Bls12_381.G1)
      (Polynomial.mul
         (Polynomial.of_coefficients [(Scalar.(copy one), d - n)])
         p)
      kzg_srs

  let verify_degree cm proof n =
    let open Tezos_error_monad.TzMonad.Result_syntax in
    let open Bls12_381 in
    let d = kzg_srs_size - 1 in
    let* commit_xk =
      commit'
        (module G2)
        (Polynomial.of_coefficients [(Scalar.(copy one), d - n)])
        kzg_srs_g2
    in
    Ok
      (Pairing.pairing_check [(cm, commit_xk); (proof, G2.(negate (copy one)))])

  let eval_to_array e =
    let open Kate_amortized.Domain in
    Array.init (length e) (get e)

  let precompute_shards_proofs () =
    let eval, m =
      Kate_amortized.preprocess_multi_reveals
        ~chunk_len:evaluations_per_proof_log
        ~degree:k
        (kate_amortized_srs_g1, kate_amortized_srs_g2_shards)
    in
    (eval_to_array eval, m)

  let save_precompute_shards_proofs (preprocess : shards_proofs_precomputation)
      filename =
    let chan = Out_channel.open_bin filename in
    Out_channel.output_bytes
      chan
      (Data_encoding.Binary.to_bytes_exn
         Encoding.shards_proofs_precomputation_encoding
         preprocess) ;
    Out_channel.close_noerr chan

  let load_precompute_shards_proofs filename =
    let chan = In_channel.open_bin filename in
    let len = Int64.to_int (In_channel.length chan) in
    let data = Bytes.create len in
    let _ = In_channel.really_input chan data 0 len in
    let precomp =
      Data_encoding.Binary.of_bytes_exn
        Encoding.shards_proofs_precomputation_encoding
        data
    in
    In_channel.close_noerr chan ;
    precomp

  let prove_shards p ~preprocess =
    Kate_amortized.multiple_multi_reveals
      ~chunk_len:evaluations_per_proof_log
      ~chunk_count:proofs_log
      ~degree:k
      ~preprocess
      (Polynomial.to_dense_coefficients p |> Array.to_list)

  let verify_shard ct (shard_index, shard_evaluations) proof =
    let d_n = Kate_amortized.Domain.build ~log:evaluations_log in
    let domain = Kate_amortized.Domain.build ~log:evaluations_per_proof_log in
    Kate_amortized.verify
      ct
      (kate_amortized_srs_g1, kate_amortized_srs_g2_shards)
      domain
      (Kate_amortized.Domain.get d_n shard_index, shard_evaluations)
      proof

  let prove_single p z =
    let q = Polynomial.(division_x_z (p - constant (evaluate p z)) z) in
    commit' (module Bls12_381.G1) q kzg_srs

  let verify_single cm ~point ~evaluation proof =
    let h_secret = Array.get kzg_srs_g2 1 in
    Bls12_381.(
      Pairing.pairing_check
        [
          ( G1.(add cm (negate (mul (copy one) evaluation))),
            G2.(negate (copy one)) );
          (proof, G2.(add h_secret (negate (mul (copy one) point))));
        ])

  let precompute_slot_segments_proofs () =
    let eval, m =
      Kate_amortized.preprocess_multi_reveals
        ~chunk_len:Z.(log2up (of_int segment_len))
        ~degree:k
        (kate_amortized_srs_g1, kate_amortized_srs_g2_segments)
    in
    (eval_to_array eval, m)

  let save_precompute_slot_segments_proofs
      (preprocess : slot_segments_proofs_precomputation) filename =
    let chan = Out_channel.open_bin filename in
    Out_channel.output_bytes
      chan
      (Data_encoding.Binary.to_bytes_exn
         Encoding.slot_segments_proofs_precomputation_encoding
         preprocess) ;
    Out_channel.close_noerr chan

  let load_precompute_slot_segments_proofs filename =
    let chan = In_channel.open_bin filename in
    let len = Int64.to_int (In_channel.length chan) in
    let data = Bytes.create len in
    let _ = In_channel.really_input chan data 0 len in
    let precomp =
      Data_encoding.Binary.of_bytes_exn
        Encoding.slot_segments_proofs_precomputation_encoding
        data
    in
    In_channel.close_noerr chan ;
    precomp

  let prove_slot_segments p ~preprocess =
    Kate_amortized.multiple_multi_reveals
      ~chunk_len:Z.(log2up (of_int segment_len))
      ~chunk_count:Z.(log2 (of_int nb_segments))
      ~degree:k
      ~preprocess
      (Polynomial.to_dense_coefficients p |> Array.to_list)

  (* Parses the [slot_segment] to get the evaluations that it contains. The
     evaluation points are given by the [slot_segment_index]. *)
  let verify_slot_segment ct ~slot_segment ~slot_segment_index proof =
    let segment_domain =
      Kate_amortized.Domain.build
        ~log:(Z.log2 (Z.of_int (segment_len * nb_segments)))
    in
    let domain =
      Kate_amortized.Domain.build ~log:Z.(log2up (of_int segment_len))
    in
    let slot_segment_evaluations =
      Array.init
        (1 lsl Z.(log2up (of_int segment_len)))
        (function
          | i when i < segment_len - 1 ->
              let dst = Bytes.create scalar_bytes_amount in
              Bytes.blit
                slot_segment
                (i * scalar_bytes_amount)
                dst
                0
                scalar_bytes_amount ;
              Scalar.of_bytes_exn dst
          | i when i = segment_len - 1 ->
              let dst = Bytes.create remaining_bytes in
              Bytes.blit
                slot_segment
                (i * scalar_bytes_amount)
                dst
                0
                remaining_bytes ;
              Scalar.of_bytes_exn dst
          | _ -> Scalar.(copy zero))
    in
    Kate_amortized.verify
      ct
      (kate_amortized_srs_g1, kate_amortized_srs_g2_segments)
      domain
      ( Kate_amortized.Domain.get segment_domain slot_segment_index,
        slot_segment_evaluations )
      proof
end
