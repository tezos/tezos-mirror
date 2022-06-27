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

(** Parameters of the DAL relevant to the cryptographic primitives. *)
module type CONFIGURATION = sig
  (** Redundancy factor of the erasure code. *)
  val redundancy_factor : int

  (** Size in bytes of a slot, must be a power of two. *)
  val slot_size : int

  (** Size in bytes of a slot segment, must be a power of two. *)
  val slot_segment_size : int

  (** Each erasure-encoded slot splits evenly into the given amount of shards. *)
  val shards_amount : int
end

(** The cryptographic primitives for the data availability layer (DAL). *)
module type DAL_cryptobox_sig = sig
  module Scalar : Ff_sig.PRIME with type t = Bls12_381.Fr.t

  module IntMap : Tezos_error_monad.TzLwtreslib.Map.S with type key = int

  type polynomial

  (** Commitment to a polynomial. *)
  type commitment

  (** Proof of evaluations of a shard. *)
  type proof_shard

  (** Proof that a polynomial has degree less than some given bound. *)
  type proof_degree

  (** Proof of evaluation at a single point. *)
  type proof_single

  (** Proof of a slot segment. *)
  type proof_slot_segment

  (** A slot segment is defined by its index and the associated part of the
      slot. *)
  type slot_segment = int * bytes

  (** A share is a part of the encoded data. *)
  type share = Scalar.t array

  (** A shard is defined by its index and the part of the encoded data it
      contains. *)
  type shard = int * share

  (** Collection of shards. *)
  type shards_map = share IntMap.t

  (** Preprocessing to compute shards' proofs that depends on the DAL cryptobox
      parameters. *)
  type shards_proofs_precomputation

  type trusted_setup

  type trusted_setup_files = {
    srs_g1_file : string;
    srs_g2_file : string;
    log_size : int;
  }

  module Encoding : sig
    val commitment_encoding : commitment Data_encoding.t

    val proof_shards_encoding : proof_shard Data_encoding.t

    val proof_degree_encoding : proof_degree Data_encoding.t

    val proof_single_encoding : proof_single Data_encoding.t

    val share_encoding : share Data_encoding.t

    val shard_encoding : shard Data_encoding.t

    val shards_encoding : shards_map Data_encoding.t

    val shards_proofs_precomputation_encoding :
      shards_proofs_precomputation Data_encoding.t
  end

  (** [build_trusted_setup_instance files] builds a trusted setup from [files]
      on disk. Warning: if [files] is [`Unsafe_for_test_only] it triggers a
      computation of an unsafe trusted setup! *)
  val build_trusted_setup_instance :
    [`Unsafe_for_test_only | `Files of trusted_setup_files] -> trusted_setup

  (** Length of the erasure-encoded slot in terms of scalar elements. *)
  val erasure_encoding_length : int

  val polynomial_degree : polynomial -> int

  (** [polynomial_evaluate p z] evaluates [p] in [z]. *)
  val polynomial_evaluate : polynomial -> Scalar.t -> Scalar.t

  (** [polynomial_from_bytes slot] returns a polynomial from the input [slot].
      Errors with [`Slot_wrong_size] when the slot size is different from
      [CONFIGURATION.slot_size]. *)
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

  (** [commit p] returns the commitment to [p]. Errors with
      [`Degree_exceeds_srs_length] if the degree of [p] exceeds the SRS size. *)
  val commit :
    trusted_setup ->
    polynomial ->
    (commitment, [> `Degree_exceeds_srs_length of string]) Result.t

  (** [prove_degree p n] produces a proof that [p] has degree less
      than [n]. The function fails with [`Degree_exceeds_srs_length] if that is
        not the case. *)
  val prove_degree :
    trusted_setup ->
    polynomial ->
    int ->
    (proof_degree, [> `Degree_exceeds_srs_length of string]) Result.t

  (** [verify_degree commitment ts proof n] returns true if and only if the
      committed polynomial has degree less than [n], using trusted setup
      [ts]. *)
  val verify_degree :
    trusted_setup ->
    commitment ->
    proof_degree ->
    int ->
    (bool, [> `Degree_exceeds_srs_length of string]) Result.t

  (** [precompute_shards_proofs ts] returns the precomputation used to prove
      shards, using trusted setup [ts]. *)
  val precompute_shards_proofs : trusted_setup -> shards_proofs_precomputation

  (** [save_precompute_shards_proofs precomputation filename ()] saves to file
      [filename] the given [precomputation]. *)
  val save_precompute_shards_proofs :
    shards_proofs_precomputation -> string -> unit

  (** [load_precompute_shards_proofs filename] loads to memory the shards'
        proofs precomputation stored in file [filename]. *)
  val load_precompute_shards_proofs : string -> shards_proofs_precomputation

  (** [prove_shards p ~preprocess] creates a proof of evaluation for each
      shard. *)
  val prove_shards :
    polynomial -> preprocess:shards_proofs_precomputation -> proof_shard array

  (** [verify_shard ts cm shard proof] returns true if and only if the
      [proof] certifies that the [shard] is comming from the erasure encoding
      of the committed polynomial whose commitment is [cm]. *)
  val verify_shard : trusted_setup -> commitment -> shard -> proof_shard -> bool

  (** [prove_single ts p z] returns a proof of evaluation of [p] at [z], using
      trusted setup [ts]. *)
  val prove_single :
    trusted_setup ->
    polynomial ->
    Scalar.t ->
    (proof_single, [> `Degree_exceeds_srs_length of string]) Result.t

  (** [verify_single ts cm ~point ~evaluation pi] returns true if the proof [pi]
    is correct with regard to the opening ([cm], [point], [evaluation]), using
    the trusted setup [ts]. *)
  val verify_single :
    trusted_setup ->
    commitment ->
    point:Scalar.t ->
    evaluation:Scalar.t ->
    proof_single ->
    bool

  (* TODO: raise error if index i is incorrect *)

  (** [prove_slot_segments ts p slot_segment_index] where [p] is the output of
      [polynomial_from_bytes slot], returns proofs for the slot segment] whose
      index is [slot_segment_index], using the trusted setup [ts]. *)
  val prove_slot_segment :
    trusted_setup ->
    polynomial ->
    int ->
    (proof_slot_segment, [> `Degree_exceeds_srs_length of string]) result

  (** [verify_slot_segment cm slot_segment proof] returns true if the [proof]
      certifies that the [slot_segment] is indeed included in the slot committed
      with commitment [cm],  using the trusted setup [ts]. *)
  val verify_slot_segment :
    trusted_setup -> commitment -> slot_segment -> proof_slot_segment -> bool
end

module Make (Params : CONFIGURATION) : DAL_cryptobox_sig = struct
  open Kate_amortized

  (* Scalars are elements of the prime field Fr from BLS. *)
  module Scalar = Bls12_381.Fr

  (* Operations on vector of scalars *)
  module Evaluations = Bls12_381_polynomial.Polynomial.Evaluations

  (* Domains for the Fast Fourier Transform (FTT). *)
  module Domains = Bls12_381_polynomial.Polynomial.Domain
  module Polynomial = Bls12_381_polynomial.Polynomial.Polynomial
  module IntMap = Tezos_error_monad.TzLwtreslib.Map.Make (Int)
  module Carray = Bls12_381_polynomial.Carray

  type polynomial = Polynomial.t

  type commitment = Kate_amortized.commitment

  type proof_shard = Kate_amortized.proof

  type proof_degree = Bls12_381.G1.t

  type proof_single = Bls12_381.G1.t

  type proof_slot_segment = Bls12_381.G1.t

  type slot_segment = int * bytes

  type share = Scalar.t array

  type shards_map = share IntMap.t

  type shard = int * share

  type shards_proofs_precomputation =
    Scalar.t array * proof_slot_segment array array

  type trusted_setup = {
    srs_g1 : Bls12_381.G1.t array;
    srs_g2 : Bls12_381.G2.t array;
    kate_amortized_srs_g2_shards : Bls12_381.G2.t;
    kate_amortized_srs_g2_segments : Bls12_381.G2.t;
  }

  type trusted_setup_files = {
    srs_g1_file : string;
    srs_g2_file : string;
    log_size : int;
  }

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
  end

  (* Number of bytes fitting in a Scalar.t. Since scalars are integer modulo
     r~2^255, we restrict ourselves to 248-bit integers (31 bytes). *)
  let scalar_bytes_amount = Scalar.size_in_bytes - 1

  (* k and n are the parameters of the erasure code. *)
  let k =
    1 lsl Z.(log2up (of_int Params.slot_size / of_int scalar_bytes_amount))

  let n = Params.redundancy_factor * k

  let erasure_encoding_length = n

  (* Memoize intermediate domains for the FFTs. *)
  let intermediate_domains : Evaluations.domain IntMap.t ref = ref IntMap.empty

  (* Builds group of nth roots of unity, a valid domain for the FFT. *)
  let make_domain n = Domains.build ~log:Z.(log2up (of_int n))

  (* Domain for the FFT on slots as polynomials to be erasure encoded. *)
  let domain_k = make_domain k

  let domain_2k = make_domain (2 * k)

  (* Domain for the FFT on erasure encoded slots (as polynomials). *)
  let domain_n = make_domain n

  (* Length of a shard in terms of scalar elements. *)
  let shard_size = Params.(n / shards_amount)

  (* Number of slot segments. *)
  let nb_segments = Params.(slot_size / slot_segment_size)

  let segment_len = Int.div Params.slot_segment_size scalar_bytes_amount + 1

  let remaining_bytes = Params.slot_segment_size mod scalar_bytes_amount

  (* Log of the number of evaluations that constitute an erasure encoded
     polynomial. *)
  let evaluations_log = Z.(log2 (of_int n))

  (* Log of the number of evaluations contained in a shard. *)
  let evaluations_per_proof_log = Z.(log2 (of_int shard_size))

  (* Log of th number of shards proofs. *)
  let proofs_log = evaluations_log - evaluations_per_proof_log

  (* Check code parameters. *)
  let () =
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

  let build_trusted_setup_instance = function
    | `Unsafe_for_test_only ->
        let module Scalar = Bls12_381.Fr in
        let build_array init next len =
          let xi = ref init in
          Array.init len (fun _ ->
              let i = !xi in
              xi := next !xi ;
              i)
        in
        let create_srs :
            type t.
            (module Bls12_381.CURVE with type t = t) ->
            int ->
            Scalar.t ->
            t array =
         fun (module G) d x -> build_array G.(copy one) (fun g -> G.(mul g x)) d
        in
        let secret =
          Scalar.of_string
            "20812168509434597367146703229805575690060615791308155437936410982393987532344"
        in
        {
          srs_g1 = create_srs (module Bls12_381.G1) k secret;
          srs_g2 = create_srs (module Bls12_381.G2) k secret;
          kate_amortized_srs_g2_shards =
            Kate_amortized.gen_srs_g2
              ~l:(1 lsl evaluations_per_proof_log)
              secret;
          kate_amortized_srs_g2_segments =
            Kate_amortized.gen_srs_g2
              ~l:(1 lsl Z.(log2up (of_int segment_len)))
              secret;
        }
    | `Files {srs_g1_file; srs_g2_file; log_size} ->
        assert (k < 1 lsl log_size) ;
        let srs_g1 =
          Bls12_381_polynomial.Srs.M.(to_array (load_from_file srs_g1_file k))
        in
        let import_srs_g2 d srsfile =
          let g2_size_compressed = Bls12_381.G2.size_in_bytes / 2 in
          let buf = Bytes.create g2_size_compressed in
          let read ic =
            Stdlib.really_input ic buf 0 g2_size_compressed ;
            Bls12_381.G2.of_compressed_bytes_exn buf
          in
          let ic = open_in srsfile in
          try
            if in_channel_length ic < d * g2_size_compressed then
              raise
                (Failure
                   (Printf.sprintf "SRS asked (%d) too big for %s" d srsfile)) ;
            let res = Array.init d (fun _ -> read ic) in
            close_in ic ;
            res
          with e ->
            close_in ic ;
            raise e
        in
        let srs_g2 = import_srs_g2 k srs_g2_file in
        {
          srs_g1;
          srs_g2;
          kate_amortized_srs_g2_shards =
            Array.get srs_g2 (1 lsl evaluations_per_proof_log);
          kate_amortized_srs_g2_segments =
            Array.get srs_g2 (1 lsl Z.(log2up (of_int segment_len)));
        }

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

  (* We encode by segments of [slot_segment_size] bytes each.
     The segments are arranged in cosets to evaluate in batch with Kate
       amortized. *)
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

  let eval_coset_array eval segment =
    let coset =
      Array.init
        (1 lsl Z.(log2up (of_int segment_len)))
        (fun _ -> Scalar.(copy zero))
    in
    for elt = 0 to segment_len - 1 do
      let idx = (elt * nb_segments) + segment in
      coset.(elt) <- Array.get eval idx
    done ;
    coset

  let eval_coset eval slot offset segment =
    for elt = 0 to segment_len - 1 do
      let idx = (elt * nb_segments) + segment in
      let coeff = Scalar.to_bytes (Array.get eval idx) in
      if elt = segment_len - 1 then (
        Bytes.blit coeff 0 slot !offset remaining_bytes ;
        offset := !offset + remaining_bytes)
      else (
        Bytes.blit coeff 0 slot !offset scalar_bytes_amount ;
        offset := !offset + scalar_bytes_amount)
    done

  (* The segments are arranged in cosets to evaluate in batch with Kate
     amortized. *)
  let polynomial_to_bytes p =
    let eval = Evaluations.evaluation_fft2 domain_k p in
    let slot = Bytes.init Params.slot_size (fun _ -> '0') in
    let offset = ref 0 in
    for segment = 0 to nb_segments - 1 do
      eval_coset eval slot offset segment
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

  let commit trusted_setup p =
    commit' (module Bls12_381.G1) p trusted_setup.srs_g1

  (* p(X) of degree n. Max degree that can be committed: d, which is also the
     SRS's length - 1. We take d = k - 1 since we don't want to commit
     polynomials with degree greater than polynomials to be erasure-encoded.

     We consider the bilinear groups (G_1, G_2, G_T) with G_1=<g> and G_2=<h>.
     - Commit (p X^{d-n}) such that deg (p X^{d-n}) = d the max degree
     that can be committed
     - Verify: checks if e(commit(p), commit(X^{d-n})) = e(commit(p X^{d-n}), h)
     using the commitments for p and p X^{d-n}, and computing the commitment for
     X^{d-n} on G_2.*)

  let prove_degree trusted_setup p n =
    let d = k - 1 in
    commit'
      (module Bls12_381.G1)
      (Polynomial.mul
         (Polynomial.of_coefficients [(Scalar.(copy one), d - n)])
         p)
      trusted_setup.srs_g1

  let verify_degree trusted_setup cm proof n =
    let open Result_syntax in
    let open Bls12_381 in
    let d = k - 1 in
    let* commit_xk =
      commit'
        (module G2)
        (Polynomial.of_coefficients [(Scalar.(copy one), d - n)])
        trusted_setup.srs_g2
    in
    Ok
      (Pairing.pairing_check [(cm, commit_xk); (proof, G2.(negate (copy one)))])

  let eval_to_array e = Array.init (Domains.length e) (Domains.get e)

  let precompute_shards_proofs trusted_setup =
    let eval, m =
      Kate_amortized.preprocess_multi_reveals
        ~chunk_len:evaluations_per_proof_log
        ~degree:k
        (trusted_setup.srs_g1, trusted_setup.kate_amortized_srs_g2_shards)
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
    let (_ : unit option) = In_channel.really_input chan data 0 len in
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

  let verify_shard trusted_setup cm (shard_index, shard_evaluations) proof =
    let d_n = Kate_amortized.Domain.build ~log:evaluations_log in
    let domain = Kate_amortized.Domain.build ~log:evaluations_per_proof_log in
    Kate_amortized.verify
      cm
      (trusted_setup.srs_g1, trusted_setup.kate_amortized_srs_g2_shards)
      domain
      (Kate_amortized.Domain.get d_n shard_index, shard_evaluations)
      proof

  let prove_single trusted_setup p z =
    let q = Polynomial.(division_x_z (p - constant (evaluate p z)) z) in
    commit' (module Bls12_381.G1) q trusted_setup.srs_g1

  let verify_single trusted_setup cm ~point ~evaluation proof =
    let h_secret = Array.get trusted_setup.srs_g2 1 in
    Bls12_381.(
      Pairing.pairing_check
        [
          ( G1.(add cm (negate (mul (copy one) evaluation))),
            G2.(negate (copy one)) );
          (proof, G2.(add h_secret (negate (mul (copy one) point))));
        ])

  (* Assumptions:
      - Polynomial.degree p = k
      - (x^l - z) | p(x)
     Computes the quotient of the division of p(x) by (x^l - z). *)
  let compute_quotient p l z =
    let div = Array.init (k - l + 1) (fun _ -> Scalar.(copy zero)) in
    let i = ref 0 in
    (* Computes 1/(x^l - z) mod x^{k - l + 1}
       = \sum_{i=0}^{+\infty} -z^{-1}^{i+1} X^{i\times l} mod x^{k - l + 1}. *)
    while !i * l < k - l + 1 do
      div.(!i * l) <-
        Scalar.negate (Scalar.inverse_exn (Scalar.pow z (Z.of_int (!i + 1)))) ;
      i := !i + 1
    done ;
    let div = Polynomial.of_dense div in
    (* p(x) * 1/(x^l - z) mod x^{k - l + 1} = q(x) since deg q <= k - l. *)
    fft_mul domain_2k [p; div] |> Polynomial.copy ~len:(k - l + 1)

  let prove_slot_segment trusted_setup p slot_segment_index =
    let l = 1 lsl Z.(log2up (of_int segment_len)) in
    let wi = Domains.get domain_k slot_segment_index in
    let domain = Domains.build ~log:Z.(log2up (of_int segment_len)) in
    let eval_p = Evaluations.evaluation_fft2 domain_k p in
    let eval_coset = eval_coset_array eval_p slot_segment_index in
    let remainder =
      Kate_amortized.interpolation_h_poly wi domain eval_coset
      |> Array.of_list |> Polynomial.of_dense
    in
    let quotient =
      compute_quotient
        (Polynomial.sub p remainder)
        l
        (Scalar.pow wi (Z.of_int l))
    in
    commit trusted_setup quotient

  (* Parses the [slot_segment] to get the evaluations that it contains. The
     evaluation points are given by the [slot_segment_index]. *)
  let verify_slot_segment trusted_setup cm (slot_segment_index, slot_segment)
      proof =
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
      cm
      (trusted_setup.srs_g1, trusted_setup.kate_amortized_srs_g2_segments)
      domain
      (Domains.get domain_k slot_segment_index, slot_segment_evaluations)
      proof
end
