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
include Cryptobox_intf
module Base58 = Tezos_crypto.Base58
module Srs_g1 = Bls12_381_polynomial.Srs.Srs_g1
module Srs_g2 = Bls12_381_polynomial.Srs.Srs_g2

type error += Failed_to_load_trusted_setup of string

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.trusted_setup_loading_failed"
    ~title:"Trusted setup loading failed"
    ~description:"Trusted setup failed to load"
    ~pp:(fun ppf msg ->
      Format.fprintf ppf "Trusted setup failed to load: %s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function
      | Failed_to_load_trusted_setup parameter -> Some parameter | _ -> None)
    (fun parameter -> Failed_to_load_trusted_setup parameter)

type initialisation_parameters = {srs_g1 : Srs_g1.t; srs_g2 : Srs_g2.t}

(* Initialisation parameters are supposed to be instantiated once. *)
let initialisation_parameters = ref None

type error += Dal_initialisation_twice

(* This function is expected to be called once. *)
let load_parameters parameters =
  let open Result_syntax in
  match !initialisation_parameters with
  | None ->
      initialisation_parameters := Some parameters ;
      return_unit
  | Some _ -> fail [Dal_initialisation_twice]

(* FIXME https://gitlab.com/tezos/tezos/-/issues/3400

   An integrity check is run to ensure the validity of the files. *)

let initialisation_parameters_from_files ~g1_path ~g2_path =
  let open Lwt_result_syntax in
  (* FIXME https://gitlab.com/tezos/tezos/-/issues/3409

     The `21` constant is the logarithmic size of the file. Can this
     constant be recomputed? Even though it should be determined by
     the integrity check. *)
  let logarithmic_size = 21 in
  let to_bigstring path =
    let open Lwt_syntax in
    let* fd = Lwt_unix.openfile path [Unix.O_RDONLY] 0o440 in
    Lwt.finalize
      (fun () ->
        return
          (Lwt_bytes.map_file
             ~fd:(Lwt_unix.unix_file_descr fd)
             ~shared:false
             ~size:(1 lsl logarithmic_size)
             ()))
      (fun () -> Lwt_unix.close fd)
  in
  let*! srs_g1_bigstring = to_bigstring g1_path in
  let*! srs_g2_bigstring = to_bigstring g2_path in
  match
    let open Result_syntax in
    let* srs_g1 = Srs_g1.of_bigstring srs_g1_bigstring in
    let* srs_g2 = Srs_g2.of_bigstring srs_g2_bigstring in
    return (srs_g1, srs_g2)
  with
  | Error (`End_of_file s) -> tzfail (Failed_to_load_trusted_setup s)
  | Error (`Invalid_point p) ->
      tzfail
        (Failed_to_load_trusted_setup (Printf.sprintf "Invalid point %i" p))
  | Ok (srs_g1, srs_g2) -> return {srs_g1; srs_g2}

(* The srs is made of the initialisation_parameters and two
   well-choosen points. Building the srs from the initialisation
   parameters is almost cost-free. *)
type srs = {
  raw : initialisation_parameters;
  kate_amortized_srs_g2_shards : Bls12_381.G2.t;
  kate_amortized_srs_g2_pages : Bls12_381.G2.t;
}

module Inner = struct
  (* Scalars are elements of the prime field Fr from BLS. *)
  module Scalar = Bls12_381.Fr
  module Polynomials = Bls12_381_polynomial.Polynomial

  (* Operations on vector of scalars *)
  module Evaluations = Bls12_381_polynomial.Evaluations

  (* Domains for the Fast Fourier Transform (FTT). *)
  module Domains = Bls12_381_polynomial.Domain

  type slot = bytes

  type scalar = Scalar.t

  type polynomial = Polynomials.t

  type commitment = Bls12_381.G1.t

  type shard_proof = Bls12_381.G1.t

  type commitment_proof = Bls12_381.G1.t

  type _proof_single = Bls12_381.G1.t

  type page_proof = Bls12_381.G1.t

  type page = bytes

  type share = Scalar.t array

  type shard = {index : int; share : share}

  type shards_proofs_precomputation = Scalar.t array * page_proof array array

  module Encoding = struct
    open Data_encoding

    let fr_encoding =
      conv
        Bls12_381.Fr.to_bytes
        Bls12_381.Fr.of_bytes_exn
        (Fixed.bytes Bls12_381.Fr.size_in_bytes)

    (* FIXME https://gitlab.com/tezos/tezos/-/issues/3391

       The commitment is not bounded. *)
    let g1_encoding =
      conv
        Bls12_381.G1.to_compressed_bytes
        Bls12_381.G1.of_compressed_bytes_exn
        bytes

    let _proof_shards_encoding = g1_encoding

    let _proof_single_encoding = g1_encoding

    let page_proof_encoding = g1_encoding

    let share_encoding = array fr_encoding

    let shard_encoding =
      conv
        (fun {index; share} -> (index, share))
        (fun (index, share) -> {index; share})
        (tup2 int31 share_encoding)

    let shards_proofs_precomputation_encoding =
      tup2 (array fr_encoding) (array (array g1_encoding))
  end

  include Encoding

  module Commitment = struct
    type t = commitment

    type Base58.data += Data of t

    let zero = Bls12_381.G1.zero

    let equal = Bls12_381.G1.eq

    let commitment_to_bytes = Bls12_381.G1.to_compressed_bytes

    let commitment_of_bytes_opt = Bls12_381.G1.of_compressed_bytes_opt

    let commitment_of_bytes_exn bytes =
      match Bls12_381.G1.of_compressed_bytes_opt bytes with
      | None ->
          Format.kasprintf Stdlib.failwith "Unexpected data (DAL commitment)"
      | Some commitment -> commitment

    (* We divide by two because we use the compressed representation. *)
    let commitment_size = Bls12_381.G1.size_in_bytes / 2

    let to_string commitment = commitment_to_bytes commitment |> Bytes.to_string

    let of_string_opt str = commitment_of_bytes_opt (String.to_bytes str)

    let b58check_encoding =
      Base58.register_encoding
        ~prefix:Base58.Prefix.slot_header
        ~length:commitment_size
        ~to_raw:to_string
        ~of_raw:of_string_opt
        ~wrap:(fun x -> Data x)

    let raw_encoding =
      let open Data_encoding in
      conv
        commitment_to_bytes
        commitment_of_bytes_exn
        (Fixed.bytes commitment_size)

    include Tezos_crypto.Helpers.Make (struct
      type t = commitment

      let name = "DAL_commitment"

      let title = "Commitment representation for the DAL"

      let b58check_encoding = b58check_encoding

      let raw_encoding = raw_encoding

      let compare = compare

      let equal = ( = )

      let hash _ =
        (* The commitment is not hashed. This is ensured by the
           function exposed. We only need the Base58 encoding and the
           rpc_arg. *)
        assert false

      let seeded_hash _ _ =
        (* Same argument. *)
        assert false
    end)
  end

  module Commitment_proof = struct
    let zero = Bls12_381.G1.zero

    let to_bytes = Bls12_381.G1.to_compressed_bytes

    let of_bytes_exn bytes =
      match Bls12_381.G1.of_compressed_bytes_opt bytes with
      | None ->
          Format.kasprintf
            Stdlib.failwith
            "Unexpected data (DAL commitment proof)"
      | Some proof -> proof

    (* We divide by two because we use the compressed representation. *)
    let size = Bls12_381.G1.size_in_bytes / 2

    let raw_encoding =
      let open Data_encoding in
      conv to_bytes of_bytes_exn (Fixed.bytes size)

    let encoding = raw_encoding
  end

  (* Number of bytes fitting in a Scalar.t. Since scalars are integer modulo
     r~2^255, we restrict ourselves to 248-bit integers (31 bytes). *)
  let scalar_bytes_amount = Scalar.size_in_bytes - 1

  (* Builds group of nth roots of unity, a valid domain for the FFT. *)
  let make_domain n = Domains.build_power_of_two Z.(log2up (of_int n))

  type t = {
    redundancy_factor : int;
    slot_size : int;
    page_size : int;
    number_of_shards : int;
    k : int;
    n : int;
    (* k and n are the parameters of the erasure code. *)
    domain_k : Domains.t;
    (* Domain for the FFT on slots as polynomials to be erasure encoded. *)
    domain_2k : Domains.t;
    domain_n : Domains.t;
    (* Domain for the FFT on erasure encoded slots (as polynomials). *)
    shard_size : int;
    (* Length of a shard in terms of scalar elements. *)
    pages_per_slot : int;
    (* Number of slot pages. *)
    page_length : int;
    remaining_bytes : int;
    evaluations_log : int;
    (* Log of the number of evaluations that constitute an erasure encoded
       polynomial. *)
    evaluations_per_proof_log : int;
    (* Log of the number of evaluations contained in a shard. *)
    proofs_log : int; (* Log of th number of shards proofs. *)
    srs : srs;
  }

  let ensure_validity t =
    let open Result_syntax in
    let srs_size = Srs_g1.size t.srs.raw.srs_g1 in
    let srs_size_g2 = Srs_g2.size t.srs.raw.srs_g2 in
    let is_pow_of_two x =
      let logx = Z.(log2 (of_int x)) in
      1 lsl logx = x
    in
    if
      not
        (is_pow_of_two t.slot_size && is_pow_of_two t.page_size
       && is_pow_of_two t.n)
    then
      (* According to the specification the lengths of a slot page are
         in MiB *)
      fail (`Fail "Wrong slot size: expected MiB")
    else if not (Z.(log2 (of_int t.n)) <= 32 && is_pow_of_two t.k && t.n > t.k)
    then
      (* n must be at most 2^32, the biggest subgroup of 2^i roots of unity in the
         multiplicative group of Fr, because the FFTs operate on such groups. *)
      fail (`Fail "Wrong computed size for n")
    else if t.k > srs_size then
      (* the committed polynomials have degree t.k - 1 at most,
         so t.k coefficients. *)
      fail
        (`Fail
          (Format.asprintf
             "SRS on G1 size is too small. Expected more than %d. Got %d"
             t.k
             srs_size))
    else if t.k > Srs_g2.size t.srs.raw.srs_g2 then
      fail
        (`Fail
          (Format.asprintf
             "SRS on G2 size is too small. Expected more than %d. Got %d"
             t.k
             srs_size_g2))
    else return t

  let slot_as_polynomial_length ~slot_size =
    1 lsl Z.(log2up (succ (of_int slot_size / of_int scalar_bytes_amount)))

  type parameters = {
    redundancy_factor : int;
    page_size : int;
    slot_size : int;
    number_of_shards : int;
  }

  let parameters_encoding =
    let open Data_encoding in
    conv
      (fun {redundancy_factor; page_size; slot_size; number_of_shards} ->
        (redundancy_factor, page_size, slot_size, number_of_shards))
      (fun (redundancy_factor, page_size, slot_size, number_of_shards) ->
        {redundancy_factor; page_size; slot_size; number_of_shards})
      (obj4
         (req "redundancy_factor" uint8)
         (req "page_size" uint16)
         (req "slot_size" int31)
         (req "number_of_shards" uint16))

  let pages_per_slot {slot_size; page_size; _} = slot_size / page_size

  (* Error cases of this functions are not encapsulated into
     `tzresult` for modularity reasons. *)
  let make
      ({redundancy_factor; slot_size; page_size; number_of_shards} as
      parameters) =
    let open Result_syntax in
    let k = slot_as_polynomial_length ~slot_size in
    let n = redundancy_factor * k in
    let shard_size = n / number_of_shards in
    let evaluations_log = Z.(log2 (of_int n)) in
    let evaluations_per_proof_log = Z.(log2 (of_int shard_size)) in
    let page_length = Int.div page_size scalar_bytes_amount + 1 in
    let* srs =
      match !initialisation_parameters with
      | None -> fail (`Fail "Dal_cryptobox.make: DAL was not initialisated.")
      | Some raw ->
          return
            {
              raw;
              kate_amortized_srs_g2_shards =
                Srs_g2.get raw.srs_g2 (1 lsl evaluations_per_proof_log);
              kate_amortized_srs_g2_pages =
                Srs_g2.get raw.srs_g2 (1 lsl Z.(log2up (of_int page_length)));
            }
    in
    let t =
      {
        redundancy_factor;
        slot_size;
        page_size;
        number_of_shards;
        k;
        n;
        domain_k = make_domain k;
        domain_2k = make_domain (2 * k);
        domain_n = make_domain n;
        shard_size;
        pages_per_slot = pages_per_slot parameters;
        page_length;
        remaining_bytes = page_size mod scalar_bytes_amount;
        evaluations_log;
        evaluations_per_proof_log;
        proofs_log = evaluations_log - evaluations_per_proof_log;
        srs;
      }
    in
    ensure_validity t

  let parameters
      ({redundancy_factor; slot_size; page_size; number_of_shards; _} : t) =
    {redundancy_factor; slot_size; page_size; number_of_shards}

  let polynomial_degree = Polynomials.degree

  let polynomial_evaluate = Polynomials.evaluate

  let fft_mul d ps =
    let open Evaluations in
    let evaluations = List.map (evaluation_fft d) ps in
    interpolation_fft d (mul_c ~evaluations ())

  (* We encode by pages of [page_size] bytes each.  The pages
     are arranged in cosets to evaluate in batch with Kate
     amortized. *)
  let polynomial_from_bytes' (t : t) slot =
    if Bytes.length slot <> t.slot_size then
      Error
        (`Slot_wrong_size
          (Printf.sprintf "message must be %d bytes long" t.slot_size))
    else
      let offset = ref 0 in
      let res = Array.init t.k (fun _ -> Scalar.(copy zero)) in
      for page = 0 to t.pages_per_slot - 1 do
        for elt = 0 to t.page_length - 1 do
          (* [!offset >= t.slot_size] because we don't want to read past
             the buffer [slot] bounds. *)
          if !offset >= t.slot_size then ()
          else if elt = t.page_length - 1 then (
            let dst = Bytes.create t.remaining_bytes in
            Bytes.blit slot !offset dst 0 t.remaining_bytes ;
            offset := !offset + t.remaining_bytes ;
            res.((elt * t.pages_per_slot) + page) <- Scalar.of_bytes_exn dst)
          else
            let dst = Bytes.create scalar_bytes_amount in
            Bytes.blit slot !offset dst 0 scalar_bytes_amount ;
            offset := !offset + scalar_bytes_amount ;
            res.((elt * t.pages_per_slot) + page) <- Scalar.of_bytes_exn dst
        done
      done ;
      Ok res

  let polynomial_from_slot t slot =
    let open Result_syntax in
    let* data = polynomial_from_bytes' t slot in
    Ok (Evaluations.interpolation_fft2 t.domain_k data)

  let eval_coset t eval slot offset page =
    for elt = 0 to t.page_length - 1 do
      let idx = (elt * t.pages_per_slot) + page in
      let coeff = Scalar.to_bytes (Array.get eval idx) in
      if elt = t.page_length - 1 then (
        Bytes.blit coeff 0 slot !offset t.remaining_bytes ;
        offset := !offset + t.remaining_bytes)
      else (
        Bytes.blit coeff 0 slot !offset scalar_bytes_amount ;
        offset := !offset + scalar_bytes_amount)
    done

  (* The pages are arranged in cosets to evaluate in batch with Kate
     amortized. *)
  let polynomial_to_bytes t p =
    let eval =
      Evaluations.evaluation_fft t.domain_k p |> Evaluations.to_array
    in
    let slot = Bytes.init t.slot_size (fun _ -> '0') in
    let offset = ref 0 in
    for page = 0 to t.pages_per_slot - 1 do
      eval_coset t eval slot offset page
    done ;
    slot

  let encode t p =
    Evaluations.evaluation_fft t.domain_n p |> Evaluations.to_array

  (* The shards are arranged in cosets to evaluate in batch with Kate
     amortized. *)
  let shards_from_polynomial t p =
    let codeword = encode t p in
    let len_shard = t.n / t.number_of_shards in
    let rec loop index seq =
      if index = t.number_of_shards then seq
      else
        let share = Array.init len_shard (fun _ -> Scalar.(copy zero)) in
        for j = 0 to len_shard - 1 do
          share.(j) <- codeword.((t.number_of_shards * j) + index)
        done ;
        loop (index + 1) (Seq.cons {index; share} seq)
    in
    loop 0 Seq.empty

  (* Computes the polynomial N(X) := \sum_{i=0}^{k-1} n_i x_i^{-1} X^{z_i}. *)
  let compute_n t eval_a' shards =
    let w = Domains.get t.domain_n 1 in
    let n_poly = Array.init t.n (fun _ -> Scalar.(copy zero)) in
    Seq.iter
      (fun {index; share} ->
        for j = 0 to Array.length share - 1 do
          let c_i = share.(j) in
          let z_i = (t.number_of_shards * j) + index in
          let x_i = Scalar.pow w (Z.of_int z_i) in
          let tmp = Evaluations.get eval_a' z_i in
          Scalar.mul_inplace tmp tmp x_i ;
          (* The call below never fails by construction, so we don't
             catch exceptions *)
          Scalar.inverse_exn_inplace tmp tmp ;
          Scalar.mul_inplace tmp tmp c_i ;
          n_poly.(z_i) <- tmp
        done)
      shards ;
    n_poly

  let encoded_share_size t =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4289
       Improve shard size computation *)
    let share_scalar_len = t.n / t.number_of_shards in
    (share_scalar_len * Scalar.size_in_bytes) + 4

  let polynomial_from_shards t shards =
    if t.k > Seq.length shards * t.shard_size then
      Error
        (`Not_enough_shards
          (Printf.sprintf
             "there must be at least %d shards to decode"
             (t.k / t.shard_size)))
    else
      (* 1. Computing A(x) = prod_{i=0}^{k-1} (x - w^{z_i}).
         Let w be a primitive nth root of unity and
         Ω_0 = {w^{number_of_shards j}}_{j=0 to (n/number_of_shards)-1}
         be the (n/number_of_shards)-th roots of unity and Ω_i = w^i Ω_0.

         Together, the Ω_i's form a partition of the subgroup of the n-th roots
         of unity: 𝕌_n = disjoint union_{i ∈ {0, ..., number_of_shards-1}} Ω_i.

         Let Z_j := Prod_{w ∈ Ω_j} (x − w). For a random set of shards
         S⊆{0, ..., number_of_shards-1} of length k/shard_size, we reorganize the
         product A(x) = Prod_{i=0}^{k-1} (x − w^{z_i}) into
         A(x) = Prod_{j ∈ S} Z_j.

         Moreover, Z_0 = x^|Ω_0| - 1 since x^|Ω_0| - 1 contains all roots of Z_0
         and conversely. Multiplying each term of the polynomial by the root w^j
         entails Z_j = x^|Ω_0| − w^{j*|Ω_0|}.

         The intermediate products Z_j have a lower Hamming weight (=2) than
         when using other ways of grouping the z_i's into shards.

         This also reduces the depth of the recursion tree of the poly_mul
         function from log(k) to log(number_of_shards), so that the decoding time
         reduces from O(k*log^2(k) + n*log(n)) to O(n*log(n)). *)
      let mul acc i =
        Polynomials.mul_xn
          acc
          t.shard_size
          (Scalar.negate (Domains.get t.domain_n (i * t.shard_size)))
      in

      let partition_products seq =
        Seq.fold_left
          (fun (l, r) {index; _} -> (mul r index, l))
          (Polynomials.one, Polynomials.one)
          seq
      in

      let shards =
        (* We always consider the first k codeword vector components. *)
        Seq.take (t.k / t.shard_size) shards
      in

      let p1, p2 = partition_products shards in

      let a_poly = fft_mul t.domain_2k [p1; p2] in

      (* 2. Computing formal derivative of A(x). *)
      let a' = Polynomials.derivative a_poly in

      (* 3. Computing A'(w^i) = A_i(w^i). *)
      let eval_a' = Evaluations.evaluation_fft t.domain_n a' in

      (* 4. Computing N(x). *)
      let n_poly = compute_n t eval_a' shards in

      (* 5. Computing B(x). *)
      let b = Evaluations.interpolation_fft2 t.domain_n n_poly in
      let b = Polynomials.copy ~len:t.k b in
      Polynomials.mul_by_scalar_inplace b (Scalar.of_int t.n) b ;

      (* 6. Computing Lagrange interpolation polynomial P(x). *)
      let p = fft_mul t.domain_2k [a_poly; b] in
      let p = Polynomials.copy ~len:t.k p in
      Polynomials.opposite_inplace p ;
      Ok p

  let commit t p = Srs_g1.pippenger t.srs.raw.srs_g1 p

  (* p(X) of degree n. Max degree that can be committed: d, which is also the
     SRS's length - 1. We take d = t.k - 1 since we don't want to commit
     polynomials with degree greater than polynomials to be erasure-encoded.

     We consider the bilinear groups (G_1, G_2, G_T) with G_1=<g> and G_2=<h>.
     - Commit (p X^{d-n}) such that deg (p X^{d-n}) = d the max degree
     that can be committed
     - Verify: checks if e(commit(p), commit(X^{d-n})) = e(commit(p X^{d-n}), h)
     using the commitments for p and p X^{d-n}, and computing the commitment for
     X^{d-n} on G_2. *)

  (* Proves that degree(p) < t.k *)
  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4192

     Generalize this function to pass the slot_size in parameter. *)
  let prove_commitment (t : t) p =
    let max_allowed_committed_poly_degree = t.k - 1 in
    let max_committable_degree = Srs_g1.size t.srs.raw.srs_g1 - 1 in
    let offset_monomial_degree =
      max_committable_degree - max_allowed_committed_poly_degree
    in
    (* Note: this reallocates a buffer of size (Srs_g1.size t.srs.raw.srs_g1)
       (2^21 elements in practice), so roughly 100MB. We can get rid of the
       allocation by giving an offset for the SRS in Pippenger. *)
    let p_with_offset =
      Polynomials.mul_xn p offset_monomial_degree Scalar.(copy zero)
    in
    (* proof = commit(p X^offset_monomial_degree), with deg p < t.k *)
    commit t p_with_offset

  (* Verifies that the degree of the committed polynomial is < t.k *)
  let verify_commitment (t : t) cm proof =
    let max_allowed_committed_poly_degree = t.k - 1 in
    let max_committable_degree = Srs_g1.size t.srs.raw.srs_g1 - 1 in
    let offset_monomial_degree =
      max_committable_degree - max_allowed_committed_poly_degree
    in
    let committed_offset_monomial =
      (* This [get] cannot raise since
         [offset_monomial_degree <= t.k <= Srs_g2.size t.srs.raw.srs_g2]. *)
      Srs_g2.get t.srs.raw.srs_g2 offset_monomial_degree
    in
    let open Bls12_381 in
    (* checking that cm * committed_offset_monomial = proof *)
    Pairing.pairing_check
      [(cm, committed_offset_monomial); (proof, G2.(negate (copy one)))]

  let inverse domain =
    let n = Array.length domain in
    Array.init n (fun i ->
        if i = 0 then Bls12_381.Fr.(copy one) else Array.get domain (n - i))

  let diff_next_power_of_two x =
    let logx = Z.log2 (Z.of_int x) in
    if 1 lsl logx = x then 0 else (1 lsl (logx + 1)) - x

  let is_pow_of_two x =
    let logx = Z.log2 (Z.of_int x) in
    1 lsl logx = x

  (* Implementation of fast amortized Kate proofs
     https://github.com/khovratovich/Kate/blob/master/Kate_amortized.pdf). *)

  (* Precompute first part of Toeplitz trick, which doesn't depends on the
     polynomial’s coefficients. *)
  let preprocess_multi_reveals ~chunk_len ~degree srs =
    let open Bls12_381 in
    let l = 1 lsl chunk_len in
    let k =
      let ratio = degree / l in
      let log_inf = Z.log2 (Z.of_int ratio) in
      if 1 lsl log_inf < ratio then log_inf else log_inf + 1
    in
    let domain = Domains.build_power_of_two k |> Domains.inverse |> inverse in
    let precompute_srsj j =
      let quotient = (degree - j) / l in
      let padding = diff_next_power_of_two (2 * quotient) in
      let points =
        Array.init
          ((2 * quotient) + padding)
          (fun i ->
            if i < quotient then
              G1.copy (Srs_g1.get srs (degree - j - ((i + 1) * l)))
            else G1.(copy zero))
      in
      G1.fft_inplace ~domain ~points ;
      points
    in
    (domain, Array.init l precompute_srsj)

  (** Generate proofs of part 3.2.
  n, r are powers of two, m = 2^(log2(n)-1)
  coefs are f polynomial’s coefficients [f₀, f₁, f₂, …, fm-1]
  domain2m is the set of 2m-th roots of unity, used for Toeplitz computation
  (domain2m, precomputed_srs_part) = preprocess_multi_reveals r n m srs1
   *)
  let multiple_multi_reveals ~chunk_len ~chunk_count ~degree
      ~preprocess:(domain2m, precomputed_srs_part) coefs =
    let open Bls12_381 in
    let n = chunk_len + chunk_count in
    assert (2 <= chunk_len) ;
    assert (chunk_len < n) ;
    assert (is_pow_of_two degree) ;
    assert (1 lsl chunk_len < degree) ;
    assert (degree <= 1 lsl n) ;
    let l = 1 lsl chunk_len in
    (* We don’t need the first coefficient f₀. *)
    let compute_h_j j =
      let rest = (degree - j) mod l in
      let quotient = (degree - j) / l in
      (* Padding in case quotient is not a power of 2 to get proper fft in
         Toeplitz matrix part. *)
      let padding = diff_next_power_of_two (2 * quotient) in
      (* fm, 0, …, 0, f₁, f₂, …, fm-1 *)
      let points =
        Array.init
          ((2 * quotient) + padding)
          (fun i ->
            if i <= quotient + (padding / 2) then Scalar.(copy zero)
            else Scalar.copy coefs.(rest + ((i - (quotient + padding)) * l)))
      in
      if j <> 0 then points.(0) <- Scalar.copy coefs.(degree - j) ;
      Scalar.fft_inplace ~domain:domain2m ~points ;
      Array.map2 G1.mul precomputed_srs_part.(j) points
    in
    let sum = compute_h_j 0 in
    let rec sum_hj j =
      if j = l then ()
      else
        let hj = compute_h_j j in
        (* sum.(i) <- sum.(i) + hj.(i) *)
        Array.iteri (fun i hij -> sum.(i) <- G1.add sum.(i) hij) hj ;
        sum_hj (j + 1)
    in
    sum_hj 1 ;

    (* Toeplitz matrix-vector multiplication *)
    G1.ifft_inplace ~domain:(inverse domain2m) ~points:sum ;
    let hl = Array.sub sum 0 (Array.length domain2m / 2) in

    let phidomain = Domains.build_power_of_two chunk_count in
    let phidomain = inverse (Domains.inverse phidomain) in
    (* Kate amortized FFT *)
    G1.fft ~domain:phidomain ~points:hl

  (* h = polynomial such that h(y×domain[i]) = zi. *)
  let interpolation_h_poly y domain z_list =
    Scalar.ifft_inplace ~domain:(Domains.inverse domain) ~points:z_list ;
    let inv_y = Scalar.inverse_exn y in
    Array.fold_left_map
      (fun inv_yi h -> Scalar.(mul inv_yi inv_y, mul h inv_yi))
      Scalar.(copy one)
      z_list
    |> snd |> Polynomials.of_dense

  (* Part 3.2 verifier : verifies that f(w×domain.(i)) = evaluations.(i). *)
  let verify t cm_f srs_point domain (w, evaluations) proof =
    let open Bls12_381 in
    let h = interpolation_h_poly w domain evaluations in
    let cm_h = commit t h in
    let l = Domains.length domain in
    let sl_min_yl =
      G2.(add srs_point (negate (mul (copy one) (Scalar.pow w (Z.of_int l)))))
    in
    let diff_commits = G1.(add cm_h (negate cm_f)) in
    Pairing.pairing_check [(diff_commits, G2.(copy one)); (proof, sl_min_yl)]

  let precompute_shards_proofs t =
    preprocess_multi_reveals
      ~chunk_len:t.evaluations_per_proof_log
      ~degree:t.k
      t.srs.raw.srs_g1

  let _save_precompute_shards_proofs (preprocess : shards_proofs_precomputation)
      filename =
    let chan = open_out_bin filename in
    output_bytes
      chan
      (Data_encoding.Binary.to_bytes_exn
         Encoding.shards_proofs_precomputation_encoding
         preprocess) ;
    close_out_noerr chan

  let _load_precompute_shards_proofs filename =
    let chan = open_in_bin filename in
    let len = Int64.to_int (LargeFile.in_channel_length chan) in
    let data = Bytes.create len in
    let () = try really_input chan data 0 len with End_of_file -> () in
    let precomp =
      Data_encoding.Binary.of_bytes_exn
        Encoding.shards_proofs_precomputation_encoding
        data
    in
    close_in_noerr chan ;
    precomp

  let prove_shards t p =
    let preprocess = precompute_shards_proofs t in
    multiple_multi_reveals
      ~chunk_len:t.evaluations_per_proof_log
      ~chunk_count:t.proofs_log
      ~degree:t.k
      ~preprocess
      (Polynomials.to_dense_coefficients p)

  let verify_shard t cm {index = shard_index; share = shard_evaluations} proof =
    let d_n = Domains.build_power_of_two t.evaluations_log in
    let domain = Domains.build_power_of_two t.evaluations_per_proof_log in
    verify
      t
      cm
      t.srs.kate_amortized_srs_g2_shards
      domain
      (Domains.get d_n shard_index, shard_evaluations)
      proof

  let _prove_single t p z =
    let q, _ =
      Polynomials.(
        division_xn (p - constant (evaluate p z)) 1 (Scalar.negate z))
    in
    commit t q

  let _verify_single t cm ~point ~evaluation proof =
    let h_secret = Srs_g2.get t.srs.raw.srs_g2 1 in
    Bls12_381.(
      Pairing.pairing_check
        [
          ( G1.(add cm (negate (mul (copy one) evaluation))),
            G2.(negate (copy one)) );
          (proof, G2.(add h_secret (negate (mul (copy one) point))));
        ])

  let prove_page t p page_index =
    if page_index < 0 || page_index >= t.pages_per_slot then
      Error `Segment_index_out_of_range
    else
      let l = 1 lsl Z.(log2up (of_int t.page_length)) in
      let wi = Domains.get t.domain_k page_index in
      let quotient, _ =
        Polynomials.(division_xn p l Scalar.(negate (pow wi (Z.of_int l))))
      in
      Ok (commit t quotient)

  (* Parses the [slot_page] to get the evaluations that it contains. The
     evaluation points are given by the [slot_page_index]. *)
  let verify_page t cm ~page_index page proof =
    if page_index < 0 || page_index >= t.pages_per_slot then
      Error `Segment_index_out_of_range
    else
      let expected_page_length = t.page_size in
      let got_page_length = Bytes.length page in
      if expected_page_length <> got_page_length then
        Error `Page_length_mismatch
      else
        let domain =
          Domains.build_power_of_two Z.(log2up (of_int t.page_length))
        in
        let slot_page_evaluations =
          Array.init
            (1 lsl Z.(log2up (of_int t.page_length)))
            (function
              | i when i < t.page_length - 1 ->
                  let dst = Bytes.create scalar_bytes_amount in
                  Bytes.blit
                    page
                    (i * scalar_bytes_amount)
                    dst
                    0
                    scalar_bytes_amount ;
                  Scalar.of_bytes_exn dst
              | i when i = t.page_length - 1 ->
                  let dst = Bytes.create t.remaining_bytes in
                  Bytes.blit
                    page
                    (i * scalar_bytes_amount)
                    dst
                    0
                    t.remaining_bytes ;
                  Scalar.of_bytes_exn dst
              | _ -> Scalar.(copy zero))
        in
        Ok
          (verify
             t
             cm
             t.srs.kate_amortized_srs_g2_pages
             domain
             (Domains.get t.domain_k page_index, slot_page_evaluations)
             proof)
end

include Inner
module Verifier = Inner

module Internal_for_tests = struct
  let initialisation_parameters_from_slot_size ~slot_size =
    let size = slot_as_polynomial_length ~slot_size in
    let secret =
      Bls12_381.Fr.of_string
        "20812168509434597367146703229805575690060615791308155437936410982393987532344"
    in
    let srs_g1 = Srs_g1.generate_insecure (size + 1) secret in
    let srs_g2 = Srs_g2.generate_insecure (size + 1) secret in
    {srs_g1; srs_g2}

  let load_parameters parameters = initialisation_parameters := Some parameters
end

module Config = struct
  type t = {activated : bool; srs_size : int option}

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {activated; srs_size} -> (activated, srs_size))
      (fun (activated, srs_size) -> {activated; srs_size})
      (obj2 (req "activated" bool) (req "srs_size" (option int31)))

  let default = {activated = false; srs_size = None}

  let init_dal ~find_srs_files dal_config =
    let open Lwt_result_syntax in
    if dal_config.activated then
      let* initialisation_parameters =
        match dal_config.srs_size with
        | None ->
            let*? g1_path, g2_path = find_srs_files () in
            initialisation_parameters_from_files ~g1_path ~g2_path
        | Some slot_size ->
            return
              (Internal_for_tests.initialisation_parameters_from_slot_size
                 ~slot_size)
      in
      Lwt.return (load_parameters initialisation_parameters)
    else return_unit
end
