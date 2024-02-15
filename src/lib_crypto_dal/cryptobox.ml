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
module Srs_g1 = Kzg.Bls.Srs_g1
module Srs_g2 = Kzg.Bls.Srs_g2
module Scalar = Kzg.Bls.Scalar
module Poly = Kzg.Bls.Poly
module Domain = Kzg.Bls.Domain
module Evals = Kzg.Bls.Evals
module FFT = Kzg.Utils.FFT
module Degree_check = Kzg.Degree_check
module Kate_amortized = Kzg.Kate_amortized
module Base58 = Tezos_crypto.Base58

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
  [@@coverage off]

type initialisation_parameters =
  | Verifier of {is_fake : bool}
  | Prover of {is_fake : bool; srs_g1 : Srs_g1.t; srs_g2 : Srs_g2.t}

(* Initialisation parameters are supposed to be instantiated once. *)
let initialisation_parameters = ref @@ Verifier {is_fake = false}

(* This function is expected to be called once. *)
let load_parameters parameters =
  let open Result_syntax in
  initialisation_parameters := parameters ;
  return_unit

(* FIXME https://gitlab.com/tezos/tezos/-/issues/3400

   An integrity check is run to ensure the validity of the files. *)
(* TODO catch Failed_to_load_trusted_setup *)
let initialisation_parameters_from_files ~srs_g1_path ~srs_g2_path ~srs_size =
  let open Lwt_syntax in
  let* srs = Srs.read_srs ~len:srs_size ~srs_g1_path ~srs_g2_path () in
  let open Result_syntax in
  Lwt.return
  @@
  match srs with
  | Error (`End_of_file s) ->
      tzfail (Failed_to_load_trusted_setup ("EOF: " ^ s))
  | Error (`Invalid_point p) ->
      tzfail
        (Failed_to_load_trusted_setup (Printf.sprintf "Invalid point %i" p))
  | Ok srs -> return srs

module Inner = struct
  module Commitment = struct
    include Kzg.Commitment.Single

    type Tezos_crypto.Base58.data += Data of t

    let b58check_encoding =
      Tezos_crypto.Base58.register_encoding
        ~prefix:Tezos_crypto.Base58.Prefix.slot_header
        ~length:size
        ~to_raw:to_string
        ~of_raw:of_string_opt
        ~wrap:(fun x -> Data x)
      [@@coverage off]

    let raw_encoding = encoding [@@coverage off]

    include Tezos_crypto.Helpers.Make (struct
      type t = Kzg.Commitment.Single.t

      let name = "DAL_commitment"

      let title = "Commitment representation for the DAL"

      let b58check_encoding = b58check_encoding

      let raw_encoding = raw_encoding

      let compare = compare

      let equal = equal

      let hash _ =
        (* The commitment is not hashed. This is ensured by the
           function exposed. We only need the Base58 encoding and the
           rpc_arg. *)
        assert false
        [@@coverage off]

      let seeded_hash _ _ =
        (* Same argument. *)
        assert false
        [@@coverage off]
    end)

    let of_b58check = of_b58check
  end

  module Proof = Commitment

  module Commitment_proof = struct
    open Kzg.Bls

    type t = G2.t

    let encoding =
      conv
        G2.to_compressed_bytes
        G2.of_compressed_bytes_exn
        (Fixed.bytes (G2.size_in_bytes / 2))

    let t : t Repr.t =
      Repr.(
        map
          (bytes_of (`Fixed (G2.size_in_bytes / 2)))
          G2.of_compressed_bytes_exn
          G2.to_compressed_bytes)

    let _random = G2.random

    let zero = G2.zero

    let alter_proof x = G2.add x G2.one
  end

  type slot = bytes

  type scalar = Scalar.t

  type polynomial = Poly.t

  type commitment = Commitment.t

  type shard_proof = Proof.t

  type commitment_proof = Kzg.Bls.G2.t

  type page_proof = Proof.t

  type page = bytes

  type share = Scalar.t array

  type shard = {index : int; share : share}

  type shards_proofs_precomputation = Kate_amortized.preprocess

  type ('a, 'b) error_container = {given : 'a; expected : 'b}

  module Encoding = struct
    open Data_encoding

    let page_proof_encoding = Proof.encoding

    let share_encoding = array Scalar.encoding

    let shard_proof_encoding = Proof.encoding

    let shard_encoding =
      conv
        (fun {index; share} -> (index, share))
        (fun (index, share) -> {index; share})
        (tup2 int31 share_encoding)
      [@@coverage off]

    let shards_proofs_precomputation_encoding =
      Kate_amortized.preprocess_encoding

    let error_container_encoding (given_encoding : 'a encoding)
        (expected_encoding : 'b encoding) : ('a, 'b) error_container encoding =
      conv
        (fun {given; expected} -> (given, expected))
        (fun (given, expected) -> {given; expected})
        (obj2 (req "given" given_encoding) (req "expected" expected_encoding))
  end

  include Encoding

  type error += Invalid_precomputation_hash of (string, string) error_container

  let () =
    register_error_kind
      `Permanent
      ~id:"dal.node.invalid_precomputation_hash"
      ~title:"Invalid_precomputation_hash"
      ~description:"Unexpected precomputation hash"
      ~pp:(fun ppf {given; expected} ->
        Format.fprintf
          ppf
          "Invalid precomputation hash: expected %s. Got %s"
          expected
          given)
      (Encoding.error_container_encoding
         Data_encoding.string
         Data_encoding.string)
      (function Invalid_precomputation_hash err -> Some err | _ -> None)
      (function err -> Invalid_precomputation_hash err)
    [@@coverage off]

  (* Builds group of nth roots of unity, a valid domain for the FFT. *)
  let make_domain n = Domain.build n

  type t = {
    redundancy_factor : int;
    slot_size : int;
    page_size : int;
    number_of_shards : int;
    (* Maximum length of the polynomial representation of a slot, also called [polynomial_length]
       in the comments. *)
    max_polynomial_length : int;
    (* Length of the erasure-encoded polynomial representation of a slot,
       also called [erasure_encoded_polynomial_length] in the comments. *)
    erasure_encoded_polynomial_length : int;
    (* Domain for the FFT on slots as polynomials to be erasure encoded. *)
    domain_polynomial_length : Domain.t;
    domain_2_times_polynomial_length : Domain.t;
    (* Domain for the FFT on erasure encoded slots (as polynomials). *)
    domain_erasure_encoded_polynomial_length : Domain.t;
    (* Length of a shard in terms of scalar elements. *)
    shard_length : int;
    (* Number of slot pages. *)
    pages_per_slot : int;
    page_length : int;
    page_length_domain : int;
    (* Log of the number of evaluations that constitute an erasure encoded
       polynomial. *)
    remaining_bytes : int;
    (* These srs_g2_* parameters are used by the verifier to check the proofs *)
    srs_verifier : Srs.srs_verifier;
    mode : [`Verifier | `Prover];
    (* Kate amortized public parameters for proving and verifying ;
       contain, among others, SRS₁ *)
    kate_amortized : Kate_amortized.public_parameters;
    (* Degree check public parameters for proving (None when mode = `Verifier) *)
    degree_check : Srs_g2.t option;
  }

  let ensure_validity ~is_fake ~mode ~slot_size ~page_size ~redundancy_factor
      ~number_of_shards =
    let open Result_syntax in
    let* () =
      Parameters_check.ensure_validity_without_srs
        ~slot_size
        ~page_size
        ~redundancy_factor
        ~number_of_shards
    in
    Srs.ensure_srs_validity
      ~is_fake
      ~mode
      ~slot_size
      ~page_size
      ~redundancy_factor
      ~number_of_shards

  type parameters = Dal_config.parameters = {
    redundancy_factor : int;
    page_size : int;
    slot_size : int;
    number_of_shards : int;
  }

  let parameters_encoding = Dal_config.parameters_encoding

  let pages_per_slot {slot_size; page_size; _} = slot_size / page_size

  module Cache = Hashtbl.Make (struct
    type t = parameters * initialisation_parameters

    let equal (param, init_param) (param', init_param') =
      param = param'
      &&
      match (init_param, init_param') with
      | Verifier {is_fake; _}, Verifier {is_fake = is_fake'; _}
        when is_fake = is_fake' ->
          true
      | Prover {is_fake; _}, Prover {is_fake = is_fake'; _}
        when is_fake = is_fake' ->
          true
      | _ -> false

    let hash = Hashtbl.hash
  end)

  (* Error cases of this functions are not encapsulated into
     `tzresult` for modularity reasons. *)
  let make =
    let open Result_syntax in
    let table = Cache.create 5 in
    let with_cache (parameters, initialisation_parameters) f =
      match Cache.find_opt table (parameters, initialisation_parameters) with
      | Some x -> return x
      | None ->
          let* x = f () in
          Cache.replace table (parameters, initialisation_parameters) x ;
          return x
    in
    fun ({redundancy_factor; slot_size; page_size; number_of_shards} as
        parameters) ->
      (* The cryptobox is deterministically computed from the DAL parameters and
         this computation takes time (on the order of 10ms) so we cache it. *)
      with_cache (parameters, !initialisation_parameters) @@ fun () ->
      let max_polynomial_length, erasure_encoded_polynomial_length, shard_length
          =
        Parameters_check.compute_lengths
          ~redundancy_factor
          ~slot_size
          ~page_size
          ~number_of_shards
      in
      let page_length = Parameters_check.page_length ~page_size in
      let page_length_domain, _, _ = FFT.select_fft_domain page_length in
      let mode, is_fake, srs_g1, degree_check =
        match !initialisation_parameters with
        | Verifier {is_fake} ->
            let srs =
              if is_fake then Srs.Internal_for_tests.get_verifier_srs1 ()
              else Srs.get_verifier_srs1 ()
            in
            (`Verifier, is_fake, srs, None)
        | Prover {is_fake; srs_g1; srs_g2} ->
            (`Prover, is_fake, srs_g1, Some srs_g2)
      in
      let* () =
        ensure_validity
          ~is_fake
          ~mode
          ~slot_size
          ~page_size
          ~redundancy_factor
          ~number_of_shards
      in
      let srs_verifier =
        (if is_fake then Srs.Internal_for_tests.get_verifier_srs2
        else Srs.get_verifier_srs2)
          ~max_polynomial_length
          ~page_length_domain
          ~shard_length
      in
      let kate_amortized =
        Kate_amortized.
          {max_polynomial_length; shard_length; srs_g1; number_of_shards}
      in
      return
        {
          redundancy_factor;
          slot_size;
          page_size;
          number_of_shards;
          max_polynomial_length;
          erasure_encoded_polynomial_length;
          domain_polynomial_length = make_domain max_polynomial_length;
          domain_2_times_polynomial_length =
            make_domain (2 * max_polynomial_length);
          domain_erasure_encoded_polynomial_length =
            make_domain erasure_encoded_polynomial_length;
          shard_length;
          pages_per_slot = pages_per_slot parameters;
          page_length;
          page_length_domain;
          remaining_bytes = page_size mod Parameters_check.scalar_bytes_amount;
          srs_verifier;
          mode;
          kate_amortized;
          degree_check;
        }

  let parameters
      ({redundancy_factor; slot_size; page_size; number_of_shards; _} : t) =
    {redundancy_factor; slot_size; page_size; number_of_shards}
    [@@coverage off]

  let polynomial_degree = Poly.degree

  let polynomial_evaluate = Poly.evaluate

  (* [polynomials_multiplication d ps] computes the product of the
     polynomials [ps]. The degree of the resulting product must
     be strictly less than the size of the domain [d].

     Runtime is [O(n log n)] where [n = Domains.length d]. *)
  let polynomials_product d ps =
    let evaluations = List.map (FFT.fft d) ps in
    FFT.ifft_inplace d (Evals.mul_c ~evaluations ())

  (* We encode by pages of [page_size] bytes each. The pages
     are arranged in cosets to produce batched KZG proofs
     [https://www.iacr.org/archive/asiacrypt2010/6477178/6477178.pdf]
     using the technique described in https://eprint.iacr.org/2023/033. *)
  let polynomial_from_slot (t : t) slot =
    (* Expects the length of the byte sequence to equal the slot size.
       This can be achieved by adding some padding with null bytes to the
       byte sequence if the length is strictly less than the slot size, or
       truncate it if the length is strictly greater than the slot size. *)
    if Bytes.length slot <> t.slot_size then
      Error
        (`Slot_wrong_size
          (Printf.sprintf "message must be %d bytes long" t.slot_size))
    else
      let offset = ref 0 in
      let coefficients =
        Array.init t.max_polynomial_length (fun _ -> Scalar.(copy zero))
      in
      (* A slot is subdivided into contiguous segments, called pages.
         The length of a page divides the slot size, that is:
         [t.page_size * t.pages_per_slot = t.slot_size]
         We parse the slot page by page. *)
      for page = 0 to t.pages_per_slot - 1 do
        (* A permutation (so a bijection) is then applied to the elements
           from a page. The code applies the permutation to a scalar
           element just after it is read from the page, but it is easier to
           picture it as two successive steps:

           1. a serialization phase where pages are converted to sequences
           of scalar elements, which is injective: if two outputs for two
           slots are equal, then the slots are equal since we’re just
           splitting a page into chunks of [Parameters_check.scalar_bytes_amount] bytes and
           a last one of [t.remaining_bytes] bytes.

           Parse the byte sequence slot = page_0 ... page_{t.pages_per_slot-1}
           page by page by chunks of [Parameters_check.scalar_bytes_amount], or [t.remaining_bytes]
           for the last page chunk.

           to obtain the vector of length [polynomial_length = t.page_length * t.pages_per_slot]
           (chunk p_i^j standing for i-th chunk of page j):

           [ p_0^0 p_1^0 ... p_{page_length-1}^0
           ....
           p_0^{t.pages_per_slot-1} ... p_{page_length-1}^{t.pages_per_slot-1}]

           2. then apply the permutation, reindexing the page elements so
           that their indices correspond to the appropriate coset.

           [ p_0^0 p_0^1 ... p_0^{t.pages_per_slot-1}
           ...
           p_{page_length-1}^0 p_{page_length-1}^1 ... p_{t.page_length-1}^{t.pages_per_slot-1}]

           For a primitive k-th root of unity, the group generated by w, <w>,
           can be split into cosets

           <w> = Disjoint union_{j=0, ..., t.pages_per_slot - 1} w^j W

           where W = { w^{t.pages_per_slot * i} }_{i=0, ..., t.page_length - 1}.

           This way, the indices of the coefficients for each page
           j=0, ..., t.pages_per_slot-1 coincide with the indices of the
           elements of the cosets (w^j W). Indeed, the coefficients
           of the elements of w^j W are
           {i * t.pages_per_slot + j}_{i=0, ..., t.page_length-1}. *)
        for elt = 0 to t.page_length - 2 do
          (* [!offset >= t.slot_size] because we don't want to read past
             the buffer [slot] bounds. *)
          if !offset >= t.slot_size then ()
          else
            let dst = Bytes.create Parameters_check.scalar_bytes_amount in
            Bytes.blit slot !offset dst 0 Parameters_check.scalar_bytes_amount ;
            offset := !offset + Parameters_check.scalar_bytes_amount ;
            (* Apply the permutation. *)
            coefficients.((elt * t.pages_per_slot) + page) <-
              Scalar.of_bytes_exn dst
        done ;
        let dst = Bytes.create t.remaining_bytes in
        Bytes.blit slot !offset dst 0 t.remaining_bytes ;
        offset := !offset + t.remaining_bytes ;
        (* Apply the permutation. *)
        coefficients.(((t.page_length - 1) * t.pages_per_slot) + page) <-
          Scalar.of_bytes_exn dst
      done ;
      (* The resulting vector is then interpolated. Polynomial
         interpolation is a linear bijection (as a ring isomorphism)
         between k-tuples of scalar elements and polynomials of degree < k
         with coefficients in the scalar field.

         Thus [polynomial_from_slot] is an injection from slots to
         polynomials (as composition preserves injectivity). *)
      Ok
        (FFT.ifft_inplace
           (Domain.build t.max_polynomial_length)
           (Evals.of_array (t.max_polynomial_length - 1, coefficients)))

  (* [polynomial_to_slot] is the left-inverse function of
     [polynomial_from_slot]. *)
  let polynomial_to_slot t p =
    (* The last operation of [polynomial_from_slot] is the interpolation,
       so we undo it with an evaluation on the same domain [t.domain_polynomial_length]. *)
    let evaluations = FFT.fft (Domain.build t.max_polynomial_length) p in
    let slot = Bytes.make t.slot_size '\x00' in
    let offset = ref 0 in
    (* Reverse permutation from [polynomial_from_slot]. *)
    for page = 0 to t.pages_per_slot - 1 do
      for elt = 0 to t.page_length - 2 do
        let idx = (elt * t.pages_per_slot) + page in
        let coeff = Scalar.to_bytes (Evals.get evaluations idx) in
        Bytes.blit coeff 0 slot !offset Parameters_check.scalar_bytes_amount ;
        offset := !offset + Parameters_check.scalar_bytes_amount
      done ;
      let idx = ((t.page_length - 1) * t.pages_per_slot) + page in
      let coeff = Scalar.to_bytes (Evals.get evaluations idx) in
      Bytes.blit coeff 0 slot !offset t.remaining_bytes ;
      offset := !offset + t.remaining_bytes
    done ;
    slot

  (* Encoding a message P = (P_0, ... ,P_{k-1}) amounts to evaluate
     its associated polynomial P(x)=sum_{i=0}^{k-1} P_i x^i at the
     evaluation points [t.domain_erasure_encoded_polynomial_length].

     This can be achieved with an n-points discrete Fourier transform
     supported by the [Scalar] field in time O(n log n). *)
  let encode t p =
    Evals.to_array
      (FFT.fft (Domain.build t.erasure_encoded_polynomial_length) p)

  (* The shards are arranged in cosets to produce batches of KZG proofs
     for the shards efficiently.

     The domain of evaluation <w> is split into cosets:
     <w> = Disjoint union_{i in ⟦0, t.number_of_shards-1⟧} W_i,

     where W_0 = {w^{t.number_of_shards * j}}_{j in ⟦0, t.shard_length-1⟧}
     and W_i = w^i W_0 (|W_0|=t.shard_length). *)
  let shards_from_polynomial t p =
    let codeword = encode t p in
    let rec loop index seq =
      if index < 0 then seq
      else
        (* For each shard index [index], a [share] consists of the
           elements from [codeword] of indices w^index W_0.
           A shard consists of its [index] and associated [share]. *)
        let share = Array.init t.shard_length (fun _ -> Scalar.(copy zero)) in
        for j = 0 to t.shard_length - 1 do
          share.(j) <- codeword.((t.number_of_shards * j) + index)
        done ;
        loop (index - 1) (Seq.cons {index; share} seq)
    in
    loop (t.number_of_shards - 1) Seq.empty

  module ShardSet = Set.Make (struct
    type t = shard

    let compare a b = Int.compare a.index b.index
  end)

  let encoded_share_size t =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4289
       Improve shard size computation *)
    let share_scalar_len =
      t.erasure_encoded_polynomial_length / t.number_of_shards
    in
    (share_scalar_len * Scalar.size_in_bytes) + 4

  (* Let w be a primitive [t.erasure_encoded_polynomial_length]-th root of unity, where
     [t.max_polynomial_length] and [t.erasure_encoded_polynomial_length = t.redundancy_factor * t.max_polynomial_length] divide
     [Scalar.order - 1]. Let F be a finite field, and in this
     context we use the prime field [Scalar].
     We can decode a codeword c from
     RS(n, k, (w^i)_i) ≔ { (P(w^i))_{i in ⟦0, n-1⟧} | P in F[x] /\ deg P < k }
     with at least k components of c. By decode we mean finding the
     underlying message represented by the polynomial P such that
     c=(P(w^i))_{i in ⟦0, n-1⟧} with at least k components of c.

     Without loss of generality, let c̃ = (c_0, ..., c_{k-1})
     be the received codeword with erasures, i.e., the first k components of
     a codeword. We can retrieve the original message with any k
     components of c thanks to the Lagrange interpolation polynomial P(x)
     defined as follows, where the x_i=w^i are the evaluation points of c̃:

     P(x) = sum_{i=0}^{k-1} c_i prod_{j=0, j != i}^{k-1} (x-x_j)/(x_i - x_j).

     As detailed in https://arxiv.org/pdf/0907.1788v1.pdf, the idea is
     to rewrite P(x) as a product of two polynomials A(x) and B(x) so
     that the convolution theorem allows us to recover P(x) using FFTs
     in O(n log n).

     Asymptotic complexity is O(n log n). *)
  let polynomial_from_shards t shards =
    let shards =
      (* We always consider the first k codeword vector components,
         the ShardSet allows collecting distinct indices.
         [Seq.take] doesn't raise any exceptions as t.max_polynomial_length / t.shard_length
         is (strictly) positive.

         [t.max_polynomial_length / t.shard_length] is strictly less than [t.number_of_shards].

         Indeed, [t.shard_length = t.erasure_encoded_polynomial_length / t.number_of_shards] where
         [t.erasure_encoded_polynomial_length = t.max_polynomial_length * t.redundancy_factor] and [t.redundancy_factor > 1].
         Thus,
         [t.max_polynomial_length / t.shard_length = t.number_of_shards / t.redundancy_factor < t.number_of_shards].

         Here, all variables are positive integers, and [t.redundancy_factor]
         divides [t.number_of_shards], [t.number_of_shards] divides [t.erasure_encoded_polynomial_length]. *)
      Seq.take (t.max_polynomial_length / t.shard_length) shards
      |> ShardSet.of_seq
    in
    (* There should be [t.max_polynomial_length / t.shard_length] distinct shard indices. *)
    if t.max_polynomial_length / t.shard_length > ShardSet.cardinal shards then
      Error
        (`Not_enough_shards
          (Printf.sprintf
             "there must be at least %d shards to decode"
             (t.max_polynomial_length / t.shard_length)))
    else if
      ShardSet.exists
        (fun {share; _} -> Array.length share <> t.shard_length)
        shards
    then
      Error
        (`Invalid_shard_length
          (Printf.sprintf
             "At least one shard of invalid length: expected length %d."
             t.shard_length))
    else if
      ShardSet.exists
        (fun {index; _} -> index >= t.number_of_shards || index < 0)
        shards
    then
      Error
        (`Shard_index_out_of_range
          (Printf.sprintf
             "At least one shard index out of range: expected indices within \
              the range [%d, %d]."
             0
             (t.number_of_shards - 1)))
    else
      (* 1. Computing A(x) = prod_{i=0}^{k-1} (x - x_i).

         Let A(x) ≔ prod_{i=0}^{k-1} (x-x_i) and
         A_i(x) ≔ prod_{j=0, j != i}^{k-1} (x-x_j).

         Let n_i ≔ c_i / A_i(x_i).

         The interpolation polynomial becomes:
         P(x) = A(x) sum_{i=0}^{k-1} c_i / ((x - x_i) A_i(x_i))
              = A(x) sum_{i=0}^{k-1} n_i / (x - x_i).

         Note that A_i(x_i) != 0 by definition, so it is invertible
         in the [Scalar] field. *)

      (* The codewords are split into chunks called shards.

         For this purpose, let [t.shard_length=t.n/t.number_of_shards]
         be the length of a shard, and w a primitive n-th root
         of unity.

         The domain of evaluation <w> is then split into cosets:
         <w> = Disjoint union_{i in ⟦0, t.number_of_shards-1⟧} W_i,

         where W_0 = {w^{t.number_of_shards * j}}_{j in ⟦0, t.shard_length-1⟧}
         and W_i = w^i W_0 (|W_0|=t.shard_length).

         For a set of [t.max_polynomial_length / shard_length] shard indices
         Z subseteq {0, t.number_of_shards-1}, we reorganize the product
         A(x)=prod_{i=0}^{k-1} (x-x_i) into

         A(x) = prod_{i in Z, |Z|=t.max_polynomial_length/t.shard_length} Z_i
         where Z_i = prod_{w' in W_i} (x - w').

         We notice that Z_0(x)=x^{|W_0|}-1 (as its roots
         are the elements of a group of order dividing |W_0|)
         entails Z_i(x)=x^{|W_0|}-w^{i*|W_0|}
         (multiplying all terms by a constant w^i in an
         integral domain), which is a sparse polynomial.
         This special form for Z_i(x) allows us to compute the product of
         the Z_i(x)s more efficiently with the [mul_xn] function, which
         multiplies an arbitrary polynomial P with a polynomial of the
         form Z_i(x) in time linear in (degree P + degree Z_i(x)).

         Thus A(x) = prod_{i in Z, |Z|=k/l} x^{|W_0|}-w^{i*|W_0|}.

         More formally: every element of W_i is of the form
         w^i w^{t.number_of_shards j} for j in ⟦0, t.shard_length-1⟧. Thus

         Z_i(w^i w^{s j}) = (w^i w^{s j})^{|W_0|}-w^{i*|W_0|}
                          = (w^i)^{|W_0|} (w^{s j})^{|W_0|} - w^{i*|W_0|}
                          = w^{i * |W_0|} * 1 - w^{i*|W_0|}=0.

         So every element of W_i is a root of Z_i(x).
         Moreover, Z_i(x) is a degree |W_0|=t.shard_length polynomial
         so has at most [t.shard_length] roots:
         Z_i(x)'s only roots are W_i

         [mul acc i] computes the polynomial acc * Z_i. *)
      let mul acc i =
        (* The complexity of [mul_xn] is linear in
           [Polynomials.degree acc + t.shard_length]. *)
        Poly.mul_xn
          acc
          t.shard_length
          (Scalar.negate
             (Domain.get
                t.domain_erasure_encoded_polynomial_length
                (i * t.shard_length)))
      in
      (* [partition_products seq] builds two polynomials whose
         product is A(x) from the input shards [seq]. *)
      let partition_products seq =
        ShardSet.fold
          (fun {index; _} (l, r) -> (mul r index, l))
          seq
          (Poly.one, Poly.one)
      in
      (* The computation of [p1], [p2] has asymptotic complexity
         [O((t.max_polynomial_length + t.shard_length) * (t.max_polynomial_length / t.shard_length))
         = O(t.max_polynomial_length * t.number_of_shards / t.redundancy_factor)].
         It is the most costly operation of this function. *)
      let p1, p2 = partition_products shards in
      (* A(x) is the product of [p1] and [p2], and has degree [polynomial_length] *)
      assert (Poly.degree p1 + Poly.degree p2 = t.max_polynomial_length) ;

      let mul_domain = Domain.build (2 * t.max_polynomial_length) in
      let eep_domain = Domain.build t.erasure_encoded_polynomial_length in

      let a_poly = polynomials_product mul_domain [p1; p2] in

      assert (Poly.degree a_poly = t.max_polynomial_length) ;

      (* 2. Computing formal derivative of A(x). *)
      let a' = Poly.derivative a_poly in

      (* 3. Computing A'(w^i) = A_i(w^i).

         In order to compute A_i(x) more efficiently, we use the
         fact that the formal derivative A'(x) of A(x) satisfies
         for all i in ⟦0, k-1⟧: A'(x_i) = A_i(x_i).
         So we can compute (A_i(x_i))_i by evaluating A'(x) at the
         points (w^i)_i with an FFT.

         Indeed:
         A'(x) = (prod_{i=0}^{k-1} (x-x_i))'
               = sum_{i=0}^{k-1} (x-x_i)' prod_{j=0, j != i}^{k-1} (x-x_j)
               = sum_{j=0}^{k-1} A_j(x).

         So A'(x_i) = sum_{j=0}^{k-1} A_j(x_i) = A_i(x_i) as the other
         polynomials A_j(x) have x_i as root. *)
      let eval_a' = FFT.fft eep_domain a' in

      (* 4. Computing N(x) ≔ sum_{i=0}^{k-1} n_i / x_i x^i
         where n_i ≔ c_i/A_i(x_i)

         Writing the fraction

         1 / (x_i-x) = sum_{j=0}^infty x^j / (x_i^{j+1}) as a formal power
         series, we obtain

         P(x) / A(x) = sum_{i=0}^{k-1} n_i / (x-x_i) mod x^k
                     = -sum_{i=0}^{k-1} [sum_{j=0}^{k-1} n_i / (x_i^{j+1}) x^j]
                     = -sum_{i=0}^{k-1} N(w^{-i}) x^i

         where N(x) ≔ sum_{i=0}^{k-1} n_i / x_i x^i. *)
      let compute_n t eval_a' shards =
        let n_poly =
          Array.init t.erasure_encoded_polynomial_length (fun _ ->
              Scalar.(copy zero))
        in
        ShardSet.iter
          (fun {index; share} ->
            for j = 0 to Array.length share - 1 do
              let c_i = share.(j) in
              let i = (t.number_of_shards * j) + index in
              let x_i =
                Domain.get t.domain_erasure_encoded_polynomial_length i
              in
              let tmp = Evals.get eval_a' i in
              Scalar.mul_inplace tmp tmp x_i ;
              (* The call below never fails, so we don't
                 catch exceptions.

                 Indeed, [tmp = A_i(x_i)] is a product of nonzero
                 elements and so is itself nonzero
                 (thus invertible). See point 1. *)
              Scalar.inverse_exn_inplace tmp tmp ;
              Scalar.mul_inplace tmp tmp c_i ;
              n_poly.(i) <- tmp
            done)
          shards ;
        Evals.of_array (t.erasure_encoded_polynomial_length - 1, n_poly)
      in
      let n_poly = compute_n t eval_a' shards in

      (* 5. Computing B(x).
         B(x) = P(x) / A(x) = -sum_{i=0}^{k-1} N(w^{-i}) x^i.

         B(x) is thus given by the first k components
         of -n * IFFT_n(N). *)
      let b =
        Poly.truncate
          ~len:t.max_polynomial_length
          (FFT.ifft_inplace eep_domain n_poly)
      in

      Poly.mul_by_scalar_inplace
        b
        (Scalar.negate (Scalar.of_int t.erasure_encoded_polynomial_length))
        b ;

      (* 6. Computing Lagrange interpolation polynomial P(x).
         The product is given by the convolution theorem:
         P = A * B = IFFT_{2k}(FFT_{2k}(A) o FFT_{2k}(B))
         where o is the pairwise product. *)
      let p = polynomials_product mul_domain [a_poly; b] in
      (* P has degree [<= max_polynomial_length - 1] so [<= max_polynomial_length]
         coefficients. *)
      Ok (Poly.truncate ~len:t.max_polynomial_length p)

  let commit t p =
    match t.mode with
    | `Verifier -> Error `Prover_SRS_not_loaded
    | `Prover -> (
        try Ok (Commitment.commit t.kate_amortized.srs_g1 p)
        with Kzg.Commitment.SRS_too_short _ ->
          Error
            (`Invalid_degree_strictly_less_than_expected
              {
                given = Poly.degree p;
                expected = Srs_g1.size t.kate_amortized.srs_g1;
              }))

  let pp_commit_error fmt = function
    | `Invalid_degree_strictly_less_than_expected {given; expected} ->
        Format.fprintf
          fmt
          "Invalid degree: expecting input polynomial to commit function to \
           have a degree strictly less than %d. Got %d."
          expected
          given
    | `Prover_SRS_not_loaded ->
        Format.fprintf
          fmt
          "The prover's SRS was not loaded: cannot commit a polynomial without \
           the prover's SRS."

  let string_of_commit_error err = Format.asprintf "%a" pp_commit_error err

  (* p(X) of degree n. Max degree that can be committed: d, which is also the
     SRS's length - 1. We take d = t.max_polynomial_length - 1 since we don't want to commit
     polynomials with degree greater than polynomials to be erasure-encoded.

     We consider the bilinear groups (G_1, G_2, G_T) with G_1=<g> and G_2=<h>.
     - Commit (p X^{d-n}) such that deg (p X^{d-n}) = d the max degree
     that can be committed
     - Verify: checks if e(commit(p), commit(X^{d-n})) = e(commit(p X^{d-n}), h)
     using the commitments for p and p X^{d-n}, and computing the commitment for
     X^{d-n} on G_2. *)

  (* Proves that degree(p) < t.max_polynomial_length *)
  let prove_commitment ({max_polynomial_length; degree_check; _} : t) p =
    match degree_check with
    | None -> Error `Prover_SRS_not_loaded
    | Some srs_g2 ->
        if Srs_g2.size srs_g2 >= max_polynomial_length then
          Ok
            (Degree_check.prove
               ~max_commit:(Srs_g2.size srs_g2 - 1)
               ~max_degree:(max_polynomial_length - 1)
               srs_g2
               p)
        else
          Error
            (`Invalid_degree_strictly_less_than_expected
              {given = max_polynomial_length; expected = Srs_g2.size srs_g2})

  (* Verifies that the degree of the committed polynomial is < t.max_polynomial_length *)
  let verify_commitment (t : t) cm proof =
    Degree_check.verify t.srs_verifier.commitment cm proof

  let save_precompute_shards_proofs precomputation ~filename =
    protect (fun () ->
        Lwt_io.with_file ~mode:Output filename (fun chan ->
            let open Lwt_result_syntax in
            let str =
              Data_encoding.Binary.to_string_exn
                Encoding.shards_proofs_precomputation_encoding
                precomputation
            in
            let*! () = Lwt_io.write chan str in
            return_unit))

  let hash_precomputation precomputation =
    let encoding =
      Data_encoding.Binary.to_bytes_exn
        Encoding.shards_proofs_precomputation_encoding
        precomputation
    in
    Tezos_crypto.Blake2B.hash_bytes [encoding]

  let load_precompute_shards_proofs ~hash ~filename () =
    protect (fun () ->
        Lwt_io.with_file ~mode:Input filename (fun chan ->
            let open Lwt_result_syntax in
            let*! str = Lwt_io.read chan in
            let precomputation =
              Data_encoding.Binary.of_string_exn
                Encoding.shards_proofs_precomputation_encoding
                str
            in
            let* () =
              match hash with
              | Some given ->
                  let expected = hash_precomputation precomputation in
                  if Tezos_crypto.Blake2B.equal given expected then return_unit
                  else
                    tzfail
                      (Invalid_precomputation_hash
                         {
                           given = Tezos_crypto.Blake2B.to_string given;
                           expected = Tezos_crypto.Blake2B.to_string expected;
                         })
              | None -> return_unit
            in
            return precomputation))

  let precompute_shards_proofs t =
    (* Precomputes step. 1 of multiple multi-reveals. *)
    Kate_amortized.preprocess_multiple_multi_reveals t.kate_amortized

  let prove_shards t ~precomputation ~polynomial =
    (* Resizing input polynomial [p] to obtain an array of length [t.max_polynomial_length + 1]. *)
    let coefficients =
      Array.init (t.max_polynomial_length + 1) (fun _ -> Scalar.(copy zero))
    in
    let p_length = Poly.degree polynomial + 1 in
    let p = Poly.to_dense_coefficients polynomial in
    Array.blit p 0 coefficients 0 p_length ;
    Kate_amortized.multiple_multi_reveals
      t.kate_amortized
      ~preprocess:precomputation
      ~coefficients

  let verify_shard (t : t) commitment {index = shard_index; share = evaluations}
      proof =
    if shard_index < 0 || shard_index >= t.number_of_shards then
      Error
        (`Shard_index_out_of_range
          (Printf.sprintf
             "Shard index out of range: got index %d, expected index within \
              range [%d, %d]."
             shard_index
             0
             (t.number_of_shards - 1)))
    else
      let expected_shard_length = t.shard_length in
      let got_shard_length = Array.length evaluations in
      if expected_shard_length <> got_shard_length then
        Error `Shard_length_mismatch
      else
        let root =
          Domain.get t.domain_erasure_encoded_polynomial_length shard_index
        in
        let domain = Domain.build t.shard_length in
        let srs_point = t.srs_verifier.shards in
        if
          Kate_amortized.verify
            t.kate_amortized
            ~commitment
            ~srs_point
            ~domain
            ~root
            ~evaluations
            ~proof
        then Ok ()
        else Error `Invalid_shard

  let prove_page t p page_index =
    if page_index < 0 || page_index >= t.pages_per_slot then
      Error `Page_index_out_of_range
    else
      let wi = Domain.get t.domain_polynomial_length page_index in
      let quotient, _ =
        Poly.division_xn
          p
          t.page_length_domain
          (Scalar.negate (Scalar.pow wi (Z.of_int t.page_length_domain)))
      in
      commit t quotient

  (* Parses the [slot_page] to get the evaluations that it contains. The
     evaluation points are given by the [slot_page_index]. *)
  let verify_page t commitment ~page_index page proof =
    if page_index < 0 || page_index >= t.pages_per_slot then
      Error `Page_index_out_of_range
    else
      let expected_page_length = t.page_size in
      let got_page_length = Bytes.length page in
      if expected_page_length <> got_page_length then
        Error `Page_length_mismatch
      else
        let domain = Domain.build t.page_length_domain in
        let evaluations =
          Array.init t.page_length_domain (function
              | i when i < t.page_length - 1 ->
                  (* Parse the [page] by chunks of [Parameters_check.scalar_bytes_amount] bytes.
                     These chunks are interpreted as [Scalar.t] elements and stored
                     in [evaluations]. *)
                  let dst = Bytes.create Parameters_check.scalar_bytes_amount in
                  Bytes.blit
                    page
                    (i * Parameters_check.scalar_bytes_amount)
                    dst
                    0
                    Parameters_check.scalar_bytes_amount ;
                  Scalar.of_bytes_exn dst
              | i when i = t.page_length - 1 ->
                  (* Store the remaining bytes in the last nonzero coefficient
                     of evaluations. *)
                  let dst = Bytes.create t.remaining_bytes in
                  Bytes.blit
                    page
                    (i * Parameters_check.scalar_bytes_amount)
                    dst
                    0
                    t.remaining_bytes ;
                  Scalar.of_bytes_exn dst
              | _ -> Scalar.(copy zero))
        in
        let root = Domain.get t.domain_polynomial_length page_index in
        let srs_point = t.srs_verifier.pages in
        if
          Kate_amortized.verify
            t.kate_amortized
            ~commitment
            ~srs_point
            ~domain
            ~root
            ~evaluations
            ~proof
        then Ok ()
        else Error `Invalid_page
end

include Inner
module Verifier = Inner

module Internal_for_tests = struct
  let parameters_initialisation () =
    Prover
      {
        is_fake = true;
        srs_g1 = Lazy.force Srs.Internal_for_tests.fake_srs1;
        srs_g2 = Lazy.force Srs.Internal_for_tests.fake_srs2;
      }

  (* Since computing fake_srs is costly, we avoid to recompute it. *)
  let init_prover_dal () =
    initialisation_parameters := parameters_initialisation ()

  let init_verifier_dal () =
    initialisation_parameters := Verifier {is_fake = true}

  let make_dummy_shards (t : t) ~state =
    Random.set_state state ;
    let rec loop index seq =
      if index = t.number_of_shards then seq
      else
        let share =
          Array.init
            (t.shard_length + 1 + Random.int 100)
            (fun _ -> Scalar.(random ~state ()))
        in
        loop (index + 1) (Seq.cons {index; share} seq)
    in
    loop 0 Seq.empty

  let polynomials_equal = Poly.equal

  let page_proof_equal = Proof.equal

  let alter_page_proof (proof : page_proof) = Proof.alter_proof proof

  let alter_shard_proof (proof : shard_proof) = Proof.alter_proof proof

  let alter_commitment_proof (proof : commitment_proof) =
    Kzg.Bls.G2.(add proof one)

  let minimum_number_of_shards_to_reconstruct_slot (t : t) =
    t.number_of_shards / t.redundancy_factor

  let select_fft_domain = FFT.select_fft_domain

  let precomputation_equal = Kate_amortized.preprocess_equal

  let dummy_commitment ~state () = Commitment.random ~state ()

  let dummy_page_proof ~state () = Proof.random ~state ()

  let dummy_shard_proof ~state () = Proof.random ~state ()

  let make_dummy_shard ~state ~index ~length =
    {index; share = Array.init length (fun _ -> Scalar.(random ~state ()))}

  let number_of_pages t = t.pages_per_slot

  let shard_length t = t.shard_length

  let dummy_polynomial ~state ~degree =
    let rec nonzero () =
      let res = Scalar.random ~state () in
      if Scalar.is_zero res then nonzero () else res
    in
    Poly.init (degree + 1) (fun i ->
        if i = degree then nonzero () else Scalar.random ~state ())

  let srs_size_g1 t = Srs_g1.size t.kate_amortized.srs_g1

  let encoded_share_size = encoded_share_size

  let ensure_validity_without_srs
      {redundancy_factor; slot_size; page_size; number_of_shards; _} =
    Parameters_check.ensure_validity_without_srs
      ~redundancy_factor
      ~slot_size
      ~page_size
      ~number_of_shards

  let ensure_validity
      {redundancy_factor; slot_size; page_size; number_of_shards} =
    let mode, is_fake =
      match !initialisation_parameters with
      | Verifier {is_fake} -> (`Verifier, is_fake)
      | Prover {is_fake; _} -> (`Prover, is_fake)
    in
    match
      ensure_validity
        ~is_fake
        ~mode
        ~slot_size
        ~page_size
        ~redundancy_factor
        ~number_of_shards
    with
    | Ok _ -> true
    | _ -> false

  let slot_as_polynomial_length = Parameters_check.slot_as_polynomial_length
end

module Config = struct
  type t = Dal_config.t = {
    activated : bool;
    use_mock_srs_for_testing : parameters option;
    bootstrap_peers : string list;
  }

  let encoding : t Data_encoding.t = Dal_config.encoding

  let default = Dal_config.default

  let init_verifier_dal dal_config =
    let open Lwt_result_syntax in
    if dal_config.activated then
      let* initialisation_parameters =
        match dal_config.use_mock_srs_for_testing with
        | Some _parameters -> return (Verifier {is_fake = true})
        | None -> return (Verifier {is_fake = false})
      in
      Lwt.return (load_parameters initialisation_parameters)
    else return_unit

  let init_prover_dal ~find_srs_files ?(srs_size_log2 = 21) dal_config =
    let open Lwt_result_syntax in
    if dal_config.activated then
      let* initialisation_parameters =
        match dal_config.use_mock_srs_for_testing with
        | Some _parameters ->
            return (Internal_for_tests.parameters_initialisation ())
        | None ->
            let*? srs_g1_path, srs_g2_path = find_srs_files () in
            let* srs_g1, srs_g2 =
              initialisation_parameters_from_files
                ~srs_g1_path
                ~srs_g2_path
                ~srs_size:(1 lsl srs_size_log2)
            in
            return (Prover {is_fake = false; srs_g1; srs_g2})
      in
      Lwt.return (load_parameters initialisation_parameters)
    else return_unit
end
