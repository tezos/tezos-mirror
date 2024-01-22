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
open Kzg.Bls
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

type initialisation_parameters = {srs_g1 : Srs_g1.t; srs_g2 : Srs_g2.t}

(* Initialisation parameters are supposed to be instantiated once. *)
let initialisation_parameters = ref None

type error += Dal_initialisation_twice

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.initialisation_twice"
    ~title:"Initialisation_twice"
    ~description:"DAL parameters were initialised twice"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "DAL parameters were initialised twice")
    Data_encoding.empty
    (function Dal_initialisation_twice -> Some () | _ -> None)
    (function () -> Dal_initialisation_twice)
  [@@coverage off]

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

let initialisation_parameters_from_files ~srs_g1_path ~srs_g2_path
    ~srs_size_log2 =
  let open Lwt_result_syntax in
  let len = 1 lsl srs_size_log2 in
  let to_bigstring ~path =
    let open Lwt_syntax in
    let* fd = Lwt_unix.openfile path [Unix.O_RDONLY] 0o440 in
    Lwt.finalize
      (fun () ->
        return
          (match
             Lwt_bytes.map_file
               ~fd:(Lwt_unix.unix_file_descr fd)
               ~shared:false
               ()
           with
          | exception Unix.Unix_error (error_code, function_name, _) ->
              Error
                [
                  Failed_to_load_trusted_setup
                    (Format.sprintf
                       "%s: Unix.Unix_error: %s"
                       function_name
                       (Unix.error_message error_code));
                ]
          | exception e ->
              Error [Failed_to_load_trusted_setup (Printexc.to_string e)]
          | res -> Ok res))
      (fun () -> Lwt_unix.close fd)
  in
  let* srs_g1_bigstring = to_bigstring ~path:srs_g1_path in
  let* srs_g2_bigstring = to_bigstring ~path:srs_g2_path in
  match
    let open Result_syntax in
    let* srs_g1 = Srs_g1.of_bigstring srs_g1_bigstring ~len in
    let* srs_g2 = Srs_g2.of_bigstring srs_g2_bigstring ~len in
    return (srs_g1, srs_g2)
  with
  | Error (`End_of_file s) ->
      tzfail (Failed_to_load_trusted_setup ("EOF: " ^ s))
  | Error (`Invalid_point p) ->
      tzfail
        (Failed_to_load_trusted_setup (Printf.sprintf "Invalid point %i" p))
  | Ok (srs_g1, srs_g2) -> return {srs_g1; srs_g2}

(* The srs is made of the initialisation_parameters and two
   well-choosen points. Building the srs from the initialisation
   parameters is almost cost-free. *)
type srs = {
  raw : initialisation_parameters;
  kate_amortized_srs_g2_shards : G2.t;
  kate_amortized_srs_g2_pages : G2.t;
}

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

  module Commitment_proof = Degree_check.Proof

  type slot = bytes

  type scalar = Scalar.t

  type polynomial = Poly.t

  type commitment = Commitment.t

  type shard_proof = Commitment_proof.t

  type commitment_proof = Commitment_proof.t

  type page_proof = Commitment_proof.t

  type page = bytes

  type share = Scalar.t array

  type shard = {index : int; share : share}

  type shards_proofs_precomputation = Kate_amortized.preprocess

  type ('a, 'b) error_container = {given : 'a; expected : 'b}

  module Encoding = struct
    open Data_encoding

    let page_proof_encoding = Commitment_proof.encoding

    let share_encoding = array Scalar.encoding

    let shard_proof_encoding = Commitment_proof.encoding

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

  (* Number of bytes fitting in a Scalar.t. Since scalars are integer modulo
     r~2^255, we restrict ourselves to 248-bit integers (31 bytes). *)
  let scalar_bytes_amount = Scalar.size_in_bytes - 1

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
    domain_polynomial_length : Domain.t;
    (* Domain for the FFT on slots as polynomials to be erasure encoded. *)
    domain_2_times_polynomial_length : Domain.t;
    domain_erasure_encoded_polynomial_length : Domain.t;
    (* Domain for the FFT on erasure encoded slots (as polynomials). *)
    shard_length : int;
    (* Length of a shard in terms of scalar elements. *)
    pages_per_slot : int;
    (* Number of slot pages. *)
    page_length : int;
    page_length_domain : int;
    remaining_bytes : int;
    (* Log of the number of evaluations that constitute an erasure encoded
       polynomial. *)
    srs : srs;
    kate_amortized : Kate_amortized.public_parameters;
  }

  let is_power_of_two = Kzg.Utils.is_power_of_two

  (* The page size is a power of two and thus not a multiple of [scalar_bytes_amount],
     hence the + 1 to account for the remainder of the division. *)
  let page_length ~page_size = Int.div page_size scalar_bytes_amount + 1

  (* [slot_as_polynomial_length ~slot_size ~page_size] returns the length of the
     polynomial of maximal degree representing a slot of size [slot_size] with
     [slot_size / page_size] pages. The returned length thus depends on the number
     of pages. *)
  let slot_as_polynomial_length ~slot_size ~page_size =
    let page_length = page_length ~page_size in
    let page_length_domain, _, _ = FFT.select_fft_domain page_length in
    slot_size / page_size * page_length_domain

  let ensure_validity ~slot_size ~page_size ~redundancy_factor ~number_of_shards
      ~srs_g1_length ~srs_g2_length =
    let open Result_syntax in
    let assert_result condition error_message =
      if not condition then fail (`Fail (error_message ())) else return_unit
    in
    let max_polynomial_length =
      slot_as_polynomial_length ~slot_size ~page_size
    in
    let erasure_encoded_polynomial_length =
      redundancy_factor * max_polynomial_length
    in
    let* () =
      assert_result (number_of_shards > 0) (fun () ->
          Format.asprintf
            "The number of shards must be a strictly positive integer. Given: \
             %d"
            number_of_shards)
    in
    let shard_length = erasure_encoded_polynomial_length / number_of_shards in
    let* () =
      assert_result
        (is_power_of_two slot_size)
        (* According to the specification the length of a slot are in MiB *)
        (fun () ->
          Format.asprintf
            "Slot size is expected to be a power of 2. Given: %d"
            slot_size)
    in
    let* () =
      assert_result
        (is_power_of_two page_size)
        (* According to the specification the lengths of a page are in MiB *)
        (fun () ->
          Format.asprintf
            "Page size is expected to be a power of 2. Given: %d"
            page_size)
    in
    let* () =
      assert_result
        (is_power_of_two redundancy_factor && redundancy_factor >= 2)
        (* The redundancy factor should be a power of 2 so that n is a power of 2
           for proper FFT sizing. The variable [polynomial_length] is assumed to be a power of 2
           as the output of [slot_as_polynomial_length]. *)
          (fun () ->
          Format.asprintf
            "Redundancy factor is expected to be a power of 2 and greater than \
             2. Given: %d"
            redundancy_factor)
    in

    (* At this point [erasure_encoded_polynomial_length] is a power of 2, and [erasure_encoded_polynomial_length > max_polynomial_length]. *)
    let* () =
      assert_result
        (page_size >= 32 && page_size < slot_size)
        (* The size of a page must be greater than 31 bytes (32 > 31 is the next
           power of two), the size in bytes of a scalar element, and strictly less
           than the slot size. *)
          (fun () ->
          Format.asprintf
            "Page size is expected to be greater than '32' and strictly less \
             than the slot_size '%d'. Got: %d"
            slot_size
            page_size)
    in
    let max_two_adicity_log = 32 in
    let two_adicity_log =
      snd Z.(remove (of_int erasure_encoded_polynomial_length) (of_int 2))
    in
    let* () =
      assert_result
        (two_adicity_log <= max_two_adicity_log)
        (* The 2-adicity of [erasure_encoded_polynomial_length] must be at most 2^32,
           the size of the biggest subgroup of 2^i roots of unity in the multiplicative group of Fr,
           because the FFTs operate on such groups. *)
        (fun () ->
          Format.asprintf
            "Slot size (%d) and/or redundancy factor (%d) is/are too high: \
             expected 2-adicity of erasure_encoded_polynomial_length (%d) to \
             be at most 2^%d, got: 2^%d"
            slot_size
            redundancy_factor
            erasure_encoded_polynomial_length
            max_two_adicity_log
            two_adicity_log)
    in
    let* () =
      assert_result
        (erasure_encoded_polynomial_length mod number_of_shards == 0
        && number_of_shards < erasure_encoded_polynomial_length)
        (* The number of shards must divide n, so [number_of_shards <= erasure_encoded_polynomial_length].
           Moreover, the inequality is strict because if [number_of_shards = erasure_encoded_polynomial_length],
           the domains for the FFT contain only one element and we cannot build
           FFT domains with only one element. Given that [erasure_encoded_polynomial_length] is a power of two,
           it follows that the maximum number of shards is [erasure_encoded_polynomial_length/2]. *)
          (fun () ->
          Format.asprintf
            "The number of shards must divide, and not be equal to %d. For the \
             given parameter, the maximum number of shards is %d. Got: %d."
            erasure_encoded_polynomial_length
            (erasure_encoded_polynomial_length / 2)
            number_of_shards)
    in
    let* () =
      assert_result
        (shard_length < max_polynomial_length)
        (* Since shard_length = n / number_of_shards, we obtain
           (all quantities are positive integers):
           shard_length < k
           => n / number_of_shards < k
           => n / k < number_of_shards
           => redundancy_factor < number_of_shards
           since number_of_shards is a power of 2 the minimum value for
           number_of_shards is 2 * redundancy_factor. *)
        (fun () ->
          Format.asprintf
            "For the given parameters, the minimum number of shards is %d. Got \
             %d."
            (redundancy_factor * 2)
            number_of_shards)
    in
    let* () =
      assert_result
        (max_polynomial_length <= srs_g1_length)
        (* The committed polynomials have degree t.max_polynomial_length - 1 at most,
           so t.max_polynomial_length coefficients. *)
        (fun () ->
          Format.asprintf
            "SRS on G1 size is too small. Expected more than %d. Got %d. Hint: \
             you can reduce the size of a slot."
            max_polynomial_length
            srs_g1_length)
    in
    let* () =
      assert_result
        (let shard_length =
           erasure_encoded_polynomial_length / number_of_shards
         in
         let srs_g2_expected_length =
           max max_polynomial_length shard_length + 1
         in
         srs_g2_expected_length <= srs_g2_length)
        (fun () ->
          Format.asprintf
            "SRS on G2 size is too small. Expected more than %d. Got %d. Hint: \
             you can increase the number of shards/number of pages."
            max_polynomial_length
            srs_g2_length)
    in
    let domain_length = 2 * max_polynomial_length / shard_length in
    let* () =
      assert_result
        (domain_length <> 0 && domain_length land (domain_length - 1) = 0)
        (* The computation of shard proofs further require the [domain_length] to
           be a power of two for correct FFT sizing, even though we could relax
           the constraint to a product of primes dividing the order of the group
           G1 thanks to the Prime Factorization Algorithm, as we currently do with
           the FFTs on scalar elements, if the need arises. *)
          (fun () ->
          (* [domain_length = 2 * max_polynomial_length / shard_length
                            = 2 * max_polynomial_length / (redundancy_factor * max_polynomial_length / number_of_shards)
                            = 2 * number_of_shards / redundancy_factor] *)
          Format.asprintf
            "The ratio (2 * number of shards / redundancy factor) must be a \
             power of two. Got 2 * %d / %d = %d"
            number_of_shards
            redundancy_factor
            domain_length)
    in
    assert_result
      (max_polynomial_length mod shard_length = 0)
      (fun () ->
        Format.asprintf
          "The length of a shard must divide %d. Got %d"
          max_polynomial_length
          shard_length)

  type parameters = Dal_config.parameters = {
    redundancy_factor : int;
    page_size : int;
    slot_size : int;
    number_of_shards : int;
  }

  let parameters_encoding = Dal_config.parameters_encoding

  let pages_per_slot {slot_size; page_size; _} = slot_size / page_size

  (* Error cases of this functions are not encapsulated into
     `tzresult` for modularity reasons. *)
  let make
      ({redundancy_factor; slot_size; page_size; number_of_shards} as
      parameters) =
    let open Result_syntax in
    let max_polynomial_length =
      slot_as_polynomial_length ~slot_size ~page_size
    in
    let erasure_encoded_polynomial_length =
      redundancy_factor * max_polynomial_length
    in
    let shard_length = erasure_encoded_polynomial_length / number_of_shards in
    let* raw =
      match !initialisation_parameters with
      | None -> fail (`Fail "Dal_cryptobox.make: DAL was not initialised.")
      | Some srs -> return srs
    in
    let* () =
      ensure_validity
        ~slot_size
        ~page_size
        ~redundancy_factor
        ~number_of_shards
        ~srs_g1_length:(Srs_g1.size raw.srs_g1)
        ~srs_g2_length:(Srs_g2.size raw.srs_g2)
    in
    let page_length = page_length ~page_size in
    let page_length_domain, _, _ = FFT.select_fft_domain page_length in
    let srs =
      {
        raw;
        kate_amortized_srs_g2_shards = Srs_g2.get raw.srs_g2 shard_length;
        kate_amortized_srs_g2_pages = Srs_g2.get raw.srs_g2 page_length_domain;
      }
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
        remaining_bytes = page_size mod scalar_bytes_amount;
        srs;
        kate_amortized =
          {
            max_polynomial_length;
            shard_length;
            srs_g1 = srs.raw.srs_g1;
            number_of_shards;
          };
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
           splitting a page into chunks of [scalar_bytes_amount] bytes and
           a last one of [t.remaining_bytes] bytes.

           Parse the byte sequence slot = page_0 ... page_{t.pages_per_slot-1}
           page by page by chunks of [scalar_bytes_amount], or [t.remaining_bytes]
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
            let dst = Bytes.create scalar_bytes_amount in
            Bytes.blit slot !offset dst 0 scalar_bytes_amount ;
            offset := !offset + scalar_bytes_amount ;
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
        Bytes.blit coeff 0 slot !offset scalar_bytes_amount ;
        offset := !offset + scalar_bytes_amount
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
    try Ok (Commitment.commit t.srs.raw.srs_g1 p)
    with Kzg.Commitment.SRS_too_short _ ->
      Error
        (`Invalid_degree_strictly_less_than_expected
          {given = Poly.degree p; expected = Srs_g1.size t.srs.raw.srs_g1})

  let pp_commit_error fmt
      (`Invalid_degree_strictly_less_than_expected {given; expected}) =
    Format.fprintf
      fmt
      "Invalid degree: expecting input polynomial to commit function to have a \
       degree strictly less than %d. Got %d."
      expected
      given

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
  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4192

     Generalize this function to pass the slot_size in parameter. *)
  let prove_commitment
      ({srs = {raw = {srs_g1; _}; _}; max_polynomial_length; _} : t) p =
    if Srs_g1.size srs_g1 >= max_polynomial_length then
      Ok
        (Degree_check.prove
           ~max_commit:(Srs_g1.size srs_g1 - 1)
           ~max_degree:(max_polynomial_length - 1)
           srs_g1
           p)
    else
      Error
        (`Invalid_degree_strictly_less_than_expected
          {given = max_polynomial_length; expected = Srs_g1.size srs_g1})

  (* Verifies that the degree of the committed polynomial is < t.max_polynomial_length *)
  let verify_commitment (t : t) cm proof =
    let max_allowed_committed_poly_degree = t.max_polynomial_length - 1 in
    let max_committable_degree = Srs_g1.size t.srs.raw.srs_g1 - 1 in
    let offset_monomial_degree =
      max_committable_degree - max_allowed_committed_poly_degree
    in
    let srs_0 = Srs_g2.get t.srs.raw.srs_g2 0 in
    let srs_n_d = Srs_g2.get t.srs.raw.srs_g2 offset_monomial_degree in
    Degree_check.verify {srs_0; srs_n_d} cm proof

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
        let srs_point = t.srs.kate_amortized_srs_g2_shards in
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
                  (* Parse the [page] by chunks of [scalar_bytes_amount] bytes.
                     These chunks are interpreted as [Scalar.t] elements and stored
                     in [evaluations]. *)
                  let dst = Bytes.create scalar_bytes_amount in
                  Bytes.blit
                    page
                    (i * scalar_bytes_amount)
                    dst
                    0
                    scalar_bytes_amount ;
                  Scalar.of_bytes_exn dst
              | i when i = t.page_length - 1 ->
                  (* Store the remaining bytes in the last nonzero coefficient
                     of evaluations. *)
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
        let root = Domain.get t.domain_polynomial_length page_index in
        if
          Kate_amortized.verify
            t.kate_amortized
            ~commitment
            ~srs_point:t.srs.kate_amortized_srs_g2_pages
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
  let parameters_initialisation
      {slot_size; page_size; number_of_shards; redundancy_factor; _} =
    let length = slot_as_polynomial_length ~slot_size ~page_size in
    let secret =
      Scalar.of_string
        "20812168509434597367146703229805575690060615791308155437936410982393987532344"
    in
    let srs_g1 = Srs_g1.generate_insecure length secret in
    (* The error is caught during the instantiation through [make]. *)
    let erasure_encoded_polynomial_length = redundancy_factor * length in
    let evaluations_per_proof =
      match erasure_encoded_polynomial_length / number_of_shards with
      | exception Invalid_argument _ -> 0
      | x -> x
    in
    (* The cryptobox will read at indices `size`, `1 lsl evaluations_per_proof_log`
       and `page_length` so we take the max + 1. Since `page_length < size`, we
       can remove the `page_length from the max. *)
    let srs_g2 =
      Srs_g2.generate_insecure (max length evaluations_per_proof + 1) secret
    in
    {srs_g1; srs_g2}

  let load_parameters parameters = initialisation_parameters := Some parameters

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

  let page_proof_equal = Commitment_proof.equal

  let alter_page_proof (proof : page_proof) = Commitment_proof.alter_proof proof

  let alter_shard_proof (proof : shard_proof) =
    Commitment_proof.alter_proof proof

  let alter_commitment_proof (proof : commitment_proof) =
    Commitment_proof.alter_proof proof

  let minimum_number_of_shards_to_reconstruct_slot (t : t) =
    t.number_of_shards / t.redundancy_factor

  let select_fft_domain = FFT.select_fft_domain

  let precomputation_equal = Kate_amortized.preprocess_equal

  let reset_initialisation_parameters () = initialisation_parameters := None

  let dummy_commitment ~state () = Commitment_proof.random ~state ()

  let dummy_page_proof ~state () = Commitment_proof.random ~state ()

  let dummy_shard_proof ~state () = Commitment_proof.random ~state ()

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

  let srs_size_g1 t = Srs_g1.size t.srs.raw.srs_g1

  let encoded_share_size = encoded_share_size

  let ensure_validity
      {redundancy_factor; slot_size; page_size; number_of_shards} =
    let open Result_syntax in
    (let* raw =
       match !initialisation_parameters with
       | None -> fail (`Fail "Dal_cryptobox.make: DAL was not initialisated.")
       | Some srs -> return srs
     in
     ensure_validity
       ~slot_size
       ~page_size
       ~redundancy_factor
       ~number_of_shards
       ~srs_g1_length:(Srs_g1.size raw.srs_g1)
       ~srs_g2_length:(Srs_g2.size raw.srs_g2))
    |> function
    | Ok _ -> true
    | _ -> false

  let slot_as_polynomial_length = slot_as_polynomial_length
end

module Config = struct
  type t = Dal_config.t = {
    activated : bool;
    use_mock_srs_for_testing : parameters option;
    bootstrap_peers : string list;
  }

  let encoding : t Data_encoding.t = Dal_config.encoding

  let default = Dal_config.default

  let init_dal ~find_srs_files ?(srs_size_log2 = 21) dal_config =
    let open Lwt_result_syntax in
    if dal_config.activated then
      let* initialisation_parameters =
        match dal_config.use_mock_srs_for_testing with
        | Some parameters ->
            return (Internal_for_tests.parameters_initialisation parameters)
        | None ->
            let*? srs_g1_path, srs_g2_path = find_srs_files () in
            initialisation_parameters_from_files
              ~srs_g1_path
              ~srs_g2_path
              ~srs_size_log2
      in
      Lwt.return (load_parameters initialisation_parameters)
    else return_unit
end
