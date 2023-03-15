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
module Srs_g1 = Tezos_bls12_381_polynomial_internal.Srs.Srs_g1
module Srs_g2 = Tezos_bls12_381_polynomial_internal.Srs.Srs_g2

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
  kate_amortized_srs_g2_shards : Bls12_381.G2.t;
  kate_amortized_srs_g2_pages : Bls12_381.G2.t;
}

module Inner = struct
  (* Scalars are elements of the prime field Fr from BLS. *)
  module Scalar = Bls12_381.Fr
  module Polynomials = Tezos_bls12_381_polynomial_internal.Polynomial

  module G1_array = Tezos_bls12_381_polynomial_internal.Ec_carray.G1_carray

  (* Operations on vector of scalars *)
  module Evaluations = Tezos_bls12_381_polynomial_internal.Evaluations

  (* Domains for the Fast Fourier Transform (FTT). *)
  module Domains = Tezos_bls12_381_polynomial_internal.Domain

  type slot = bytes

  type scalar = Scalar.t

  type polynomial = Polynomials.t

  type commitment = Bls12_381.G1.t

  type shard_proof = Bls12_381.G1.t

  type commitment_proof = Bls12_381.G1.t

  type page_proof = Bls12_381.G1.t

  type page = bytes

  type share = Scalar.t array

  type shard = {index : int; share : share}

  type shards_proofs_precomputation = scalar array * shard_proof array array

  type ('a, 'b) error_container = {given : 'a; expected : 'b}

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

    let page_proof_encoding = g1_encoding

    let share_encoding = array fr_encoding

    let shard_encoding =
      conv
        (fun {index; share} -> (index, share))
        (fun (index, share) -> {index; share})
        (tup2 int31 share_encoding)
      [@@coverage off]

    let shards_proofs_precomputation_encoding =
      tup2 (array fr_encoding) (array (array g1_encoding))

    let error_container_encoding (given_encoding : 'a encoding)
        (expected_encoding : 'b encoding) : ('a, 'b) error_container encoding =
      conv
        (fun {given; expected} -> (given, expected))
        (fun (given, expected) -> {given; expected})
        (obj2 (req "given" given_encoding) (req "expected" expected_encoding))
  end

  include Encoding

  module Commitment = struct
    type t = commitment

    type Base58.data += Data of t

    let zero = Bls12_381.G1.zero

    let equal = Bls12_381.G1.eq

    let commitment_to_bytes = Bls12_381.G1.to_compressed_bytes

    let commitment_of_bytes_opt = Bls12_381.G1.of_compressed_bytes_opt
      [@@coverage off]

    let commitment_of_bytes_exn bytes =
      match Bls12_381.G1.of_compressed_bytes_opt bytes with
      | None ->
          Format.kasprintf Stdlib.failwith "Unexpected data (DAL commitment)"
      | Some commitment -> commitment
      [@@coverage off]

    let commitment_size = Bls12_381.G1.compressed_size_in_bytes [@@coverage off]

    let to_string commitment = commitment_to_bytes commitment |> Bytes.to_string
      [@@coverage off]

    let of_string_opt str = commitment_of_bytes_opt (String.to_bytes str)
      [@@coverage off]

    let b58check_encoding =
      Base58.register_encoding
        ~prefix:Base58.Prefix.slot_header
        ~length:commitment_size
        ~to_raw:to_string
        ~of_raw:of_string_opt
        ~wrap:(fun x -> Data x)
      [@@coverage off]

    let raw_encoding =
      let open Data_encoding in
      conv
        commitment_to_bytes
        commitment_of_bytes_exn
        (Fixed.bytes commitment_size)
      [@@coverage off]

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
        [@@coverage off]

      let seeded_hash _ _ =
        (* Same argument. *)
        assert false
        [@@coverage off]
    end)

    let of_b58check = of_b58check
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
      [@@coverage off]

    let size = Bls12_381.G1.compressed_size_in_bytes

    let raw_encoding =
      let open Data_encoding in
      conv to_bytes of_bytes_exn (Fixed.bytes size)

    let encoding = raw_encoding
  end

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
  let make_domain n = Domains.build n

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
    domain_polynomial_length : Domains.t;
    (* Domain for the FFT on slots as polynomials to be erasure encoded. *)
    domain_2_times_polynomial_length : Domains.t;
    domain_erasure_encoded_polynomial_length : Domains.t;
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
  }

  (* The input is expected to be a positive integer. *)
  let is_power_of_two n =
    assert (n >= 0) ;
    n <> 0 && n land (n - 1) = 0

  (* Return the powerset of {3,11,19}. *)
  let combinations_factors =
    let rec powerset = function
      | [] -> [[]]
      | x :: xs ->
          let ps = powerset xs in
          List.concat [ps; List.map (fun ss -> x :: ss) ps]
    in
    powerset [3; 11; 19]

  (* [select_fft_domain domain_size] selects a suitable domain for the FFT.

     The domain size [domain_size] is expected to be strictly positive.
     Return [(size, power_of_two, remainder)] such that:
     * If [domain_size > 1], then [size] is the smallest integer greater or
     equal to [domain_size] and is of the form 2^a * 3^b * 11^c * 19^d,
     where a ∈ ⟦0, 32⟧, b ∈ {0, 1}, c ∈ {0, 1}, d ∈ {0, 1}.
     * If [domain_size = 1], then [size = 2].
     * [size = power_of_two * remainder], [power_of_two] is a power of two,
     and [remainder] is not divisible by 2.

     The function works as follows: each product of elements from
     an element of the powerset of {3,11,19} is multiplied by 2
     until the product is greater than [domain_size]. *)
  let select_fft_domain (domain_size : int) : int * int * int =
    assert (domain_size > 0) ;
    (* {3,11,19} are small prime factors dividing [Scalar.order - 1],
       the order of the multiplicative group Fr\{0}. *)
    let order_multiplicative_group = Z.pred Scalar.order in
    assert (
      List.for_all
        (fun x -> Z.(divisible order_multiplicative_group (of_int x)))
        [3; 11; 19]) ;
    (* This case is needed because the code in the else clause will return
       (1, 1, 1) and 1 is not a valid domain size. *)
    if domain_size = 1 then (2, 2, 1)
    else
      (* [domain_from_factors] computes the power of two to be used in the
         decomposition N = 2^k * factors >= domain_size where [factors] is an
         element of [combinations_factors]. *)
      let domain_from_factors (factors : int list) : int * int list =
        let prod_factors = List.fold_left ( * ) 1 factors in
        let rec get_next_power_of_two k =
          if prod_factors lsl k >= domain_size then 1 lsl k
          else get_next_power_of_two (k + 1)
        in
        let next_power_of_two = get_next_power_of_two 0 in
        let size = prod_factors * next_power_of_two in
        (size, next_power_of_two :: factors)
      in
      let candidate_domains =
        List.map domain_from_factors combinations_factors
      in
      (* The list contains at least an element: the next power of 2 of domain_size *)
      let domain_length, prime_factor_decomposition =
        List.fold_left
          min
          (List.hd candidate_domains)
          (List.tl candidate_domains)
      in
      let power_of_two = List.hd prime_factor_decomposition in
      let remainder_product =
        List.fold_left ( * ) 1 (List.tl prime_factor_decomposition)
      in
      (domain_length, power_of_two, remainder_product)

  let fft_aux ~dft ~fft ~fft_pfa size coefficients =
    (* domain_length = power_of_two * remainder_product *)
    let _domain_length, power_of_two, remainder_product =
      select_fft_domain size
    in
    if size = power_of_two || size = remainder_product then
      let domain = Domains.build size in
      (if is_power_of_two size then fft else dft) domain coefficients
    else
      let domain1 = Domains.build power_of_two in
      let domain2 = Domains.build remainder_product in
      fft_pfa ~domain1 ~domain2 coefficients

  let fft =
    fft_aux
      ~dft:Evaluations.dft
      ~fft:Evaluations.evaluation_fft
      ~fft_pfa:Evaluations.evaluation_fft_prime_factor_algorithm

  let ifft_inplace =
    fft_aux
      ~dft:Evaluations.idft
      ~fft:Evaluations.interpolation_fft
      ~fft_pfa:Evaluations.interpolation_fft_prime_factor_algorithm

  (* The page size is a power of two and thus not a multiple of [scalar_bytes_amount],
     hence the + 1 to account for the remainder of the division. *)
  let page_length ~page_size = Int.div page_size scalar_bytes_amount + 1

  (* [slot_as_polynomial_length ~slot_size ~page_size] returns the length of the
     polynomial of maximal degree representing a slot of size [slot_size] with
     [slot_size / page_size] pages. The returned length thus depends on the number
     of pages. *)
  let slot_as_polynomial_length ~slot_size ~page_size =
    let page_length = page_length ~page_size in
    let page_length_domain, _, _ = select_fft_domain page_length in
    slot_size / page_size * page_length_domain

  let ensure_validity ~slot_size ~page_size ~erasure_encoded_polynomial_length
      ~max_polynomial_length ~redundancy_factor ~number_of_shards ~shard_length
      ~srs_g1_length ~srs_g2_length =
    let open Result_syntax in
    let assert_result condition error_message =
      if not condition then fail (`Fail (error_message ())) else return_unit
    in
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
            "SRS on G1 size is too small. Expected more than %d. Got %d"
            max_polynomial_length
            srs_g1_length)
    in
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
          "SRS on G2 size is too small. Expected more than %d. Got %d"
          max_polynomial_length
          srs_g2_length)

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
    [@@coverage off]

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
      | None -> fail (`Fail "Dal_cryptobox.make: DAL was not initialisated.")
      | Some srs -> return srs
    in
    let* () =
      ensure_validity
        ~slot_size
        ~page_size
        ~erasure_encoded_polynomial_length
        ~max_polynomial_length
        ~redundancy_factor
        ~number_of_shards
        ~shard_length
        ~srs_g1_length:(Srs_g1.size raw.srs_g1)
        ~srs_g2_length:(Srs_g2.size raw.srs_g2)
    in
    let page_length = page_length ~page_size in
    let page_length_domain, _, _ = select_fft_domain page_length in
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
      }

  let parameters
      ({redundancy_factor; slot_size; page_size; number_of_shards; _} : t) =
    {redundancy_factor; slot_size; page_size; number_of_shards}
    [@@coverage off]

  let polynomial_degree = Polynomials.degree

  let polynomial_evaluate = Polynomials.evaluate

  (* [polynomials_multiplication d ps] computes the product of the
     polynomials [ps]. The degree of the resulting product must
     be strictly less than the size of the domain [d].

     Runtime is [O(n log n)] where [n = Domains.length d]. *)
  let polynomials_product d ps =
    let evaluations = List.map (fft d) ps in
    ifft_inplace d (Evaluations.mul_c ~evaluations ())

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
        (ifft_inplace
           t.max_polynomial_length
           (Evaluations.of_array (t.max_polynomial_length - 1, coefficients)))

  (* [polynomial_to_slot] is the left-inverse function of
     [polynomial_from_slot]. *)
  let polynomial_to_slot t p =
    (* The last operation of [polynomial_from_slot] is the interpolation,
       so we undo it with an evaluation on the same domain [t.domain_polynomial_length]. *)
    let evaluations = fft t.max_polynomial_length p in
    let slot = Bytes.make t.slot_size '\x00' in
    let offset = ref 0 in
    (* Reverse permutation from [polynomial_from_slot]. *)
    for page = 0 to t.pages_per_slot - 1 do
      for elt = 0 to t.page_length - 2 do
        let idx = (elt * t.pages_per_slot) + page in
        let coeff = Scalar.to_bytes (Evaluations.get evaluations idx) in
        Bytes.blit coeff 0 slot !offset scalar_bytes_amount ;
        offset := !offset + scalar_bytes_amount
      done ;
      let idx = ((t.page_length - 1) * t.pages_per_slot) + page in
      let coeff = Scalar.to_bytes (Evaluations.get evaluations idx) in
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
    Evaluations.to_array (fft t.erasure_encoded_polynomial_length p)

  (* The shards are arranged in cosets to produce batches of KZG proofs
     for the shards efficiently.

     The domain of evaluation <w> is split into cosets:
     <w> = Disjoint union_{i in ⟦0, t.number_of_shards-1⟧} W_i,

     where W_0 = {w^{t.number_of_shards * j}}_{j in ⟦0, t.shard_length-1⟧}
     and W_i = w^i W_0 (|W_0|=t.shard_length). *)
  let shards_from_polynomial t p =
    let codeword = encode t p in
    let rec loop index seq =
      if index = t.number_of_shards then seq
      else
        (* For each shard index [index], a [share] consists of the
           elements from [codeword] of indices w^index W_0.
           A shard consists of its [index] and associated [share]. *)
        let share = Array.init t.shard_length (fun _ -> Scalar.(copy zero)) in
        for j = 0 to t.shard_length - 1 do
          share.(j) <- codeword.((t.number_of_shards * j) + index)
        done ;
        loop (index + 1) (Seq.cons {index; share} seq)
    in
    loop 0 Seq.empty

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
        Polynomials.mul_xn
          acc
          t.shard_length
          (Scalar.negate
             (Domains.get
                t.domain_erasure_encoded_polynomial_length
                (i * t.shard_length)))
      in
      (* [partition_products seq] builds two polynomials whose
         product is A(x) from the input shards [seq]. *)
      let partition_products seq =
        ShardSet.fold
          (fun {index; _} (l, r) -> (mul r index, l))
          seq
          (Polynomials.one, Polynomials.one)
      in
      (* The computation of [p1], [p2] has asymptotic complexity
         [O((t.max_polynomial_length + t.shard_length) * (t.max_polynomial_length / t.shard_length))
         = O(t.max_polynomial_length * t.number_of_shards / t.redundancy_factor)].
         It is the most costly operation of this function. *)
      let p1, p2 = partition_products shards in
      (* A(x) is the product of [p1] and [p2], and has degree [polynomial_length] *)
      assert (
        Polynomials.degree p1 + Polynomials.degree p2 = t.max_polynomial_length) ;

      let a_poly = polynomials_product (2 * t.max_polynomial_length) [p1; p2] in

      assert (Polynomials.degree a_poly = t.max_polynomial_length) ;

      (* 2. Computing formal derivative of A(x). *)
      let a' = Polynomials.derivative a_poly in

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
      let eval_a' = fft t.erasure_encoded_polynomial_length a' in

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
                Domains.get t.domain_erasure_encoded_polynomial_length i
              in
              let tmp = Evaluations.get eval_a' i in
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
        Evaluations.of_array (t.erasure_encoded_polynomial_length - 1, n_poly)
      in
      let n_poly = compute_n t eval_a' shards in

      (* 5. Computing B(x).
         B(x) = P(x) / A(x) = -sum_{i=0}^{k-1} N(w^{-i}) x^i.

         B(x) is thus given by the first k components
         of -n * IFFT_n(N). *)
      let b =
        Polynomials.truncate
          ~len:t.max_polynomial_length
          (ifft_inplace t.erasure_encoded_polynomial_length n_poly)
      in

      Polynomials.mul_by_scalar_inplace
        b
        Scalar.(negate (of_int t.erasure_encoded_polynomial_length))
        b ;

      (* 6. Computing Lagrange interpolation polynomial P(x).
         The product is given by the convolution theorem:
         P = A * B = IFFT_{2k}(FFT_{2k}(A) o FFT_{2k}(B))
         where o is the pairwise product. *)
      let p = polynomials_product (2 * t.max_polynomial_length) [a_poly; b] in
      (* P has degree [<= max_polynomial_length - 1] so [<= max_polynomial_length]
         coefficients. *)
      Ok (Polynomials.truncate ~len:t.max_polynomial_length p)

  let commit t p =
    let degree = Polynomials.degree p in
    let srs_g1_size = Srs_g1.size t.srs.raw.srs_g1 in
    if degree >= srs_g1_size then
      Error
        (`Invalid_degree_strictly_less_than_expected
          {given = degree; expected = srs_g1_size})
    else Ok (Srs_g1.pippenger t.srs.raw.srs_g1 p)

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
  let prove_commitment (t : t) p =
    let max_allowed_committed_poly_degree = t.max_polynomial_length - 1 in
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
    (* proof = commit(p X^offset_monomial_degree), with deg p < t.max_polynomial_length *)
    commit t p_with_offset

  (* Verifies that the degree of the committed polynomial is < t.max_polynomial_length *)
  let verify_commitment (t : t) cm proof =
    let max_allowed_committed_poly_degree = t.max_polynomial_length - 1 in
    let max_committable_degree = Srs_g1.size t.srs.raw.srs_g1 - 1 in
    let offset_monomial_degree =
      max_committable_degree - max_allowed_committed_poly_degree
    in
    let committed_offset_monomial =
      (* This [get] cannot raise since
         [offset_monomial_degree <= t.max_polynomial_length <= Srs_g2.size t.srs.raw.srs_g2]. *)
      Srs_g2.get t.srs.raw.srs_g2 offset_monomial_degree
    in
    let open Bls12_381 in
    (* checking that cm * committed_offset_monomial = proof *)
    Pairing.pairing_check
      [(cm, committed_offset_monomial); (proof, G2.(negate (copy one)))]

  let diff_next_power_of_two x = (1 lsl Z.log2up (Z.of_int x)) - x

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
    let domain = Domains.build domain_length in
    let srs = t.srs.raw.srs_g1 in
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
        G1_array.init domain_length (fun i ->
            if i < quotient then
              Srs_g1.get
                srs
                (t.max_polynomial_length - j - ((i + 1) * t.shard_length))
            else Bls12_381.G1.(copy zero))
      in
      G1_array.evaluation_ecfft ~domain ~points
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
    let domain_length = Domains.length domain in
    let h_j j =
      let remainder = (t.max_polynomial_length - j) mod t.shard_length in
      let quotient = (t.max_polynomial_length - j) / t.shard_length in
      let padding = diff_next_power_of_two quotient in
      (* points = P_{m-j} || 0^{q+2*padding+1} || P_{r+l} P_{r+2l}
                 ... P_{r+(q-1)l=m-j-l} || 0^{2m/l- (2*q+2*padding+1)}
                 where [q = floor ((m-j)/l) = quotient]. *)
      let points =
        Polynomials.init domain_length (fun i ->
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
            then Scalar.(copy zero)
            else Scalar.copy coefficients.(idx))
      in
      (* FFT of step 2. *)
      Evaluations.evaluation_fft domain points
    in

    (* Pairwise product of step 3. *)
    let evaluations = Array.init t.shard_length h_j in
    let h_j = G1_array.mul_arrays ~evaluations ~arrays:sj in
    (* Sum of step 3. *)
    let sum = h_j.(0) in
    for i = 1 to t.shard_length - 1 do
      G1_array.add_arrays_inplace sum h_j.(i)
    done ;

    (* Step 3. Toeplitz matrix-vector multiplication *)
    G1_array.interpolation_ecfft_inplace ~domain ~points:sum ;

    (* Keep first n / l coefficients *)
    let len = Domains.length domain / 2 in
    let points = G1_array.sub sum ~off:0 ~len in
    (* Step 4. *)
    let domain = Domains.build t.number_of_shards in
    G1_array.(to_array (evaluation_ecfft ~domain ~points))

  (* [interpolation_poly root domain evaluations] returns the unique
     polynomial P of degree [< Domains.length domain] verifying
     P(root * domain[i]) = evaluations[i].

     Requires:
     - [(Array.length evaluations = Domains.length domain)] *)
  let interpolation_poly ~root ~domain ~evaluations =
    assert (Array.length evaluations = Domains.length domain) ;
    let size = Domains.length domain in
    let evaluations =
      ifft_inplace size (Evaluations.of_array (size - 1, evaluations))
    in
    (* Computes root_inverse = 1/root. *)
    let root_inverse = Scalar.inverse_exn root in
    (* Computes evaluations[i] = evaluations[i] * root_inverse^i. *)
    snd
      (Polynomials.fold_left_map
         (fun root_pow_inverse coefficient ->
           ( Scalar.mul root_pow_inverse root_inverse,
             Scalar.mul coefficient root_pow_inverse ))
         Scalar.(copy one)
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
    let open Result_syntax in
    (* Compute r_i(x). *)
    let remainder = interpolation_poly ~root ~domain ~evaluations in
    (* Compute [r_i(τ)]_1. *)
    let* commitment_remainder = commit t remainder in
    (* Compute [w^{i * l}]. *)
    let root_pow = Scalar.pow root (Z.of_int (Domains.length domain)) in
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
    Ok
      (Pairing.pairing_check
         [
           (diff_commits, G2.(copy one));
           (proof, commit_srs_point_minus_root_pow);
         ])

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
    let domain, precomputation = preprocess_multiple_multi_reveals t in
    ( Domains.Domain_unsafe.to_array domain,
      Array.map G1_array.to_array precomputation )

  let prove_shards t ~precomputation:(domain, precomp) ~polynomial =
    let setup =
      ( Domains.Domain_unsafe.of_array domain,
        Array.map G1_array.of_array precomp )
    in
    (* Resizing input polynomial [p] to obtain an array of length [t.max_polynomial_length + 1]. *)
    let coefficients =
      Array.init (t.max_polynomial_length + 1) (fun _ -> Scalar.(copy zero))
    in
    let p_length = Polynomials.degree polynomial + 1 in
    let p = Polynomials.to_dense_coefficients polynomial in
    Array.blit p 0 coefficients 0 p_length ;
    multiple_multi_reveals t ~preprocess:setup ~coefficients

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
      let root =
        Domains.get t.domain_erasure_encoded_polynomial_length shard_index
      in
      let domain = Domains.build t.shard_length in
      let srs_point = t.srs.kate_amortized_srs_g2_shards in
      match
        verify t ~commitment ~srs_point ~domain ~root ~evaluations ~proof
      with
      | Ok true -> Ok ()
      | Ok false -> Error `Invalid_shard
      | Error e -> Error e

  let prove_page t p page_index =
    if page_index < 0 || page_index >= t.pages_per_slot then
      Error `Page_index_out_of_range
    else
      let wi = Domains.get t.domain_polynomial_length page_index in
      let quotient, _ =
        Polynomials.division_xn
          p
          t.page_length_domain
          Scalar.(negate (pow wi (Z.of_int t.page_length_domain)))
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
        let domain = Domains.build t.page_length_domain in
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
        let root = Domains.get t.domain_polynomial_length page_index in
        match
          verify
            t
            ~commitment
            ~srs_point:t.srs.kate_amortized_srs_g2_pages
            ~domain
            ~root
            ~evaluations
            ~proof
        with
        | Ok true -> Ok ()
        | Ok false -> Error `Invalid_page
        | Error e -> Error e
end

include Inner
module Verifier = Inner

module Internal_for_tests = struct
  let parameters_initialisation
      {slot_size; page_size; number_of_shards; redundancy_factor; _} =
    let length = slot_as_polynomial_length ~slot_size ~page_size in
    let secret =
      Bls12_381.Fr.of_string
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

  let polynomials_equal = Polynomials.equal

  let page_proof_equal = Bls12_381.G1.eq

  let alter_proof proof = Bls12_381.G1.(add proof one)

  let alter_page_proof (proof : page_proof) = alter_proof proof

  let alter_shard_proof (proof : shard_proof) = alter_proof proof

  let alter_commitment_proof (proof : commitment_proof) = alter_proof proof

  let minimum_number_of_shards_to_reconstruct_slot (t : t) =
    t.number_of_shards / t.redundancy_factor

  let select_fft_domain = select_fft_domain

  let precomputation_equal ((d1, a1) : shards_proofs_precomputation)
      ((d2, a2) : shards_proofs_precomputation) =
    Array.for_all2 Scalar.eq d1 d2
    && Array.for_all2 (Array.for_all2 Bls12_381.G1.eq) a1 a2

  let reset_initialisation_parameters () = initialisation_parameters := None

  let dummy_commitment ~state () = Bls12_381.G1.random ~state ()

  let dummy_page_proof ~state () = Bls12_381.G1.random ~state ()

  let dummy_shard_proof ~state () = Bls12_381.G1.random ~state ()

  let make_dummy_shard ~state ~index ~length =
    {index; share = Array.init length (fun _ -> Scalar.(random ~state ()))}

  let number_of_pages t = t.pages_per_slot

  let shard_length t = t.shard_length

  let dummy_polynomial ~state ~degree =
    let rec nonzero () =
      let res = Bls12_381.Fr.random ~state () in
      if Bls12_381.Fr.is_zero res then nonzero () else res
    in
    Polynomials.init (degree + 1) (fun i ->
        if i = degree then nonzero () else Bls12_381.Fr.random ~state ())

  let srs_size_g1 t = Srs_g1.size t.srs.raw.srs_g1

  let encoded_share_size = encoded_share_size

  let ensure_validity
      {redundancy_factor; slot_size; page_size; number_of_shards} =
    let max_polynomial_length =
      slot_as_polynomial_length ~slot_size ~page_size
    in
    let erasure_encoded_polynomial_length =
      redundancy_factor * max_polynomial_length
    in
    let shard_length = erasure_encoded_polynomial_length / number_of_shards in
    let open Result_syntax in
    (let* raw =
       match !initialisation_parameters with
       | None -> fail (`Fail "Dal_cryptobox.make: DAL was not initialisated.")
       | Some srs -> return srs
     in
     ensure_validity
       ~slot_size
       ~page_size
       ~erasure_encoded_polynomial_length
       ~max_polynomial_length
       ~redundancy_factor
       ~number_of_shards
       ~shard_length
       ~srs_g1_length:(Srs_g1.size raw.srs_g1)
       ~srs_g2_length:(Srs_g2.size raw.srs_g2))
    |> function
    | Ok _ -> true
    | _ -> false
end

module Config = struct
  type t = {activated : bool; use_mock_srs_for_testing : parameters option}

  let parameters_encoding : parameters Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {slot_size; page_size; redundancy_factor; number_of_shards} ->
        (slot_size, page_size, redundancy_factor, number_of_shards))
      (fun (slot_size, page_size, redundancy_factor, number_of_shards) ->
        {slot_size; page_size; redundancy_factor; number_of_shards})
      (obj4
         (req "slot_size" int31)
         (req "page_size" int31)
         (req "redundancy_factor" int31)
         (req "number_of_shards" int31))
    [@@coverage off]

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {activated; use_mock_srs_for_testing} ->
        (activated, use_mock_srs_for_testing))
      (fun (activated, use_mock_srs_for_testing) ->
        {activated; use_mock_srs_for_testing})
      (obj2
         (req "activated" bool)
         (req "use_mock_srs_for_testing" (option parameters_encoding)))
    [@@coverage off]

  let default = {activated = false; use_mock_srs_for_testing = None}

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
