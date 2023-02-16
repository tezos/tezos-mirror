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

  let is_power_of_two n = n <> 0 && n land (n - 1) = 0

  let slot_as_polynomial_length ~slot_size =
    1 lsl Z.(log2up (succ (of_int slot_size / of_int scalar_bytes_amount)))

  let evaluations_per_proof_log ~n ~number_of_shards =
    (* [n / number_of_shard] is an integer if [n] and [number_of_shards]
       were validated by [ensure_validity]. *)
    Z.log2up (Z.of_int (n / number_of_shards))

  (* The page size is a power of two and thus not a multiple of [scalar_bytes_amount],
     hence the + 1 to account for the remainder of the division. *)
  let page_length ~page_size = Int.div page_size scalar_bytes_amount + 1

  let ensure_validity ~slot_size ~page_size ~n ~k ~redundancy_factor
      ~number_of_shards ~shard_size ~srs_g1_length ~srs_g2_length =
    let open Result_syntax in
    if not (is_power_of_two slot_size) then
      (* According to the specification the length of a slot are in MiB *)
      fail
        (`Fail
          (Format.asprintf
             "Slot size is expected to be a power of 2. Given: %d"
             slot_size))
    else if not (is_power_of_two page_size) then
      (* According to the specification the lengths of a page are in MiB *)
      fail
        (`Fail
          (Format.asprintf
             "Page size is expected to be a power of 2. Given: %d"
             page_size))
    else if not (is_power_of_two redundancy_factor && redundancy_factor >= 2)
    then
      (* The redundancy factor should be a power of 2 so that n is a power of 2
          for proper FFT sizing. The variable [k] is assumed to be a power of 2
          as the output of [slot_as_polynomial_length]. *)
      fail
        (`Fail
          (Format.asprintf
             "Redundancy factor is expected to be a power of 2 and greater \
              than 2. Given: %d"
             redundancy_factor))
    else if
      (* At this point [n] is a power of 2, and [n > k]. *)
      not (page_size >= 32 && page_size < slot_size)
    then
      (* The size of a page must be greater than 31 bytes (32 > 31 is the next
         power of two), the size in bytes of a scalar element, and strictly less
         than the slot size. *)
      fail
        (`Fail
          (Format.asprintf
             "Page size is expected to be greater than '32' and strictly less \
              than the slot_size '%d'. Got: %d"
             slot_size
             page_size))
    else if not (Z.(log2 (of_int n)) <= 32) then
      (* n must be at most 2^32, the cardinal of the biggest subgroup of 2^i
         roots of unity in the multiplicative group of Fr, because the FFTs
         operate on such groups. *)
      fail
        (`Fail
          (Format.asprintf
             "Slot size is expected to be less than '%d'. Got: %d"
             (1 lsl 36)
             slot_size))
    else if number_of_shards >= n then
      fail
        (`Fail
          (Format.asprintf
             "For the given parameters, the maximum number of shards is %d. \
              Got: %d."
             (n / 2)
             number_of_shards))
    else if n mod number_of_shards <> 0 then
      fail (`Fail (Format.asprintf "The number of shards must divide %d" n))
    else if 2 * shard_size >= k then
      (* Since shard_size = n / number_of_shards, we obtain
         (all quantities are positive integers):
         2 * shard_size < k
         => 2 (n / number_of_shards) < k
         => 2 * n / k < number_of_shards
         => 2 * redundancy_factor < number_of_shards
         since number_of_shards is a power of 2 the minimum value for
         number_of_shards is 4 * redundancy_factor *)
      fail
        (`Fail
          (Format.asprintf
             "For the given parameters, the minimum number of shards is %d. \
              Got %d."
             (redundancy_factor * 4)
             number_of_shards))
    else if k > srs_g1_length then
      (* the committed polynomials have degree t.k - 1 at most,
         so t.k coefficients. *)
      fail
        (`Fail
          (Format.asprintf
             "SRS on G1 size is too small. Expected more than %d. Got %d"
             k
             srs_g1_length))
    else if
      let evaluations_per_proof_log =
        evaluations_per_proof_log ~n ~number_of_shards
      in
      let srs_g2_expected_length =
        max k (1 lsl evaluations_per_proof_log) + 1
      in
      srs_g2_expected_length > srs_g2_length
    then
      fail
        (`Fail
          (Format.asprintf
             "SRS on G2 size is too small. Expected more than %d. Got %d"
             k
             srs_g2_length))
    else return_unit

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
    let* raw =
      match !initialisation_parameters with
      | None -> fail (`Fail "Dal_cryptobox.make: DAL was not initialisated.")
      | Some srs -> return srs
    in
    let* () =
      ensure_validity
        ~slot_size
        ~page_size
        ~n
        ~k
        ~redundancy_factor
        ~number_of_shards
        ~shard_size
        ~srs_g1_length:(Srs_g1.size raw.srs_g1)
        ~srs_g2_length:(Srs_g2.size raw.srs_g2)
    in
    let evaluations_log = Z.(log2 (of_int n)) in
    let evaluations_per_proof_log =
      evaluations_per_proof_log ~n ~number_of_shards
    in
    let page_length = page_length ~page_size in
    let srs =
      {
        raw;
        kate_amortized_srs_g2_shards =
          Srs_g2.get raw.srs_g2 (1 lsl evaluations_per_proof_log);
        kate_amortized_srs_g2_pages =
          Srs_g2.get raw.srs_g2 (1 lsl Z.(log2up (of_int page_length)));
      }
    in
    return
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

  let parameters
      ({redundancy_factor; slot_size; page_size; number_of_shards; _} : t) =
    {redundancy_factor; slot_size; page_size; number_of_shards}

  let polynomial_degree = Polynomials.degree

  let polynomial_evaluate = Polynomials.evaluate

  let fft_mul d ps =
    let open Evaluations in
    let evaluations = List.map (evaluation_fft d) ps in
    interpolation_fft d (mul_c ~evaluations ())

  (* We encode by pages of [page_size] bytes each. The pages
     are arranged in cosets to produce batched KZG proofs
     [https://www.iacr.org/archive/asiacrypt2010/6477178/6477178.pdf]
     using the technique described in https://eprint.iacr.org/2023/033. *)
  let polynomial_from_bytes' (t : t) slot =
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
      let res = Array.init t.k (fun _ -> Scalar.(copy zero)) in
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

           to obtain the vector of length [k = t.page_length * t.pages_per_slot]
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
            (* Apply the permutation *)
            res.((elt * t.pages_per_slot) + page) <- Scalar.of_bytes_exn dst
        done ;
        let dst = Bytes.create t.remaining_bytes in
        Bytes.blit slot !offset dst 0 t.remaining_bytes ;
        offset := !offset + t.remaining_bytes ;
        (* Apply the permutation *)
        res.(((t.page_length - 1) * t.pages_per_slot) + page) <-
          Scalar.of_bytes_exn dst
      done ;
      Ok res

  let polynomial_from_slot t slot =
    let open Result_syntax in
    let* data = polynomial_from_bytes' t slot in
    (* The resulting vector is then interpolated. Polynomial
       interpolation is a linear bijection (as a ring isomorphism)
       between k-tuples of scalar elements and polynomials of degree < k
       with coefficients in the scalar field.

       Thus [polynomial_from_slot] is an injection from slots to
       polynomials (as composition preserves injectivity). *)
    Ok (Evaluations.interpolation_fft2 t.domain_k data)

  (* [polynomial_to_slot] is the left-inverse function of
     [polynomial_from_slot]. *)
  let polynomial_to_slot t p =
    (* The last operation of [polynomial_from_slot] is the interpolation,
       so we undo it with an evaluation on the same domain [t.domain_k]. *)
    let eval =
      Evaluations.evaluation_fft t.domain_k p |> Evaluations.to_array
    in
    let slot = Bytes.make t.slot_size '0' in
    let offset = ref 0 in
    (* Reverse permutation from [polynomial_from_slot]. *)
    for page = 0 to t.pages_per_slot - 1 do
      for elt = 0 to t.page_length - 2 do
        let idx = (elt * t.pages_per_slot) + page in
        let coeff = Scalar.to_bytes (Array.get eval idx) in
        Bytes.blit coeff 0 slot !offset scalar_bytes_amount ;
        offset := !offset + scalar_bytes_amount
      done ;
      let idx = ((t.page_length - 1) * t.pages_per_slot) + page in
      let coeff = Scalar.to_bytes (Array.get eval idx) in
      Bytes.blit coeff 0 slot !offset t.remaining_bytes ;
      offset := !offset + t.remaining_bytes
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

  module ShardSet = Set.Make (struct
    type t = shard

    let compare a b = Int.compare a.index b.index
  end)

  let encoded_share_size t =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4289
       Improve shard size computation *)
    let share_scalar_len = t.n / t.number_of_shards in
    (share_scalar_len * Scalar.size_in_bytes) + 4

  let polynomial_from_shards t shards =
    let shards =
      (* We always consider the first k codeword vector components,
         the ShardSet allows collecting distinct indices.
         [Seq.take] doesn't raise any exceptions as t.k / t.shard_size
         is (strictly) positive. *)
      Seq.take (t.k / t.shard_size) shards |> ShardSet.of_seq
    in
    (* There should be t.k / t.shard_size distinct shard indices. *)
    if t.k / t.shard_size > ShardSet.cardinal shards then
      Error
        (`Not_enough_shards
          (Printf.sprintf
             "there must be at least %d shards to decode"
             (t.k / t.shard_size)))
    else if
      ShardSet.exists
        (fun {index; _} -> index >= t.number_of_shards || index < 0)
        shards
    then
      Error
        (`Shard_index_out_of_range
          (Printf.sprintf
             "At least one shard index out of range: expected indexes within \
              range [%d, %d]."
             0
             (t.number_of_shards - 1)))
    else if
      ShardSet.exists
        (fun {share; _} -> Array.length share <> t.shard_size)
        shards
    then
      Error
        (`Invalid_shard_length
          (Printf.sprintf
             "At least one shard of invalid length: expected length %d"
             t.shard_size))
    else
      (* 1. Computing A(x) = prod_{i=0}^{k-1} (x - w^{z_i}). *)
      let mul acc i =
        (* The complexity of mul_xn is linear in [t.shard_size]. *)
        Polynomials.mul_xn
          acc
          t.shard_size
          (Scalar.negate (Domains.get t.domain_n (i * t.shard_size)))
      in
      let partition_products seq =
        ShardSet.fold
          (fun {index; _} (l, r) -> (mul r index, l))
          seq
          (Polynomials.one, Polynomials.one)
      in
      let p1, p2 = partition_products shards in
      let a_poly = fft_mul t.domain_2k [p1; p2] in

      (* 2. Computing formal derivative of A(x). *)
      let a' = Polynomials.derivative a_poly in

      (* 3. Computing A'(w^i) = A_i(w^i). *)
      let eval_a' = Evaluations.evaluation_fft t.domain_n a' in

      (* 4. Computing the polynomial N(X) := \sum_{i=0}^{k-1} n_i x_i^{-1} X^{z_i}. *)
      let compute_n t eval_a' shards =
        let w = Domains.get t.domain_n 1 in
        let n_poly = Array.init t.n (fun _ -> Scalar.(copy zero)) in
        ShardSet.iter
          (fun {index; share} ->
            for j = 0 to Array.length share - 1 do
              let c_i = share.(j) in
              let z_i = (t.number_of_shards * j) + index in
              let x_i = Scalar.pow w (Z.of_int z_i) in
              let tmp = Evaluations.get eval_a' z_i in
              Scalar.mul_inplace tmp tmp x_i ;
              (* The call below never fails by construction, so we don't
                 catch exceptions. *)
              Scalar.inverse_exn_inplace tmp tmp ;
              Scalar.mul_inplace tmp tmp c_i ;
              n_poly.(z_i) <- tmp
            done)
          shards ;
        n_poly
      in
      let n_poly = compute_n t eval_a' shards in

      (* 5. Computing B(x). *)
      let b = Evaluations.interpolation_fft2 t.domain_n n_poly in
      let b = Polynomials.copy ~len:t.k b in
      Polynomials.mul_by_scalar_inplace b (Scalar.of_int t.n) b ;

      (* 6. Computing Lagrange interpolation polynomial P(x). *)
      let p = fft_mul t.domain_2k [a_poly; b] in
      let len =
        if Polynomials.is_zero p then 1 else min t.k (Polynomials.degree p + 1)
      in
      let p = Polynomials.copy ~len p in
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
    assert (chunk_len < n) ;
    assert (is_power_of_two degree) ;
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
      points.(0) <- Scalar.copy coefs.(degree - j) ;
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
    let p' = Array.init (t.k + 1) (fun _ -> Scalar.(copy zero)) in
    let p_length = Polynomials.degree p + 1 in
    let p = Polynomials.to_dense_coefficients p in
    Array.blit p 0 p' 0 p_length ;
    multiple_multi_reveals
      ~chunk_len:t.evaluations_per_proof_log
      ~chunk_count:t.proofs_log
      ~degree:t.k
      ~preprocess
      p'

  let verify_shard (t : t) cm {index = shard_index; share = shard_evaluations}
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
      let d_n = Domains.build_power_of_two t.evaluations_log in
      let domain = Domains.build_power_of_two t.evaluations_per_proof_log in
      if
        verify
          t
          cm
          t.srs.kate_amortized_srs_g2_shards
          domain
          (Domains.get d_n shard_index, shard_evaluations)
          proof
      then Ok ()
      else Error `Invalid_shard

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
        if
          verify
            t
            cm
            t.srs.kate_amortized_srs_g2_pages
            domain
            (Domains.get t.domain_k page_index, slot_page_evaluations)
            proof
        then Ok ()
        else Error `Invalid_page
end

include Inner
module Verifier = Inner

module Internal_for_tests = struct
  let parameters_initialisation parameters =
    let length = slot_as_polynomial_length ~slot_size:parameters.slot_size in
    let secret =
      Bls12_381.Fr.of_string
        "20812168509434597367146703229805575690060615791308155437936410982393987532344"
    in
    let srs_g1 = Srs_g1.generate_insecure length secret in
    (* The error is caught during the instantiation through [make]. *)
    let evaluations_per_proof_log =
      match
        evaluations_per_proof_log
          ~n:(parameters.redundancy_factor * length)
          ~number_of_shards:parameters.number_of_shards
      with
      | exception Invalid_argument _ -> 0
      | x -> x
    in
    (* The cryptobox will read at indices `size`, `1 lsl evaluations_per_proof_log`
       and `page_length` so we take the max + 1. Since `page_length < size`, we
       can remove the `page_length from the max. *)
    let srs_g2 =
      Srs_g2.generate_insecure
        (max length (1 lsl evaluations_per_proof_log) + 1)
        secret
    in
    {srs_g1; srs_g2}

  let load_parameters parameters = initialisation_parameters := Some parameters

  let make_dummy_shards t =
    let len_shard = t.n / t.number_of_shards in
    let rec loop index seq =
      if index = t.number_of_shards then seq
      else
        let share = Array.init len_shard (fun _ -> Scalar.(copy zero)) in
        loop (index + 1) (Seq.cons {index; share} seq)
    in
    loop 0 Seq.empty
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

  let default = {activated = false; use_mock_srs_for_testing = None}

  let init_dal ~find_srs_files dal_config =
    let open Lwt_result_syntax in
    if dal_config.activated then
      let* initialisation_parameters =
        match dal_config.use_mock_srs_for_testing with
        | Some parameters ->
            return (Internal_for_tests.parameters_initialisation parameters)
        | None ->
            let*? g1_path, g2_path = find_srs_files () in
            initialisation_parameters_from_files ~g1_path ~g2_path
      in
      Lwt.return (load_parameters initialisation_parameters)
    else return_unit
end
